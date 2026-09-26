#!/usr/bin/env python3
"""Block respondent data from entering a git repo.

Our repos are pushed to GitHub, which is outside our protected Google
Workspace, so respondent data must never be committed; see
projects/README.md and projects/repo-template/README.md in the team Drive.

    pii_scan.py             staged changes (the pre-commit hook)
    pii_scan.py --all       every file git tracks or would add (new repos, CI)

Flags:
- data files (CSV, spreadsheets, Stata/SPSS, JSONL, databases) and anything
  under a data/ directory, even when force-added past .gitignore;
- phone numbers with a country code we work in, and Kenyan local numbers;
- 15-17 digit numbers: Messenger PSIDs and Fly userids have this shape;
- email addresses outside our and our clients' domains.

A line containing `pii-ok` is skipped: a test number, or a one-off Meta
page, ad account or ad ID (same shape as a PSID).

.githooks/pii-allow, one entry per line:
- a path glob: a data file someone has checked holds no respondent-level rows,
  such as an aggregate results table. Its lines are still scanned.
- a bare number: an ID known not to be a respondent's, such as a Meta page,
  business or ad account ID, allowed wherever it appears.
"""
import fnmatch
import re
import subprocess
import sys
from pathlib import PurePosixPath

DATA_SUFFIXES = {
    ".csv", ".tsv", ".xlsx", ".xls", ".sav", ".dta", ".rds", ".rdata",
    ".parquet", ".feather", ".jsonl", ".ndjson", ".db", ".sqlite", ".sqlite3",
}

# Country calling codes of the countries our studies have run in.
COUNTRY_CODES = (
    "254|255|256|250|251|234|233|260|265|258|27|"   # East, West, Southern Africa
    "91|92|880|62|63|971|966|20|"                    # Asia, Middle East, Egypt
    "52|57|51|55|54|56|593|502|503|504|505|506|"    # Americas
    r"\+1"  # needs its +, or every Unix timestamp would match
)
PATTERNS = {
    "phone number": re.compile(
        rf"(?<![\w.])\+?(?:{COUNTRY_CODES})[ -]?\d(?:[ -]?\d){{7,10}}(?![\d.])"
    ),
    "Kenyan phone number": re.compile(r"(?<![\w.])0[17]\d{8}(?![\d.])"),
    "PSID/userid-shaped number": re.compile(r"(?<![\w.])\d{15,17}(?![\d.])"),
    "email address": re.compile(r"[\w.+-]+@((?:[\w-]+\.)+[A-Za-z]{2,})\b"),
}
OK_EMAIL_DOMAINS = (
    "vlab.digital", "worldbank.org", "unicef.org", "girleffect.org",
    "example.com", "example.org", "users.noreply.github.com", "anthropic.com",
)
ISO_DATE = re.compile(r"\d{4}-\d{2}-\d{2}")
SKIP_MARKER = "pii-ok"
ALLOW_FILE = ".githooks/pii-allow"
# Generated from package metadata: full of maintainers' emails.
UNSCANNED = {"renv.lock"}


def git(*args):
    return subprocess.run(["git", *args], capture_output=True, text=True,
                          check=True).stdout


def load_allowed():
    """Return (path globs, allowed numbers) from ALLOW_FILE."""
    try:
        with open(ALLOW_FILE) as f:
            entries = [l.split("#")[0].strip() for l in f]
    except FileNotFoundError:
        return [], set()
    entries = [e for e in entries if e]
    numbers = {e for e in entries if e.isdigit()}
    return [e for e in entries if e not in numbers], numbers


def is_data_path(path):
    p = PurePosixPath(path)
    return p.suffix.lower() in DATA_SUFFIXES or "data" in p.parts[:-1]


def email_ok(domain):
    domain = domain.lower()
    return any(domain == d or domain.endswith("." + d) for d in OK_EMAIL_DOMAINS)


def scan_line(line, allowed_numbers=frozenset()):
    if SKIP_MARKER in line:
        return []
    hits = []
    for label, pattern in PATTERNS.items():
        for m in pattern.finditer(line):
            if label == "email address" and email_ok(m.group(1)):
                continue
            if ISO_DATE.match(m.group(0)):
                continue
            if re.sub(r"\D", "", m.group(0)) in allowed_numbers:
                continue
            hits.append((label, m.group(0)))
    return hits


def staged_lines():
    """Yield (path, line number, text) for every line added in the index."""
    diff = git("diff", "--cached", "-U0", "--no-color", "--no-ext-diff",
               "--diff-filter=ACMR")
    path, lineno = None, 0
    for raw in diff.splitlines():
        if raw.startswith("+++ "):
            path = raw[6:] if raw.startswith("+++ b/") else None
        elif raw.startswith("@@"):
            lineno = int(re.search(r"\+(\d+)", raw).group(1))
        elif raw.startswith("+") and path:
            yield path, lineno, raw[1:]
            lineno += 1


def all_lines(paths):
    for path in paths:
        try:
            with open(path, encoding="utf-8") as f:
                for i, line in enumerate(f, 1):
                    yield path, i, line
        except (UnicodeDecodeError, FileNotFoundError, IsADirectoryError):
            continue


def mask(value):
    return value[:3] + "*" * max(len(value) - 5, 1) + value[-2:]


def main():
    if "--all" in sys.argv[1:]:
        paths = git("ls-files", "--cached", "--others", "--exclude-standard",
                    "-z").split("\0")
        lines = all_lines(p for p in paths if p)
    else:
        paths = git("diff", "--cached", "--name-only", "--diff-filter=ACMR",
                    "-z").split("\0")
        lines = staged_lines()
    paths = [p for p in paths if p]

    globs, numbers = load_allowed()

    def reviewed(path):
        return any(fnmatch.fnmatch(path, g) for g in globs)

    problems = [f"{p}: data file" for p in paths
                if is_data_path(p) and not reviewed(p)]
    for path, lineno, text in lines:
        if PurePosixPath(path).name in UNSCANNED:
            continue
        if is_data_path(path) and not reviewed(path):
            continue
        for label, value in scan_line(text, numbers):
            problems.append(f"{path}:{lineno}: {label} {mask(value)}")

    if problems:
        print("pii_scan: possible respondent data. Remove it, or mark a line "
              f"that is safe with `{SKIP_MARKER}`:", file=sys.stderr)
        for p in problems:
            print("  " + p, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
