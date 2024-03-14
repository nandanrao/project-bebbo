folder <- "report/balance"
outcomes <- construct_cols
fmla <- reformulate(outcomes, "treatment")
dat <- pooled

endlined <- pooled %>%
    filter(wave == 1) %>%
    pull(userid)

for (co in c("Serbia", "Bulgaria")) {
    dd <- dat %>%
        filter(country == co)

    bt <- balanceTest(fmla, dd %>% filter(wave == 0))
    res <- data.frame(bt$results) %>%
        select("Control...", "Treatment...", "std.diff...", "z...") %>%
        rename(
            control_mean = "Control...",
            treatment_mean = "Treatment...",
            standardized_diff = "std.diff...",
            z_score = "z..."
        )

    overall_p <- round(bt$overall$p.value, 3)

    write_table(
        res,
        folder,
        glue("Baseline Balance {co}"),
        notes = glue("Overall P-Value: {overall_p}")
    )
}
