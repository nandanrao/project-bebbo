library(readr)
library(dplyr)
library(cobalt)
library(glue)
library(stargazer)
library(ggplot2)

binarize_col <- function(df, col, targ, new_col = NULL) {
    if (is.null(new_col)) {
        new_col <- col
    }
    c <- df[[col]]

    s <- as.integer(dplyr::case_when(
        is.na(c) ~ NA,
        c %in% targ ~ TRUE,
        TRUE ~ FALSE
    ))

    dplyr::mutate(df, "{new_col}" := s)
}


bin_conf <- function(col, target, new_col = NULL) {
    list(col = col, target = target, new_col = new_col)
}

likert_conf <- function(col, target, answers) {
    list(col = col, target = target, answers = answers)
}

binarize <- function(df, cols) {
    for (c in cols) {
        if (c$col %in% colnames(df)) {
            df <- binarize_col(df, c$col, c$target, c$new_col)
        }
    }
    df
}

write_table <- function(x, folder, filename, ...) {
    stargazer(
        as.matrix(x),
        out = glue("{folder}/{filename}.tex"),
        digits = 2,
        title = filename,
        label = glue("tbl:{filename}"),
        ...
    )

    stargazer(
        as.matrix(x),
        type = "html",
        out = glue("{folder}/{filename}.html"),
        digits = 2,
        title = filename,
        label = glue("tbl:{filename}"),
        ...
    )
}


write_regressions <- function(models, folder, filename, ...) {
    stargazer(
        models,
        out = glue("{folder}/{filename}.tex"),
        digits = 2,
        title = filename,
        label = glue("tbl:{filename}"),
        ...
    )

    stargazer(
        models,
        type = "html",
        out = glue("{folder}/{filename}.html"),
        digits = 2,
        title = filename,
        label = glue("tbl:{filename}"),
        ...
    )
}

tag_impacted <- function(df, code, versions) {
    users <- df %>%
        filter(shortcode == code) %>%
        filter(version %in% versions) %>%
        distinct(userid) %>%
        pull(userid)

    df %>% mutate(impacted = userid %in% users)
}


pick_serbia_resps <- function(df) {
    df %>%
        filter(shortcode != "bebborsintermediatebail") %>%
        tag_impacted("bebborsendeng", c(1, 3)) %>%
        mutate(country = "Serbia")
}

pick_bulgaria_resps <- function(df) {
    df %>%
        tag_impacted("bebbobgendeng", c(6, 12, 13)) %>%
        mutate(country = "Bulgaria")
}

ind_treatment_control <- function(df, treated) {
    df %>%
        mutate(seed_2 = seed %% 2 + 1) %>% # create seed_2
        mutate(treatment = case_when(
            seed_2 == treated ~ "treated",
            seed_2 != treated ~ "control"
        ))
}

ind_endline <- function(df, country) {
    if (country == "serbia") {
        df %>% mutate(
            wave = recode(shortcode,
                bebborsendeng = 1,
                bebborsbaseserb = 0,
                bebborsfueng = 2
            )
        )
    } else if (country == "bulgaria") {
        df %>% mutate(
            wave = recode(shortcode,
                bebbobgendeng = 1,
                bebbobg2basebul = 0,
                bebbobgfueng = 2
            )
        )
    }
}

parse_mult_choice <- function(x) {
    temp <- match(unlist(strsplit(x, split = ", ")), c("A", "B", "C", "D", "E"))
    c("A", "B", "C", "D", "E")[temp]
}

likert_col <- function(df, col, targ, ans) {
    # new_col <- paste0(col,'_likert')
    new_col <- col

    a_pattern <- "-A. ([A-Za-z ]+)\n"
    b_pattern <- "-B. ([A-Za-z ]+)\n"
    c_pattern <- "-C. ([A-Za-z ]+)\n"
    d_pattern <- "-D. ([A-Za-z ]+)"
    e_pattern <- "-E. ([A-Za-z ]+)"

    a_matched <- str_extract(ans, pattern = a_pattern, group = 1)
    b_matched <- str_extract(ans, pattern = b_pattern, group = 1)
    c_matched <- str_extract(ans, pattern = c_pattern, group = 1)
    d_matched <- str_extract(ans, pattern = d_pattern, group = 1)
    e_matched <- str_extract(ans, pattern = e_pattern, group = 1)

    targ <- parse_mult_choice(targ)[1]

    likert_asc <- recode(df[[col]], "{a_matched}" := 1, "{b_matched}" := 2, "{c_matched}" := 3, "{d_matched}" := 4)
    likert_desc <- recode(df[[col]], "{a_matched}" := 4, "{b_matched}" := 3, "{c_matched}" := 2, "{d_matched}" := 1)

    df %>% mutate("{new_col}" := case_when(targ %in% c("C", "D", "E") ~ likert_asc, targ %in% c("A", "B") ~ likert_desc))
}

likert <- function(df, cols) {
    for (c in cols) {
        if (c$col %in% colnames(df)) {
            df <- likert_col(df, c$col, c$target, c$answers)
        }
    }
    df
}

descriptives <- function(df, cols, type = "binary") {
    if (type == "binary") {
        df %>%
            ungroup() %>%
            pivot_longer(everything()) %>%
            group_by(name) %>%
            summarise(
                mean = mean(value, na.rm = TRUE),
                sd = sd(value, na.rm = TRUE),
                prop_na = mean(is.na(value)),
                prop_0 = sum(case_when(value == 0 ~ 1, value == 1 ~ 0, TRUE ~ NA), na.rm = TRUE) / length(value),
                prop_1 = sum(case_when(value == 1 ~ 1, value == 0 ~ 0, TRUE ~ NA), na.rm = TRUE) / length(value)
            ) %>%
            arrange(desc(mean)) %>%
            mutate_if(is.numeric, round, 2)
    } else if (type == "likert") {
        df %>%
            ungroup() %>%
            pivot_longer(everything()) %>%
            group_by(name) %>%
            summarise(
                mean = mean(value, na.rm = TRUE),
                sd = sd(value, na.rm = TRUE),
                prop_na = mean(is.na(value)),
                prop_1 = sum(case_when(value == 1 ~ 1, is.na(value) ~ NA, TRUE ~ 0), na.rm = TRUE) / length(value),
                prop_2 = sum(case_when(value == 2 ~ 1, is.na(value) ~ NA, TRUE ~ 0), na.rm = TRUE) / length(value),
                prop_3 = sum(case_when(value == 3 ~ 1, is.na(value) ~ NA, TRUE ~ 0), na.rm = TRUE) / length(value),
                prop_4 = sum(case_when(value == 4 ~ 1, is.na(value) ~ NA, TRUE ~ 0), na.rm = TRUE) / length(value)
            ) %>%
            arrange(desc(mean)) %>%
            mutate_if(is.numeric, round, 2)
    } else if (type == "summary") {
        df %>%
            ungroup() %>%
            pivot_longer(everything()) %>%
            group_by(name) %>%
            summarise(
                mean = mean(value, na.rm = TRUE),
                median = median(value, na.rm = TRUE),
                min = min(value, na.rm = TRUE),
                max = max(value, na.rm = TRUE),
                sd = sd(value, na.rm = TRUE),
                prop_max = mean(value[!is.na(value)] == max(value, na.rm = TRUE)),
                prop_na = mean(is.na(value))
            ) %>%
            arrange(desc(mean)) %>%
            mutate_if(is.numeric, round, 2)
    }
}

parse_bq_date <- function(i) {
    as.POSIXct(i / 1000 / 1000)
}

flip_likert <- function(df, col) {
    c <- df[[col]]

    df %>%
        mutate(
            "{col}" := dplyr::case_when(
                c == 4 ~ 1,
                c == 3 ~ 2,
                c == 2 ~ 3,
                c == 1 ~ 4
            )
        )
}

flip_likerts <- function(df, cols) {
    for (c in cols) {
        df <- flip_likert(df, c)
    }
    df
}

prettify <- function(df, col) {
    dplyr::mutate(df, "{col}" := sapply(.data[[col]], function(n) pretty_vars[[n]]))
}
