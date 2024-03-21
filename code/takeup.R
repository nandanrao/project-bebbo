datasets <- list(
    `Serbia` = serbia,
    `Bulgaria` = bulgaria,
    `Pooled` = pooled
)

##########################################
# Takeup Table
#########################################

out <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    dat <- dat %>%
        filter(wave == 1) %>%
        filter(treatment == "treated")

    d <- dat %>%
        summarise(
            treated = length(userid),
            has_downloaded = length(userid[has_any_bebbo_event]),
            has_learned = length(userid[has_learning_event]),
            learned_more_than_one_day = length(userid[!is.na(days_learned) & days_learned > 1]),
            learned_more_than_three_days = length(userid[!is.na(days_learned) & days_learned > 3])
        ) %>%
        mutate(
            dataset = dataset,
            has_learned_perc = glue("{round(has_learned / treated, 3) * 100}%"),
            more_than_one_day_perc = glue("{round(learned_more_than_one_day / treated, 3) * 100}%"),
            more_than_three_days_perc = glue("{round(learned_more_than_three_days / treated, 3) * 100}%"),
        ) %>%
        rename(
            `Dataset` = dataset,
            `Treated` = treated,
            `Downloaded` = has_downloaded,
            `Used` = has_learned,
            `Used (%)` = has_learned_perc,
            `> 1 Day` = learned_more_than_one_day,
            `> 1 Day (%)` = more_than_one_day_perc,
            `> 3 Days` = learned_more_than_three_days,
            `> 3 Days (%)` = more_than_three_days_perc
        ) %>%
        relocate(Dataset, .before = Treated)


    out <- rbind(out, d)
}

write_table(out, "report/descriptives/tables", "Treatment Takeup", table.placement = "H")


##########################################
# APP RETENTION FUNNEL
#########################################


create_funnel <- function(dat, dataset) {
    dat %>%
        summarise(
            has_downloaded = length(userid[has_any_bebbo_event]),
            has_learned = length(userid[has_learning_event]),
            learned_more_than_one_day = length(userid[!is.na(days_learned) & days_learned > 1]),
            learned_more_than_three_days = length(userid[!is.na(days_learned) & days_learned > 3])
        ) %>%
        mutate(
            dataset = dataset,
            has_learned_perc = glue("{round(has_learned / has_downloaded, 3) * 100}%"),
            more_than_one_day_perc = glue("{round(learned_more_than_one_day / has_downloaded, 3) * 100}%"),
            more_than_three_days_perc = glue("{round(learned_more_than_three_days / has_downloaded, 3) * 100}%"),
        ) %>%
        rename(
            `Dataset` = dataset,
            `Downloaded` = has_downloaded,
            `Used (%)` = has_learned_perc,
            `> 1 Day (%)` = more_than_one_day_perc,
            `> 3 Days (%)` = more_than_three_days_perc
        ) %>%
        relocate(Dataset, .before = Downloaded) %>%
        select(-has_learned, -learned_more_than_one_day, -learned_more_than_three_days)
}

non_study <- all_app %>%
    filter(event_timestamp < ymd(20230901)) %>%
    filter(event_name == "first_open") %>%
    mutate(first_open = date(event_timestamp)) %>%
    select(userid, first_open) %>%
    inner_join(all_app)

window <- non_study %>%
    mutate(days_after = event_day - first_open) %>%
    filter(days_after < 30)

non_study <- non_study %>%
    distinct(userid) %>%
    left_join(rollup_events(window, c())) %>%
    mutate(has_any_bebbo_event = TRUE) %>%
    replace_na(list(has_learning_event = FALSE)) %>%
    mutate(wave = 1, treatment = "treated")


datasets <- list(
    `Serbia` = serbia,
    `Bulgaria` = bulgaria,
    `Pooled` = pooled,
    `Non-Study` = non_study
)

out <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    dat <- dat %>%
        filter(wave == 1) %>%
        filter(treatment == "treated")

    d <- create_funnel(dat, dataset)
    out <- rbind(out, d)
}

write_table(out, "report/descriptives/tables", "App Engagement (30 days)", table.placement = "H")

##########################################
# Days with Learning Events Histogram
#########################################

pooled %>%
    filter(wave == 1) %>%
    filter(treatment == "treated") %>%
    select(userid, country, days_learned) %>%
    ggplot(aes(x = days_learned)) +
    stat_bin(binwidth = 1, fill = "steelblue") +
    stat_bin(binwidth = 1, geom = "text", aes(label = ..count..), vjust = -0.5) +
    theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
    )

ggsave(glue("report/plots/Treatment Takeup Baseline - Endline.png"), width = 10, height = 5)


##########################################
# Takeup Regression
#########################################

takeup_regression <- function(dat, covariates, outcome) {
    fmla <- reformulate(covariates, outcome)
    res <- lm(fmla, data = dat)
    res
}

outcomes <- list(
    `Used the App` = "has_learning_event",
    `Used More Than 1 Day` = "more_than_one_day_learned",
    `Used More Than 3 Days` = "more_than_three_days_learned"
)


dat <- pooled %>%
    filter(wave == 1) %>%
    filter(treatment == "treated") %>%
    mutate(more_than_one_day_learned = days_learned > 1) %>%
    mutate(more_than_three_days_learned = days_learned > 3) %>%
    replace_na(list(has_learning_event = FALSE, more_than_one_day_learned = FALSE, more_than_three_days_learned = FALSE))


datasets <- list(
    `All` = dat,
    `With Children 0-2` = dat %>% filter(age_flag == "0-2")
)

for (dataset in names(datasets)) {
    d <- datasets[[dataset]]

    covariates <- c(control_cols, construct_cols)

    if (dataset == "All") {
        covariates <- covariates[!(covariates %in% c("was_breastfed", "health_knw"))]
    }

    if (dataset == "With Children 0-2") {
        covariates <- covariates[!(covariates %in% c("age_flag"))]
    }

    res <- list()

    for (outcome in names(outcomes)) {
        var <- outcomes[[outcome]]
        res[[outcome]] <- takeup_regression(d, covariates, var)
    }

    covariate_labels <- sapply(covariates, function(n) pretty_vars[[n]])

    dep_vars <- names(outcomes)

    write_regressions(res, "report/regressions/", glue("App Usage ({dataset})"), dep.var.labels = dep_vars, covariate.labels = covariate_labels, single.row = TRUE, align = TRUE, table.placement = "H")
}




####################################
# 30/60/90 day retention
####################################


user_retention_funnel <- function(dat) {
    has_downloaded <- window %>%
        distinct(userid) %>%
        count() %>%
        pull(n)

    dat %>%
        filter(event_name %in% learning_events) %>%
        group_by(userid) %>%
        summarise(
            learned = TRUE,
            after_1 = (sum(days_after > 1)) > 0,
            after_30 = (sum(days_after > 30)) > 0,
            after_60 = (sum(days_after > 60)) > 0,
            after_90 = (sum(days_after > 90)) > 0
        ) %>%
        select(-userid) %>%
        summarise_all(sum) %>%
        mutate(
            has_downloaded = has_downloaded,
            learned_perc = glue("{round(learned / has_downloaded, 3) * 100}%"),
            after_1_perc = glue("{round(after_1 / has_downloaded, 3) * 100}%"),
            after_30_perc = glue("{round(after_30 / has_downloaded, 3) * 100}%"),
            after_60_perc = glue("{round(after_60 / has_downloaded, 3) * 100}%"),
            after_90_perc = glue("{round(after_90 / has_downloaded, 3) * 100}%"),
        ) %>%
        rename(
            `Downloaded` = has_downloaded,
            `Used (%)` = learned_perc,
            `After 1 day (%)` = after_1_perc,
            `After 30 days (%)` = after_30_perc,
            `After 60 days (%)` = after_60_perc,
            `After 90 days (%)` = after_90_perc,
        ) %>%
        select(-learned, -after_1, -after_30, -after_60, -after_90)
}

non_study <- all_app %>%
    filter(event_name == "first_open") %>%
    filter(event_timestamp < ymd(20230901)) %>%
    mutate(first_open = date(event_timestamp)) %>%
    select(userid, first_open) %>%
    inner_join(all_app) %>%
    mutate(days_after = event_day - first_open)

out <- user_retention_funnel(non_study)

write_table(out, "report/descriptives/tables", "App Retention Funnel (Non Study)", table.placement = "H")
