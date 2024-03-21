binary_outcomes <- c("health_knw", "was_breastfed", "attitude")

standard_dev <- function(dat, outcome) {
    ## if (outcome %in% binary_outcomes) {
    ##     m <- mean(dat[[outcome]])
    ##     n <- nrow(dat)
    ##     res <- sqrt((m * (1 - m)) / n)
    ##     return(res)
    ## }

    sd(dat[[outcome]])
}

normalize <- function(dat, outcome) {
    base <- dat %>%
        filter(wave == 0) %>%
        filter(treatment == "control")
    m <- mean(base[[outcome]], na.rm = TRUE)
    sd <- sd(base[[outcome]], na.rm = TRUE)

    ## if (outcome %in% binary_outcomes) {
    ##     sd <- 1
    ## }

    dat %>% mutate(
        "{outcome}" := (.data[[outcome]] - m) / sd
    )
}


plot_pre_post <- function(dat, construct_cols, dataset) {
    takeup <- dat %>%
        filter(wave == 1) %>%
        mutate(
            takeup = case_when(
                treatment == "control" ~ "control",
                treatment == "treated" & days_learned > 3 ~ "treated-high-takeup",
                TRUE ~ "treated-low-takeup"
            )
        ) %>%
        select(userid, takeup)


    endline_users <- dat %>%
        filter(wave == 1) %>%
        distinct(userid) %>%
        pull(userid)

    dat <- dat %>%
        filter(userid %in% endline_users) %>%
        inner_join(takeup, by = "userid")

    dat <- dat %>% filter(wave != 2)

    out <- tibble()

    for (outcome in construct_cols) {
        dat <- dat %>% normalize(outcome)

        tmp <- dat %>%
            filter(!is.na(across(outcome))) %>%
            group_by(takeup, wave) %>%
            summarise(
                outcome = outcome,
                m = mean(.data[[outcome]]),
                var = standard_dev(cur_data(), outcome)**2 / n()
            ) %>%
            mutate(sd = sqrt(var)) %>%
            mutate(upper = m + sd * 1.96, lower = m - sd * 1.96) %>%
            ungroup()

        out <- rbind(out, tmp)
    }

    out <- prettify(out, "outcome")
    pretty_constructs <- as.character(sapply(construct_cols, function(n) pretty_vars[[n]]))
    splits <- split(pretty_constructs, ceiling(seq_along(pretty_constructs) / 2))

    for (outcomes_ in splits) {
        d <- out %>% filter(outcome %in% outcomes_)

        ggplot(d, aes(x = wave, y = m, color = takeup)) +
            geom_point() +
            geom_errorbar(aes(ymin = lower, ymax = upper)) +
            facet_grid(rows = . ~ outcome + takeup, scale = "free_y") +
            ylab("Mean Outcome") +
            xlab("Survey Wave") +
            theme(
                legend.position = "none",
                axis.text.x = element_blank()
            )

        ggsave(glue("report/plots/pre_post/{dataset}: {outcomes_}.png"), width = 10, height = 4)
    }
}


datasets <- list(
    `Serbia` = serbia_without_impacted,
    `Pooled` = pooled
)

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]
    plot_pre_post(dat, construct_cols, dataset)
}
