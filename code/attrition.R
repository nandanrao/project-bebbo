at_stage <- function(dat) {
    started <- dat %>%
        filter(wave == 0) %>%
        nrow()

    l <- list(
        `Started Baseline` = dat %>%
            filter(wave == 0) %>%
            nrow(),
        `Finished Baseline` = dat %>%
            filter(wave == 0) %>%
            filter(!(is.na(thankyou_you_qualify))) %>%
            nrow(),
        `Started Endline` = dat %>%
            filter(wave == 1) %>%
            nrow(),
        `Finished Endline` = dat %>%
            filter(wave == 1) %>%
            filter(!(is.na(thankyou_you_qualify))) %>%
            nrow(),
        `Started Followup` = dat %>%
            filter(wave == 2) %>%
            filter(impacted == FALSE) %>%
            nrow(),
        `Finished Followup` = dat %>%
            filter(wave == 2) %>%
            filter(impacted == FALSE) %>%
            filter(!(is.na(thankyou_you_qualify))) %>%
            nrow()
    )

    values <- as.integer(l)
    names <- names(l)

    data.frame(stage = names, count = values) %>%
        mutate(attrition = 1 - count / lag(count))
}

attrition_table <- function(dat, dataset) {
    df <- at_stage(dat) %>%
        left_join(at_stage(
            dat %>%
                filter(treatment == "treated")
        ) %>%
            select(stage, attrition) %>%
            rename(treated_attrition = attrition)) %>%
        left_join(at_stage(
            dat %>%
                filter(treatment == "control")
        ) %>%
            select(stage, attrition) %>%
            rename(control_attrition = attrition)) %>%
        mutate(attrition_dif = treated_attrition - control_attrition) %>%
        mutate_if(is.numeric, round, 2)

    filename <- glue("Attrition: {dataset}")
    write_table(df, "report/descriptives/tables", filename, table.placement = "H")
}


attrition_table(pooled_all_starts, "Pooled")
attrition_table(pooled_all_starts %>% filter(country == "Serbia"), "Serbia")
attrition_table(pooled_all_starts %>% filter(country == "Bulgaria"), "Bulgaria")


a <- pooled %>%
    summarise(
        min = min(delta_be, na.rm = T),
        quantile_05 = quantile(delta_be, 0.05, na.rm = T),
        median = median(delta_be, na.rm = T),
        quantile_95 = quantile(delta_be, 0.95, na.rm = T),
        max = max(delta_be, na.rm = T)
    ) %>%
    mutate(`Time Gap` = "Baseline - Endline")

b <- pooled_without_impacted %>%
    summarise(
        min = min(delta_ef, na.rm = T),
        quantile_05 = quantile(delta_ef, 0.05, na.rm = T),
        median = median(delta_ef, na.rm = T),
        quantile_95 = quantile(delta_ef, 0.95, na.rm = T),
        max = max(delta_ef, na.rm = T)
    ) %>%
    mutate(`Time Gap` = "Endline - Followup")

gap_metrics <- rbind(a, b) %>% mutate_if(is.difftime, round, 0)

write_table(gap_metrics, "report/descriptives/tables", "Time Gap Descriptives")
