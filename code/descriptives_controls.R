#############################################################
# Number of Respondents
#############################################################

create_respondent_counts <- function(dat, col) {
    print(col)
    dat %>%
        group_by(country, .data[[col]]) %>%
        count() %>%
        pivot_wider(id_cols = {{ col }}, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
        ungroup() %>%
        mutate(
            variable = col,
            `Bulgaria %` = Bulgaria / sum(Bulgaria, rm.na = TRUE),
            `Serbia %` = Serbia / sum(Serbia, rm.na = TRUE)
        ) %>%
        slice(-1)
}

dat <- pooled

survey_stage <- dat %>%
    mutate(wave = recode(wave, `1` = "endline", `0` = "baseline", `2` = "followup")) %>%
    group_by(country, wave) %>%
    count() %>%
    pivot_wider(id_cols = wave, id_expand = TRUE, names_from = country, names_expand = FALSE, values_from = n) %>%
    ungroup() %>%
    mutate(
        variable = "survey stage",
        `Bulgaria %` = Bulgaria / sum(Bulgaria, rm.na = TRUE),
        `Serbia %` = Serbia / sum(Serbia, rm.na = TRUE)
    )



vars_ <- control_cols
results <- lapply(vars_, function(col) create_respondent_counts(dat %>% filter(wave == 0), col))

final <- rbindlist(results, use.names = FALSE)

final <- final %>%
    relocate(variable, .before = woman) %>%
    relocate(`Bulgaria %`, .before = Serbia)

names(final) <- c("Variable", "Value", "Bulgaria", "Bulgaria %", "Serbia", "Serbia %")

final <- final %>%
    mutate_if(is.numeric, round, 2) %>%
    prettify("Variable")

final %>% write_table("report/descriptives/tables", "Baseline Respondent Characteristics")
