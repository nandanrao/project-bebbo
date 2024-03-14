make_outcome_difs <- function(dat, outcomes) {
    baselines <- dat %>%
        filter(wave == 0) %>%
        select(userid, all_of(construct_cols))


    for (outcome in outcomes) {
        baselines <- baselines %>%
            rename("{outcome}_baseline" := {{ outcome }})
    }

    waves <- dat %>%
        filter(wave != 0) %>%
        inner_join(baselines, by = "userid")

    for (outcome in outcomes) {
        base <- paste0(outcome, "_baseline")
        waves <- waves %>%
            mutate("{outcome}" := waves[[outcome]] - waves[[base]])
    }

    waves
}


format_for_reg <- function(dat, outcome, endline_flag) {
    dat %>%
        filter(wave == endline_flag)
}


pick_controls <- function(dat, outcome) {
    singular_outcomes <- c("was_breastfed", "health_knw")
    if (outcome %in% singular_outcomes) {
        return(control_cols[!(control_cols %in% c("age_flag"))])
    }
    control_cols
}

run_regression <- function(dat, outcome, endline_flag, ...) {
    controls <- pick_controls(dat, outcome)
    fmla <- reformulate(c("treatment", controls, ...), outcome)
    dat <- format_for_reg(dat, outcome, endline_flag)
    model <- lm(fmla, dat, na.action = na.omit)
    model
}


run_tot_regression <- function(dat, outcome, endline_flag, ...) {
    controls <- pick_controls(dat, outcome)
    control_formula <- paste0(c(controls, ...), collapse = " + ")
    endog <- "has_learning_event"

    ff <- glue("{outcome} ~ {endog} + {control_formula} | treatment + {control_formula}")
    fmla <- formula(ff)
    model <- ivreg(fmla, data = format_for_reg(dat, outcome, endline_flag))
    model
}


adjust_p_value <- function(p_values, p_value) {
    adjusted <- p.adjust(flatten(p_values), method = "BH")
    flattened <- as.numeric(flatten(p_values))
    l <- list()
    for (i in seq_along(flattened)) {
        if (flattened[[i]] == p_value) {
            return(adjusted[[i]])
        }
    }
}

get_p_values <- function(model) {
    summary(model)$coefficients[, 4]
}

fix_p_values <- function(p_values, model, var_of_interest) {
    p_value <- get_p_value(model, var_of_interest)
    adjusted <- adjust_p_value(p_values, p_value)
    ps <- summary(model)$coefficients[, 4]
    ps <- sapply(ps, function(x) 1)
    ps[var_of_interest] <- adjusted
    ps
}

get_p_value <- function(model, var) {
    summary(model)$coefficients[, 4][var]
}

get_diagnostic_p <- function(model, diagnostic) {
    p <- summary(model)$diagnostics[diagnostic, 4]
    format(p, digits = 3)
}


bf_adjusted_ci <- function(model, var, alpha, total_measures) {
    df <- summary(model)$df[2]
    adjusted_alpha <- alpha / total_measures
    t_score <- qt(p = adjusted_alpha / 2, df = df, lower.tail = F)

    mean <- summary(model)$coefficients[, 1][var]
    sd <- summary(model)$coefficients[, 2][var]
    ci <- unname(sd * t_score)
    mean <- unname(mean)

    variable <- all.vars(terms(model))[1]

    list(variable = variable, mean = mean, lower = mean - ci, upper = mean + ci)
}

# TODO: bulgaria doesn't work for follow up -- missing variation on some vars.
datasets <- list(
    `Serbia` = serbia_without_impacted,
    `Bulgaria` = bulgaria,
    `Pooled` = pooled,
    `Pooled for Follow Up` = pooled_without_impacted
)

models <- list(
    `OLS` = run_regression,
    `2SLS` = run_tot_regression
)

waves <- list(
    `Endline` = 1,
    `Follow Up` = 2
)

adjusted_coefficients <- tibble()

for (wave in names(waves)) {
    endline_flag <- waves[[wave]]

    for (model in names(models)) {
        fun <- models[[model]]

        for (dataset in names(datasets)) {
            #
            if (all(dataset == "Bulgaria" & wave == "Follow Up")) {
                next
            }

            dat <- datasets[[dataset]]

            dat <- make_outcome_difs(dat, construct_cols)

            res <- lapply(domains, function(outcomes) {
                if (dataset != "Pooled") {
                    return(lapply(outcomes, function(o) fun(dat, o, endline_flag)))
                }
                if (dataset == "Pooled") {
                    return(lapply(outcomes, function(o) fun(dat, o, endline_flag, "country")))
                }
            })

            var_of_interest <- if_else(model == "2SLS", "has_learning_eventTRUE", "treatmenttreated")

            p_values <- lapply(res, function(x) lapply(x, function(y) get_p_value(y, var_of_interest)))

            for (name in names(res)) {
                results <- res[[name]]

                local_p_values <- lapply(results, function(model) fix_p_values(p_values, model, var_of_interest))

                local_treatment_p_values <- sapply(local_p_values, function(x) format(x[var_of_interest], digits = 3))


                if (model == "2SLS") {
                    lines <- list(
                        c("Adjusted Treatment p-value", local_treatment_p_values),
                        c("Weak instruments p-value", sapply(results, function(r) get_diagnostic_p(r, "Weak instruments"))),
                        c("Wu-Hausman p-value", sapply(results, function(r) get_diagnostic_p(r, "Wu-Hausman")))
                    )
                }
                if (model == "OLS") {
                    lines <- list(
                        c("Adjusted Treatment p-value", local_treatment_p_values)
                    )
                }

                # Created adjusted coefficients for coef plot
                for (m in results) {
                    l <- c(bf_adjusted_ci(m, var_of_interest, 0.10, 8), list(model = model, wave = wave, dataset = dataset, name = name))
                    adjusted_coefficients <- rbind(adjusted_coefficients, l)
                }

                controls <- pick_controls(dat, outcome)

                covariate_labels <- c(pretty_vars[[var_of_interest]])
                dep_vars <- as.character(sapply(domains[[name]], function(n) pretty_vars[[n]]))

                write_regressions(
                    results,
                    "report/regressions",
                    glue("{dataset}: {model} - {wave} - {name}"),
                    add.lines = lines,
                    dep.var.labels = dep_vars,
                    covariate.labels = covariate_labels,
                    p = local_p_values,
                    star.cutoffs = c(0.10, 0.05, 0.01),
                    omit = c(controls, "Constant", "countrySerbia"),
                    keep.stat = c("n", "rsq")
                )
            }
        }
    }
}

################################################
# PLOT COEFFICIENTS WITH ADJUSTED CI's
################################################

variable_order <- adjusted_coefficients %>%
    distinct(variable) %>%
    pull(variable)
adjusted_coefficients <- adjusted_coefficients %>% mutate(variable = factor(variable, levels = variable_order))

groups <- list(
    `Practices` = c("Practices"),
    `Knowledge and Attitudes` = c("Knowledge and Awareness", "Confidence and Attitudes")
)

for (group in names(groups)) {
    outcomes <- groups[[group]]

    dd <- adjusted_coefficients %>% filter(model == "2SLS")

    dd <- dd %>% filter(name %in% outcomes)

    dd <- dd %>% mutate(group = glue("{dataset} {wave}"))

    ggplot(dd, aes(x = mean, y = group, color = group)) +
        geom_point(size = 3) +
        geom_errorbarh(aes(y = group, xmin = lower, xmax = upper, height = 0)) +
        geom_vline(xintercept = 0) +
        facet_grid(rows = vars(variable)) +
        theme(
            axis.title.y = element_blank(), axis.title.x = element_blank(),
            legend.position = "none"
        )

    ggsave(glue("report/plots/Adjusted Coefficient Plot {group}.png"), width = 10, height = 5)
}



################################################
# Subgroups
################################################

vaccine_failers <- pooled %>%
    filter(wave == 0) %>%
    filter(health_knw == 0) %>%
    distinct(userid) %>%
    pull(userid)

dat <- pooled %>%
    filter(userid %in% vaccine_failers) %>%
    make_outcome_difs(construct_cols)

reg <- run_regression(dat, "health_knw", 1)
li <- list(`Vaccine Knowledge` = reg)

write_regressions(
    li,
    "report/regressions",
    glue("Vaccine Failures Subgroup OLS - Endline"),
    ## add.lines = lines,
    ## dep.var.labels = dep_vars,
    ## covariate.labels = covariate_labels,
    ## p = local_p_values,
    star.cutoffs = c(0.10, 0.05, 0.01),
    omit = c(controls, "Constant", "countrySerbia"),
    keep.stat = c("n", "rsq")
)


###############################################
# ADD MOTHERS/FATHERS/OTHER
###############################################
