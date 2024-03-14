source("code/functions.R")
source("code/data.R")

check_var_outcome <- function(dat, var, country_) {
    dat %>%
        filter(wave == 2) %>%
        filter(treatment == "control") %>%
        filter(!is.na(across(var))) %>%
        group_by(across(var)) %>%
        summarise(count = n()) %>%
        mutate(freq = count / sum(count)) %>%
        pivot_longer(var) %>%
        rename(variable = name) %>%
        mutate(country = country_) %>%
        slice(-1)
}

vars <- c("control_bebbo_knowledge", "control_bebbo_usage")


# Don't really have the data in bulgaria, treated/control
# messed up in Follow up for everyone, it seems...
datasets <- list(
    ## `Pooled` = dat
    `Serbia` = serbia
    ## `Serbia With Impacted` = serbia_with_impacted
)


out <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    for (var in vars) {
        tmp <- check_var_outcome(dat, var, dataset)
        out <- rbind(out, tmp)
    }
}


# print out to table
