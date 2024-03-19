make_outcome_difs <- function(dat, outcomes) {
    baselines <- dat %>%
        filter(wave == 0) %>%
        select(userid, construct_cols)


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


power_per_size <- function(n, d, sig) {
    takeup <- 0.28
    res <- pwr.t.test(n = n, d = d * takeup, sig.level = sig)
    res$power
}

get_n <- function(dat) {
    dat %>%
        filter(wave == 1) %>%
        group_by(treatment) %>%
        count() %>%
        pull(n) %>%
        min()
}


datasets <- list(
    `Serbia` = serbia,
    `Bulgaria` = bulgaria,
    `Pooled` = pooled
)

df <- tibble()

for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]
    n <- get_n(dat)

    for (z in c(0.10 / 8, 0.05)) {
        x <- seq(0, .8, 0.025)
        y <- sapply(x, function(d) power_per_size(n, d, z))
        d <- tibble(x, y)
        d <- mutate(d, z = z, Dataset = dataset)
        df <- rbind(df, d)
    }
}


df <- df %>%
    mutate(z = factor(z)) %>%
    rename(`Power` = y, `Effect Size` = x, `Significance` = z)

ggplot(df, aes(x = `Effect Size`, y = Power, color = Dataset)) +
    geom_line() +
    facet_grid(cols = vars(Significance))

ggsave("report/plots/Power Calculations.png", width = 10, height = 10)




##############################################
# What effect size is possible given baseline?
##############################################

dat <- pooled

difs <- make_outcome_difs(dat, construct_cols)

sd_lookup <- difs %>%
    filter(wave == 1) %>%
    select(all_of(construct_cols)) %>%
    summarise(across(all_of(construct_cols), ~ sd(.x, na.rm = T))) %>%
    pivot_longer(cols = construct_cols) %>%
    rename(sd = value)

max_lookup <- dat %>%
    filter(wave == 0) %>%
    select(construct_cols) %>%
    summarise(across(all_of(construct_cols), ~ max(.x, na.rm = T))) %>%
    pivot_longer(cols = construct_cols) %>%
    rename(max = value)


for (domain in names(domains)) {
    d <- dat %>%
        select(userid, wave, treatment, construct_cols) %>%
        pivot_longer(cols = construct_cols) %>%
        filter(wave == 0) %>%
        inner_join(sd_lookup, by = "name") %>%
        filter(name %in% domains[[domain]]) %>%
        mutate(name = as.character(name)) %>%
        mutate(name = map_chr(name, \(x) pretty_vars[[x]]))

    ggplot(d, aes(x = value, y = after_stat(density), color = treatment)) +
        geom_histogram(position = "identity", alpha = 0.2) +
        facet_wrap(vars(name), nrow = 2, ncol = 2)

    ggsave(glue("report/plots/Original Data - {domain}.png"), width = 10, height = 5)
}



# these are standard deviations of Y for regression.
# 0.5 * sd is the medium effect size.

d <- difs %>%
    select(userid, wave, treatment, construct_cols) %>%
    pivot_longer(cols = all_of(construct_cols)) %>%
    filter(wave == 1) %>%
    inner_join(sd_lookup, by = "name") %>%
    mutate(name = factor(name, levels = construct_cols)) %>%
    mutate(normalized_value = value / sd)

means <- d %>%
    group_by(name, treatment) %>%
    summarise(mean = mean(value, na.rm = T))

d <- d %>% inner_join(means, by = c("name", "treatment"))

ggplot(d, aes(x = normalized_value, y = after_stat(density), color = treatment)) +
    geom_histogram(position = "identity", alpha = 0.2) +
    geom_vline(aes(xintercept = mean, color = treatment)) +
    facet_wrap(vars(name), nrow = 4, ncol = 2)

ggsave(glue("report/plots/Transformed Data.png"), width = 10, height = 10)
