source("code/data.R")

# get users 

# get intercept of a regression that controls for everything? 


make_outcome_difs <- function(dat, outcomes) {
    baselines <- dat %>%
        filter(endline == 0) %>%
        select(userid, construct_cols)


    for (outcome in outcomes) {
        baselines <- baselines %>% 
            rename("{outcome}_baseline" := {{ outcome }})
    }

    waves <- dat %>%
        filter(endline != 0) %>%
        inner_join(baselines, by = "userid")

    for (outcome in outcomes) {
        base <- paste0(outcome, "_baseline")
        waves <- waves %>% 
            mutate("{outcome}" := waves[[outcome]] - waves[[base]])
    }

    waves
}

waves <- make_outcome_difs(pooled, construct_cols)


fmla <- reformulate(c("treatment", controls, "country"), outcome)

model <- lm(fmla, dat)

mean(predict(model, dat))

mean(dat$practices_24, na.rm=TRUE)

summary(glm(fmla, family="binomial", data=dat, weights = dat$weights))
