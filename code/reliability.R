#############################################################
# Reliability Analysis
#############################################################


datasets <- datasets <- list(
    `Serbia` = serbia_without_impacted,
    `Bulgaria` = bulgaria,
    `Pooled` = pooled
)

run_reliability <- function(dat, construct_mapping, dataset) {
    alpha_matrix <- matrix(ncol = 4, nrow = length(names(construct_mapping)))
    alpha_drop_df <- data.frame()
    # recreate ss

    for (att in names(construct_mapping)) {
        var_count <- length(construct_mapping[[att]])

        if (var_count > 1) {
            temp <- dat %>%
                filter(wave == 0) %>%
                select(all_of(construct_mapping[[att]])) %>%
                select_if(~ !all(is.na(.))) %>%
                psych::alpha()

            raw_alpha <- round(temp$total[1]$raw_alpha, 2)
            std_alpha <- round(temp$total[2]$std.alpha, 2)

            alpha_drop <- temp$alpha.drop[, c("raw_alpha", "std.alpha")] %>% round(2)
            alpha_drop$construct <- att

            alpha_drop_df <- rbind(alpha_drop_df, alpha_drop)
        } else {
            raw_alpha <- NA
            std_alpha <- NA
        }

        ind <- which(names(construct_mapping) == att)
        alpha_matrix[ind, ] <- c(att, var_count, raw_alpha, std_alpha)
    }

    colnames(alpha_matrix) <- c("construct", "variable count", "raw.alpha", "std.alpha")

    alpha_matrix <- as.data.frame(alpha_matrix)

    write_table(prettify(alpha_matrix, "construct"), "report/descriptives/tables/", glue("Reliability: {dataset} Alpha Matrix"), align = TRUE)

    alpha_drop_df$variable.dropped <- rownames(alpha_drop_df)
    rownames(alpha_drop_df) <- NULL

    alpha_drop_df <- alpha_drop_df %>%
        merge(alpha_matrix[, c("construct", "raw.alpha")], by.x = "construct", by.y = "construct") %>%
        mutate(raw.alpha = as.numeric(raw.alpha)) %>%
        mutate(increment = raw_alpha - raw.alpha) %>%
        mutate(raw.alpha = NULL) %>%
        relocate(c(variable.dropped, increment), .after = construct)

    write_table(alpha_drop_df, "report/descriptives/tables/", glue("Reliability: {dataset} Alpha Drop"), align = TRUE)
}


for (dataset in names(datasets)) {
    dat <- datasets[[dataset]]

    run_reliability(dat, construct_mapping, dataset)
}
