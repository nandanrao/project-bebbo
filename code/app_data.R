library(readr)
library(lubridate)


learning_events <- c(
    "advise_details_opened",
    "game_details_opened",
    "child_milestone_tracked",
    "child_measurement_entered",
    "child_vaccine_entered",
    "child_health_checkup_entered"
)

## with t as (
##   SELECT
##     user_pseudo_id,
##     event_timestamp,
##     event_name,
##     traffic_source.source as traffic_source,
##     traffic_source.name as traffic_source_name,
##     geo.country as country,
##     ROW_NUMBER() OVER (PARTITION BY user_pseudo_id ORDER BY event_timestamp) as event_number
##   FROM `ecaro-bebbo.analytics_275292753.events_2023*`
##   WHERE traffic_source.name != 'evaluation study'
##   AND geo.country IN ('Bulgaria', 'Serbia')
## )
## SELECT *
## FROM T
## WHERE event_number = 1 OR event_name IN ('advise_details_opened', 'game_details_opened', 'child_milestone_tracked', 'child_measurement_entered', 'child_vaccine_entered', 'child_health_checkup_entered')


parse_bq_date <- function(i) {
    as.POSIXct(i / 1000 / 1000)
}

rollup_events <- function(events, variables) {
    events %>%
        filter(event_name %in% learning_events) %>%
        group_by(across(all_of(c("userid", variables)))) %>%
        summarize(
            learning_events = n(),
            days_learned = n_distinct(event_day),
        ) %>%
        mutate(
            has_learning_event = if_else(is.na(learning_events), FALSE, learning_events > 0),
        )
}

#######################################################
## QUERY:
##
## SELECT user_pseudo_id, event_timestamp, event_name, value.string_value as facebook_id FROM `ecaro-bebbo.analytics_275292753.events_202*`
## JOIN unnest(user_properties)
## WHERE key = 'facebook_id'
##
#######################################################

raw_app <- read_csv("data/raw/bq-results-20240312.csv")

app_events <- raw_app %>%
    mutate(event_timestamp = parse_bq_date(event_timestamp)) %>%
    mutate(
        event_day = date(event_timestamp),
        userid = as.numeric(facebook_id)
    )


d <- rollup_events(app_events, c())

d %>% ggplot(aes(x = days_learned)) +
    geom_histogram()


##########################################



##########################################

all_app <- read_csv("data/raw/bq-results-all-2023.csv") %>%
    mutate(event_timestamp = parse_bq_date(event_timestamp)) %>%
    rename(userid = user_pseudo_id) %>%
    mutate(
        event_day = date(event_timestamp)
    )


# 24459 / 68797 (.35) all app download > learn
# 21666  > 1 days learned
# 19280 > 3 days learned

started_in_2023 <- all_app %>%
    filter(event_number == 1) %>%
    filter(event_name == "first_open") %>%
    pull(userid)


all_app <- all_app %>% filter(userid %in% started_in_2023)
