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
## WITH t AS (
##   SELECT
##     user_pseudo_id,
##     TIMESTAMP_MICROS(event_timestamp) AS event_timestamp,
##     event_name,
##     event_name IN ('advise_details_opened', 'game_details_opened', 'child_milestone_tracked', 'child_measurement_entered', 'child_vaccine_entered', 'child_health_checkup_entered') AS is_learning_event,
##     traffic_source.SOURCE AS traffic_source,
##     traffic_source.NAME AS traffic_source_name,
##     geo.country AS country,
##     ROW_NUMBER() OVER (PARTITION BY user_pseudo_id ORDER BY event_timestamp) AS event_number
##   FROM `ecaro-bebbo.analytics_275292753.events_202*`
##   WHERE traffic_source.NAME != 'evaluation study'
##   AND geo.country IN ('Bulgaria', 'Serbia')
##   AND TIMESTAMP_MICROS(event_timestamp) < CAST('2024-04-01' AS TIMESTAMP)
## ),
## first_events AS (
##   SELECT user_pseudo_id, MIN(event_timestamp) AS first_event_time
##   FROM t
##   WHERE event_name = 'first_open'
##   GROUP BY user_pseudo_id
## ),
## events_of_interest AS (
##   SELECT *
##   FROM t
##   INNER JOIN first_events USING(user_pseudo_id)
##   WHERE (is_learning_event = TRUE OR event_name = 'first_open')
##   AND first_event_time >= CAST('2023-01-01' AS TIMESTAMP)
##   AND first_event_time < CAST('2023-10-01' AS TIMESTAMP)
##   AND event_timestamp - first_event_time < CAST('P6M' AS INTERVAL)
## )
## SELECT *
## FROM events_of_interest
##########################################

all_app <- read_csv("data/raw/bq-results-all-2024-04.csv") %>%
    mutate(
        event_timestamp = parse_date_time(event_timestamp, c("Ymd HMS")),
        first_event_time = parse_date_time(first_event_time, c("Ymd HMS"))
    ) %>%
    rename(userid = user_pseudo_id) %>%
    mutate(
        event_day = date(event_timestamp)
    )

started_in_2023 <- all_app %>%
    filter(event_number == 1) %>%
    filter(event_name == "first_open") %>%
    pull(userid)

all_app <- all_app %>% filter(userid %in% started_in_2023)



########################################################
## RETENTION FUNNEL QUERY:
##
## WITH t AS (
##   SELECT
##     user_pseudo_id,
##     TIMESTAMP_MICROS(event_timestamp) AS event_timestamp,
##     event_name,
##     event_name IN ('advise_details_opened', 'game_details_opened', 'child_milestone_tracked', 'child_measurement_entered', 'child_vaccine_entered', 'child_health_checkup_entered') AS is_learning_event,
##     traffic_source.SOURCE AS traffic_source,
##     traffic_source.NAME AS traffic_source_name,
##     geo.country AS country,
##     ROW_NUMBER() OVER (PARTITION BY user_pseudo_id ORDER BY event_timestamp) AS event_number
##   FROM `ecaro-bebbo.analytics_275292753.events_202*`
##   WHERE traffic_source.NAME != 'evaluation study'
##   AND geo.country IN ('Bulgaria', 'Serbia')
##   AND TIMESTAMP_MICROS(event_timestamp) < CAST('2024-04-01' AS TIMESTAMP)
## ),
## first_events AS (
##   SELECT user_pseudo_id, MIN(event_timestamp) AS first_event_time
##   FROM t
##   WHERE event_name = 'first_open'
##   GROUP BY user_pseudo_id
## ),
## events_of_interest AS (
##   SELECT *
##   FROM t
##   INNER JOIN first_events USING(user_pseudo_id)
##   WHERE (is_learning_event = TRUE OR event_name = 'first_open')
##   AND first_event_time >= CAST('2023-01-01' AS TIMESTAMP)
##   AND first_event_time < CAST('2023-10-01' AS TIMESTAMP)
##   AND event_timestamp - first_event_time < CAST('P6M' AS INTERVAL)
## ),
## first_opens AS (
##   SELECT user_pseudo_id, first_event_time
##   FROM events_of_interest
##   GROUP BY user_pseudo_id, first_event_time
## ),
## last_learning_events AS (
##   SELECT user_pseudo_id, MAX(event_timestamp) - first_event_time AS user_retention
##   FROM events_of_interest
##   WHERE is_learning_event = TRUE
##   GROUP BY user_pseudo_id, first_event_time
## )
## SELECT COUNT(user_pseudo_id) AS opened_app,
##   COUNTIF(user_retention IS NOT NULL) AS had_learning_event,
##   COUNTIF(user_retention >= CAST('P1D' AS INTERVAL)) AS retention_over_1_day,
##   COUNTIF(user_retention >= CAST('P30D' AS INTERVAL)) AS retention_30_days,
##   COUNTIF(user_retention >= CAST('P60D' AS INTERVAL)) AS retention_60_days,
##   COUNTIF(user_retention >= CAST('P90D' AS INTERVAL)) AS retention_90_days
## FROM first_opens
## LEFT JOIN last_learning_events USING(user_pseudo_id)
