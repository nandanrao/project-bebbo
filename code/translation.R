make_lookup <- function(long) {
    long %>%
        group_by(shortcode, surveyid, question_ref) %>%
        filter(!is.na(response) & !is.na(translated_response)) %>%
        slice(1) %>%
        select(shortcode, surveyid, question_ref, response, translated_response)
}
