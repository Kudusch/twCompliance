combine_jobs <- function(
    all_tweets,
    compliance_tweets,
    compliance_users
) {
    if (
        ((length(names(all_tweets)) != 2) &
         ("status_id" %in% names(all_tweets)) &
         ("user_id" %in% names(all_tweets)))
    ) {
        usethis::ui_oops("Wrong variables ...")
        stop("Stopped due to wrong names")
    } else {
        usethis::ui_info("Joining datasets, this might take a while ...")
    }

    data.table::setDT(all_tweets)
    if(!is.null(compliance_users)) {data.table::setDT(compliance_users)}
    if(!is.null(compliance_tweets)) {data.table::setDT(compliance_tweets)}

    if (!is.null(compliance_users) & !is.null(compliance_tweets)) {
        compliance_users <- compliance_users[order(redacted_at)][, .SD[1], by = id]
        compliance_users <- compliance_users[id %in% unique(all_tweets$user_id)]

        compliance_tweets <- compliance_tweets[order(redacted_at)][, .SD[1], by = id]
        compliance_tweets <- compliance_tweets[id %in% unique(all_tweets$status_id)]

        combinded_tweets <- all_tweets %>%
            select(status_id, user_id) %>%
            left_join((compliance_tweets %>% select(-action)), by = c("status_id" = "id"), suffix = c("", "_tweets")) %>%
            left_join((compliance_users %>% select(-action)), by = c("user_id" = "id"), suffix = c("", "_users")) %>%
            rename(
                reason_tweets = reason,
                redacted_at_tweets = redacted_at,
                created_at_tweets = created_at
            ) %>%
            filter(!duplicated(.))
    } else if (is.null(compliance_users)) {
        compliance_tweets <- compliance_tweets[order(redacted_at)][, .SD[1], by = id]
        compliance_tweets <- compliance_tweets[id %in% unique(all_tweets$status_id)]

        combinded_tweets <- all_tweets %>%
            select(status_id, user_id) %>%
            left_join((compliance_tweets %>% select(-action)), by = c("status_id" = "id"), suffix = c("", "_tweets")) %>%
            mutate(
                reason_users = rep(c(reason[0], NA), length(reason)),
                redacted_at_users = rep(c(redacted_at[0], NA), length(redacted_at)),
                created_at_users = rep(c(created_at[0], NA), length(created_at))
            ) %>%
            rename(
                reason_tweets = reason,
                redacted_at_tweets = redacted_at,
                created_at_tweets = created_at
            ) %>%
            filter(!duplicated(.))
    } else if (is.null(compliance_tweets)) {
        compliance_users <- compliance_users[order(redacted_at)][, .SD[1], by = id]
        compliance_users <- compliance_users[id %in% unique(all_tweets$user_id)]

        combinded_tweets <- all_tweets %>%
            select(status_id, user_id) %>%
            left_join((compliance_users %>% select(-action)), by = c("status_id" = "id"), suffix = c("", "_users")) %>%
            mutate(
                reason_tweets = rep(c(reason[0], NA), length(reason_users)),
                redacted_at_tweets = rep(c(redacted_at[0], NA), length(redacted_at)),
                created_at_tweets = rep(c(created_at[0], NA), length(created_at))
            ) %>%
            rename(
                reason_users = reason,
                redacted_at_users = redacted_at,
                created_at_users = created_at
            ) %>%
            filter(!duplicated(.))
    }
    combinded_tweets <- combinded_tweets %>%
        mutate(redacted_at = case_when(
            (is.na(redacted_at_tweets) & !is.na(redacted_at_users)) ~ redacted_at_users,
            (is.na(redacted_at_users) & !is.na(redacted_at_tweets)) ~ redacted_at_tweets,
            (redacted_at_users > redacted_at_tweets) ~ redacted_at_users,
            (redacted_at_users < redacted_at_tweets) ~ redacted_at_tweets
        )) %>%
        mutate(online_status = case_when(
            is.na(reason_users) & is.na(reason_tweets) ~ "online",
            is.na(reason_tweets) ~ paste0("user_", reason_users),
            is.na(reason_users) ~ paste0("tweet_", reason_tweets),
            is.na(redacted_at_tweets) | redacted_at_tweets < redacted_at_users ~ paste0("user_", reason_users),
            is.na(redacted_at_users) | redacted_at_tweets > redacted_at_users ~ paste0("tweet_", reason_tweets)
        )) %>%
        select(status_id, user_id, online_status, redacted_at)
    if (nrow(combinded_tweets) == nrow(all_tweets)) {
        return(combinded_tweets)
    } else {
        return(NA)
    }

}
