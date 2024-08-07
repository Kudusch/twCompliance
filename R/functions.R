# Start compliance job
#
# This function attempts to start a compliance job with a given list of tweet status ids.
#
# return string ID of the compliance job. Save this to check the jobs status and download the results.
#
# param ids A character vector containing the status ids of the tweets that will be checked.
# param type Currently only "tweets" is allowed here.
# @param verbose If FALSE the function will not print any messages.
# @param bearer_token API bearer token.
#
start_job <- function(
        ids,
        type = "tweets",
        verbose = TRUE,
        bearer_token = get_bearer()
) {
    # check arguments
    if (!type %in% c("users", "tweets")) {
        stop("Wrong type")
    }
    r <- httr::GET(
        sprintf(
            "https://api.twitter.com/2/compliance/jobs?type=%s",
            type
        ),
        httr::add_headers("Authorization" = paste0("Bearer ", bearer_token))
    )

    if ("in_progress" %in% (unlist(lapply(httr::content(r)$data, function(x) {
        x[["status"]]
    })))) {
        compliance_job <-
            httr::content(r)$data[[match("in_progress", (unlist(
                lapply(httr::content(r)$data, function(x) {
                    x[["status"]]
                })
            )))]]
        stop(
            sprintf(
                "Cannot start a new job, as there is an ongoing job with id `%s`",
                compliance_job$id
            )
        )
    }

    if (!"created" %in% (unlist(lapply(httr::content(r)$data, function(x) {
        x[["status"]]
    })))) {
        r <- httr::POST(
            "https://api.twitter.com/2/compliance/jobs",
            httr::add_headers(
                "Authorization" = paste0("Bearer ", bearer_token),
                "Content-Type" = "application/json"
            ),
            body = sprintf('{ "type": "%s" }', type)
        )

        if (httr::status_code(r) != 200) {
            stop("There was an error starting the compliance job")
        } else {
            compliance_job <- httr::content(r)$data
        }
    } else {
        compliance_job <-
            httr::content(r)$data[[match("created", (unlist(
                lapply(httr::content(r)$data, function(x) {
                    x[["status"]]
                })
            )))]]
    }

    f <- tempfile()
    con <- file(f)
    writeLines(as.character(ids), f)
    close(con)

    r <- httr::PUT(
        compliance_job$upload_url,
        httr::add_headers("Content-Type" = "text/plain"),
        body = httr::upload_file(f)
    )

    if (verbose) {
        if (httr::status_code(r) != 200) {
            stop("There was an error starting the compliance job")
        } else {
            usethis::ui_info(
                "Job started with {usethis::ui_field('job ID')}: {usethis::ui_value(compliance_job$id)}"
            )
            expires_id <-
                round(as.numeric(
                    difftime(
                        lubridate::fast_strptime(
                            compliance_job$download_expires_at,
                            "%Y-%m-%dT%H:%M:%OS%z"
                        ),
                        lubridate::now(),
                        units = "hours"
                    )
                ), 0)
            usethis::ui_line(
                "  {usethis::ui_field('Expires in')}: {usethis::ui_value(expires_id)} hours"
            )
            usethis::ui_line(
                "You can check the job status with {usethis::ui_code(paste0('check_job(\"', compliance_job$id, '\")'))} and when it is ready download the data with {usethis::ui_code(paste0('download_job(\"', compliance_job$id, '\")'))}"
            )
            return(compliance_job$id)
        }
    } else {
        return(compliance_job$id)
    }

}

# Check status of a compliance job
#
# This function checks the status of a given compliance job.
#
# @return If `verbose` is set to FALSE, return the jobs download URL if ready or FALSE if still in progress.
#
# @param job_id A string containing the ID of the compliance job that will be checked.
# @param bearer_token API bearer token.
# @param verbose If FALSE, the function will not print any messages.
#
check_job <- function(job_id,
                      bearer_token = get_bearer(),
                      verbose = TRUE) {
    if (typeof(job_id) == "list") {
        job_tweets <- FALSE
        job_users <- FALSE
        if (!is.null(job_id$tweets)) {
            job_tweets <- check_job(job_id$tweets, verbose = FALSE)
        }
        if (!is.null(job_id$users)) {
            job_users <- check_job(job_id$users, verbose = FALSE)
        }
        return(list("tweets" = job_tweets, "users" = job_users))
    } else {
        r <- httr::GET(
            sprintf(
                "https://api.twitter.com/2/compliance/jobs/%s",
                job_id
            ),
            httr::add_headers(
                "Authorization" = paste0("Bearer ", bearer_token),
                "Content-Type" = "application/json"
            )
        )
        job <- httr::content(r)$data
        if (verbose) {
            print_job(job)
        } else {
            if (job$status == "complete") {
                return(job$download_url)
            } else {
                return(FALSE)
            }
        }
    }
}


# Download compliance job
#
# This function attempts to download a compliance job if it is ready to download.
#
# @return dataframe A dataframe of all returned tweets.
#
# @param job_id A string containing the ID of the compliance job that will be downloaded.
# @param verbose If FALSE, the function will not print any messages.
#
download_job <- function(job_id,
                         verbose = FALSE) {
    job_status <- check_job(job_id, verbose = FALSE)
    if (job_status == FALSE) {
        stop("The job is not complete yet.")
    }

    if (verbose) {
        usethis::ui_info("Downloading results, this might take a while ...")
    }
    r <- httr::GET(job_status)

    if (httr::status_code(r) != 200) {
        stop("There was an error downloading the compliance job")
    }

    tweet_status <- httr::content(r, "text",  encoding = "UTF-8")
    tweet_status <- stringr::str_split(tweet_status, "\\n")[[1]]
    tweet_status <- tweet_status[tweet_status != ""]
    tweet_status <-
        jsonlite::fromJSON(paste0("[", paste(tweet_status, collapse = ", "), "]"))

    # return early if no items in response
    if (length(tweet_status) == 0) {
        if (verbose) {
            usethis::ui_oops(
                "All tweets/users are still online and publicly reachable, nothing to download."
            )
        }
        return(NULL)
    }

    tweet_status <- tweet_status[!is.na(tweet_status["id"]), ]
    tweet_status <- tweet_status[tweet_status["id"] != "", ]

    rownames(tweet_status) <- NULL

    if (nrow(tweet_status) > 0) {
        tweet_status$redacted_at <-
            lubridate::fast_strptime(tweet_status$redacted_at, "%Y-%m-%dT%H:%M:%OS%z")
        tweet_status$redacted_at <-
            as.POSIXct(tweet_status$redacted_at)

        tweet_status$created_at <-
            lubridate::fast_strptime(tweet_status$created_at, "%Y-%m-%dT%H:%M:%OS%z")
        tweet_status$created_at <-
            as.POSIXct(tweet_status$created_at)

        return(tweet_status)
    } else {
        if (verbose) {
            usethis::ui_oops(
                "All tweets/users are still online and publicly reachable, nothing to download."
            )
        }
        return(NULL)
    }
}

print_job <- function(job) {
    usethis::ui_line("{usethis::ui_field('Job ID')}: {usethis::ui_value(job$id)}")

    usethis::ui_line("  {usethis::ui_field('Type')}: {usethis::ui_value(job$type)}")

    created_at_minutes <-
        round(as.numeric(
            difftime(
                lubridate::now(),
                lubridate::fast_strptime(job$created_at, "%Y-%m-%dT%H:%M:%OS%z"),
                units = "mins"
            )
        ), 0)
    created_at_hours <-
        round(as.numeric(
            difftime(
                lubridate::now(),
                lubridate::fast_strptime(job$created_at, "%Y-%m-%dT%H:%M:%OS%z"),
                units = "hours"
            )
        ), 0)
    created_at_days <-
        round(as.numeric(
            difftime(
                lubridate::now(),
                lubridate::fast_strptime(job$created_at, "%Y-%m-%dT%H:%M:%OS%z"),
                units = "days"
            )
        ), 0)

    if (created_at_minutes < 120) {
        usethis::ui_line(
            "  {usethis::ui_field('Created at')}: {usethis::ui_value(created_at_minutes)} minutes ago"
        )
    } else if (created_at_minutes < 48) {
        usethis::ui_line(
            "  {usethis::ui_field('Created at')}: {usethis::ui_value(created_at_hours)} hours ago"
        )
    } else {
        usethis::ui_line(
            "  {usethis::ui_field('Created at')}: {usethis::ui_value(created_at_days)} days ago"
        )
    }

    usethis::ui_line("  {usethis::ui_field('Status')}: {usethis::ui_value(job$status)}")
    if (job$status != "expired") {
        expires_id <-
            round(as.numeric(
                difftime(
                    lubridate::fast_strptime(job$download_expires_at, "%Y-%m-%dT%H:%M:%OS%z"),
                    lubridate::now(),
                    units = "hours"
                )
            ), 0)
        usethis::ui_line(
            "  {usethis::ui_field('Expires in')}: {usethis::ui_value(expires_id)} hours"
        )
    }
    usethis::ui_line("")
}
# Start compliance job
#
# @param all_tweets Currently only "tweets" is allowed here.
# @param compliance_tweets If FALSE the function will not print any messages.
# @param compliance_users API bearer token.
#
#' @importFrom magrittr %>%
combine_jobs <- function(
        all_tweets,
        compliance_tweets,
        compliance_users
    ) {
    usethis::ui_info("Joining datasets, this might take a while ...")

    #data.table::setDT(all_tweets)
    if (!is.null(compliance_users)) {
        #data.table::setDT(compliance_users)
        compliance_users$created_at <- NULL
    }
    if (!is.null(compliance_tweets)) {
        #data.table::setDT(compliance_tweets)
        compliance_tweets$created_at <- NULL
    }

    selected_vars <- list()
    if (!is.null(compliance_tweets)) {
        selected_vars <- append(selected_vars, "status_id")
    }
    if (!is.null(compliance_users)) {
        selected_vars <- append(selected_vars, "user_id")
    }

    if (all(c("created_at", "account_created_at") %in% names(all_tweets))) {
        if (lubridate::is.POSIXct(all_tweets$created_at) &
            lubridate::is.POSIXct(all_tweets$account_created_at)) {
            selected_vars <-
                append(selected_vars,
                       c("created_at", "account_created_at"))
            selected_vars <- unlist(selected_vars)
            combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))

        } else {
            usethis::ui_oops(
                "The variables {usethis::ui_field('created_at')} and {usethis::ui_field('account_created_at')} must be object of class POSIXct."
            )
            usethis::ui_info(
                "Only {usethis::ui_field('status_id')} and {usethis::ui_field('user_id')} will be used."
            )
            selected_vars <- unlist(selected_vars)
            combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))
        }
    } else if ("created_at" %in% names(all_tweets)) {
        if (lubridate::is.POSIXct(all_tweets$created_at)) {
            selected_vars <- append(selected_vars, "created_at")
            selected_vars <- unlist(selected_vars)
            combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))

        } else {
            usethis::ui_oops(
                "The variables {usethis::ui_field('created_at')} and {usethis::ui_field('account_created_at')} must be object of class POSIXct."
            )
            usethis::ui_info(
                "Only {usethis::ui_field('status_id')} and {usethis::ui_field('user_id')} will be used."
            )
            selected_vars <- unlist(selected_vars)
            combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))
        }
    } else if ("account_created_at" %in% names(all_tweets)) {
        if (lubridate::is.POSIXct(all_tweets$account_created_at)) {
            selected_vars <- append(selected_vars,"account_created_at")
            selected_vars <- unlist(selected_vars)
            combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))

        } else {
            usethis::ui_oops(
                "The variables {usethis::ui_field('created_at')} and {usethis::ui_field('account_created_at')} must be object of class POSIXct."
            )
            usethis::ui_info(
                "Only {usethis::ui_field('status_id')} and {usethis::ui_field('user_id')} will be used."
            )
            selected_vars <- unlist(selected_vars)
            combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))
        }
    } else {
        selected_vars <- unlist(selected_vars)
        combined_tweets <- all_tweets %>% dplyr::select(tidyr::all_of(selected_vars))
    }

    combined_tweets <- unique(combined_tweets)

    if (!is.null(compliance_users) & !is.null(compliance_tweets)) {
        compliance_users <- compliance_users %>%
            dplyr::arrange(redacted_at) %>%
            dplyr::group_by(id) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::filter(id %in% unique(all_tweets$user_id))

        compliance_tweets <- compliance_tweets %>%
            dplyr::arrange(redacted_at) %>%
            dplyr::group_by(id) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::filter(id %in% unique(all_tweets$status_id)) %>%
            dplyr::mutate(reason = ifelse(reason == "geo_scrubbed", NA, reason))

        combined_tweets <- combined_tweets %>%
            dplyr::left_join((compliance_tweets %>% dplyr::select(-action)),
                      by = c("status_id" = "id")) %>%
            dplyr::left_join((compliance_users %>% dplyr::select(-action)),
                      by = c("user_id" = "id"),
                      suffix = c("_tweets", "_users")
            ) %>%
            dplyr::filter(!duplicated(.))

    } else if (is.null(compliance_users)) {
        compliance_tweets <- compliance_tweets %>%
            dplyr::group_by(id) %>%
            dplyr::arrange(redacted_at) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::filter(id %in% unique(all_tweets$status_id)) %>%
            dplyr::mutate(reason = ifelse(reason == "geo_scrubbed", NA, reason))

        combined_tweets <- combined_tweets %>%
            dplyr::left_join((compliance_tweets %>% dplyr::select(-action)),
                      by = c("status_id" = "id"),
                      suffix = c("", "_tweets")
            ) %>%
            dplyr::mutate(
                reason_users = rep(c(reason[0], NA), length(reason)), # needs to be character(0)
                redacted_at_users = rep(c(redacted_at[0], NA), length(redacted_at)) # needs to be character(0)
            ) %>%
            dplyr::rename(reason_tweets = reason,
                   redacted_at_tweets = redacted_at) %>%
            dplyr::filter(!duplicated(.))

    } else if (is.null(compliance_tweets)) {
        compliance_users <- compliance_users %>%
            dplyr::group_by(id) %>%
            dplyr::arrange(redacted_at) %>%
            dplyr::slice(1) %>%
            dplyr::ungroup() %>%
            dplyr::filter(id %in% unique(all_tweets$user_id))

        combined_tweets <- combined_tweets %>%
            dplyr::left_join((compliance_users %>% dplyr::select(-action)),
                      by = c("user_id" = "id"),
                      suffix = c("", "_users")
            ) %>%
            dplyr::mutate(
                reason_tweets = rep(c(reason[0], NA), length(reason)),
                redacted_at_tweets = rep(c(redacted_at[0], NA), length(redacted_at))
            ) %>%
            dplyr::rename(reason_users = reason,
                   redacted_at_users = redacted_at) %>%
            dplyr::filter(!duplicated(.))
    }

    combined_tweets <- combined_tweets %>%
        # take the available OR first time of redaction
        dplyr::mutate(
            redacted_at = dplyr::case_when(
                (is.na(redacted_at_tweets) &
                     !is.na(redacted_at_users)) ~ redacted_at_users,
                (is.na(redacted_at_users) &
                     !is.na(redacted_at_tweets)) ~ redacted_at_tweets,
                (redacted_at_users < redacted_at_tweets) ~ redacted_at_users,
                (redacted_at_users > redacted_at_tweets) ~ redacted_at_tweets
            )
        ) %>%
        # take the available OR first reason for redaction
        dplyr::mutate(
            online_status = dplyr::case_when(
                is.na(reason_users) & is.na(reason_tweets) ~ "online",
                is.na(reason_tweets) &
                    !is.na(reason_users) ~ reason_users,
                is.na(reason_users) &
                    !is.na(reason_tweets) ~ reason_tweets,
                is.na(redacted_at_tweets) |
                    redacted_at_tweets > redacted_at_users ~ reason_users,
                is.na(redacted_at_users) |
                    redacted_at_users < redacted_at_tweets ~ reason_tweets,
                (!is.na(reason_users) & !is.na(reason_tweets)) &
                    redacted_at_tweets > redacted_at_users~ reason_users,
                (!is.na(reason_users) & !is.na(reason_tweets)) &
                    redacted_at_tweets < redacted_at_users~ reason_tweets
            )
        ) %>%
        dplyr::mutate(
            online_status = dplyr::case_when(
                online_status == "deleted" & is.na(reason_users) ~ "tweet_deleted",
                online_status == "deleted" & is.na(reason_tweets) ~ "user_deleted",
                online_status == "deleted" &
                    (
                        is.na(redacted_at_tweets) |
                            redacted_at_tweets > redacted_at_users
                    ) ~ "user_deleted",
                online_status == "deleted" &
                    (
                        is.na(redacted_at_users) |
                            redacted_at_tweets < redacted_at_users
                    ) ~ "tweet_deleted",
                reason_users == reason_tweets & redacted_at_users < redacted_at_tweets ~ paste0("user_", reason_users),
                reason_users == reason_tweets & redacted_at_tweets < redacted_at_users ~ paste0("tweet_", reason_users),
                TRUE ~ online_status
            )
        )

    combined_tweets <- combined_tweets %>%
        dplyr::select(
            tidyr::all_of(selected_vars),
            tidyr::ends_with("created_at"),
            online_status,
            redacted_at
    )
    return(combined_tweets)

}
#' Prints a twCompliance object
#'
#' @param x A twCompliance object.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
print.twCompliance <- function(x, ...) {
    cat(x$print_string)
}

#' Generate print string
#'
#' @param check_id A twCompliance object.
#'
gen_print <- function(check_id) {
    if ("user_id" %in% names(check_id$data)) {
        n_users <- format(length(unique(check_id$data$user_id)), big.mark = ",")
        type <- "users"
    }
    if ("status_id" %in% names(check_id$data)) {
        n_tweets <- format(length(unique(check_id$data$status_id)), big.mark = ",")
        type <- "tweets"
    }

    if ("user_id" %in% names(check_id$data) & ("status_id" %in% names(check_id$data))) {
        return(sprintf(
            "A twCompliance check for\n\t %s tweets\n\t %s users\nStarted at %s",
            n_tweets,
            n_users,
            check_id$checked_at
        ))
    } else {
        if (type == "users"){n <- n_users}
        if (type == "tweets"){n <- n_tweets}
        return(sprintf(
            "A twCompliance check for\n\t %s %s\nStarted at %s",
            n,
            type,
            check_id$checked_at
        ))
    }

}
report_text <- function(df, type = "tweets") {
    tbl <- df %>%
        dplyr::count(online_status, sort = T) %>%
        dplyr::mutate(p = round((n/sum(n))*100, 2)) %>%
        dplyr::select(-n) %>%
        tidyr::pivot_wider(names_from = online_status, values_from = p)

    report_list <- c(sprintf("On %s", format(lubridate::date(unique(df$checked_at)), "%b %d, %Y")))

    if ("online" %in% names(tbl)) {
        if (length(names(tbl)) == 1) {
            report_list <- c(report_list, sprintf("all %s were still available through the API.", type))
            return(paste(report_list, collapse = " "))
        }
        report_list <- c(report_list, paste0(tbl$online, "% of all ", type))
    } else {
        report_list <- c(report_list, "no ", type)
    }
    report_list <- c(report_list, "were still available through the API. Unavailability was due to")

    unavailability_list <- c()
    unavailability_reasons <- names(tbl[names(tbl) != "online"])
    for (x in unavailability_reasons) {
        if (x == "suspended") {
            unavailability_list <- c(unavailability_list, paste0("account suspension (", tbl[[x]], "%)"))
        } else if (x == "user_deleted") {
            unavailability_list <- c(unavailability_list, paste0("deleted user accounts (", tbl[[x]], "%)"))
        } else if (x == "tweet_deleted") {
            unavailability_list <- c(unavailability_list, paste0("deleted tweets (", tbl[[x]], "%)"))
        } else if (x == "protected") {
            unavailability_list <- c(unavailability_list, paste0("protected user accounts (", tbl[[x]], "%)"))
        } else if (x == "deactivated") {
            unavailability_list <- c(unavailability_list, paste0("deactivated user accounts (", tbl[[x]], "%)"))
        }
    }
    if (length(unavailability_reasons) == 2) {
        unavailability_list <- paste0(unavailability_list, collapse = " and ")
    }  else if (length(unavailability_reasons) > 2) {
        unavailability_list <- paste0(
            paste(unavailability_list[1:length(unavailability_list)-1], collapse = ", "),
            ", and ",
            unavailability_list[length(unavailability_list)]
        )
    }
    if (any(c("created_at", "account_created_at") %in% names(df))) {
        if ("account_created_at" %in% names(df) & !"created_at" %in% names(df)) {
            df <- df %>% dplyr::rename(created_at = account_created_at)
        }
        tbl_age <- df %>%
            dplyr::filter(online_status != "online") %>%
            dplyr::select(created_at, checked_at) %>%
            dplyr::mutate(delta = checked_at-created_at) %>%
            dplyr::summarise(M = mean(delta), MIN = min(delta), MAX = max(delta))
        report_age <- sprintf("At the time of checking, the %s were on average %s %s old.", type, round(tbl_age$M, 0), units(tbl_age$M))

        p_has_redacted <- df %>% dplyr::filter(online_status != "online") %>% dplyr::summarise(p = mean(!is.na(redacted_at)))
        tbl_delta <- df %>%
            dplyr::filter(online_status != "online") %>%
            dplyr::mutate(delta = redacted_at - created_at) %>%
            dplyr::filter(!is.na(delta)) %>%
            dplyr::summarise(M = mean(delta), MIN = min(delta), MAX = max(delta))
        report_delta <- sprintf(
            " The %s percent of %s where an redacted_at time was available, were on average made unavailabe %s %s after creation (MIN = %s %s, MAX = %s %s)",
            round(p_has_redacted*100, 2),
            type,
            round(tbl_delta$M, 0),
            units(tbl_delta$M),
            round(tbl_delta$MIN, 0),
            units(tbl_delta$MIN),
            round(tbl_delta$MAX, 0),
            units(tbl_delta$MAX)
        )
        return(paste0(paste(c(report_list, unavailability_list), collapse = " "), ". ", report_age, report_delta, "\n"))
    } else {
        return(paste0(paste(c(report_list, unavailability_list), collapse = " "), ".\n"))
    }
}
#' @importFrom patchwork plot_layout
report_plot <- function(df, type = "tweets", combine_plots = TRUE) {

    clr_plt <- list(
        "online" = "#66a61e",
        "tweet_deleted" = "#d95f02",
        "suspended" = "#7570b3",
        "user_deleted" = "#e7298a",
        "deactivated" = "#e6ab02",
        "protected" = "#a6761d"
    )
    clr_plt <- clr_plt[names(clr_plt) %in% unique(df$online_status)]

    if (type == "users") {
        viz <- dplyr::rename(df, created_at = account_created_at) |> dplyr::filter(!is.na(created_at))
    } else {
        viz <- dplyr::filter(df, !is.na(created_at))
    }

    time_span <- max(viz$created_at, na.rm = T) - min(viz$created_at, na.rm = T)
    time_span_units <- units(time_span)
    if (time_span_units == "days") {
        if (time_span > 365*7) {
            viz <- viz |>
                dplyr::mutate(y = format(created_at, "%Y")) |>
                dplyr::mutate(w = format(created_at, "%U")) |>
                dplyr::mutate(created_at = lubridate::ymd(paste0(y,"-01-01")) + lubridate::weeks(w))
            time_span_units <- "weeks"
        }
        viz <- dplyr::mutate(viz, created_at = lubridate::date(created_at))
    } else if (time_span_units == "hours") {
        viz <- dplyr::mutate(viz, created_at = as.POSIXct(format(created_at, "%Y-%m-%dT%H:00:00"), format = "%Y-%m-%dT%H:%M:%S"))
    } else if (time_span_units == "minutes") {
        viz <- dplyr::mutate(viz, created_at = as.POSIXct(format(created_at, "%Y-%m-%dT%H:%M:00"), format = "%Y-%m-%dT%H:%M:%S"))
    }

    fig.1 <- viz |>
        dplyr::group_by(created_at) |>
        dplyr::count(online_status) |>
        dplyr::ungroup() |>
        tidyr::pivot_wider(names_from = online_status, values_from = n, values_fill = 0) |>
        tidyr::pivot_longer(-created_at, names_to = "online_status", values_to = "n") |>
        dplyr::mutate(online_status = factor(online_status, levels = c(
            "online",
            "tweet_deleted",
            "suspended",
            "user_deleted",
            "deactivated",
            "protected"
        ))) |>
        ggplot2::ggplot(ggplot2::aes(x = created_at, y = n, fill = online_status)) +
        ggplot2::geom_area(position = "stack") +
        ggplot2::scale_fill_manual(values = clr_plt) +
        ggplot2::labs(
            title = "Absolute counts by time of publishing",
            subtitle = sprintf("Aggregated by %s", time_span_units),
            y = "Count",
            x = sprintf("Created at (by %s)", time_span_units),
            fill = "Online Status"
        )

    fig.2 <- viz |>
        dplyr::group_by(created_at) |>
        dplyr::count(online_status) |>
        dplyr::mutate(p = n/sum(n)) |>
        dplyr::ungroup() |>
        dplyr::select(-n) |>
        tidyr::pivot_wider(names_from = online_status, values_from = p, values_fill = 0) |>
        tidyr::pivot_longer(-created_at, names_to = "online_status", values_to = "p") |>
        dplyr::mutate(online_status = factor(online_status, levels = c(
            "online",
            "tweet_deleted",
            "suspended",
            "user_deleted",
            "deactivated",
            "protected"
        ))) |>
        ggplot2::ggplot(ggplot2::aes(x = created_at, y = p, fill = online_status)) +
        ggplot2::geom_area(position = "stack") +
        ggplot2::scale_fill_manual(values = clr_plt) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(
            title = "Relative shares by time of publishing",
            subtitle = sprintf("Aggregated by %s", time_span_units),
            y = "Share",
            x = sprintf("Created at (by %s)", time_span_units),
            fill = "Online Status"
        )

    time_span_units <- units(as.POSIXct(viz$redacted_at) - as.POSIXct(viz$created_at))
    p_redacted_at <- format(mean(!is.na(viz$redacted_at))*100, digits = 2, nsmall = 2)

    quantiles_delta <- stats::quantile(as.numeric(as.POSIXct(viz$redacted_at) - as.POSIXct(viz$created_at)), c(.25, .5, .75), na.rm = T)

    fig.3 <- viz |>
        dplyr::filter(!is.na(redacted_at)) |>
        dplyr::filter(online_status != "online") |>
        dplyr::mutate(delta = as.numeric(as.POSIXct(redacted_at) - as.POSIXct(created_at))) |>
        ggplot2::ggplot(ggplot2::aes(x = delta)) +
        ggplot2::geom_density() +
        ggplot2::geom_vline(ggplot2::aes(xintercept = quantiles_delta[1]), linetype = "dotted") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = quantiles_delta[2]), size = 1, linetype = "dotted") +
        ggplot2::geom_vline(ggplot2::aes(xintercept = quantiles_delta[3]), linetype = "dotted") +
        ggplot2::scale_x_continuous() +
        ggplot2::labs(
            title = sprintf("Time difference between creation and retraction of %s", type),
            subtitle = sprintf(
                "Redaction time information available for %s%% of %s. \nDotted lines represent 25%%, 50%%, and 75%% quantiles.",
                p_redacted_at, type
            ),
            x = sprintf("Time difference (%s)", time_span_units),
            y = "Density"
        )
    if (combine_plots == TRUE) {
        fig.1 <- (fig.1 + ggplot2::theme(legend.position = "none"))
        fig.2 <- (fig.2 + ggplot2::theme(legend.position = "bottom", legend.box = "horizontal"))
        fig <- (fig.1 / fig.2 / fig.3)
        fig + patchwork::plot_annotation(
            title = sprintf("Online status of %s", type)
        )
    } else {
        return(list("abs_counts"=fig.1, "rel_shares"=fig.2, "redacted_delta"=fig.3))
    }
}
