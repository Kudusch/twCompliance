#' Start compliance job
#'
#' This function attempts to start a compliance job with a given list of tweet status ids.
#'
#' @return string ID of the compliance job. Save this to check the jobs status and download the results.
#'
#' @param ids A character vector containing the status ids of the tweets that will be checked.
#' @param type Currently only "tweets" is allowed here.
#' @param bearer_token API bearer token.
#'
#' @export
start_job <- function(
    ids,
    type="tweets",
    verbose=TRUE,
    bearer_token=get_bearer()
) {
    # check arguments
    if (!type %in% c("users", "tweets")) {stop("Wrong type")}
    r <- httr::GET(
        sprintf("https://api.twitter.com/2/compliance/jobs?type=%s", type),
        httr::add_headers(
            "Authorization" = paste0("Bearer ", bearer_token)
        )
    )

    if ("in_progress" %in% (unlist(lapply(httr::content(r)$data, function(x){x[["status"]]})))) {
        compliance_job <- httr::content(r)$data[[match("in_progress", (unlist(lapply(httr::content(r)$data, function(x){x[["status"]]}))))]]
        stop(sprintf("Cannot start a new job, as there is an ongoing job with id `%s`", compliance_job$id))
    }

    if (!"created" %in% (unlist(lapply(httr::content(r)$data, function(x){x[["status"]]})))) {
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
        compliance_job <- httr::content(r)$data[[match("created", (unlist(lapply(httr::content(r)$data, function(x){x[["status"]]}))))]]
    }

    f <- tempfile()
    con <- file(f)
    writeLines(ids, f)
    close(con)

    r <- httr::PUT(
        compliance_job$upload_url,
        httr::add_headers(
            "Content-Type" = "text/plain"
        ),
        body = httr::upload_file(f)
    )

    if (verbose) {
        if (httr::status_code(r) != 200) {
            stop("There was an error starting the compliance job")
        } else {
            usethis::ui_info("Job started with {usethis::ui_field('job ID')}: {usethis::ui_value(compliance_job$id)}")
            expires_id <- round(as.numeric(difftime(lubridate::fast_strptime(compliance_job$download_expires_at, "%Y-%m-%dT%H:%M:%OS%z"), lubridate::now(), units="hours")), 0)
            usethis::ui_line("  {usethis::ui_field('Expires in')}: {usethis::ui_value(expires_id)} hours")
            usethis::ui_line("You can check the job status with {usethis::ui_code(paste0('check_job(\"', compliance_job$id, '\")'))} and when it is ready download the data with {usethis::ui_code(paste0('download_job(\"', compliance_job$id, '\")'))}")
            return(compliance_job$id)
        }
    } else {
        return(compliance_job$id)
    }

}

#' Check status of a compliance job
#'
#' This function checks the status of a given compliance job.
#'
#' @return If `verbose` is set to FALSE, return the jobs download URL if ready or FALSE if still in progress.
#'
#' @param job_id A string containing the ID of the compliance job that will be checked.
#' @param bearer_token API bearer token.
#' @param verbose A logical setting the verbosity.
#'
#' @export
check_job <- function(
    job_id,
    bearer_token=get_bearer(),
    verbose=TRUE
) {
    if (typeof(job_id) == "list") {
        status_tweets <- check_job(job_id$tweets, verbose = FALSE)
        status_users <- check_job(job_id$users, verbose = FALSE)
        if (typeof(status_tweets) == "character" & typeof(status_users) == "character") {
            job_tweets <- download_job(job_id$tweets, verbose = FALSE)
            job_users <- download_job(job_id$users, verbose = FALSE)
            return(list("tweets"=job_tweets, "users"=job_users))
        } else {
            if (verbose) {
                usethis::ui_info("Job is not ready yet")
            } else {
                return(FALSE)
            }
        }
    } else {
        r <- httr::GET(
            sprintf("https://api.twitter.com/2/compliance/jobs/%s", job_id),
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


#' Download compliance job
#'
#' This function attempts to download a compliance job if it is ready to download.
#'
#' @return dataframe A dataframe of all returned tweets.
#'
#' @param job_id A string containing the ID of the compliance job that will be downloaded.
#'
#' @export
download_job <- function(
    job_id,
    verbose=FALSE
) {
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
    tweet_status <- jsonlite::fromJSON(paste0("[", paste(tweet_status, collapse = ", "), "]"))

    # return early if no items in response
    if (length(tweet_status) == 0) {
        if (verbose) {
            usethis::ui_oops("All tweets/users are still online and publicly reachable, nothing to download.")
        }
        return(NULL)
    }

    tweet_status <- tweet_status[!is.na(tweet_status["id"]),]
    tweet_status <- tweet_status[tweet_status["id"] != "",]

    rownames(tweet_status) <- NULL

    if (nrow(tweet_status) > 0) {

        tweet_status$redacted_at <- lubridate::fast_strptime(tweet_status$redacted_at, "%Y-%m-%dT%H:%M:%OS%z")
        tweet_status$redacted_at <- as.POSIXct(tweet_status$redacted_at)

        tweet_status$created_at <- lubridate::fast_strptime(tweet_status$created_at, "%Y-%m-%dT%H:%M:%OS%z")
        tweet_status$created_at <- as.POSIXct(tweet_status$created_at)

        return(tweet_status)
    } else {
        if (verbose) {
            usethis::ui_oops("All tweets/users are still online and publicly reachable, nothing to download.")
        }
        return(NULL)
    }
}


#' List all compliance jobs
#'
#' This function prints a list of all compliance jobs.
#'
#' @param bearer_token API bearer token.
#' @param type Only tweets is currently supported.
#'
#' @export
list_jobs <- function(
    type="tweets",
    bearer_token=get_bearer()
) {
    if (!type %in% c("users", "tweets")) {stop("Wrong type")}

    r <- httr::GET(
        sprintf("https://api.twitter.com/2/compliance/jobs?type=%s", type),
        httr::add_headers(
            "Authorization" = paste0("Bearer ", bearer_token)
        )
    )

    if (httr::status_code(r) == 200) {
        usethis::ui_info("{length(httr::content(r)$data)} jobs(s) of type {usethis::ui_value(type)} found")
        for (job in httr::content(r)$data) {
            print_job(job)
        }
    }

}

print_job <- function(job) {
    usethis::ui_line("{usethis::ui_field('Job ID')}: {usethis::ui_value(job$id)}")

    usethis::ui_line("  {usethis::ui_field('Type')}: {usethis::ui_value(job$type)}")

    created_at_minutes <- round(as.numeric(difftime(lubridate::now(), lubridate::fast_strptime(job$created_at, "%Y-%m-%dT%H:%M:%OS%z"), units="mins")), 0)
    created_at_hours <- round(as.numeric(difftime(lubridate::now(), lubridate::fast_strptime(job$created_at, "%Y-%m-%dT%H:%M:%OS%z"), units="hours")), 0)
    created_at_days <- round(as.numeric(difftime(lubridate::now(), lubridate::fast_strptime(job$created_at, "%Y-%m-%dT%H:%M:%OS%z"), units="days")), 0)

    if (created_at_minutes < 120) {
        usethis::ui_line("  {usethis::ui_field('Created at')}: {usethis::ui_value(created_at_minutes)} minutes ago")
    } else if (created_at_minutes < 48) {
        usethis::ui_line("  {usethis::ui_field('Created at')}: {usethis::ui_value(created_at_hours)} hours ago")
    } else {
        usethis::ui_line("  {usethis::ui_field('Created at')}: {usethis::ui_value(created_at_days)} days ago")
    }

    usethis::ui_line("  {usethis::ui_field('Status')}: {usethis::ui_value(job$status)}")
    if (job$status != "expired") {
        expires_id <- round(as.numeric(difftime(lubridate::fast_strptime(job$download_expires_at, "%Y-%m-%dT%H:%M:%OS%z"), lubridate::now(), units="hours")), 0)
        usethis::ui_line("  {usethis::ui_field('Expires in')}: {usethis::ui_value(expires_id)} hours")
    }
    usethis::ui_line("")
}
