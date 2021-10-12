start_compliance_check <- function(data) {
    var_list <- names(data)

    if (length(var_list) > 2) {stop("Wrong format")}

    if (!any(c("status_id", "user_id") %in% var_list)) {
        stop("Wrong format")
    }

    r <- httr::GET(
        sprintf("https://api.twitter.com/2/compliance/jobs?type=%s", "users"),
        httr::add_headers(
            "Authorization" = paste0("Bearer ", get_bearer())
        )
    )
    if ("in_progress" %in% (unlist(lapply(httr::content(r)$data, function(x){x[["status"]]})))) {
        stop(sprintf("Cannot start a new job, as there is an ongoing job"))
    }

    r <- httr::GET(
        sprintf("https://api.twitter.com/2/compliance/jobs?type=%s", "tweets"),
        httr::add_headers(
            "Authorization" = paste0("Bearer ", get_bearer())
        )
    )
    if ("in_progress" %in% (unlist(lapply(httr::content(r)$data, function(x){x[["status"]]})))) {
        stop(sprintf("Cannot start a new job, as there is an ongoing job"))
    }

    if ("status_id" %in% var_list) {
        tweet_job_id <- start_job(data$status_id, type = "tweets", verbose = FALSE)
    }

    if ("user_id" %in% var_list) {
        user_job_id <- start_job(data$user_id, type = "users", verbose = FALSE)
    }
    return(list("tweets"=tweet_job_id, "users"=user_job_id))
}

combine_status <- function(data, job_id) {
    downloaded_jobs <- check_job(job_id, verbose = FALSE)
    if (typeof(downloaded_jobs) != "list") {
        usethis::ui_info("Jobs are not ready yet\n\n")
        check_job(job_id$tweets)
        check_job(job_id$users)
    } else {
        combinded_data <- combine_jobs(data, downloaded_jobs$tweets, downloaded_jobs$users)
        return(combinded_data)
    }
}


