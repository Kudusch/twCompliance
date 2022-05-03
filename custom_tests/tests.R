source("R/twCompliance.R")
source("R/manage_job.R")
source("R/get_bearer.R")

check_users <- readRDS("custom_tests/check_users.RDS")
check_tweets_users <- readRDS("custom_tests/check_tweets_users.RDS")

df.check_users <- download_compliance_check(check_users)
df.check_tweets_users <- download_compliance_check(check_tweets_users)

report_compliance(df.check_tweets_users)

tests <- c("tweets", "tweets_time", "tweets_users_time", "users", "users_time", "users_tweets")

for (test in tests) {
    message(sprintf("~/Desktop/test/Output/check_id.%s.RDS", test))
    job_id <- readRDS(sprintf("~/Desktop/test/Output/check_id.%s.RDS", test))
    downloaded_jobs <- readRDS(sprintf("~/Desktop/test/Output/check_raw.%s.RDS", test))

    all_tweets <- job_id$data
    compliance_tweets <- downloaded_jobs$tweets
    compliance_users <- downloaded_jobs$users

    df <- combine_jobs(all_tweets, compliance_tweets, compliance_users)
    df$checked_at <- job_id$checked_at
    saveRDS(df, sprintf("~/Desktop/test/Output/check_df.%s.RDS", test))
}


