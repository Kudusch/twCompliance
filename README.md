# twCompliance

An R-package to query the [compliance-endpoint of the Twitter API v2](https://developer.twitter.com/en/docs/twitter-api/compliance/batch-compliance/introduction).

This package provides easy to use functions to check if a set of tweets (and/or users) are available through the Twitter API or if they have been deleted, suspended, deactivated, or otherwise made unavailable. 

Providing a dataset of status and/or user ids, researchers with access to the [“academic research product track”](https://developer.twitter.com/en/products/twitter-api/academic-research) can easily check the compliance status of a historical dataset and generate a report and optional graphics on the missing tweets/users.

This goes beyond a simple dehydration of tweets (e.g. with [`academictwitteR::hydrate_tweets()`](https://rdrr.io/cran/academictwitteR/man/hydrate_tweets.html), as the `GET /2/tweets` endpoint only returns `Not Found Error` or `Authorization Error` and does not differentiate between deleted tweets or deleted, suspended, protected, or deactivated user accounts.

One caveat to be noted when using this package is that for complete data on unavailable tweets, you have to supply both tweet and user ids. This is because the compliance-endpoint does not record a tweet to be deleted, when the whole account has been deleted, suspended, deactivated or set to protected. So, when possible, supply both `user_id` and `status_id` when checking the unavailability of tweets with this package.

## Example usage

Install the package via `devtools` from this repo.

```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("Kudusch/twCompliance@main")
```

## Example usage

First, you need to set your API bearer token. To do that, just call the `set_bearer()` function and follow the instructions.

```r 
> head(df.example_data)
# A tibble: 6 × 4
  status_id  user_id   created_at          account_created_at 
  <chr>      <chr>     <dttm>              <dttm>             
1 115999…    331889…   2019-08-10 00:57:39 2015-08-18 16:05:16
2 116096…    846248…   2019-08-12 17:34:43 2012-09-25 21:26:35
3 116079…    727327…   2019-08-12 06:23:51 2016-05-03 02:41:50
4 116002…    965768…   2019-08-10 03:04:19 2018-02-20 02:03:48
5 116512…    180266…   2019-08-24 04:40:35 2008-12-10 18:47:23
6 116068…    764950…   2019-08-11 22:47:13 2016-08-14 22:21:50
```

Given a data.frame of tweets with the according `status_id`, `user_id`, `created_at`, and `account_created_at` column, we can use the `start_compliance_check()` function to start a compliance check.

```r
> check_id.example_data <- start_compliance_check(df.example_data)
ℹ Starting compliance check and uploading data (this might take a while)
✓ Compliance check started successfully:
  Tweets: '785,347'
  Users: '329,997'
> check_id.example_data
A twCompliance check for
	 785,347 tweets
	 329,997 users
Started at 2022-05-16 12:08:28
```

The returned object stores the ids needed to download the compliance data. The compliance check now runs in the background on Twitter’s servers and you are free to spend the time waiting anyway you want. The time it takes for the checks to be done varies, but might take up to an hour.

You can check the progress on the compliance check by calling `download_compliance_check()` on the check_id object returned from `start_compliance_check()` like so:

```r
> check_df.example_data <- download_compliance_check(check_id.example_data)
ℹ Jobs are not ready yet
  
Job ID: '15265…'
  Type: 'tweets'
  Created at: 2 minutes ago
  Status: 'in_progress'
  Expires in: 168 hours

Job ID: '15265…'
  Type: 'users'
  Created at: 2 minutes ago
  Status: 'in_progress'
  Expires in: 168 hours
```

You can see that there are two checks running simultaneously (you can only have one check running per type (tweets/users). As long as any check has the status `in_progress`, you’ll have to wait a bit more. But as you can see, the checks expire only after a week, so you can download the data at a later time (just make sure you save the check_id object with `saveRDS()`).

When both checks are done, just call `download_compliance_check()` again and save the resulting data.frame:

```r
check_df.example_data <- download_compliance_check(check_id.example_data)
```

The resulting data.frame includes the original tweets you checked and three new columns: `online_status`, `redacted_at`, and `checked_at`.

- `online_status` is either “online” (the tweet is still available through the API), “suspended” (the account was suspended by Twitter),“tweet_deleted” (the tweet was deleted by the user), “account_deleted” (the whole account was deleted by the user, “protected” (the account was set to protected by the user), or “deactivated” (the account was deactivated).
- If available `redacted_at` will give you the datetime when the tweet was made unavailable. Sadly, not all compliance events will include this information.
- Lastly, `checked_at` is the datetime when the compliance check was started.

Using `report_compliance()` you can now generate a short textual report to include in a paper or data statement:

```r
> report_compliance(check_df.example_data, plot = TRUE)
On May 16, 2022 57.47% of all tweets were still available through the API. Unavailability was due to account suspension (25.33%), deleted user accounts (7.76%), deleted tweets (6.15%), protected user accounts (2.93%), and deactivated user accounts (0.04%). At the time of checking, the tweets were on average 1006 days old. The 33.57 percent of tweets where an redacted_at time was available, were on average made unavailabe 418 days after creation (MIN = 29 days, MAX = 1007 days).
```

Setting `plot = TRUE` returns a visual representation of that data as well:

![Compliance plot](https://kudusch.de/projects/uploads/files/plot.png)


## Compliance status and sampling bias

Before the introduction of the Twitter API V2 it was only possible to ascertain *if* a specific tweet or account was still publicly available through the API, but not *why* it was made unavailable. There was no distinction between users deleting their tweets, setting their whole account to private or Twitter suspending an account.

This has changed with the `GET /2/compliance/` endpoint of the current beta-release of the Twitter API V2. This endpoint enables researchers to “upload large datasets of Tweet or user IDs to retrieve their compliance status”. The term “compliance status” refers to the possible reasons a tweet or account could be unavailable through the API and are limited to be any of 1) deleted, 2) deactivated, 3) protected, 4) suspended, or 5) scrub_geo. Compliance status 1) – 4) all mean that a tweet or an account are completely missing from any API response, while status 5) indicates that a user has removed the geo information from a tweet or account.

To give researchers easier access to this API endpoint, we introduce twCompliance. With this R-package researchers can start, manage and download multiple compliance jobs through with Twitter API with just a few lines of R code. As the `/2/compliance/` endpoint is relatively complicated and requires users to make multiple API requests, involving one to create the compliance job, a `POST` request to upload the tweet status or user ids, and one to download the corresponding results, many researchers that do not have a background in programming might not be able to use this API endpoint at all. 

With twCompliance researcher with access to the “academic research product track” can simply pass a data frame of Twitter status_id and user_id to the start_compliance_check() function, while the package handles to creation and management of the corresponding compliance checks. The resulting data frame includes the compliance status for each tweet, the reason why the tweet is no longer available through the API, and if available, when the tweet was retracted from public access (see Figure 1). Combining the multiple responses from the API, the tool also choses the correct reason for why a tweet was made unavailable for the first time (e.g., a user first deletes a tweet and gets their account suspended a year later). The tool also generates simple statements that can be used to report the amount of missing content in Twitter data sets (e.g., “On October 28, 2021 59.38% of all tweets are still available through the API. Unavailability was due to account suspension (24.68%), deleted user accounts (9.30%), deleted tweets (3.81%), protected user accounts (2.72%) and deactivated user accounts (0.11%)”, see Figure 2). Additionally, the package generates simple graphs to explore the compliance data (see Figure 3).

## Why to use

There are three reasons why researchers working with Twitter data can benefit from using twCompliance: a transparency reason, a methodological reason and lastly an ethical reason. First, from an open science perspective, it is important that researchers understand if the data they analyze could still be accessed by other researchers to reproduce their findings. As Twitter prohibits sharing full datasets and limits researchers to sharing only the status ids of tweets, other researchers will not have access to any tweets that are no longer accessible through the API. Researchers sharing their data like this should be aware of the amount of deleted content in their data to transparently communicate these findings with other researchers interested in reanalyzing their data. Reporting the amount of deleted content could become a common practice in studies using historical Twitter data, as it not only makes the reproduction of these studies easier, but also gives readers a better understanding of the analyzed data (e.g. a large share of suspended accounts can be evidence of spam activity or an orchestrated propaganda attack that Twitter has dealt with by suspending offending accounts (Author et al., 2019)).

Second, being aware of the deletion of content in historical datasets can aid the study of the sampling biases introduced by deleted content as outlined above. This methodological study of deleted content will gain relevance as many researchers will gain access to historical Twitter data through the “academic research product track” and current research on sampling biases caused by deleted content it is still scarce. twCompliance will aid researchers in better understanding this phenomenon and how to address it. Some early research has shown how certain content is over- or underrepresented in historical data. For example, political content (e.g., tweets mentioning a political actor) is deleted less often, while hate speech gets deleted at a higher rate (Author, in press). Thus, a historical dataset will contain more political content and less hate speech than a dataset gathered in real-time.

Lastly, there is an ethical component to consider. The “Twitter Developer Agreement and Policy” states that “[i]f you store Twitter Content offline, you must keep it up to date with the current state of that content on Twitter. Specifically, you must delete or modify any content you have if it is deleted or modified on Twitter” (Twitter, 2021). To be in compliance with Twitter’s terms of service, researchers would either need to delete their Twitter data immediately after they are analyzed or use the compliance API endpoint to regular check if content was deleted and delete that local content accordingly. As both options go against the common practices of research data management and could potentially hinder future research (e.g. analyzing accounts that have been suspended for the spreading of disinformation would not be possible under direct compliance with Twitter’s “Developer Agreement and Policy”), researchers have to find a way to deal with this constraint and twCompliance gives them the necessary information to do so.
twCompliance offers a user-friendly way to use Twitter’s GET /2/compliance/ API endpoint and can be used to tackle the methodological and ethical issues arising from deleted content.


