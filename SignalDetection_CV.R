# Part 3
# Signal Detection Analysis for Covid Vaccines

library(httr)
library(jsonlite)

# Use the results of the Topic Modeling to identify adverse events
Effects <- Reddit.df %>%
  left_join(entity.match, by = c("Thread" = "thread")) %>%
  filter(word %in% AE_list) %>%
  group_by(Post_ID, word) %>%
  tally() %>%
  mutate(n = 1) %>%
  group_by(word) %>%
  tally()

## build the contingency table for each Side Effect from Reddit
reaction_names <- gsub(" ", replacement = "%20", x = Effects$word)
reaction_names <- str_to_upper(reaction_names)
reaction_counts <- numeric()

subreddit.string <- paste(subreddits, collapse = ",")


## This method uses the pushshift api, which can be buggy
## this method seems to return more robust results but can only be used when pushshift is up and running

endpoint <- "https://api.pushshift.io/reddit/search/submission/?"
for (j in 1:length(reaction_names)) {
  if (j %% 8 == 0) {
    Sys.sleep(30)
  }
  reaction <- reaction_names[j]
  print(reaction)
  url <- paste0(endpoint,"q=",reaction,"&subreddit=",subreddit.string,
                "&size=0&metadata=True")
  query <- GET(url = url)
  queryContent <- try(expr = content(query, "text", encoding = "UTF-8"),
                      silent =  TRUE)
  query.df <- fromJSON(queryContent, simplifyVector = T)
  reaction_counts <- c(reaction_counts, sum(query.df$metadata$total_results))
}

####################################################
# build D, contingency table value for no drug exposure and no adverse event

url <- paste0(endpoint,"subreddit=",subreddit.string,"&metadata=true&size=0")
query <- GET(url = url, user_agent("alexander.salamun@marquette.edu"))
queryContent <- checkQuery(query)
query.df <- fromJSON(queryContent)
totals <- query.df$metadata$total_results

drug_name <- covid_vaccines[1]

reddit.draft <- data.frame(Drug = rep(drug_name, length(reaction_names)),
                           AE = str_to_upper(Effects$word),
                           A = Effects$n,
                           B = reaction_counts - Effects$n,
                           C = rep(length(post_ids), length(reaction_names)) - Effects$n,
                           D = rep(totals, length(reaction_names))- reaction_counts - rep(length(post_ids), length(reaction_names))
)


# now calculate the EBGM values for the Reddit data
r.df <- data.frame(N = as.integer(),
                   E = as.numeric(),
                   RR = as.numeric(),
                   PRR = as.numeric(),
                   EBGM = as.numeric(),
                   EB05 = as.numeric(),
                   EB95 = as.numeric())

for (i in 1:length(reddit.draft$Drug)) {
  r.df <- rbind(r.df, EBGM(reddit.draft$A[i],reddit.draft$B[i],reddit.draft$C[i],reddit.draft$D[i]))
}

options(digits = 3, scipen = 999)
reddit.dpa <- data.frame(Drug = reddit.draft$Drug, 
                         AdverseEvent = reddit.draft$AE,
                         r.df) %>% arrange(desc(N)) %>%
  mutate("Signal Detected" = ifelse(EB05 >= 2.000,"*","")) %>%
  #filter(EB05 >= 2.000) %>%
  arrange(desc(EB05))
reddit.dpa <- reddit.dpa[,c(1:4,7:8,10)]

