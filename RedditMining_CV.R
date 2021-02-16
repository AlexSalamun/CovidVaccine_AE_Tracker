# COVID Vaccine Step 2
# Text mining

library(tidyverse)
library(httr)
library(jsonlite)
library(rvest)
library(tidytext)
library(qdapDictionaries)

# Grab all the information from SIDER to create a potential side effect dictionary
# searching by drug name can be removed
############################### Download the MedDRA data #############################################

# side effects
se.url <- "http://sideeffects.embl.de/media/download/meddra_all_se.tsv.gz"
tmp <- tempfile()
download.file(se.url, tmp)
data <- read.csv(
  gzfile(tmp),
  sep="\t",
  header=F,
  stringsAsFactors=FALSE)

SideEffects <- data
names(SideEffects) <- c("Flat_ID","Stereo_ID", "UMLS ID for Label",
                        "MedDRA Concept Type", "UMLS ID for MedDRA", "Side Effect Name")


# indications
ind.url <- "http://sideeffects.embl.de/media/download/meddra_all_indications.tsv.gz"
tmp <- tempfile()
download.file(ind.url, tmp)
data <- read.csv(
  gzfile(tmp),
  sep="\t",
  header=F,
  stringsAsFactors=FALSE)

Indications <- data
names(Indications) <- c("Flat_ID","UMLS ID for Label", "Method of Detection",
                        "Concept Name", "MedDRA Concept Type", "UMLS ID for MedDRA", 
                        "MedDRA Concept Name")


# Drug names
drugs.url <- "http://sideeffects.embl.de/media/download/drug_names.tsv"
tmp <- tempfile()
download.file(drugs.url, tmp)
data <- read.csv(
  gzfile(tmp),
  sep="\t",
  header=F,
  stringsAsFactors=FALSE)

DrugNames <- data
names(DrugNames) <- c("Flat_ID", "Drug Name")

DrugIndications <- DrugNames %>%
  inner_join(y = Indications, by = "Flat_ID") %>%
  select(c(Drug.Name = `Drug Name`, "Name" = `MedDRA Concept Name`)) %>%
  mutate(Association = "Indication") %>%
  unique()

DrugSideEffects <- DrugNames %>%
  inner_join(y = SideEffects, by = "Flat_ID") %>%
  select(c(Drug.Name = `Drug Name`, "Name" = `Side Effect Name`)) %>%
  mutate(Association = "Side Effect") %>%
  unique() %>%
  arrange(Drug.Name)

AllDrugVocab <- DrugIndications %>%
  union(DrugSideEffects) %>%
  mutate(Name = str_to_lower(Name)) %>%
  arrange(Name)
oe_words <- AllDrugVocab %>%
  filter(grepl(pattern = "oe", x = Name, ignore.case = T))
oe_words$Name <- gsub(pattern = "oe", replacement = "e", x = oe_words$Name)
AllDrugVocab <- rbind(AllDrugVocab, oe_words)

ae_words <- AllDrugVocab %>%
  filter(grepl(pattern = "ae", x = Name, ignore.case = T)) 
ae_words$Name <- gsub(pattern = "ae", replacement = "e", x = ae_words$Name)
AllDrugVocab <- rbind(AllDrugVocab, ae_words) %>%
  select(c(Name, Association)) %>%
  distinct()

n_occurs <- data.frame(table(AllDrugVocab$Name))
AllDrugVocab$Association[AllDrugVocab$Name %in% n_occurs$Var1[n_occurs$Freq > 1]] <- "Mix"
AllDrugVocab <- AllDrugVocab %>% unique()

# No need for the UMLS or RX Norm data since there are only the specific vaccine names
################################# Next step is to create the reddit data and add it to this list ########################
# scrape the list of medical related subreddits
# this list will be looped through as a targeted way to reduce query time

# updating the list of subreddits in response to the trials of the Covid Vaccine
url <- 'https://www.reddit.com/r/ListOfSubreddits/wiki/health'
subreddits <- read_html(url) %>%
  html_nodes(xpath = '//ul') %>%
  html_text() %>%
  tail(1) %>%
  strsplit(split = '\n') %>%
  unlist() %>%
  str_remove("/r/|'") %>%
  c('cancer',
    'MultipleSclerosis',
    'rheumatoid',
    'CrohnsDisease',
    'Asthma',
    'testicularcancer',
    'Parkinsons',
    'Hashimotos',
    'Alzheimers',
    'breastcancer',
    'braincancer',
    'pancreaticcancer',
    'lymphoma',
    'leukemia',
    'kidney',
    'multiplemyeloma',
    'thyroidcancer',
    'lungcancer',
    'skincancer',
    'adverseeffects',
    'braincancer',
    'CovidVaccine',
    'Covid19',
    'Coronavirus',
    'Vaccines',
    'CovidVax',
    'AMA',
    'casualiama'
  ) %>%
  toupper() %>%
  unique() %>%
  data.frame() %>%
  filter(!grepl("fit|supp|run|lbs|veg|sport|iron|beaut|lift|paleo|nat|juic|form|protein|border|fat|cong",.,ignore.case = TRUE)) %>%
  select(Subreddits = ".")
subreddits <- gsub(pattern = " ", "", x = subreddits$Subreddits)


# create the drug_names vector
drugnames <- c(covid_vaccines[1],
               paste0(manufacturers[1]," covid vaccine"),
               paste0(manufacturers[1], " vaccine trial"),
               paste0(manufacturers[1]," covid-19 trial"),
               paste0(manufacturers[1]," covid-19 vaccine"))
drugnames <- str_replace_all(drugnames, " ","%20")

#############################
# Now search through all of Reddit to find possible mentions
post_ids <- character()
products <- character()
thread_poster <- character()
reddit_searches <- character()

for (j in 1:length(drugnames)) {
  drug_name <- str_to_lower(drugnames[j])
  for (i in 1:length(subreddits)){
    url <- paste0("https://www.reddit.com/r/",subreddits[i],"/search.json?q=",drug_name,"&limit=100&sort=new")
    query <- GET(url = url, user_agent(user_agent))
    queryContent <- checkQuery(query)
    query.df <- fromJSON(queryContent)
    if(length(query.df$data$children) == 0) {
      next
    }
    if(query.df$data$dist == 100) {
      querySize <- as.numeric(query.df$data$dist)
      iter <- 0
      while (querySize == 100) {
        iter = iter + 1
        url <- paste0("https://www.reddit.com/r/",subreddits[i],"/search.json?q=",drug_name,
                      "&restrict_sr=1&limit=100&sort=new&after=",query.df$data$after)
        query <- GET(url = url, user_agent(user_agent))
        queryContent <- checkQuery(query)
        query.df <- fromJSON(queryContent)
        querySize = as.numeric(query.df$data$dist)
        post_ids <- c(post_ids, query.df$data$children$data$id)
        thread_poster <- c(thread_poster, query.df$data$children$data$author)
        products <- c(products, rep(drug_name, length(query.df$data$children$data$name)))
        reddit_searches <- c(reddit_searches, rep(subreddits[i], length(query.df$data$children$data$name)))
      }
      next
    }
    post_ids <- c(post_ids, query.df$data$children$data$id)
    thread_poster <- c(thread_poster, query.df$data$children$data$author)
    products <- c(products, rep(drug_name, length(query.df$data$children$data$name)))
    reddit_searches <- c(reddit_searches, rep(subreddits[i], length(query.df$data$children$data$name)))
  }
}

threads <- character()
posttitles <- character()

# loop through every post and get the content out of it
# length(post_ids)
for (i in 1:length(post_ids)) {
  comments.url <- paste0("https://www.reddit.com/comments/",post_ids[i],".json")
  comments.query <- GET(url = comments.url, user_agent(user_agent))
  comments.queryContent <- checkQuery(comments.query)
  comments.query.df <- fromJSON(comments.queryContent)
  postTitle <- comments.query.df$data$children[[1]]$data$title
  postbody <- comments.query.df$data$children[[1]]$data$selftext
  comments <- comments.query.df$data$children[[2]]$data$body
  comments.true <- comments[!grepl("I am a bot", comments)]
  comments.responses <- comments.query.df$data$children[[2]]$data$replies
  responses <- comments.responses[lengths(comments.responses)>1]
  if (is.null(responses)) {
    Thread <- paste(postbody)
    posttitles <- c(posttitles, postTitle)
    threads <- c(threads, Thread)
    next
  }
  responseData <- unlist(responses) %>% data.frame() %>% select(responses = ".")
  UserResponseBody <- responseData["data.children.data.body",]
  OtherResponses <- responseData["data.children.data.replies.data.children.data.body",]
  UserThread <- paste(postbody, UserResponseBody, collapse = "")
  OtherThread <- paste(comments.true, OtherResponses, collapse = "")
  Thread <- paste(UserThread, OtherThread, collapse = "")
  threads <- c(threads, Thread)
  posttitles <- c(posttitles, postTitle)
}


# # separate out threads into their own data frame
Reddit.df <- data.frame(Post_ID = post_ids,
                        Product = products,
                        Subreddit = reddit_searches, 
                        Username = thread_poster, 
                        PostTitle = posttitles, 
                        Thread = threads)



