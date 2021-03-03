# Part 4
# Reddit data cleaning and processing 
# run this on the Reddit.df

# This portion will perform text cleaning on the reddit threads to prepare them for topic modeling
Reddit.df$Thread <- gsub(pattern = "(f|ht)tp(s?)://(.*)[.][a-z]+",replacement = "",x = Reddit.df$Thread)
Reddit.df$Thread <- gsub(pattern = "[[:punct:]]", replacement = "", x = Reddit.df$Thread)
Reddit.df$Thread <- gsub(pattern = "NA", replacement = "", x = Reddit.df$Thread)
Reddit.df$Thread <- gsub(pattern = "\\s+", replacement = " ", x = Reddit.df$Thread)
Reddit.df$Thread <- trimws(Reddit.df$Thread, "both")


# make a dataframe and vector version of the cleaned threads
thread.df <- Reddit.df$Thread %>% data.frame()

# Building a loop to check through different levels of ngrams 1 thru 6
entities <- data.frame()
for (i in 1:6) {
  windows.ngram <- unnest_tokens(tbl = thread.df,
                                 output = word, 
                                 input = ".", 
                                 token ="ngrams", 
                                 n = i, 
                                 drop = F,
                                 collapse = F) %>%
    mutate(word = tolower(word))
  entities <- rbind(entities, windows.ngram)
}
names(entities) <- c("thread", "word")

# create the stopwords
extra_stopwords <- data.frame(word = c("thank", "you", "character0",
                                       "app","get","think","will","name","can","dont",
                                       "list()","hi","i","i'm","pain"))
stopwords <- stop_words %>%
  select(-lexicon) %>%
  rbind(extra_stopwords) %>%
  distinct()

# create a word list of the GradyAugmented english dictionary
`%notin%` <- Negate(`%in%`)

grady <- data.frame(GradyAugmented) %>%
  mutate(Name = as.character(GradyAugmented)) %>%
  mutate(Association = "English") %>%
  subset(select = -GradyAugmented) %>%
  filter(Name %notin% AllDrugVocab$Name)

# All Drug Version
TotalDrugVocab <- AllDrugVocab %>%
  select(Name) %>% unique() %>%
  data.frame()

TotalDrugVocab <- TotalDrugVocab %>%
  distinct() %>%
  mutate(Name = str_to_lower(Name))

# Remove stopwords and filter to only the domain vocabulary
entity.match <- entities %>%
  anti_join(stopwords) %>%
  mutate(MedTerm = ifelse(word %in% TotalDrugVocab$Name, 1, 0)) %>%
  filter(nchar(word) >= 3) %>%
  filter(MedTerm == 1)


#partition the data from entity match
train.rows <- sample(rownames(entity.match), dim(entity.match)[1]*0.5)
train.df <- entity.match[train.rows, ]
validation.rows <- setdiff(rownames(entity.match),train.rows)
valid.df <- entity.match[validation.rows,]

# cast as a document term matrix
# create a training dtm for modeling the data
train_dtm <- train.df %>%
  count(word, thread) %>%
  cast_dtm(thread, word, n)

# create a testing dtm to determine the accuracy
valid_dtm <- valid.df %>%
  count(word, thread) %>%
  cast_dtm(thread, word, n)

# create a full dtm to be used after a k-value has been established
full_dtm <- entity.match %>%
  count(word, thread) %>%
  cast_dtm(thread, word, n)


