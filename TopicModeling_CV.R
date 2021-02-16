# Topic Modeling of the Coronavirus Reports

# this portion will calculate the log likelihoods and perplexity scores for each amount of topics
# It will also generate visuals to identify best number of clusters for the elbow method
library(topicmodels)
library(caret)
library(e1071)
library(tidyverse)
library(tidytext)
library(dplyr)
start.time <- Sys.time()
perplexity_score <- as.numeric()
logLik_score <- as.numeric()
AIC_score <- as.numeric()
BIC_score <- as.numeric()
accuracy_scores <- as.numeric()

j <- 10
for (i in 2:j) {
  k <- i
  mod <- LDA(x=train_dtm, k = k,
             method = "Gibbs",
             control=list(alpha = 1, seed=123))
  logLik_score <- cbind(logLik_score, logLik(mod))
  AIC_score <- cbind(AIC_score, AIC(mod))
  BIC_score <- cbind(BIC_score, BIC(mod))
  perplexity_score <- cbind(perplexity_score,perplexity(object=mod, newdata=train_dtm))
  
  # add the accuracy into the loop
  sentence_probs <- tidy(mod, matrix = "gamma") %>%
    spread(topic, gamma)
  sentence_probs$Topic = as.factor(colnames(sentence_probs)[apply(sentence_probs,1,which.max)])
  
  # now apply the model to the validations set
  valid_topics <- posterior(object=mod, newdata=valid_dtm)
  valid.results <- valid_topics$topics
  sentence_valid <-  as.factor(colnames(valid.results)[apply(valid.results,1,which.max)])
  sentence_valid.results <- data.frame(sentences = rownames(valid.results),
                                       topic = sentence_valid,
                                       stringsAsFactors = F)
  
  combo <- sentence_probs %>%
    inner_join(y = sentence_valid.results, by = c("document" = "sentences")) %>%
    select(-c(2:k+1)) %>%
    filter(Topic != "document") 
  levels(combo$Topic)
  levels(combo$topic)
  
  if (length(levels(combo$topic)) > length(levels(combo$Topic))) {
    levels(combo$Topic) <- levels(combo$topic)
  } else {
    levels(combo$topic) <- levels(combo$Topic)
  }
  
  # now evaluate accuracy of the model
  accuracy_scores <- c(accuracy_scores, confusionMatrix(combo$Topic, combo$topic)$overall[1])
}


end.time <- Sys.time()
Gibbs_runtime <- end.time - start.time

k <- seq(2,j,1)
x <- 2
# observe perplexity
models <- data.frame(k, t(perplexity_score)) %>% rename("perplexity" = t.perplexity_score.)
ggplot(models, aes(x = k, y = perplexity)) +
  geom_point() +
  geom_line() +
  ggtitle("Perplexity Score by k-value") +
  scale_x_continuous(breaks = seq(2,j,1)) +
  scale_y_continuous(breaks = seq(min(perplexity_score),
                                  max(perplexity_score),
                                  (max(perplexity_score)-min(perplexity_score))/10),
                     limits = c(min(perplexity_score),max(perplexity_score))) +
  geom_point(aes(x = k[x], y = perplexity[x]), color = "red", size = 3)

(max(logLik_score)-min(logLik_score))/10

# observe log likelihood
logLik_models <- data.frame(k, t(logLik_score)) %>% rename("logLik" = t.logLik_score.)
ggplot(logLik_models, aes(x = k, y = logLik)) +
  geom_point() +
  geom_line() +
  ggtitle("Log Likelihood by k-value") +
  scale_x_continuous(breaks = seq(2,j,1)) +
  scale_y_continuous(breaks = seq(min(logLik_score),
                                  max(logLik_score),
                                  (max(logLik_score)-min(logLik_score))/10), 
                     limits = c(min(logLik_score),max(logLik_score))) +
  geom_point(aes(x = k[x], y = logLik[x]), color = "red", size = 3)

# observe accuracy
accuracy_models <- data.frame(k, accuracy_scores)
ggplot(accuracy_models, aes(x = k, y = accuracy_scores)) +
  geom_point() +
  geom_line() +
  ggtitle("Accuracy Score by k-value") +
  scale_x_continuous(breaks = seq(2,j,1)) +
  scale_y_continuous(breaks = seq(0,1,0.1), limits = c(0,1)) +
  geom_point(aes(x = k[x], y = accuracy_scores[x]), color = "red", size = 3)

# similar to the results from the previous scripting the results are estimating 3 different topics


library(forcats)
library(stringr)
k <- 3
mod <- LDA(full_dtm,
           k = k, 
           method = "Gibbs",
           control = list(alpha = 1, seed = 1234))

# identify the most likely topics for each word
word_max_probs <- tidy(mod, matrix = "beta") %>%
  spread(topic, beta) #%>%
#filter(term %in% drug.vocab$word)

word_max_probs$Topic = as.factor(colnames(word_max_probs)[apply(word_max_probs,1,which.max)])
word_max_probs$Topic.value = apply(word_max_probs[, 2:k+1], 1, max)

AdverseEventTopic <- word_max_probs %>%
  inner_join(TotalDrugVocab, by = c("term" = "Name")) %>%
  select(c("term","Topic", "Topic.value")) %>%
  arrange(Topic) %>%
  droplevels()

# visualize the words in each topic
word_probs <- AdverseEventTopic %>%
  group_by(Topic) %>%
  ungroup() %>%
  mutate(term2 = fct_reorder(term, Topic.value)) %>%
  group_by(Topic) %>%
  arrange(desc(Topic.value)) %>%
  filter(Topic.value >= 0.01)

#Add abstractions to the topics after examination
abstracts <- c("Mix","Adverse Events","Indication")

word_probs <- word_probs %>%
  mutate(Topic = case_when(
    Topic == 1 ~ abstracts[1],
    Topic == 2 ~ abstracts[2],
    Topic == 3 ~ abstracts[3]
  ))

AE_list <- word_probs$term[word_probs$Topic == "Adverse Events"]

# plot
ggplot(word_probs, aes(term2, Topic.value, fill = as.factor(Topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ Topic, scales = "free") +
  coord_flip() +
  xlab("") +
  ylab("") + 
  theme(axis.text.x = element_text(angle = 90))
