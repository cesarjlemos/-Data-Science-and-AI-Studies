###### Naive Bayes ######

# Library
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(caret)
library(e1071)

# Import dataset and transform
dt <- read_csv(choose.files())

dt$type <- as.factor(dt$type)

# Exploratory Analysis

## Check is.na
sapply(dt, function(x) sum(is.na(x)))

## Check number of spams
table(dt$type)
## Check number of spams in percentuals
round(prop.table(table(dt$type))*100,2)

# Data preparation - cleaning and standardizing text data
dt_corpus <- VCorpus(VectorSource(dt$text))

dt_corpus_clean <- tm_map(dt_corpus, content_transformer(tolower))
dt_corpus_clean <- tm_map(dt_corpus_clean, removeNumbers)
dt_corpus_clean <- tm_map(dt_corpus_clean, removeWords, stopwords())
dt_corpus_clean <- tm_map(dt_corpus_clean, removePunctuation)
dt_corpus_clean <- tm_map(dt_corpus_clean, stemDocument)
dt_corpus_clean <- tm_map(dt_corpus_clean, stripWhitespace)

dt_dtm <- DocumentTermMatrix(dt_corpus_clean)

dt_dtm2 <- DocumentTermMatrix(dt_corpus, control = list(
  tolower = T,
  removeNumbers = T,
  stopwords = T,
  removePunctuation = T,
  stemming = T
))

# Train and test data
log <- createDataPartition(dt$type, 0.7, list = F, times = 1)


train <- dt_dtm[log,]
test <- dt_dtm[-log,]

train_label <- dt[log,]$type
test_label <- dt[-log,]$type

prop.table(table(train_label))
prop.table(table(test_label))

pal = brewer.pal(9,"BuGn")
wordcloud(dt_corpus_clean, min.freq = 50, random.order = F, random.color = F, colors = pal)

freq_words <- findFreqTerms(train, 2)

freq_train <- train[,freq_words]
freq_test <- test[,freq_words]

convert_counts <- function(x){
  x <- ifelse(x>0,"yes","no")
}

dtm_train <- apply(freq_train, MARGIN = 2, convert_counts)
dtm_test <- apply(freq_test, MARGIN = 2, convert_counts)

sms_classifier <- naiveBayes(dtm_train, train_label)
pred <- predict(sms_classifier, dtm_test)

confusionMatrix(pred, test_label)
