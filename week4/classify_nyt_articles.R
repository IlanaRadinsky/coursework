
library(tm)
library(Matrix)
library(glmnet)
library(ROCR)
library(ggplot2)
library(dplyr)

setwd("C:/MSR-DS3/coursework/week4")

# read business and world articles into one data frame
df1 <- read.delim(file = 'articles_business.tsv', sep = '\t', header = TRUE)
df2 <- read.delim(file = 'articles_world.tsv', sep = '\t', header = TRUE)
df <- rbind(df1, df2)

# create a Corpus from the article snippets
dfCorpus <- Corpus(VectorSource(df$snippet)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers)

# create a DocumentTermMatrix from the snippet Corpus
dtm <- DocumentTermMatrix(dfCorpus)

# remove punctuation and numbers

# convert the DocumentTermMatrix to a sparseMatrix, required by cv.glmnet
# helper function
dtm_to_sparse <- function(dtm) {
 sparseMatrix(i=dtm$i, j=dtm$j, x=dtm$v, dims=c(dtm$nrow, dtm$ncol), dimnames=dtm$dimnames)
}

sparse_matrix <- dtm_to_sparse(dtm)

# create a train / test split
inx <- sample(nrow(sparse_matrix), floor(0.8*nrow(sparse_matrix)))
X_train <- sparse_matrix[inx, ]
X_test <- sparse_matrix[-inx, ]

y <- df['section_name']
y_train <- y[inx, ]
y_test <- y[-inx, ]

# cross-validate logistic regression with cv.glmnet, measuring auc
model <- cv.glmnet(X_train, y_train, family = "binomial")
pred <- predict(model, newx=X_test, type="response", s="lambda.min")

# evaluate performance for the best-fit model
predictions <- prediction(pred, y_test)
roc.perf <- performance(predictions, "tpr", "fpr")
plot(roc.perf, print.auc=TRUE)

# plot ROC curve and output accuracy and AUC
acc.perf <- performance(predictions, measure="acc")
plot(acc.perf)

ind <- which.max( slot(acc.perf, "y.values")[[1]] )
acc <- slot(acc.perf, "y.values")[[1]][ind]
cutoff <- slot(acc.perf, "x.values")[[1]][ind]
print(c(accuracy= acc, cutoff = cutoff))

auc.perf <- performance(predictions, measure="auc")
auc <- auc.perf@y.values[[1]]
auc

# extract coefficients for words with non-zero weight
# helper function
get_informative_words <- function(crossval) {
  coefs <- coef(crossval, s="lambda.min")
  coefs <- as.data.frame(as.matrix(coefs))
  names(coefs) <- "weight"
  coefs$word <- row.names(coefs)
  row.names(coefs) <- NULL
  subset(coefs, weight != 0)
}

informative_words <- get_informative_words(model)
informative_words <- informative_words[informative_words$word != "(Intercept)", ]

View(informative_words)

# show weights on words with top 10 weights for business
top_bus <- informative_words %>%
  arrange(desc(weight)) %>%
  head(10)

# show weights on words with top 10 weights for world
top_world <- informative_words %>%
  arrange(weight) %>%
  head(10)

save(roc.perf, acc.perf, acc, cutoff, auc, top_bus, top_world, file="outputs.RData")
