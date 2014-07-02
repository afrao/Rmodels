### Alexander Rao

#######################################
############### loading in the libraies
#######################################

library(tm)
library(rjson)
library(plyr)
library(SnowballC)
library(e1071)
library(RTextTools)
library(Matrix)
library(slam)
library(LiblineaR)
library(stargazer)


#######################################
############## loading some of my tools
#######################################

source("tools.R")




#######################################
############### loading in the data now
#######################################

##### this is to read in all the ham after testing
## ham.lines <- readLines("ham.txt")
## spam.lines <- readLines("spam.txt")

## just going to read in 100 lines for testing purposes
ham.lines <- readLines("ham.txt",n=100)
spam.lines <- readLines("spam.txt",n=100)

#######################################
################# manipulating the data
#######################################


### turning the data into a list
ham.l <- apply(as.matrix(ham.lines),1,fromJSON)
spam.l <- apply(as.matrix(spam.lines),1,fromJSON)


## then trimming the lists

trimmer <- function(x) {
    return(x$content)
}


## and creating the data frames

ham.df <- ldply(ham.l,trimmer)
spam.df <- ldply(spam.l,trimmer)
bigOne <- ldply(c(ham.l,spam.l),trimmer)

## making labels
n.ham <- length(ham.l)
n.spam <- length(spam.l)
labels <- c(rep(1,times = n.ham),rep(0,times = n.spam))



## creating the corpus

ham.corpus <- VCorpus(DataframeSource(ham.df))
spam.corpus <- VCorpus(DataframeSource(spam.df))

## making a single corpus from the big one

bigOne.corpus <- VCorpus(DataframeSource(bigOne))


## transforming the corpus

## white space
ham.corpus <- tm_map(ham.corpus, stripWhitespace)
spam.corpus <- tm_map(spam.corpus, stripWhitespace)
bigOne.corpus <- tm_map(bigOne.corpus, stripWhitespace)

## to lower case
ham.corpus <- tm_map(ham.corpus, content_transformer(tolower))
spam.corpus <- tm_map(spam.corpus, content_transformer(tolower))
bigOne.corpus <- tm_map(bigOne.corpus, content_transformer(tolower))

## removing stopwords
ham.corpus <- tm_map(ham.corpus, removeWords, stopwords("english"))
spam.corpus <- tm_map(spam.corpus, removeWords, stopwords("english"))
bigOne.corpus <- tm_map(bigOne.corpus, removeWords, stopwords("english"))

## stemming the document
tm_map(ham.corpus, stemDocument)
tm_map(spam.corpus, stemDocument)
tm_map(bigOne.corpus, stemDocument)

## creating a document term matrix
ham.dtm <- DocumentTermMatrix(ham.corpus)
spam.dtm <- DocumentTermMatrix(spam.corpus)
bigOne.dtm <- DocumentTermMatrix(bigOne.corpus)

## finding frequent terms that show up at least 10 times
findFreqTerms(ham.dtm,10)
findFreqTerms(spam.dtm,10)
findFreqTerms(bigOne.dtm,10)

## finding relationships between words
## first arg is the document term matrix
## second is the vector of terms that your interested in
## and third arg is the limit on the minimum correlation allowed
findAssocs(ham.dtm, "youtube", 0.5)
findAssocs(spam.dtm, "youtube", 0.5)
findAssocs(bigOne.dtm, "youtube", 0.5)





#######################################
#################### Building the Model
#######################################

## making the labels factors
fac.labs <- as.factor(labels)


## creating test and training sets
set.seed(2014)
percent.train <- .75

ham.train.indexs <- sample(1:n.ham,percent.train*n.ham)
spam.train.indexs <- n.ham + sample(1:n.spam,percent.train*n.spam)
train.indexs <- c(ham.train.indexs,spam.train.indexs)
test.indexs <- (1:(n.ham+n.spam))[-train.indexs]

## creating a container
container <- create_container(bigOne.dtm,fac.labs,trainSize=train.indexs,testSize=test.indexs,virgin=FALSE)

## creating the svm models
svm.model <- train_model(container, "SVM",kernal = "linear")
svm.model.2 <- train_model(container, "SVM",kernal = "linear",use_sgd = TRUE)


## all of the models

SVM <- train_model(container,"SVM")
GLMNET <- train_model(container,"GLMNET")
MAXENT <- train_model(container,"MAXENT")
SLDA <- train_model(container,"SLDA")
BOOSTING <- train_model(container,"BOOSTING")
BAGGING <- train_model(container,"BAGGING")
RF <- train_model(container,"RF")
NNET <- train_model(container,"NNET")
TREE <- train_model(container,"TREE")



SVM_CLASSIFY <- classify_model(container, SVM)
GLMNET_CLASSIFY <- classify_model(container, GLMNET)
MAXENT_CLASSIFY <- classify_model(container, MAXENT)
SLDA_CLASSIFY <- classify_model(container, SLDA)
BOOSTING_CLASSIFY <- classify_model(container, BOOSTING)
BAGGING_CLASSIFY <- classify_model(container, BAGGING)
RF_CLASSIFY <- classify_model(container, RF)
NNET_CLASSIFY <- classify_model(container, NNET)
TREE_CLASSIFY <- classify_model(container, TREE)



models <- list(svm.model,svm.model.2,MAXENT,SLDA,BOOSTING,BAGGING,RF,NNET,TREE)

#######################################
################### Doing some anaylsis
#######################################
## getting some kind of result
svm.result <- classify_model(container,svm.model)
svm.analytic  <- create_analytics(container, svm.result)
svm.doc       <- svm.analytic@document_summary
svm_spam.doc  <- svm.doc[svm.doc$MANUAL_CODE==0, ]
svm_ham.doc   <- svm.doc[svm.doc$MANUAL_CODE==1, ]
svm.true.pos  <- nrow(svm_spam.doc[svm_spam.doc$CONSENSUS_CODE==0,]) / nrow(svm_spam.doc)
svm.false.neg <- nrow(svm_spam.doc[svm_spam.doc$CONSENSUS_CODE==1,]) / nrow(svm_spam.doc)
svm.true.neg  <- nrow(svm_ham.doc[svm_ham.doc$CONSENSUS_CODE==1,]) / nrow(svm_ham.doc)
svm.false.pos <- nrow(svm_ham.doc[svm_ham.doc$CONSENSUS_CODE==0,]) / nrow(svm_ham.doc)


confusion.mat <- data.frame(svm.true.pos,svm.false.pos,svm.false.neg,svm.true.neg)
Make.sweet.table(confusion.mat,"svmConfusion.tex")

### the second model

svm.result <- classify_model(container,svm.model.2)
svm.analytic  <- create_analytics(container, svm.result)
svm.doc       <- svm.analytic@document_summary
svm_spam.doc  <- svm.doc[svm.doc$MANUAL_CODE==0, ]
svm_ham.doc   <- svm.doc[svm.doc$MANUAL_CODE==1, ]
svm.true.pos  <- nrow(svm_spam.doc[svm_spam.doc$CONSENSUS_CODE==0,]) / nrow(svm_spam.doc)
svm.false.neg <- nrow(svm_spam.doc[svm_spam.doc$CONSENSUS_CODE==1,]) / nrow(svm_spam.doc)
svm.true.neg  <- nrow(svm_ham.doc[svm_ham.doc$CONSENSUS_CODE==1,]) / nrow(svm_ham.doc)
svm.false.pos <- nrow(svm_ham.doc[svm_ham.doc$CONSENSUS_CODE==0,]) / nrow(svm_ham.doc)



confusion.mat <- data.frame(svm.true.pos,svm.false.pos,svm.false.neg,svm.true.neg)
Make.sweet.table(confusion.mat,"svmConfusion2.tex")


### making a loop

confusion.mat <- matrix(nrow = length(models),ncol= 4)
counter <- 0
for(mod in models){
    counter <- counter + 1

    mod.result <- classify_model(container,mod)
    mod.analytic  <- create_analytics(container, mod.result)
    mod.doc       <- mod.analytic@document_summary
    mod_spam.doc  <- mod.doc[mod.doc$MANUAL_CODE==0, ]
    mod_ham.doc   <- mod.doc[mod.doc$MANUAL_CODE==1, ]
    mod.true.pos  <- nrow(mod_spam.doc[mod_spam.doc$CONSENSUS_CODE==0,]) / nrow(mod_spam.doc)
    mod.false.neg <- nrow(mod_spam.doc[mod_spam.doc$CONSENSUS_CODE==1,]) / nrow(mod_spam.doc)
    mod.true.neg  <- nrow(mod_ham.doc[mod_ham.doc$CONSENSUS_CODE==1,]) / nrow(mod_ham.doc)
    mod.false.pos <- nrow(mod_ham.doc[mod_ham.doc$CONSENSUS_CODE==0,]) / nrow(mod_ham.doc)



    confusion.mat[counter,1] <- mod.true.pos
    confusion.mat[counter,2] <- mod.false.pos
    confusion.mat[counter,3] <- mod.false.neg
    confusion.mat[counter,4] <- mod.true.neg

}

rownames(confusion.mat) = c("svm1","svm.sgd","MAXENT","SLDA","BOOSTING","BAGGING","RF","NNET","TREE")
Make.sweet.table(confusion.mat,"confusions.tex")




