library(RJDBC)
library(rjson)
library(rlist)
library(pipeR)
library(slam)
library(tm)


mycon <- odbcConnect("sql_server_native", "qiuqiu", "qq@20150118")
setwd("~/R_workspace/classify by topic/X-tfidf-svm")


# mycon <- odbcConnect("mysql4.107_htnewsroom", "root", "123456")
# ID <- 0
# data <- sqlQuery(mycon, paste("SELECT ID, content_wordseg FROM article where ID >", ID, sep = ""), stringsAsFactors = F)
# odbcClose(mycon)


# data <- data[data$site != "中证网", ]
# data <- data[grep("hexun.com", data$URL, invert = T), ]
# data <- data[data$site != "和讯网", ]
# data <- data[data$site != "大智慧", ]
# data <- data[data$site != "金融界", ]
# data <- data[data$site != "黄金网", ]
# data <- data[data$site != "新浪财经", ]

# set.seed <- 2015
# data0 <- subset(data, ID %in% sample(data$ID, 500))

# write.csv(data0, "testtrain.csv")


test.train <- read.csv("test_train.csv", stringsAsFactors = F)
# test.train <- test.train[c('ID', 'Category')]
# test.train$Category <- as.character(test.train$Category)
# test.train$content <- data$content_wordseg[match(test.train$ID, data$ID)]
# rm(data)

train <- list()
for (i in 1:length(test.train$ID)){
  try(train[[i]] <- fromJSON(test.train$content[i]), silent = T)
}
names(train) <- test.train$ID
train <- train[!sapply(train, is.null)]

encode <- c('UTF-8')
for (i in 1:length(train)){
  for(l in 1:length(train[[i]])){
    Encoding(train[[i]][[l]]) <- encode
    train[[i]][[l]] <- strsplit(train[[i]][[l]], split = ',')
    cat('-------', l, '-------\n')
  }
  cat('-------', i, '-------\n')
}

train.title <- list()
train.des <- list()
train.content <- list()
for(i in 1:length(train)){
  train.title[[i]] <- train[[i]]$title
  train.des[[i]] <- train[[i]]$description
  train.content[[i]] <- train[[i]]$content
}

## des 有许多为空，尝试title和content

corpus.train.title <- Corpus(VectorSource(train.title), readerControl = list(language = "ZHCN"))
# corpus.train.des <- Corpus(VectorSource(train.des), readerControl = list(language = "ZHCN"))
corpus.train.content <- Corpus(VectorSource(train.content), readerControl = list(language = "ZHCN"))
for (i in 1:length(corpus.train.title)){
  corpus.train.title[[i]]$content <- sub("c", "", corpus.train.title[[i]]$content)
}
for (i in 1:length(corpus.train.title)){
  meta(corpus.train.title[[i]], tag = 'id') <- test.train$ID[i]
}

# for (i in 1:length(corpus.train.des)){
#   corpus.train.des[[i]]$content <- sub("c", "", corpus.train.des[[i]]$)
# }
# for (i in 1:length(corpus.train.des)){
#   meta(corpus.train.des[[i]], tag = 'id') <- test.train$ID[i]
# }

for (i in 1:length(corpus.train.content)){
  corpus.train.content[[i]]$content <- sub("c", "", corpus.train.content[[i]]$content)
}
for (i in 1:length(corpus.train.content)){
  meta(corpus.train.content[[i]], tag = 'id') <- test.train$ID[i]
}


control.tfidf <- list(removePunctuation = T, removeNumbers = F, stripWhitespace = T, wordLengths = c(2, 10), weighting = function(x)weightTfIdf(x, normalize = F))
dtm.tfidf.train.title <- DocumentTermMatrix(corpus.train.title, control.tfidf)
# dtm.tfidf.train.des <- DocumentTermMatrix(corpus.train.des, control)
dtm.tfidf.train.content <- DocumentTermMatrix(corpus.train.content, control.tfidf)

## 提取特征词
feature.terms.title <- Terms(dtm.tfidf.train.title)
# feature.terms.des <- Terms(dtm.tfidf.train.des)
feature.terms.content <- Terms(dtm.tfidf.train.content)

## 清理数字开头的单词
feature.title <- feature.terms.title[grep("^[0-9]", feature.terms.title, invert = T)]

feature.content <- feature.terms.content[grep("^[0-9]", feature.terms.content, invert = T)]

## 卡方检验
## t为特征,c为类别
chi.test.title <- matrix(0, nrow = length(feature.title), ncol = 11)
for(i in 1:length(feature.title)){
  for(j in 1:11){
    CATEGORY <- paste("|", j, "|", sep = "")
    feature.terms.id <- match(feature.title[i], feature.terms.title)
    feature.doc.id <- test.train$ID[dtm.tfidf.train.title$i[dtm.tfidf.train.title$j == feature.terms.id]]
    category.doc.id <- test.train$ID[grep(CATEGORY, test.train$Category, fixed = T)]
    category.feature.doc.id <- c(feature.doc.id, category.doc.id)
    category.feature.doc.id <- category.feature.doc.id[duplicated(c(feature.doc.id, category.doc.id))]
    df.t <- length(feature.doc.id)
    df.c <- length(category.doc.id)
    df.tc <- length(category.feature.doc.id)
    df.tC <- df.t - df.tc
    df.Tc <- df.c - df.tc
    df.TC <- 500 - df.tc - df.Tc - df.tC
    Xmatrix <- matrix(c(df.tc, df.Tc, df.tC, df.TC), nrow = 2)
    chi.test.title[i,j] <- chisq.test(Xmatrix, correct = T)$statistic
    rm(df.t, df.tc, df.tC, df.c, df.Tc, df.TC)
    cat("-------", j, "-------\n")
  }
  cat("-------", i, "-------\n")
}


chi.test.content <- matrix(0, nrow = length(feature.content), ncol = 11)
for(i in 1:length(feature.content)){
  for(j in 1:11){
    CATEGORY <- paste("|", j, "|", sep = "")
    feature.terms.id <- match(feature.content[i], feature.terms.content)
    feature.doc.id <- test.train$ID[dtm.tfidf.train.content$i[dtm.tfidf.train.content$j == feature.terms.id]]
    category.doc.id <- test.train$ID[grep(CATEGORY, test.train$Category, fixed = T)]
    category.feature.doc.id <- c(feature.doc.id, category.doc.id)
    category.feature.doc.id <- category.feature.doc.id[duplicated(c(feature.doc.id, category.doc.id))]
    df.t <- length(feature.doc.id)
    df.c <- length(category.doc.id)
    df.tc <- length(category.feature.doc.id)
    df.tC <- df.t - df.tc
    df.Tc <- df.c - df.tc
    df.TC <- 500 - df.tc - df.Tc - df.tC
    Xmatrix <- matrix(c(df.tc, df.Tc, df.tC, df.TC), nrow = 2)
    chi.test.content[i,j] <- chisq.test(Xmatrix, correct = T)$statistic
    rm(df.t, df.tc, df.tC, df.c, df.Tc, df.TC)
    cat("-------", j, "-------\n")
  }
  cat("-------", i, "-------\n")
}


rm(Xmatrix, CATEGORY, category.doc.id, category.feature.doc.id, control.tfidf, 
   dtm.tfidf.train.content, dtm.tfidf.train.title, feature.doc.id, feature.terms.content, 
   feature.terms.id, feature.terms.title, i, j, l, train, train.content, train.des, train.title, corpus.train.content, corpus.train.title)

Xchi.threshold <- 3.8

path.title <- c('~/R_workspace/classify by topic/X-tfidf-svm/category_feature_title/')
for(i in 1:11){
  k <- paste(path.title, "category_feature_", i, "_title", sep = "")
  write.table(feature.title[chi.test.title[, i] > Xchi.threshold], file = k ,row.names = F, col.names = F, fileEncoding = 'utf-8', quote = F)
  rm(k)
  cat("-------", i, "-------\n")
}


path.content <- c('~/R_workspace/classify by topic/X-tfidf-svm/category_feature_content/')
for(i in 1:11){
  k <- paste(path.content, "category_feature_", i, "_content", sep = "")
  write.table(feature.content[chi.test.content[, i] > Xchi.threshold], file = k, row.names = F, col.names = F, fileEncoding = 'utf-8', quote = F)
  rm(k)
  cat("-------", i, "-------\n")
}


feature.title[chi.test.title[, 9] > Xchi.threshold]
feature.content[chi.test.content[, 1] > Xchi.threshold]

