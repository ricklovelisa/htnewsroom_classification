source("Function.R", encoding = "UTF-8")

library(RJDBC)
library(tm)
library(slam)
library(e1071)

SQL.select.test <- "SELECT ID, title, content_wordseg, site FROM article where ID between "
path.JDBC.driver <- "/home/qiuqiu/JDBC_driver/mysql-connector-java-5.1.7-bin.jar"
JDBC.char <- "jdbc:mysql://172.16.4.107/htnewsroom"
path.train <- "/home/qiuqiu/R_workspace/htnewsroom_classification/train_corpus/train_corpus_"
DB.usr <- "root"
DB.psd <- "123456"
svmcate <- c(3, 4, 5, 6, 7, 9, 10)

id <- list()
for(i in 1:7){
  id[[i]] <- read.csv(paste(path.train, svmcate[i], ".csv", sep = ""), header = T)
}
ID <- unique(unlist(id))

jdbcdrv <- JDBC('com.mysql.jdbc.Driver', path.JDBC.driver)
mycon <- dbConnect(jdbcdrv, JDBC.char, DB.usr, DB.psd)
test.train <- data.frame(matrix("0", ncol = 3), stringsAsFactors = F)
names(test.train) <- c("ID", "content_wordseg", "site")
test.train$ID <- as.numeric(test.train$ID)

while (i < max(ID)) {
  i <- max(test.train$ID)
  a <- dbGetQuery(mycon, paste("SELECT ID, content_wordseg, site FROM article where id > ", i, " limit 10000", sep = ""))
  test.train <- rbind(test.train, a)
  cat(i, "\n")
}

test.train$category <- 0
for(i in 1:7){
  test.train$category[test.train$ID %in% id[[i]]$Article_id] <- svmcate[i]
  cat(i, "\n")
}

test <- subset(test.train, category == 0)
test <- subset(test, ID %in% sample(test$ID, length(test.train$ID[test.train$category != 0])))
test.train <- rbind(test, test.train[test.train$category != 0, ])

saveRDS(test.train, "test_train.rds")

# 添加性能测试的训练集 #
jdbcdrv <- JDBC('com.mysql.jdbc.Driver', path.JDBC.driver)
mycon <- dbConnect(jdbcdrv, "jdbc:mysql://172.16.4.107/spiderdata", DB.usr, DB.psd)
data <- dbGetQuery(mycon, "select id, title, content from tuicool")
data <- data[(nchar(data$content) != 0 & nchar(data$title) != 0), ]


######################################################################################
######################################################################################
######################################################################################
######################################################################################
source("Function_module.R", encoding = "UTF-8")

library(RJDBC)
library(tm)
library(slam)
library(e1071)
library(rjson)
library(jiebaR)

stopwordsCN <- readLines("stopwordsCN.dic")

test.train <- readRDS("test_train.rds")
train <- list()
for (i in 1:length(test.train$ID)){
  try(train[[i]] <- fromJSON(test.train$content_wordseg[i]), silent = T)
}
names(train) <- paste(test.train$ID, "_", test.train$category, sep = "")
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

train.title <- sapply(train, function(x) list(removePunctuation(removeNumbers(removeWords(x$title[[1]], stopwordsCN)))))
train.content <- sapply(train, function(x) list(removePunctuation(removeNumbers(removeWords(x$content[[1]], stopwordsCN)))))
train.title <- sapply(train.title, function(x) list(list(x[nchar(x) != 0])))
train.content <- sapply(train.content, function(x) list(list(x[nchar(x) != 0])))

cutter <- worker()
data.title <- sapply(data$title, function(x) list(cutter[x]))
data.content <- sapply(data$content, function(x) list(cutter[x]))
names(data.title) <- paste(max(test.train$ID):(max(test.train$ID) + length(data.title) - 1), "_", 12, sep = "")
names(data.content) <- names(data.title)

data.title <- sapply(data.title, function(x) list(removePunctuation(removeNumbers(removeWords(x, stopwordsCN)))))
data.content <- sapply(data.content, function(x) list(removePunctuation(removeNumbers(removeWords(x, stopwordsCN)))))
data.title <- sapply(data.title, function(x) list(list(x[nchar(x) != 0])))
data.content <- sapply(data.content, function(x) list(list(x[nchar(x) != 0])))

train.content <- c(train.content, data.content)
train.title <- c(train.title, data.title)
saveRDS(train.content, "train/train_content.rds")
saveRDS(train.title, "train/train_title.rds")


# 构建词频矩阵 #
corpus.title <- Corpus(VectorSource(train.title))
corpus.content <- Corpus(VectorSource(train.content))

for (i in 1:length(corpus.title)){
  corpus.title[[i]]$content <- sub("c", "", corpus.title[[i]]$content)
}
for (i in 1:length(corpus.title)){
  meta(corpus.title[[i]], tag = 'id') <- names(train.title)[i]
}

for (i in 1:length(corpus.content)){
  corpus.content[[i]]$content <- sub("c", "", corpus.content[[i]]$content)
}
for (i in 1:length(corpus.content)){
  meta(corpus.content[[i]], tag = 'id') <- names(train.content)[i]
}


tf <- list(removePunctuation = T, removeNumbers = T, stripWhitespace = T, wordLengths = c(1, 10))
dtm.title <- DocumentTermMatrix(corpus.title, tf)
dtm.content <- DocumentTermMatrix(corpus.content, tf)

saveRDS(dtm.title, "train/dtm_title_noclean.rds")
saveRDS(dtm.content, "train/dtm_content_noclean.rds")

category.title <- as.vector(sapply(rownames(dtm.title), function(x) strsplit(x,split = "_")[[1]][2]))
chisq.title <- ChisqareTest(dtm.title, category, 0.05)

category.content <- as.vector(sapply(rownames(dtm.content), function(x) strsplit(x,split = "_")[[1]][2]))
chisq.content <- ChisqareTest(dtm.content, category, 0.05)
feature.words.title <- sapply(as.data.frame(chisq.title), function(x) Terms(dtm.title)[x >= 3.8])
feature.words.content <- sapply(as.data.frame(chisq.content), function(x) Terms(dtm.content)[x >= 3.8])























