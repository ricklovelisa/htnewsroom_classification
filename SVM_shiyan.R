library(RJDBC)
library(rjson)
library(jiebaR)
library(tm)
library(e1071)
library(slam)

source("Function_module.R", encoding = 'UTF-8')

# cutter <- worker()

# id <- read.table("Data/zhinengshebei_id.txt", stringsAsFactors = F)
# mycon <- odbcConnect("128.172", "root", "123456")
# data1 <- sqlQuery(mycon, paste("select id, content_wordseg from article where id in (", id, ")", sep = ""), stringsAsFactors = F)
# data2 <- sqlQuery(mycon, "select article.id, article.content_wordseg from article limit 3000", stringsAsFactors = F)
# data2 <- data2[!(data2$id %in% data1$id), ]
# data <- rbind(data1, data2)
# rm(data1, data2)
# id <- as.integer(strsplit(id$V1, split = ",")[[1]])
# data$category <- 2
# data$category[data$id %in% id] <- 1
# data$category <- ifelse(data$category == 1, 'ture', 'false')
# rownames(data) <- paste(data$id, "_", data$category, sep = "")
# saveRDS(data, "Data/data_8.rds")


jdbcdrv <- JDBC('com.mysql.jdbc.Driver', "/home/qiuqiu/JDBC_driver/mysql-connector-java-5.1.7-bin.jar")
mycon <- dbConnect(jdbcdrv, "jdbc:mysql://172.16.128.172/htnewsroom", "root", "123456")
id <- read.table("Data/zhinengshebei_id.txt", stringsAsFactors = F)
data1 <- dbGetQuery(mycon, paste("select id, content_wordseg from article where id in (", id, ")", sep = ""))
data2 <- dbGetQuery(mycon, "select article.id, article.content_wordseg from article limit 3000")
data2 <- data2[!(data2$ID %in% data1$ID), ]
data <- rbind(data1, data2)
rm(data1, data2)
id <- as.integer(strsplit(id$V1, split = ",")[[1]])
data$category <- 2
data$category[data$id %in% id] <- 1
data$category <- ifelse(data$category == 1, 'ture', 'false')
rownames(data) <- paste(data$ID, "_", data$category, sep = "")
saveRDS(data, "Data/data_8.rds")


data <- readRDS("Data/data_8.rds")
Encoding(data$content_wordseg) <- 'UTF-8'
# data$category[-c(1:149)] <- 'false'
# data <- data[data$category == 'ture', ] one-classification

stopwordsCN <- readLines("stopwordsCN.dic", encoding = 'UTF-8')
# data$content <- gsub("<.*?>", "", data$content, perl = T)
# data$content <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/]", "", data$content, perl = T)
# data$content <- gsub("(www)(.*)[.|/]", "", data$content, perl = T)
# list.title <- sapply(data$title, function(x) cutter[x])
# list.content <- sapply(data$content, function(x) cutter[x])

# names(list.title) <- rownames(data)
# names(list.content) <- rownames(data)

data.list <- list()
for(i in 1:length(data$id)){
  try(data.list[[i]] <- fromJSON(data$content_wordseg[i]), silent = T)
}
names(data.list) <- data$ID
null.id <- sapply(data.list, function(x) is.null(x))
data.list <- data.list[!null.id]



list.title <- sapply(list.title, function(x) removePunctuation(removeWords(x, stopwordsCN)))
list.content <- sapply(list.content, function(x) removePunctuation(removeWords(x, stopwordsCN)))

list.title <- sapply(list.title, function(x) x[nchar(x) != 0])
list.content <- sapply(list.content, function(x) x[nchar(x) != 0])

list.both <- sapply(c(1:length(data$id)), function(x) list(list(c(list.title[[x]], list.content[[x]]))))
names(list.both) <- names(list.title)
rm(list.title, list.content)

corpus.both <- Corpus(VectorSource(list.both))
for (i in 1:length(corpus.both)){
  corpus.both[[i]]$content <- sub("c", "", corpus.both[[i]]$content)
  cat(i,"\n")
}
for (i in 1:length(corpus.both)){
  meta(corpus.both[[i]], tag = 'id') <- names(list.both)[i]
  cat(i,"\n")
}

control.tf <- list(removePunctuation = T, stripWhitespace = T, wordLengths = c(2, 10))
dtm.both <- DocumentTermMatrix(corpus.both, control.tf)
DocF <- DocFreq(dtm.both)
dtm.both <- dtm.both[, DocF > 2]
# dtm.both <- dtm.both[, -c(1:567,569:3850)]
# dtm.both <- dtm.both[, -c(1:6646)]
# dtm.both <- dtm.both[, -c(1:166)]
dtm.both <- dtm.both[row_sums(dtm.both) > 0, ]

Category <- as.factor(sapply(rownames(dtm.both), function(x) strsplit(x, split = "_")[[1]][2]))

# dtm.both.tfidf <- weightTfIdf(dtm.both, normalize = T)

chisq <- ChisqareTest(dtm.both, Category, 0.1)
rownames(chisq) <- Terms(dtm.both)
# chisq <- readRDS('chisq.rds')

# dims <- chisq[chisq[,1] > 150, ]
# dtm.both.tfidf2 <- dtm.both.tfidf[, match(rownames(dims), Terms(dtm.both))]
# dtm.both.tfidf2 <- dtm.both.tfidf2[row_sums(dtm.both.tfidf2) > 0, ]

# Cate <- as.factor(sapply(rownames(dtm.both.tfidf2), function(x) strsplit(x, split = "_")[[1]][2]))

# SVM_model <- tune('svm',  dtm.both.tfidf2, Cate, ranges = list(class.weights = list(c('1' = 0.95, '2' = 0.05)), gamma = 10^(-6:-1), cost = 10^(-3:3)), kernel = 'radial', type = 'C-classification', tunecontrol = tune.control(sampling = 'cross', cross = 5))
# # SVM_model <- readRDS("SVM_model.rds")

# SVM <- svm(dtm.both.tfidf2, Cate, type = "C-classification", kernel = 'radial', class.weights = c('1' = 0.95, '2' = 0.05), gamma = SVM_model$best.parameters[2], cost = SVM_model$best.parameters[3], probability = T)

# 整理测试集 #
# mycon <- odbcConnect("3.96", "root", "123456")
# data1 <- sqlQuery(mycon, paste("select id, title, content from article where id in (", as.String(c(33821,34929,34931,35127,35330,35857,36222,36532,36601,36613,36646,36900,37158,37197,37199,37259,37308,37312,37318,37325,37784,38164,38177,38332,38346,38649,38667,38684,38758,38790,38799,38855)), ")", sep = ""), stringsAsFactors = F)
# data2 <- sqlQuery(mycon, "select article.id, article.title, article.content from article, article_classified where article.ID = article_classified.article_id and article_classified.category_id != 3 and article.ID > 30000 limit 1000", stringsAsFactors = F)
# data2 <- data2[match(unique(data2$id), data2$id), ]
# data2 <- data2[!(data2$id %in% data1$id), ]
# data2 <- data2[match(sample(data2$id, 600), data2$id), ]
# test <- rbind(data1,data2)
# rm(data1, data2)

test <- readRDS('Data/test.rds')
# test$category[-c(1:32)] <- 'false'
# test$category <- ifelse(test$category == 1, 'ture', 'false')
# rownames(test) <- paste(test$id, "_", test$category, sep = "")
# test$category <- 2
# test$category[1:32] <- 1
# rownames(test) <- paste(test$id, "_", test$category, sep = "")
# test$content <- gsub("<.*?>", "", test$content)
test.title <- sapply(test$title, function(x) cutter[x])
test.content <- sapply(test$content, function(x) cutter[x])

names(test.title) <- rownames(test)
names(test.content) <- rownames(test)

test.title <- sapply(test.title, function(x) removePunctuation(removeWords(x, stopwordsCN)))
test.content <- sapply(test.content, function(x) removePunctuation(removeWords(x, stopwordsCN)))

test.title <- sapply(test.title, function(x) x[nchar(x) != 0])
test.content <- sapply(test.content, function(x) x[nchar(x) != 0])

test.both <- sapply(c(1:length(test$id)), function(x) list(list(c(test.title[[x]], test.content[[x]]))))
names(test.both) <- names(test.title)
rm(test.title, test.content)

corpus.both <- Corpus(VectorSource(test.both))
for (i in 1:length(corpus.both)){
  corpus.both[[i]]$content <- sub("c", "", corpus.both[[i]]$content)
  cat(i,"\n")
}
for (i in 1:length(corpus.both)){
  meta(corpus.both[[i]], tag = 'id') <- names(test.both)[i]
  cat(i,"\n")
}

control.tf <- list(removePunctuation = T, stripWhitespace = T, wordLengths = c(2, 10))
test.both <- DocumentTermMatrix(corpus.both, control.tf)


# term_tfidf <- tapply(dtm.both$v/row_sums(dtm.both)[dtm.both$i], dtm.both$j, mean) * log2(nDocs(dtm.both)/col_sums(dtm.both > 0))
# cont <- c((1:20)/200)
cont <- c(170, 190, 210, 220, 230, 240, 250, 260)
SVM_model <- list()
SVM <- list()
pred <- list()
test.cate <- list()
dtm.both.tfidf <- list()
for(i in 1:length(cont)){
  words <- rownames(chisq)[chisq[,1] > cont[i]]
  # dtm.both.tf <- dtm.both[, match(rownames(dims), Terms(dtm.both))]
  # dtm.both.tf <- dtm.both.tf[row_sums(dtm.both.tf) > 0, ]
  # dtm.both.tfidf2 <- dtm.both.tfidf[, match(words, Terms(dtm.both))]
  # dtm.both.tfidf2 <- dtm.both.tfidf[, term_tfidf > cont[i]]
  # dtm.both.tfidf2 <- dtm.both.tfidf2[row_sums(dtm.both.tfidf2) > 0, ]
  dtm.both.tfidf[[i]] <- dtm.both[, match(words, Terms(dtm.both))]
  dtm.both.tfidf[[i]] <- weightTfIdf(dtm.both.tfidf[[i]][row_sums(dtm.both.tfidf[[i]]) > 0, ], normalize = F)
  
  # Cate <- as.factor(sapply(rownames(dtm.both.tf), function(x) strsplit(x, split = "_")[[1]][2]))
  Cate <- as.factor(sapply(rownames(dtm.both.tfidf[[i]]), function(x) strsplit(x, split = "_")[[1]][2]))
  N <- length(Cate)
  # SVM_model <- tune('svm',  dtm.both.tf, Cate, ranges = list(class.weights = list(c('1' = 0.95, '2' = 0.05)), gamma = 10^(-6:-1), cost = 10^(-3:3)), kernel = 'radial', type = 'C-classification', tunecontrol = tune.control(sampling = 'cross', cross = 5))
  SVM_model <- tune('svm',  dtm.both.tfidf[[i]], Cate, ranges = list(class.weights = list(N/table(Cate)), gamma = 10^(-6:-1), cost = 10^(-1:5)), kernel = 'radial', type = 'C-classification', tunecontrol = tune.control(sampling = 'cross', cross = 5))
  # SVM_model <- readRDS("SVM_model.rds")
  # SVM_model <- tune('svm',  dtm.both.tfidf2, Cate, ranges = list(nu = 2^(-5:-1), gamma = 10^(-6:-1), cost = 10^(-3:3)), kernel = 'radial', type = 'one-classification', tunecontrol = tune.control(sampling = 'fix'))
  
  # SVM[[i]] <- svm(dtm.both.tf, Cate, type = "C-classification", kernel = 'radial', class.weights = c('1' = 0.95, '2' = 0.05), gamma = SVM_model$best.parameters[2], cost = SVM_model$best.parameters[3], probability = T)
  # SVM[[i]] <- svm(dtm.both.tfidf2, Cate, scale = T, type = "C-classification", kernel = 'radial', class.weights = N/table(Cate), gamma = SVM_model$best.parameters[2], cost = SVM_model$best.parameters[3], probability = T)
  SVM[[i]] <- svm(dtm.both.tfidf[[i]], Cate, type = "C-classification", kernel = 'radial', class.weights = N/table(Cate), gamma = SVM_model$best.parameters$gamma, cost = SVM_model$best.parameter$cost)
  # test.both2 <- weightTfIdf(test.both, F)
  # test.both2 <- MakePredDtm(test.both, dtm.both.tf)
  
  test.both2 <- MakePredDtm(test.both, dtm.both.tfidf[[i]])
  test.both2 <- weightSameIDF(test.both2[row_sums(test.both2) > 0, ], dtm.both.tfidf[[i]], normalize = F)
  
  # pred[[i]] <- predict(SVM[[i]], test.both2, probability = T)
  pred[[i]] <- predict(SVM[[i]], test.both2)
  test.cate[[i]] <- as.factor(sapply(rownames(test.both2), function(x) strsplit(x, split = "_")[[1]][2]))
  # test.cate <- as.factor(sapply(rownames(test.both2), function(x) strsplit(x, split = "_")[[1]][2]))
  cat(i,'\n')
}

TB <- sapply(1:length(cont), function(x) list(table(pred[[x]], test.cate[[x]])[c(2,1), c(2,1)]))

acc <- sapply(1:length(cont), function(x) TB[[x]][1, 1]/sum(TB[[x]][1, ]))
recall <- sapply(1:length(cont), function(x) TB[[x]][1, 1]/sum(TB[[x]][, 1]))
rbind(acc,recall)
















test.both2 <- MakePredDtm(test.both, dtm.both)
test.both2 <- weightSameIDF(test.both2, dtm.both, normalize = F)
test.both2 <- MakePredDtm(test.both2, dtm.both.tfidf2)
test.both2 <- test.both2[row_sums(test.both2) > 0, ]

predict(SVM, test.both2)
table(predict(SVM, test.both2))
test.cate <- as.factor(sapply(rownames(test.both2), function(x) strsplit(x, split = "_")[[1]][2]))
table(predict(SVM, test.both2), test.cate)


