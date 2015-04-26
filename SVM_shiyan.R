# library(RODBC)
library(rjson)
library(jiebaR)
library(tm)
library(e1071)
library(slam)

cutter <- worker()

# id <- read.table("ecommerce_id.txt", stringsAsFactors = F)

# mycon <- odbcConnect("3.96", "root", "123456")
# data1 <- sqlQuery(mycon, paste("select id, content_wordseg from article where id in (", id, ")", sep = ""), stringsAsFactors = F)
# data2 <- sqlQuery(mycon, "select article.id, article.content_wordseg from article limit 4000", stringsAsFactors = F)

#　data2 <- data2[!(data2$id %in% data1$id), ]

# data <- rbind(data1, data2)
# rm(data1, data2)
# id <- as.integer(strsplit(id$V1, split = ",")[[1]])

# data$category <- 2
# data$category[data$id %in% id] <- 1

# rownames(data) <- paste(data$id, "_", data$category, sep = "")

data <- readRDS("data.rds")
stopwordsCN <- readLines("stopwordsCN.dic", encoding = 'UTF-8')
data$content <- gsub("<.*?>", "", data$content)
list.title <- sapply(data$title, function(x) cutter[x])
list.content <- sapply(data$content, function(x) cutter[x])

names(list.title) <- rownames(data)
names(list.content) <- rownames(data)

list.title <- sapply(list.title, function(x) removePunctuation(removeWords(x, stopwordsCN)))
list.content <- sapply(list.content, function(x) removePunctuation(removeWords(x, stopwordsCN)))

list.title <- sapply(list.title, function(x) x[nchar(x) != 0])
list.content <- sapply(list.content, function(x) x[nchar(x) != 0])

list.both <- sapply(c(1:length(data$id)), function(x) list(list(c(list.title[[x]], list.content[[x]]))))
names(list.both) <- names(list.title)

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
dtm.both <- dtm.both[, -c(1:31347)]
dtm.both <- dtm.both[row_sums(dtm.both) > 0, ]
terms.tfidf <- tapply(dtm.both$v/row_sums(dtm.both)[dtm.both$i], dtm.both$j, mean) * log2(nDocs(dtm.both)/col_sums(dtm.both > 0))
terms.idf <- log2(nDocs(dtm.both)/col_sums(dtm.both > 0))

Category <- as.factor(sapply(rownames(dtm.both), function(x) strsplit(x, split = "_")[[1]][2]))

dtm.both.tfidf <- weightTfIdf(dtm.both, normalize = F)

aa <- tune('svm',  dtm.both.tfidf, Category, ranges = list(class.weights = c('1' = 0.9, '2' = 0.1), gamma = 10^(-6:-1), cost = 10^(-2:2)), tunecontrol = tune.control(sampling = 'cross', cross = 5))


