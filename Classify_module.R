#####################################################################################################################################
### Classify module #################################################################################################################
#####################################################################################################################################

### 加载包 ###
library(tm)
library(e1071)
library(slam)
library(rjson)
library(pipeR)
library(rlist)
# library(jiebaR)
source(paste(path.Function_module, "Function_module.R", sep = ""))

### 清理特征词 ###
grep.keywords1 <- category$grep_keywords %>>% strsplit(split = ',')
grep.keywords2 <- category$svm_keywords %>>% strsplit(split = ',')
grep.keywords <- sapply(1:10, function(x) unique(c(grep.keywords1[[x]], grep.keywords2[[x]])))
grep.keywords <- grep.keywords[!is.na(grep.keywords)]
rm(grep.keywords1, grep.keywords2)
### 清理无效字符 ###

# for(i in 2:length(test)){
#   test[,i] <- gsub("[\f\n\r\t\v]", "", test[,i])
#   test[,i] <- gsub("&(.*?);", "", test[,i])
# }

### 匹配分类 ###
test$category_old <- 0
unclasstest <- test
unclasstest$category_old <- 0
for(i in grepcate){
  #title匹配关键词
  cat('-------', category$name[i], 'title匹配关键词-------\n')
  temp <- list()
  for(j in 1:length(grep.keywords[[i]])){
    temp[[j]] <- grep(grep.keywords[[i]][j], unclasstest$title)
  }
  unclasstest$category_old[unique(unlist(temp))] <- i
  result <- unclasstest[unclasstest$category_old > 0, ]
  test$category_old[match(result$ID, test$ID)] <- result$category_old
  unclasstest <- unclasstest[unclasstest$category_old == 0,]
}

rm(result, unclasstest, temp, mycon, i, j, grep.keywords, grepcate)



### SVM分类 ###
#### 正例测试集 ####
# cutter <- worker() #加载分词器
stopwordsCN <- readLines("stopwordsCN.dic", encoding = 'UTF-8') #加载特定的停用词表

# rownames(test) <- test$ID
# test.title <- sapply(test$title, function(x) cutter[x]) 
# test.content <- sapply(test$content, function(x) cutter[x])

test.list <- list()
for(i in 1:length(test$ID)){
  try(test.list[[i]] <- fromJSON(test$content_wordseg[i]), silent = T)
}
names(test.list) <- test$ID
null.id <- sapply(test.list, function(x) is.null(x))
test.list <- test.list[!null.id]

test.title <- sapply(test.list, function(x) list(x$title))
test.content <- sapply(test.list, function(x) list(x$content))

test.title <- sapply(test.title, function(x) strsplit(x[[1]], split = ","))
test.content <- sapply(test.content, function(x) strsplit(x[[1]], split = ","))

test.title <- sapply(test.title, function(x) removePunctuation(removeWords(x, stopwordsCN)))
test.content <- sapply(test.content, function(x) removePunctuation(removeWords(x, stopwordsCN)))

test.title <- sapply(test.title, function(x) x[nchar(x) != 0])
test.content <- sapply(test.content, function(x) x[nchar(x) != 0])

test.both <- sapply(c(1:length(test.list)), function(x) list(list(c(test.title[[x]], test.content[[x]]))))
names(test.both) <- names(test.list)
rm(test.title, test.content)

corpus.both <- Corpus(VectorSource(test.both))
for (i in 1:length(corpus.both)){
  corpus.both[[i]]$content <- sub("c", "", corpus.both[[i]]$content)
}
for (i in 1:length(corpus.both)){
  meta(corpus.both[[i]], tag = 'id') <- names(test.both)[i]
}

control.tf <- list(removePunctuation = T, stripWhitespace = T, wordLengths = c(2, 10))
test.both <- DocumentTermMatrix(corpus.both, control.tf)
 

#### 加载模型并分类 ####
for(i in svmcate){
  DTM <- readRDS(paste(path.DTM, "DTM_", i, ".rds", sep = ""))
  TEST <- MakePredDtm(test.both, DTM)
  TEST <- weightSameIDF(TEST[row_sums(TEST) > 0, ], DTM, normalize = F)
  SVM <- readRDS(paste(path.model, "SVM_", i,  ".rds",sep = ""))
  PRED <- predict(SVM, TEST)
  PRED <- ifelse(PRED == 'ture', i, 0)
  ture.id <- as.integer(rownames(TEST)[PRED == i])
  CATE <- matrix(0, length(test$ID))
  CATE[test$ID %in% ture.id] <- i
  test <- cbind(test, CATE)
  cat("SVM分类完成类别ID =====", i, "=====\n")
}


test$category <- apply(test[,-c(1:4)], identifyCategory, MARGIN = 1)
test$category_id <- paste("|", test$category, "|", sep = "")


