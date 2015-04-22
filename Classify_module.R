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
source(paste(path.Function_module, "Function_module.R", sep = ""))

### 清理特征词 ###
grep.keywords <- category$grep_keywords %>>% strsplit(split = ',')
grep.keywords <- grep.keywords[!is.na(grep.keywords)]

svm.feature.title <- list()
svm.feature.content <- list()
for(i in svmcate){
  svm.feature.title[[i]] <- readLines(paste(path.feature.title, "category_feature_", i, "_title", sep = ""))
  svm.feature.content[[i]] <- readLines(paste(path.feature.content, "category_feature_", i, "_content", sep = ""))
}

### 清理无效字符 ###
for(i in 2:length(test)){
  test[,i] <- gsub("[\f\n\r\t\v]", "", test[,i])
  test[,i] <- gsub("&(.*?);", "", test[,i])
}

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

for(i in grepcate){
  #description匹配二级关键词
  cat('-------', category$name[i], 'description匹配关键词-------\n')
  temp <- list()
  for(j in length(grep.keywords[[i]])){
    temp[[j]] <- grep(grep.keywords[[i]][j], unclasstest$description)
  }
  unclasstest$category_old[unique(unlist(temp))] <- i
  result <- unclasstest[unclasstest$category_old > 0, ]
  test$category_old[match(result$ID, test$ID)] <- result$category_old
  unclasstest <- unclasstest[unclasstest$category_old == 0,]
}

rm(result, unclasstest, temp, mycon, i, j, grep.keywords, grepcate)



### 提取训练集中每个类别中的文章id ###
train <- read.csv(paste(path.train, "train.csv", sep = ""), stringsAsFactors = F)
text.id <- list()
for(i in svmcate){
  text.id[[i]] <- train$ID[grep(paste("|", i, "|", sep = ""), train$Category, fixed = T)]
}

### svm classify ###

## 建立语料库 ##
# 测试集.list #
test.list <- list()
for (i in 1:length(test$ID)){
  try(test.list[[i]] <- fromJSON(test$content_wordseg[i]), silent = T)
}
names(test.list) <- test$ID

# 训练集.list #
train.list <- list()
for (i in 1:length(train$ID)){
  try(train.list[[i]] <- fromJSON(train$content[i]), silent = T)
}
names(train.list) <- train$ID

# 清理test中的空值并合并训练集和测试集 #
test <- test[!sapply(test.list, is.null), ]
test.list <- test.list[!sapply(test.list, is.null)]
data.list <- c(train.list, test.list)

# 清除无用变量 #
rm(test.list, train.list)


# SVM分类 #
if(feature.switch == "title"){
  ## title ##
  # 数据集.标题 #
  LIST <- list()
  for (i in 1:length(data.list)){
    LIST[[i]] <- data.list[[i]]$title
    Encoding(LIST[[i]]) <- encode
    LIST[[i]] <- strsplit(LIST[[i]], split = ',')
  }
  names(LIST) <- as.numeric(names(data.list))
  
  # 构建语料库 #
  CORPUS <- Corpus(VectorSource(LIST), readerControl = list(language = "ZHCN"))
  for (i in 1:length(CORPUS)){
    CORPUS[[i]]$content <- sub("c", "", CORPUS[[i]]$content)
  }
  for (i in 1:length(CORPUS)){
    meta(CORPUS[[i]], tag = 'id') <- as.numeric(names(LIST[i]))
  }
  
  # 构建词频矩阵 #
  control.tfidf <- list(removePunctuation = T, removeNumbers = F, stripWhitespace = T, wordLengths = c(2, 10), weighting = function(x)weightTfIdf(x, normalize = F))
  DTM.tfidf <- DocumentTermMatrix(CORPUS, control.tfidf)
  
  for(SVM in svmcate){
    # 词频矩阵根据svm.feature降维 #
    FEATURE <- unlist(strsplit(svm.feature.title[[SVM]], split = ','))
    termsid <- match(FEATURE, Terms(DTM.tfidf))
    
    # 构建VSM #
    VSM <- as.data.frame(as.matrix(DTM.tfidf[, termsid]))
    
    # 添加标注 #
    VSM$category <- 0
    VSM$category[match(text.id[[SVM]], as.numeric(rownames(VSM)), nomatch = 0)] <- SVM
    
    # 区分训练集和测试集 #
    VSM.train <- VSM[match(train$ID, as.numeric(rownames(VSM))), ]
    VSM.test <- VSM[match(test$ID, as.numeric(rownames(VSM))), ]
    
    # SVM分类 #
    SVM.model <- svm(as.factor(VSM.train$category)~., data = VSM.train, kernel = 'linear', type = 'C-classification', probability = T, tolerance = 0.0001)
    pred <- ifelse(as.numeric(predict(SVM.model, VSM.test)) == 1, 0, SVM)
    test <- cbind(test, pred)
    cat('-------- 分类 ---', category$name[SVM], '--------\n')
  }  
}else if(feature.switch == "content"){
  ## content ##
  # 数据集.标题 #
  LIST <- list()
  for (i in 1:length(data.list)){
    LIST[[i]] <- data.list[[i]]$content
    Encoding(LIST[[i]]) <- encode
    LIST[[i]] <- strsplit(LIST[[i]], split = ',')
  }
  names(LIST) <- as.numeric(names(data.list))
  
  # 构建语料库 #
  CORPUS <- Corpus(VectorSource(LIST), readerControl = list(language = "ZHCN"))
  for (i in 1:length(CORPUS)){
    CORPUS[[i]]$content <- sub("c", "", CORPUS[[i]]$content)
  }
  for (i in 1:length(CORPUS)){
    meta(CORPUS[[i]], tag = 'id') <- as.numeric(names(LIST[i]))
  }
  
  # 构建词频矩阵 #
  control.tfidf <- list(removePunctuation = T, removeNumbers = F, stripWhitespace = T, wordLengths = c(2, 10), weighting = function(x)weightTfIdf(x, normalize = F))
  DTM.tfidf <- DocumentTermMatrix(CORPUS, control.tfidf)
  
  for(SVM in svmcate){
    # 词频矩阵根据svm.feature降维 #
    FEATURE <- unlist(strsplit(svm.feature.content[[SVM]], split = ','))
    termsid <- match(FEATURE, Terms(DTM.tfidf))
    
    # 构建VSM #
    VSM <- as.data.frame(as.matrix(DTM.tfidf[, termsid]))
    
    # 添加标注 #
    VSM$category <- 0
    VSM$category[match(text.id[[SVM]], as.numeric(rownames(VSM)), nomatch = 0)] <- SVM
    
    # 区分训练集和测试集 #
    VSM.train <- VSM[match(train$ID, as.numeric(rownames(VSM))), ]
    VSM.test <- VSM[match(test$ID, as.numeric(rownames(VSM))), ]
    
    # SVM分类 #
    SVM.model <- svm(as.factor(VSM.train$category)~., data = VSM.train, kernel = 'linear', type = 'C-classification', probability = T, tolerance = 0.0001)
    pred <- ifelse(as.numeric(predict(SVM.model, VSM.test)) == 1, 0, SVM)
    test <- cbind(test, pred)
    cat('-------- 分类 ---', category$name[SVM], '--------\n')
  }  
}

test$category <- apply(test[,-c(1:6)], identifyCategory, MARGIN = 1)
test$category_id <- paste("|", test$category, "|", sep = "")

