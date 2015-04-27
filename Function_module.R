# 将测试集和训练集统一 #
MakePredDtm <- function(pred, dtm){ # weighting = "tf" 暂时不要用tfidf
  terms <- colnames(dtm[, which(!colnames(dtm) %in% colnames(pred))])
  amat <- matrix(0, nrow = nrow(pred), ncol = length(terms))
  colnames(amat) <- terms
  rownames(amat) <- rownames(pred)
  #   if(weighting == "tf"){
  fixed <- as.DocumentTermMatrix(cbind(pred[, which(colnames(pred) %in% colnames(dtm))], amat), weighting=weightTf)
  #   }
  # else if(weighting == "tfidf"){
  # fixed <- as.DocumentTermMatrix(cbind(pred[, which(colnames(pred) %in% colnames(dtm))], amat), weighting=weightTf)
  # fixed[, pred[, which(colnames(pred) %in% colnames(dtm))]] <- 
  # }
  pred <- fixed
  return(pred)
}


# 计算df #
DocFreq <- function(dtm){
  dtm$v[dtm$v != 1] <- 1
  df <- col_sums(dtm)
  return(df)
}

# 支持dtm的卡方检验 #
ChisqareTest <- function(dtm, label, prob){
  Category <- unique(label)
  chisq <- data.frame(matrix(0, nrow = length(Terms(dtm)), ncol = length(Category)))
  names(chisq) <- Category
  cate <- list()
  for(i in 1:length(Category)){
    cate[[i]] <- label
    cate[[i]] <- ifelse(cate[[i]] == Category[i], Category[i], "-1")
  }
  size <- floor(quantile(1:length(Terms(dtm)), probs = seq(0, 1, prob)))
  
  # 计算第一个词 #
  terms <- as.matrix(dtm[, 1])
  terms[terms != 0] <- 1
  for(j in 1:length(Category)){
    # terms[terms == 0] <- 2
    XsqMatrix <- table(terms, cate[[j]]) # confusion matrix
    chisq[1, j] <- chisq.test(XsqMatrix, correct = F)$statistic
  }
  cat("===== 已完成第", 1, "个词 =====\n")
  ##############
  
  # 计算其他词 #
  for(i in size[-length(size)]){
    n <- which(size == i)
    terms.m <- as.matrix(dtm[, (size[n] + 1):size[n + 1]])
    for(k in 1:(size[n + 1] - (size[n] + 1) + 1)){
      terms <- terms.m[, k]
      terms[terms != 0] <- 1
      for(j in 1:length(Category)){
        # terms[terms == 0] <- 2
        XsqMatrix <- table(terms, cate[[j]]) # confusion matrix
        chisq[k + i, j] <- chisq.test(XsqMatrix, correct = F)$statistic
      }
      cat("===== 已完成第", k + i, "个词 =====\n")
    }
  }
  return(chisq)
}


# 情感分析 #
ClassifyEmotion <- function(dtm, algorithm="bayes", prior=1.0, verbose=FALSE, ...) {
  Index <- read.table("index_emotion.txt", header=F, stringsAsFactors = F, fileEncoding = "utf-8")
  lexicon <- read.table("SentWords_emotion.txt", header=T, stringsAsFactors = F, fileEncoding = "utf-8")
  counts <- c(as.list(table(lexicon$情感分类)))
  counts <- c(counts, total = nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(dtm)) {
    if (verbose) print(paste("DOCUMENT", i))
    n <- length(counts) - 1
    scores <- as.list(matrix(0, nrow = n))
    names(scores) <- names(counts)[1:n]
    doc <- dtm[i, ]
    words <- findFreqTerms(doc, lowfreq = 1)
    
    for (word in words) {
      for (key in names(scores)) {
        emotions <- lexicon[which(lexicon[, 2] == key), ]
        index <- match(word, emotions[, 1], nomatch = 0)
        if (index > 0) {
          entry <- emotions[index, ]
          
          category <- as.character(entry[[2]])
          count <- counts[[category]]
          
          score <- col_sums(dtm[i, word])
          if (algorithm=="bayes") score <- abs(score*log(prior/count))
          
          if (verbose) {
            print(paste("WORD:", word, "CAT:", Index$V1[which(Index$V2 == category)], "SCORE:", score))
          }
          
          scores[[category]] <- scores[[category]]+score
        }
      }
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]] + score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    best_fit <- Index[grep(best_fit, Index[, 2]), 1]
    # if (best_fit == "disgust" && as.numeric(unlist(scores[2]))-3.09234 < .01) best_fit <- NA
    documents <- rbind(documents, best_fit)
  }
  
  return(documents)
}


# 极性分析 #
ClassifyPolarity <- function(dtm, algorithm="bayes", pstrong=0.5, pweak=1.0, prior=1.0, verbose=FALSE, ...) {
  lexicon <- read.table("SentWords_PN.txt", header=T, stringsAsFactors = F, fileEncoding = "utf-8")
  lexicon <- lexicon[lexicon[, 3] != 0, ]
  
  counts <- list(positive=length(which(lexicon[, 3]=="positive")), negative=length(which(lexicon[, 3]=="negative")), total=nrow(lexicon))
  documents <- c()
  
  for (i in 1:nrow(dtm)) {
    if (verbose) print(paste("DOCUMENT", i))
    scores <- list(positive=0, negative=0)
    doc <- dtm[i, ]
    words <- findFreqTerms(doc, lowfreq=1)
    
    for (word in words) {
      index <- pmatch(word, lexicon[, 1], nomatch=0)
      if (index > 0) {
        entry <- lexicon[index, ]
        
        polarity <- as.character(entry[[2]])
        category <- as.character(entry[[3]])
        count <- counts[[category]]
        
        score <- pweak
        if (polarity == "strongsubj") score <- pstrong
        if (polarity == "midsubj") socre <- pmid
        if (algorithm=="bayes") score <- abs(log(score*prior/count))
        
        if (verbose) {
          print(paste("WORD:", word, "CAT:", category, "POL:", polarity, "SCORE:", score))
        }
        
        scores[[category]] <- scores[[category]]+score
      }  	
    }
    
    if (algorithm=="bayes") {
      for (key in names(scores)) {
        count <- counts[[key]]
        total <- counts[["total"]]
        score <- abs(log(count/total))
        scores[[key]] <- scores[[key]]+score
      }
    } else {
      for (key in names(scores)) {
        scores[[key]] <- scores[[key]]+0.000001
      }
    }
    
    best_fit <- names(scores)[which.max(unlist(scores))]
    ratio <- as.integer(abs(scores$positive/scores$negative))
    if (ratio==1) best_fit <- "neutral"
    documents <- rbind(documents, c(scores$positive, scores$negative, abs(scores$positive/scores$negative), best_fit))
    if (verbose) {
      print(paste("POS:", scores$positive, "NEG:", scores$negative, "RATIO:", abs(scores$positive/scores$negative)))
      cat("\n")
    }
  }
  
  colnames(documents) <- c("POS", "NEG", "POS/NEG", "BEST_FIT")
  return(documents)
}


# 根据原始矩阵计算目标矩阵的tfidf #
weightSameIDF <-
  WeightFunction(function(m, originDTM, normalize = TRUE) {
    isDTM <- inherits(m, "DocumentTermMatrix")
    isorginDTM <- inherits(originDTM, "DocumentTermMatrix")
    if (isDTM) m <- t(m)
    if (isorginDTM) originDTM <- t(originDTM)
    if (normalize) {
      cs <- col_sums(m)
      if (any(cs == 0))
        warning("empty document(s): ", paste(Docs(m)[cs == 0], collapse = " "))
      names(cs) <- seq_len(nDocs(m))
      m$v <- m$v / cs[m$j]
    }
    originrs <- row_sums(originDTM > 0)
    if (any(originrs == 0))
      warning("unreferenced term(s): ", paste(Terms(originDTM)[originrs == 0], collapse = " "))
    originlnrs <- log2(nDocs(originDTM) /originrs)
    originlnrs[!is.finite(originlnrs)] <- 0
    m <- m * originlnrs
    attr(m, "weighting") <- c(sprintf("%s%s",
                                      "term frequency - inverse document frequency",
                                      if (normalize) " (normalized)" else ""),
                              "tf-idf")
    if (isDTM) t(m) else m
  }, "term frequency - inverse document frequency", "tf-idf")
