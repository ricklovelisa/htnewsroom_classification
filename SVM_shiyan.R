library(RODBC)
library(rJson)


id <- read.table("ecommerce_id.txt", stringsAsFactors = F)

mycon <- odbcConnect("3.96", "root", "123456")
data1 <- sqlQuery(mycon, paste("select id, content_wordseg from article where id in (", id, ")", sep = ""), stringsAsFactors = F)
data2 <- sqlQuery(mycon, "select article.id, article.content_wordseg from article limit 4000", stringsAsFactors = F)

data2 <- data2[!(data2$id %in% data1$id), ]

data <- rbind(data1, data2)
rm(data1, data2)
id <- as.integer(strsplit(id$V1, split = ",")[[1]])

data$category <- 2
data$category[data$id %in% id] <- 1

rownames(data) <- paste(data$id, "_", data$category, sep = "")


data.list() <- 
