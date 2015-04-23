library(RODBC)

id <- read.table("电子商务文本ID", stringsAsFactors = F)

mycon <- odbcConnect("3.96", "root", "123456")
data1 <- sqlQuery(mycon, paste("select id content_wordseg from article where id in (", id, ")", sep = ""), stringsAsFactors = F)
data2 <- sqlQuery(mycon, "select article.id article.content_wordseg from article, article_classified where article.ID = article_classified.article_id and article_classified.category_id != 3 limit 3000")
