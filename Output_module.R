#####################################################################################################################################
### Output module ###################################################################################################################
#####################################################################################################################################

# 加载需要的包 #
library(RJDBC)

jdbcdrv <- JDBC('com.mysql.jdbc.Driver', path.JDBC.driver)
mycon <- dbConnect(jdbcdrv, JDBC.char, DB.usr, DB.psd)

for(i in 1:length(test$ID)){
  category <- as.numeric(strsplit(test$category[i], split = "\\|")[[1]])
  score <- as.numeric(strsplit(test$recommend_score[i], split = "\\|")[[1]])
  for(j in 1:length(category)){
    dbSendUpdate(mycon, paste("insert into article_classified (article_id, category_id, recommend_score) VALUES (", test$ID[i], ",", category[j], ",", score[j], ")", sep = ""))
    cat('---------- 写入数据库中，第', i, '条记录对应的第', j, '个主题 ----------\n')
  }
}
dbDisconnect(mycon)

# 取消包加载 #
detach("package:RJDBC")

#####################################################################################################################################
#####################################################################################################################################
