#####################################################################################################################################
### Input module ####################################################################################################################
#####################################################################################################################################

# 加载需要的包 #
library(RJDBC)

# 配置JDBC并接入数据库 #
jdbcdrv <- JDBC('com.mysql.jdbc.Driver', path.JDBC.driver)
mycon <- dbConnect(jdbcdrv, JDBC.char, DB.usr, DB.psd)

# 获取最大ID #
ID <- dbGetQuery(mycon, SQL.select.maxID)
ID <- ifelse(is.na(ID), 0, ID)

# 读取数据 #
test <- dbGetQuery(mycon, paste(SQL.select.test, ID, sep = ""))

# 读取类别信息 #
category <- dbGetQuery(mycon, SQL.select.category)
category <- category[grep("其他", category$name, invert = T), ]
cate <- category

# 读取标签信息 #
tag<-dbGetQuery(mycon,SQL.select.tag)

# 关闭链接 #
dbDisconnect(mycon)

# 清除无效变量 #
rm(ID)

# 取消包加载 #
detach("package:RJDBC")
cat("========== Data Loading Finished ==========\n")

#####################################################################################################################################
#####################################################################################################################################