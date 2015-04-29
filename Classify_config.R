#####################################################################################################################################
### config text #####################################################################################################################
#####################################################################################################################################

##########
## path ##
##########

# 日志路径 #
path.log <- "/home/qiuqiu/R_workspace/log/"
name.log <- "log"

# 脚本路径 #
path.VSM_module <- "/home/qiuqiu/R_workspace/classify by topic/X-tfidf-svm/"
path.Function_module <- "/home/qiuqiu/R_workspace/classify by topic/X-tfidf-svm/"

# 特征词路径 #
path.feature.title <- "/home/qiuqiu/R_workspace/classify by topic/X-tfidf-svm/category_feature_title/"
path.feature.content <- "/home/qiuqiu/R_workspace/classify by topic/X-tfidf-svm/category_feature_content/"

# JDBC驱动路径 #
path.JDBC.driver <- "/home/qiuqiu/JDBC_driver/mysql-connector-java-5.1.7-bin.jar"

# 模型路径 #
path.model <- "/home/qiuqiu/R_workspace/Model/"

############
## DB配置 ##
############

# JDBC字串 #
JDBC.char <- "jdbc:mysql://172.16.3.96/htnewsroom"

# 查询SQL #
SQL.select.maxID <- "SELECT max(Article_id) FROM article_classified"
SQL.select.test <- "SELECT ID, title, content, site FROM article where ID > "
SQL.select.category <- "SELECT * from category"
SQL.select.tag <- "SELECT id, name from htnewsroom.tag"

# 数据库账户 #
DB.usr <- "root"
DB.psd <- "123456"


##################
## 文本字符信息 ##
##################

# Encode #
encode <- "UTF-8"

# SVM分类类别 #
svmcate <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)

# 匹配分类类别 #
grepcate <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

#####################################################################################################################################
#####################################################################################################################################