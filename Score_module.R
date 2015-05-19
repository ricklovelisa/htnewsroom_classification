#####################################################################################################################################
### Score module ####################################################################################################################
#####################################################################################################################################

### 提取得分关键词 ###
keywords_1<-cate$score_keywords[which(cate$id==1)]
keywords_1<-strsplit(keywords_1,",")[[1]]

keywords_2<-cate$score_keywords[which(cate$id==2)]
keywords_2<-strsplit(keywords_2,",")[[1]]

keywords_3<-cate$score_keywords[which(cate$id==3)]
keywords_3<-strsplit(keywords_3,",")[[1]]

keywords_4<-cate$score_keywords[which(cate$id==4)]
keywords_4<-strsplit(keywords_4,",")[[1]]

keywords_5<-cate$score_keywords[which(cate$id==5)]
keywords_5<-strsplit(keywords_5,",")[[1]]

keywords_6<-cate$score_keywords[which(cate$id==6)]
keywords_6<-strsplit(keywords_6,",")[[1]]

keywords_7<-cate$score_keywords[which(cate$id==7)]
keywords_7<-strsplit(keywords_7,",")[[1]]

keywords_8<-cate$score_keywords[which(cate$id==8)]
keywords_8<-strsplit(keywords_8,",")[[1]]

keywords_9<-cate$score_keywords[which(cate$id==9)]
keywords_9<-strsplit(keywords_9,",")[[1]]

keywords_10<-cate$score_keywords[which(cate$id==10)]
keywords_10<-strsplit(keywords_10,",")[[1]]

keywords_11<-cate$score_keywords[which(cate$id==11)]
keywords_11<-strsplit(keywords_11,",")[[1]]

##############其他类别的关键字#############
keywords_other<-tag$name[-1]

################获取数据#########################
#myconn<- odbcConnect("test_nutch", "root", "123456")
#data<-sqlQuery(myconn,"select ID,title,site from `concrawler`.`article` where to_days(create_time)>to_days('2014-12-08') group by title",stringsAsFactors = F)

#category_id<-c("|1|3|4|-1|","|2|4|5|9|","|5|6|7|8|10|")
#data<-cbind(data,category_id)


####################################
#这里开始用你的结果数据
#############site###############

test$site_weigth<-1
test$site_weigth[which(test$site=="虎嗅")] <- 3
test$site_weigth[which(test$site=="虎嗅网")] <- 3
test$site_weigth[which(test$site=="大数据网")] <- 3
test$site_weigth[which(test$site=="置顶网")] <- 2
test$site_weigth[which(test$site=="openstack")] <- 2
test$site_weigth[which(test$site=="人人都是产品经理")] <- 3
test$site_weigth[which(test$site=="36氪")] <- 3
test$site_weigth[which(test$site=="极客公园")] <- 3
test$site_weigth[which(test$site=="创业邦")] <- 3
test$site_weigth[which(test$site=="速途网")] <- 3
test$site_weigth[which(test$site=="站长网")] <- 2
test$site_weigth[which(test$site=="极客公园")] <- 3
test$site_weigth[which(test$site=="TechWeb")] <- 2
test$site_weigth[which(test$site=="科技娲母")] <- 2
test$site_weigth[which(test$site=="engadget中文版")] <- 3
test$site_weigth[which(test$site=="互联网的那点事")] <- 2
test$site_weigth[which(test$site=="飞象手机")] <- 2
test$site_weigth[which(test$site=="和讯科技")] <- 2
test$site_weigth[which(test$site=="爱范儿")] <- 3
test$site_weigth[which(test$site=="cnBeta")] <- 2
test$site_weigth[which(test$site=="网易科技频道")] <- 2
test$site_weigth[which(test$site=="钛媒体")] <- 3
test$sum_key_freq<-0
test$recommend_score<-array()

####################################################


test1<-test[grep("|1|",test$category_id, fixed = T),]
if(length(test1$ID) != 0){
  for(i in 1:length(keywords_1))
  {
    test1$sum_key_freq[grep(keywords_1[i],test1$title)] <- 1+test1$sum_key_freq[grep(keywords_1[i],test1$title)]
  }
  test1$recommend_score<-paste(test1$recommend_score,"|",test1$sum_key_freq*3+test1$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test1$ID))
  names(dd) <- names(test1)
  test<-rbind(test1,dd)
}
test$sum_key_freq <- 0

############################
test2<-test[grep("|2|",test$category_id,  fixed = T),]
if(length(test2$ID) != 0){
  for(i in 1:length(keywords_2))
  {
    test2$sum_key_freq[grep(keywords_2[i],test2$title)] <- 1+test2$sum_key_freq[grep(keywords_2[i],test2$title)]
  }
  
  test2$recommend_score<-paste(test2$recommend_score,"|",test2$sum_key_freq*3+test2$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test2$ID))
  names(dd) <- names(test2)
  test<-rbind(test2,dd)
}
test$sum_key_freq <- 0

###########################
test3<-test[grep("|3|",test$category_id,  fixed = T),]
if(length(test3$ID) != 0){
  for(i in 1:length(keywords_3))
  {
    # test3$sum_key_freq[grep(keywords_3[i],test3$title)] <- 1+test3$sum_key_freq[grep(keywords_3[i],test3$title)]
    test3$sum_key_freq <- 1
  }
  
  test3$recommend_score<-paste(test3$recommend_score,"|",test3$sum_key_freq*3+test3$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test3$ID))
  names(dd) <- names(test3)
  test<-rbind(test3,dd)
}
test$sum_key_freq <- 0

#############################
test4<-test[grep("|4|",test$category_id,  fixed = T),]
if(length(test4$ID) != 0){
  for(i in 1:length(keywords_4))
  {
    test4$sum_key_freq[grep(keywords_4[i],test4$title)] <- 1+test4$sum_key_freq[grep(keywords_4[i],test4$title)]
  }
  
  test4$recommend_score<-paste(test4$recommend_score,"|",test4$sum_key_freq*3+test4$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test4$ID))
  names(dd) <- names(test4)
  test<-rbind(test4,dd)
}
test$sum_key_freq <- 0

######################################
test5<-test[grep("|5|",test$category_id,  fixed = T),]
if(length(test5$ID) != 0){
  for(i in 1:length(keywords_5))
  {
    test5$sum_key_freq[grep(keywords_5[i],test5$title)] <- 1+test5$sum_key_freq[grep(keywords_5[i],test5$title)]
  }
  
  test5$recommend_score<-paste(test5$recommend_score,"|",test5$sum_key_freq*3+test5$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test5$ID))
  names(dd) <- names(test5)
  test<-rbind(test5,dd)
}
test$sum_key_freq <- 0

###################################
test6<-test[grep("|6|",test$category_id,  fixed = T),]
if(length(test6$ID) != 0){
  for(i in 1:length(keywords_6))
  {
    try(test6$sum_key_freq[grep(keywords_6[i],test6$title)] <- 1+test6$sum_key_freq[grep(keywords_6[i],test6$title)])
  }
  
  test6$recommend_score<-paste(test6$recommend_score,"|",test6$sum_key_freq*3+test6$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test6$ID))
  names(dd) <- names(test6)
  test<-rbind(test6,dd)
}
test$sum_key_freq <- 0

###################################
test7<-test[grep("|7|",test$category_id,  fixed = T),]
if(length(test7$ID) != 0){
  for(i in 1:length(keywords_7))
  {
    test7$sum_key_freq[grep(keywords_7[i],test7$title)] <- 1+test7$sum_key_freq[grep(keywords_7[i],test7$title)]
  }
  
  test7$recommend_score<-paste(test7$recommend_score,"|",test7$sum_key_freq*3+test7$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test7$ID))
  names(dd) <- names(test7)
  test<-rbind(test7,dd)
}
test$sum_key_freq <- 0

################################
test8<-test[grep("|8|",test$category_id,  fixed = T),]
if(length(test8$ID) != 0){
  for(i in 1:length(keywords_8))
  {
    test8$sum_key_freq[grep(keywords_8[i],test8$title)] <- 1+test8$sum_key_freq[grep(keywords_8[i],test8$title)]
  }
  
  test8$recommend_score<-paste(test8$recommend_score,"|",test8$sum_key_freq*3+test8$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test8$ID))
  names(dd) <- names(test8)
  test<-rbind(test8,dd)
}
test$sum_key_freq <- 0

###################################
test9<-test[grep("|9|",test$category_id,  fixed = T),]
if(length(test9$ID) != 0){
  for(i in 1:length(keywords_9))
  {
    test9$sum_key_freq[grep(keywords_9[i],test9$title)] <- 1+test9$sum_key_freq[grep(keywords_9[i],test9$title)]
  }
  
  test9$recommend_score<-paste(test9$recommend_score,"|",test9$sum_key_freq*3+test9$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test9$ID))
  names(dd) <- names(test9)
  test<-rbind(test9,dd)
}
test$sum_key_freq <- 0

#############################
test10<-test[grep("|10|",test$category_id,  fixed = T),]
if(length(test10$ID) != 0){
  for(i in 1:length(keywords_10))
  {
    try(test10$sum_key_freq[grep(keywords_10[i],test10$title)] <- 1+test10$sum_key_freq[grep(keywords_10[i],test10$title)])
  }
  
  test10$recommend_score<-paste(test10$recommend_score,"|",test10$sum_key_freq*3+test10$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test10$ID))
  names(dd) <- names(test10)
  test<-rbind(test10,dd)
}
test$sum_key_freq <- 0

################################
test_other<-test[grep("|-1|",test$category_id,  fixed = T),]
if(length(test_other$ID) != 0){
  for(i in 1:length(keywords_other))
  {
    try(test_other$sum_key_freq[grep(keywords_other[i],test_other$title)] <- 1+test_other$sum_key_freq[grep(keywords_other[i],test_other$title)])
  }
  
  test_other$recommend_score<-paste(test_other$recommend_score,"|",test_other$sum_key_freq*3+test_other$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test_other$ID))
  names(dd) <- names(test_other)
  test<-rbind(test_other,dd)
}
test$sum_key_freq <- 0

#####################################
test11<-test[grep("|11|",test$category_id,  fixed = T),]
if(length(test11$ID) != 0){
  test11$sum_key_freq<-0
  for(i in 1:length(keywords_11))
  {
    try(test11$sum_key_freq[grep(keywords_11[i],test11$title)] <- 1+test11$sum_key_freq[grep(keywords_11[i],test11$title)])
  }
  
  test11$recommend_score<-paste(test11$recommend_score,"|",test11$sum_key_freq*3+test11$site_weigth,sep="")
  dd<-subset(test, !(test$ID %in% test11$ID))
  names(dd) <- names(test11)
  test<-rbind(test11,dd)
}

##############################
test$recommend_score<-gsub("NA\\|","",test$recommend_score)

test <- test[order(test$ID),]