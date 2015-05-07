#####################################################################################################################################
### Htnewsroom classify by topic ####################################################################################################
#####################################################################################################################################

# Load config #
source("/home/qiuqiu/R_workspace/htnewsroom_classification/Classify_config.R")

# log begin #
sink(paste(path.log, "classification.log", sep = ""), append = T , type = c("output", "message"))
cat("\n")
cat("\n========== Classify Begin ==========\n")
cat("\n",as.character(Sys.time()),"\n")
cat("\n")

# Read data #
source("/home/qiuqiu/R_workspace/htnewsroom_classification/Input_module.R")

if(length(data$ID != 0)){
  # Classify #
  source("/home/qiuqiu/R_workspace/htnewsroom_classification/Classify_module.R")
  
  # Score #
  source("/home/qiuqiu/R_workspace/htnewsroom_classification/Score_module.R")
  
  # Save data in DB #
  source("/home/qiuqiu/R_workspace/htnewsroom_classification/Output_module.R")
}else {cat("========== Data is zero ==========\n")}

# log end #
cat("\n",as.character(Sys.time()),"\n")
cat("\n")
cat("========== Classify Finished ==========\n")
sink()

#####################################################################################################################################
#####################################################################################################################################