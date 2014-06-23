# Laurie Zhang, Practical Machine Learning project
# free memory
rm(list = ls())
gc()
setwd("/Users/lauriezhang/Dropbox/Practical Machine Learning/R code/Project")
load("project_data.RData")

as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}
# use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants
# create training data and testing data for cross-validation
library(caret)
inBuild <- read.table("./pml-training.csv", header=TRUE, sep=",")
validation <- read.table("./pml-testing.csv", header=TRUE, sep=",")
validation <- validation[,-1]
inBuild <- inBuild[,-1]
inBuildVA <- inBuild[,colSums(is.na(inBuild)) <= 13000]
inBuildVA <- inBuildVA[, colSums(inBuildVA == "") <= 13000]
inBuildVA <- inBuildVA[,!names(inBuildVA) %in% c("user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window")]
str(inBuildVA)

inTrain <- createDataPartition(y=inBuildVA$classe, p=0.5, list=FALSE)
training <- inBuildVA[inTrain,]
testing <- inBuildVA[-inTrain,]
str(training)

# trainingNums <- training[,sapply(training, is.numeric)]
M <- abs(cor(training[,-53]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)

# # Pick useful predictors using PCA
# preProc <- preProcess(trainingNew[,-53], method="pca")
# trainPC <- predict(preProc, trainingNew[,-53])

# train different models
model_rpart <- train(training$classe ~ .,method="rpart",data=training)
model_rpart
predict(model_rpart,newdata=testing)
# mod1 <- train(classe ~.,method="glm",data=training)
mod2 <- train(classe ~.,method="rf",
              data=training, 
              trControl = trainControl(method="cv"),number=3)
# pred1 <- predict(mod1,testing)
answers <- predict(mod2,validation)
# function to store result into txt
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)