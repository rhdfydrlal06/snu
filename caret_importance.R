set.seed(100)
library(mlbench)
library(caret)
##import data
data<-read.csv("c:/snuR/0202/PLIN1_PADEL_potency.csv",header = T, stringsAsFactors = F)
##pick the data which we need
data_t<-data.frame(as.factor(data[,2]))
cnt = 1
colnames(data_t)<-c("potency")
for(i in 3:ncol(data)){
  if(is.na(data[1,i])==F){
    cnt = cnt+1
    data_t<-cbind(data_t,data[,i])
    colnames(data_t)[[cnt]]<-colnames(data)[[i]]
  }
}
#set of train
control <- trainControl(
  method="repeatedcv", number=10, repeats=3)
#trainning model
model <- train(potency~., data=data_t, method="lvq",
               preProcess="scale", trControl=control)
#calculate importance
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)


data_top<-read.csv("c:/snuR/0202/caret_importance_PLIN1/top_20.csv",header = F, stringsAsFactors = F)
top<-data.frame(data_t[,1])
cnt = 1
colnames(top)<-"potency"
for(i in 1:nrow(data_top)){
  for(j in 1:ncol(data_t)){
    if(data_top[i,1]==colnames(data_t)[[j]]){
      cnt=cnt+1
      top<-cbind(top,data_t[,j])
      colnames(top)[[cnt]]<-colnames(data_t)[[j]]
    }
  }
}