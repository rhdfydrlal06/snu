set.seed(10000)
library(mlbench)
library(caret)
##import data
data<-read.csv("C:\\snuR\\0303_NR3C1\\mordred_result_rm_NA.csv",header = T)
data_t<-cbind(data[,2],data[,4:1138])
colnames(data_t)[[1]]="potency"
#결측치 확인
sum(is.na(data_t))
#NA가 있는 열 삭제
cnt<-0
for(i in 3:ncol(data)){
  for(j in 1:nrow(data)){
    if(is.na(data[j,i])==T){
      tmp<-cnt+i
      data_t<-data_t[,-tmp]
      cnt<-cnt+1
      break
    }
  }
}
#lmFuncs
control <- rfeControl(functions=lmFuncs, method="cv", number=20)
results <- rfe(data_t[,3:1186], data_t[,1], rfeControl=control)
#nbFuncs
control <- rfeControl(functions=nbFuncs, method="cv", number=20)
results <- rfe(as.factor(data_t[,10:20]), as.matrix(as.factor(data_t[,1])), rfeControl=control)
#rfFuncs
control <- rfeControl(functions=rfFuncs, method="cv", number=20)
results <- rfe(data_t[,2:1136], data_t[,1], rfeControl=control)


results
# Visualize results for set sizes
print(results)
# List chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

write.csv(data_t,"C:\\snuR\\0316_add_mordred\\tmp.csv")
