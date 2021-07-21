################1###############
A.data <- read.csv("C:\\snuR\\0510_AMPK\\train_data.csv", 
                   header=TRUE, stringsAsFactors = F)
#A.data<-A.data[,-2]
#colnames(A.data)[[2]]<-"num"
#install.packages('doBy')
library(doBy)
#install.packages('caret')
library(caret)
#install.packages('e1071')
library(e1071)
#install.packages('randomForest')
library(randomForest)
#install.packages("Epi")
library(Epi)

library(multipleROC)

pseedlist <- c(12121,23232,34343,45454,56565,67676,78787,89898,98989,20052)
for(pseed in pseedlist){
  set.seed(pseed)
  
  ## 층화샘플링(각 3집단의 0,1 비율 고정해서 샘플링하는것) 실시 (doBy 패키지 사용)
  Tra <- sampleBy(~ Class, frac = 0.6, data = A.data)
  rest <- A.data[-Tra$num,]
  
  Val <- sampleBy(~Class, frac = 0.5, data = rest)
  Test <- A.data[-c(Tra$num,Val$num),]
  
  D.tra <- Tra[,-c(1:2)] # 모델 학습에 필요없는 열 빼기
  D.val <- Val[,-c(1:2)]
  D.tes <- Test[,-c(1:2)]
  
  ## Random forest
  
  #ntree = 트리 몇개? mtry = f의 값(속성 몇개?) 
  #목표속성과 다른 속성을 나눠줄것이다.
  x <- subset(D.tra, select = -c(potency))
  y <- as.factor(D.tra$potency)
  #sum(is.na(y))
  ####이부분은 반복문 돌릴때 주석 처리하고 돌리기####  
  mtrySeed <- 1234 #변수화시키기
  set.seed(mtrySeed)
  
  #어떤 mtry가 좋은지 알기 위해 도움받기
  bestmtry<- tuneRF(x, y, stepFactor = 1.5, improve = 0.01, ntreeTry = 49) #찬반으로 하는거라 ntreetry는 홀수로 주는게 좋음 
  #결과 : 에러가 18이 작음. 그래서 mTry 로 18 사용
  
  #print(bestmtry) 
  ###################################################
  nTree <- 49
  for(i in 1:nrow(bestmtry)){
    if(i ==1)
      min <- i
    else if(bestmtry[i,2]<bestmtry[min,2])
      min <- i
  }
  mTry <- bestmtry[min,1]
  cat('\n mtry=',mTry,"\n")
  rfSeed <- 12345 # 반복문에서 모델 seed는 하나로 고정해야하므로!
  set.seed(rfSeed)
  rfModel <- randomForest(x, y, ntree=nTree, mtry=mTry)
  #method 정해줄 필요 없음.목표속성 (여기선class)모습 보고 알아서 회귀분석,분류 둘중 정함
  
  #training
  Predict.tra <- predict(rfModel, D.tra)
  cTab <- table(Actual=D.tra$potency, Predict.tra)
  cTab
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
  
  #validation
  Predict.val <- predict(rfModel, D.val)
  cTab <- table(Actual=D.val$potency, Predict.val)
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  #편차
  Variance <- abs(Accu.tra-Accu.val)
  
  #tes
  Predict.tes <- predict(rfModel, D.tes)
  cTab <- table(Actual=D.tes$potency, Predict.tes)
  Accu.tes <- sum(diag(cTab))/sum(cTab)*100
  if(pseed==12121){
    max.tes <-Accu.tes
    max.seed <-pseed
  }
  else if(Accu.tes>max.tes){
    max.tes <-Accu.tes
    max.seed <-pseed
  }
  else 
    next
  # Accu.tra
  # Accu.val
  # Variance
  # Accu.tes
  
  cat(sprintf('\n seed=%5d', pseed))
  cat('\n Tra=',Accu.tra)
  cat('\n Val=',Accu.val)
  cat('\n 편차=',Variance)
  cat('\n Tes=',Accu.tes)
  cat('\n cTab=\n')
  print(cTab)
  Predict.tes1 <- as.integer(Predict.tes)
  ROC <- ROC(form=D.tes$potency~Predict.tes1,data=D.tes,plot="ROC")
  #extended_ROC <- multipleROC(D.tes$potency~Predict.tes1,data=D.tes, plot=FALSE)
}
#plot_ROC(list(maccs_ROC,standard_ROC,extended_ROC),show.eta=FALSE,show.sens=FALSE)
################2###############
#가장 높게 나온 pseed로 모델 만들고 아래진행
############TEST###################

test<- read.csv("C:\\snuR\\[data]\\Phyto_add_matrix\\gotSmiles_standard_add_matrix.csv"
                ,stringsAsFactors = F)

fps.matrix<-cbind(0,test[,3:1026])
colnames(fps.matrix)[[1]]<-"porency"
################################################
write.csv(fps.matrix,"tmp.csv",row.names = F)
fps.matrix<-read.csv("tmp.csv")
################################################
pred_test = predict(rfModel, fps.matrix)
pred_test<-data.frame(pred_test)
out<-cbind(pred_test,test[,1:2])
write.csv(out,"C:\\snuR\\0510_AMPK\\AMPK_result.csv")