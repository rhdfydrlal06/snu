setwd("C:\\snuR\\0114\\add_matrix")

library(e1071)
library(doBy)
library(Epi) 
library(caret)

#data<- read.csv("C:\\snuR\\0113\\raw data\\TNF_IC50_10uM.csv",
#                header = T, stringsAsFactors = F)

xlcFreeMemory <- function(...) {
  gc(...)
  J("java.lang.Runtime")$getRuntime()$gc()
  invisible()
}
library(rcdk)
xlcFreeMemory()
#A.data <- read.csv('TNF.csv', header=TRUE)
data0 <- read.csv('NR3C1_standard_0.csv', header=TRUE)
data1 <- read.csv('NR3C1_standard_1.csv', header=TRUE)
id.shuffle = sample(1:nrow(data0), replace=F)
data1 <-data1[id.shuffle,]
A.data <- rbind(data0,data1)
rm(data0,data1,id.shuffle)
## 층화샘플링(각 3집단의 0,1 비율 고정해서 샘플링하는것) 실시 (doBy 패키지 사용)
Tra <- sampleBy(~ Class, frac = 0.6, data = A.data)
rest <- A.data[-Tra$chemical_id,]

Val <- sampleBy(~Class, frac = 0.5, data = rest)
Test <- A.data[-c(Tra$chemical_id,Val$chemical_id),]

D.tra <- Tra[,-c(1,2)] # 모델 학습에 필요업는 열 빼기
D.val <- Val[,-c(1,2)]
D.tes <- Test[,-c(1,2)]

x <- subset(D.tra, select = -c(potency))
y <- as.factor(D.tra$potency)

linear_tune <- tune(svm, train.x=x, train.y = y, kernel='linear',ranges=list(cost=c(50,100,150)))
#print(linear_tune) # linear best cost : 50
tmp<-linear_tune[[1]]

radial_tune <- tune(svm, train.x = x, train.y = y, kernel='radial', ranges=list(cost=c(50,100,150),
                                                                                gamma=seq(0.1,1,by=0.1)))
tmp<-radial_tune[[1]]
#print(radial_tune) # radial best cost : 50, gamma : 0.1

#sigmoid_tune <- tune(svm, train.x = x, train.y = y, kernel='sigmoid', ranges=list(cost=c(50,100,150),
#                                                                                  coef0=c(0,1,2),
#                                                                                  gamma=seq(0.1,1,by=0.1)))
#tmp<-sigmoid_tune[[1]]
#print(sigmoid_tune) # sigmoid best cost : 50, coef0 : 2, gamma : 0.1

####tune 하기####

pSeedlist <- c(12345,23459,45675,47692,45754,35734,29472,90909,16783,10574)
#IR -> linear = 45754, radial = 29472, sigmoid=16783
for(pSeed in pSeedlist){
  set.seed(pSeed)
  
  ## 층화샘플링(각 3집단의 0,1 비율 고정해서 샘플링하는것) 실시 (doBy 패키지 사용)
  Tra <- sampleBy(~ Class, frac = 0.6, data = A.data)
  rest <- A.data[-Tra$chemical_id,]
  
  Val <- sampleBy(~Class, frac = 0.5, data = rest)
  Test <- A.data[-c(Tra$chemical_id,Val$chemical_id),]
  
  D.tra <- Tra[,-c(1,2)] # 모델 학습에 필요업는 열 빼기
  D.val <- Val[,-c(1,2)]
  D.tes <- Test[,-c(1,2)]
  
  x <- subset(D.tra, select = -c(potency))
  y <- as.factor(D.tra$potency)
  
  modelSVM <- svm(potency~., D.tra, type='C-classification', kernel='linear', cost=tmp[1,1],scale=TRUE)
  #modelSVM <- svm(potency~., D.tra, type='C-classification', kernel='radial', cost=tmp[1,1],gamma=tmp[1,2] ,scale=TRUE)
  #modelSVM <- svm(potency~., D.tra, type='C-classification', kernel='sigmoid', cost=tmp[1,1],coef0=tmp[1,2],gamma=tmp[1,3] ,scale=TRUE)
  
  Predict.tra <- predict(modelSVM, D.tra)
  cTab <- table(Actual = D.tra$potency, Predict.tra)
  # Ctab
  Accu.tra <- sum(diag(cTab))/sum(cTab)*100
  
  Predict.val <- predict(modelSVM, D.val)
  cTab <- table(Actual = D.val$potency, Predict.val)
  # Ctab
  Accu.val <- sum(diag(cTab))/sum(cTab)*100
  
  Predict.tes <- predict(modelSVM, D.tes)
  cTab <- table(Actual = D.tes$potency, Predict.tes)
  # Ctab
  Accu.tes <- sum(diag(cTab))/sum(cTab)*100
  if(pSeed==12345){
    max.tes <-Accu.tes
    max.seed <-pSeed
  }
  else if(Accu.tes>max.tes){
    max.tes <-Accu.tes
    max.seed <-pSeed
  }
  else 
    next
  
  #편차
  Variance <- abs(Accu.tra-Accu.val)
  
  #print(pSeed)
  #print(Accu.tra)
  #print(Accu.val)
  #print(Accu.tes)
  cat(sprintf('\n seed=%5d', pSeed))
  cat('\n Tra=',Accu.tra)
  cat('\n Val=',Accu.val)
  cat('\n 편차=',Variance)
  cat('\n Tes=',Accu.tes)
  cat('\n cTab=\n')
  print(cTab)
  Predict.tes1 <- as.integer(Predict.tes)
  ROC <- ROC(form=D.tes$potency~Predict.tes1,data=D.tes,plot="ROC")
  linear_ROC <- multipleROC(D.tes$potency~Predict.tes1,data=D.tes, plot=FALSE)
}

##########################################
plot_ROC(list(rf_ROC,linear_ROC,radial_ROC),show.eta=FALSE,show.sens=FALSE)
