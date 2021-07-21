setwd("C:\\snuR\\0.단백질별 model평가 작업\\addNegativeData\\addmatrix")
A.data <- read.csv('standard\\NR3C1_standard.csv', header=TRUE, stringsAsFactors = F)
A.data<-A.data[,-1]
colnames(A.data)[[2]]<-"chemical_id"
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

#pseed<-best seed값 넣기
set.seed(23232)

## 층화샘플링(각 3집단의 0,1 비율 고정해서 샘플링하는것) 실시 (doBy 패키지 사용)
Tra <- sampleBy(~ Class, frac = 0.6, data = A.data)
rest <- A.data[-Tra$chemical_id,]

Val <- sampleBy(~Class, frac = 0.5, data = rest)
Test <- A.data[-c(Tra$chemical_id,Val$chemical_id),]

D.tra <- Tra[,-c(1,2)] # 모델 학습에 필요없는 열 빼기
D.val <- Val[,-c(1,2)]
D.tes <- Test[,-c(1,2)]

## Random forest

#ntree = 트리 몇개? mtry = f의 값(속성 몇개?) 
#목표속성과 다른 속성을 나눠줄것이다.
x <- subset(D.tra, select = -c(potency))
y <- as.factor(D.tra$potency)

####이부분은 반복문 돌릴때 주석 처리하고 돌리기####  
mtrySeed <- 1234 #변수화시키기
set.seed(mtrySeed)

#어떤 mtry가 좋은지 알기 위해 도움받기
bestmtry<- tuneRF(x, y, stepFactor = 1.5, improve = 0.01, ntreeTry = 49) #찬반으로 하는거라 ntreetry는 홀수로 주는게 좋음 
#결과 : 에러가 18이 작음. 그래서 mTry 로 18 사용

print(bestmtry) 
###################################################
nTree <- 48
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

###############################TEST############################################

test<- read.csv("C:\\snuR\\[data]\\Phyto_add_matrix\\got_smiles_standard_add_matrix.csv"
                ,stringsAsFactors = F)
mord1<-read.csv("C:\\snuR\\0310\\1)47467.csv",colnames=F,stringsAsFactors = F)
mord2<-read.csv("C:\\snuR\\0310\\2)37733.csv",colnames=F, stringsAsFactors = F)

fps.matrix<-cbind(test[,1],test[,4:1027])

for(i in 1:nrow(fps.matrix)){
  fps.matrix[i,1]<-0
}
colnames(fps.matrix)[[1]]<-"porency"
################################################
write.csv(fps.matrix,"C:\\snuR\\0310\\tmp.csv",row.names = F)
fps.matrix<-read.csv("C:\\snuR\\0310\\tmp.csv")
################################################
pred_test = predict(rfModel, fps.matrix)
pred_test<-data.frame(pred_test)
out<-cbind(pred_test,test[,2:3])
write.csv(out,"C:\\snuR\\0310\\step1_result.csv")
