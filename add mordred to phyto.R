####데이터 불러오기####
test<- read.csv("C:\\snuR\\[data]\\Phyto_add_matrix\\got_smiles_standard_add_matrix.csv"
                ,stringsAsFactors = F)
mord1<-read.csv("C:\\snuR\\0310-0316_add_mordred\\1-1. mordred1.csv",stringsAsFactors = F)
mord2<-read.csv("C:\\snuR\\0310-0316_add_mordred\\1-2. mordred2.csv", stringsAsFactors = F)
mord<-rbind(mord1,mord2) #bind mord1&&mord2
rm(mord1,mord2)

####데이터 가공####
test<-test[,-1]
colnames(mord)[[1]]<-"SMILES"
test_t<-merge(test,mord,by="SMILES",all=FALSE)
test_t = test_t[!duplicated(test_t[,c('SMILES')]),]
test_m<-test_t[,1:1026]
col<-c("PEOE_VSA3","n6aHRing","NaasN","SaasN","NaaNH","SaaNH")
####결측치 제거####
for(i in 1:length(col)){
  for(j in 1026:ncol(test_t)){
    if(colnames(test_t)[[j]]==col[i]){
      test_m<-cbind(test_m,test_t[,j])
      colnames(test_m)[[1026+i]]=col[i]
      next
    }
  }
}
test_m<-na.omit(test_m)
#rm(test_t)
#####add potency#####
fps.matrix<-cbind(test_m[,3],test_m[,3:ncol(test_m)])

for(i in 1:nrow(fps.matrix)){
  fps.matrix[i,1]<-0
}
colnames(fps.matrix)[[1]]<-"porency"
################################################
write.csv(fps.matrix,"C:\\snuR\\tmp.csv",row.names = F)
fps.matrix<-read.csv("C:\\snuR\\tmp.csv")
################################################
pred_test = predict(rfModel, fps.matrix)
pred_test<-data.frame(pred_test)
out<-cbind(pred_test,test_m[,c(1,2)])
write.csv(out,"C:\\snuR\\0323_add_mordred_by_sklearn\\potency_result.csv")
