library(dplyr)
library(rcdk)
library(XLConnect)
library(xlsx)
#메모리 정리
xlcFreeMemory <- function(...) {
  gc(...)
  J("java.lang.Runtime")$getRuntime()$gc()
  invisible()
}
xlcFreeMemory()
#데이터 읽어오기
result = read.csv(file="C:/snuR/findSmiles/gotSmiles(210121수정).csv", 
                  header = TRUE, stringsAsFactors=F)
#데이터 가공(필요 column만 남기고 중복제거)
result = result[,c(2:3)]
result = result[!duplicated(result[,c('Name','SMILES')]),] 
#output dataframe 생성
output = data.frame(matrix(nrow=0, ncol=2))
##############################################
tmp = result[80001:90446,2]
mols1 <-parse.smiles(tmp)
#변형된 구조 찾아 데이터프레임 형태로 저장
trans <- data.frame(tmp[!sapply(mols1, is.null)], stringsAsFactors=F)
colnames(trans) = c("SMILES")
#해당 구조의 번호 sample파일에서 찾아 병합
ans = merge(trans,result[80001:90446,], by="SMILES", all.x = TRUE)
ans = ans[!duplicated(ans[,c('SMILES','Name')]),]
colnames(output) = c('SMILES','Name')
output = rbind(output,ans)
rm(ans, tmp, mols1, trans)
##############################################################################
data<-read.csv("C:/snuR/findSmiles/gotSmiles_parsing_duplicated(1229수정_npass80000개).csv",
               header = TRUE, stringsAsFactors=F)
#이전 data와 비교
data<-rbind(data,output)
data = data[!duplicated(data[,c('SMILES','Name')]),]

write.csv(data, "C:/snuR/findSmiles/gotSmiles_parsing_duplicated(0203).csv")
