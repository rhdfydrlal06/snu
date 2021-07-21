#####Import library&&function#####
library(dplyr)
library(rcdk)
library(caret)
library(readxl)
xlcFreeMemory <- function(...) {
  gc(...)
  J("java.lang.Runtime")$getRuntime()$gc()
  invisible()
}

#####Import data#####
#data <- read_excel("C:\\snuR\\IC50_EDNRA, NFKB.xlsx")
 data<-read.csv("C:\\Users\\gmlwo\\snuR\\0510_AMPK\\AMPK_traindata.csv"
                ,stringsAsFactors = F,fill=T)
#colnames(data)[[1]]<-"id"

#####Add matrix#####
for(i in 1:as.integer(nrow(data)/10000)+1){
  xlcFreeMemory()
  start <- (i-1)*10000+1
  end<-i*10000
  
  if (end>nrow(data))
    end = nrow(data)
  
  tmp<-data.frame(data[start:end,3])
  tmp<-as.character(tmp[[1]])
  jmols = parse.smiles(tmp)
  
  trans <- data.frame(tmp[!sapply(jmols, is.null)], stringsAsFactors=F)
  colnames(trans) = c("SMILES")
  #해당 구조의 번호 sample파일에서 찾아 병합
  ans = merge(trans,data[start:end,], by="SMILES", all.x = TRUE)
  ans = ans[!duplicated(ans[,c("SMILES","num")]),]
  jmols = parse.smiles(ans[,1])
  
  jfps = lapply(jmols, get.fingerprint, type='standard')
  fps.matrix = fingerprint::fp.factor.matrix(jfps)
  if(i == 1){
    mid_matrix<- fps.matrix
    mid_data<- ans
  } else{
    mid_matrix<- rbind(mid_matrix,fps.matrix)
    mid_data<- rbind(mid_data,ans)
  }
}

#####Export data#####
result<- cbind(mid_data,mid_matrix)
write.csv(result,"C:\\snuR\\0510_AMPK\\AMPK_traindata_add_matrix.csv",row.names = F)

################################################################################
################################################################################

#####nrow(data)<20000#####
xlcFreeMemory()
jmols = parse.smiles(data[,4])
jfps = lapply(jmols, get.fingerprint, type='standard')
fps.matrix = fingerprint::fp.factor.matrix(jfps)

result<- cbind(data,fps.matrix)
write.csv(result,"C:\\Users\\gmlwo\\Desktop\\n1_standard.csv",row.names = F)
