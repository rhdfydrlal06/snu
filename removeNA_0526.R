data<- read.csv("C:\\Users\\gmlwo\\Documents\\카카오톡 받은 파일\\GP\\GP.csv")

result <- data[,1:2]
for (i in 3:ncol(data)){
  err <- data.frame(table(is.na(data[,i])))
  if(nrow(err)==1 & err[1,1] == "FALSE"){
    result<-cbind(result,data[,i])
    colnames(result)[[ncol(result)]] <- colnames(data)[[i]]
  }
    
}

table(is.na(result))
write.csv(result,"C:\\Users\\gmlwo\\Documents\\카카오톡 받은 파일\\GP\\GP_wo_NA.csv", row.names = F)
