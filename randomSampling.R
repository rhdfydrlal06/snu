data<- read.csv("C:/Users/gmlwo/snuR/0525_merge_decoey/merge_total_dataset_0526.csv")

rdm <- sample(1:nrow(data), 22391, replace=F) #22391: 랜덤 추출할 개수
data_rdm <- data[rdm,1:4]

write.csv(data_rdm,"decoy_random_sample.csv", row.names = F)
