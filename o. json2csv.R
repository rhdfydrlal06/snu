#install.packages('jsonlite')
library(jsonlite)

data = data.frame(fromJSON("data.json"))

write.csv(data,"C:\\snuR\\data.csv")