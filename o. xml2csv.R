install.packages('XML')
library(XML)

result <- xmlToDataFrame("C:\\snuR\\desc2021.xml")

write.csv(result,"C:\\snuR\\desc2021.csv")
