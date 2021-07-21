##source: https://www.r-bloggers.com/2011/05/accessing-mysql-through-r/

#install.packages("RMySQL")
library(RMySQL)
mydb = dbConnect(MySQL(), user='snu', password='snu', dbname='snu', host='127.0.0.1')

##해당 table의 data 불러오기 (ex)rdb_chemicaltable)
rs = dbSendQuery(mydb, "select * from rdb_chemicaltable")
data = fetch(rs, n=-1)
