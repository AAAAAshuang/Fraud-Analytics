setwd("~/2019 spring/DSO 562/Project3")


data = fread('applications.csv')    # stringsAsFactors = F)
#d = fread('train100k.csv', stringsAsFactors = F)
data$date=as.Date(data$date)
# See it has a data.table class
str(data)
# Create another new copy
data2 = data
setDT(data2)

Velocity = function(data, n, byVar){
  dt1 = data
  dt1$join_ts1 = dt1$date
  dt1$join_ts2 = dt1$date + n
  dt1$join_rec = dt1$record
  keys = c(byVar, 'join_ts1<=date', 'join_ts2>=date', 'record<=record')
  dt2 = dt1[data, on=keys, allow.cartesian=T]
  return(dt2)
}

single_groupby = c('ssn','fulladdress','nameDOB','homephone')

for (i in c(0,1,3,7,14,30)){
  for (j in single_groupby){
    assign(paste0("data",j,i), Velocity(data2, i, j))
  }
}
