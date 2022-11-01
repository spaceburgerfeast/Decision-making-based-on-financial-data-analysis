a<- read.csv("USD_TRY Historical Data.csv",stringsAsFactors=F)
b<- read.csv("USD_TRY Historical Data (1).csv",stringsAsFactors=F)
#d<- read.csv("US Dollar Index Historical Data (2).csv",stringsAsFactors=F)

c<- rbind(b,a)
#c<- rbind(d,rbind(b,a))
USD_TRY <- xts(x=data.frame(Price=as.numeric(c$Price),Change=as.numeric(gsub("%","",c$Change..))),order.by=mdy(c$ï..Date))


saveRDS(USD_TRY,file="USD_TRY.rds")
