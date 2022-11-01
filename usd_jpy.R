#Getting USD/JPY  FX from OANDA
getFX("USD/JPY")
#Deleting the weekend rows
Days <- weekdays(index(USDJPY))
USDJPY <- USDJPY[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_JPY FX from Drive
drive_download("USD_JPY.rds",overwrite = TRUE)
USD_JPY  <- readRDS("USD_JPY.rds")
if(!sum(as.numeric(is.na(USD_JPY$Price)))==0){USD_JPY$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_JPY$Price)))}
if(!sum(as.numeric(is.na(USD_JPY$Change)))==0){USD_JPY$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_JPY$Change)))}

if(end(USD_JPY)==Sys.Date()){
print("USD_JPY.RDS FILE IS UP TO DATE !!!")
}else{
USDJPY_2 <-USDJPY[!(index(USDJPY)%in%index(USD_JPY))]
a <- as.numeric(coredata(USD_JPY[end(USD_JPY)]$Price))
b <- as.numeric(coredata(USDJPY_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDJPY_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDJPY_2))
USD_JPY <- rbind(USD_JPY,USDJPY_NEW)


#Saving and Uploading to Drive
file.remove("USD_JPY.rds")
saveRDS(USD_JPY,file="USD_JPY.rds")
drive_rm("USD_JPY.rds")
drive_upload("USD_JPY.rds",path=as_id(drive_get("Data")))}
