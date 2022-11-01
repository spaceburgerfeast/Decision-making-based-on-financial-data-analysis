#Getting USD/AUD FX from OANDA
getFX("USD/AUD")
#Deleting the weekend rows
Days <- weekdays(index(USDAUD))
USDAUD <- USDAUD[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_AUD FX from Drive
drive_download("USD_AUD.rds",overwrite = TRUE)
USD_AUD  <- readRDS("USD_AUD.rds")
if(!sum(as.numeric(is.na(USD_AUD$Price)))==0){USD_AUD$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_AUD$Price)))}
if(!sum(as.numeric(is.na(USD_AUD$Change)))==0){USD_AUD$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_AUD$Change)))}

if(end(USD_AUD)==Sys.Date()){
print("USD_AUD.RDS FILE IS UP TO DATE !!!")
}else{
USDAUD_2 <-USDAUD[!(index(USDAUD)%in%index(USD_AUD))]
a <- as.numeric(coredata(USD_AUD[end(USD_AUD)]$Price))
b <- as.numeric(coredata(USDAUD_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDAUD_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDAUD_2))
USD_AUD <- rbind(USD_AUD,USDAUD_NEW)


#Saving and Uploading to Drive
file.remove("USD_AUD.rds")
saveRDS(USD_AUD,file="USD_AUD.rds")
drive_rm("USD_AUD.rds")
drive_upload("USD_AUD.rds",path=as_id(drive_get("Data")))}
