#Getting USD/GBP  FX from OANDA
getFX("USD/GBP")
#Deleting the weekend rows
Days <- weekdays(index(USDGBP))
USDGBP <- USDGBP[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_GBP FX from Drive
drive_download("USD_GBP.rds",overwrite = TRUE)
USD_GBP  <- readRDS("USD_GBP.rds")
if(!sum(as.numeric(is.na(USD_GBP$Price)))==0){USD_GBP$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_GBP$Price)))}
if(!sum(as.numeric(is.na(USD_GBP$Change)))==0){USD_GBP$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_GBP$Change)))}

if(end(USD_GBP)==Sys.Date()){
print("USD_GBP.RDS FILE IS UP TO DATE !!!")
}else{
USDGBP_2 <-USDGBP[!(index(USDGBP)%in%index(USD_GBP))]
a <- as.numeric(coredata(USD_GBP[end(USD_GBP)]$Price))
b <- as.numeric(coredata(USDGBP_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDGBP_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDGBP_2))
USD_GBP <- rbind(USD_GBP,USDGBP_NEW)


#Saving and Uploading to Drive
file.remove("USD_GBP.rds")
saveRDS(USD_GBP,file="USD_GBP.rds")
drive_rm("USD_GBP.rds")
drive_upload("USD_GBP.rds",path=as_id(drive_get("Data")))}