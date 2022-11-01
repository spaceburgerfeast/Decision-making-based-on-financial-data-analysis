
#---------------------------------------------------------------------
#Getting USD/CHF FX from OANDA
getFX("USD/CHF")
#Deleting the weekend rows
Days <- weekdays(index(USDCHF))
USDCHF <- USDCHF[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_CHF FX from Drive
drive_download("USD_CHF.rds",overwrite = TRUE)
USD_CHF  <- readRDS("USD_CHF.rds")
if(!sum(as.numeric(is.na(USD_CHF$Price)))==0){USD_CHF$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_CHF$Price)))}
if(!sum(as.numeric(is.na(USD_CHF$Change)))==0){USD_CHF$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_CHF$Change)))}

if(end(USD_CHF)==Sys.Date()){
print("USD_CHF.RDS FILE IS UP TO DATE !!!")
}else{
USDCHF_2 <-USDCHF[!(index(USDCHF)%in%index(USD_CHF))]
a <- as.numeric(coredata(USD_CHF[end(USD_CHF)]$Price))
b <- as.numeric(coredata(USDCHF_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDCHF_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDCHF_2))
USD_CHF <- rbind(USD_CHF,USDCHF_NEW)


#Saving and Uploading to Drive
file.remove("USD_CHF.rds")
saveRDS(USD_CHF,file="USD_CHF.rds")
drive_rm("USD_CHF.rds")
drive_upload("USD_CHF.rds",path=as_id(drive_get("Data")))}
