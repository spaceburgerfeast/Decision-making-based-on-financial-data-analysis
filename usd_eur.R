#Getting USD/EUR FX from OANDA
getFX("USD/EUR")
#Deleting the weekend rows
Days <- weekdays(index(USDEUR))
USDEUR <- USDEUR[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_EUR FX from Drive
drive_download("USD_EUR.rds",overwrite = TRUE)
USD_EUR  <- readRDS("USD_EUR.rds")
if(!sum(as.numeric(is.na(USD_EUR$Price)))==0){USD_EUR$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_EUR$Price)))}
if(!sum(as.numeric(is.na(USD_EUR$Change)))==0){USD_EUR$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_EUR$Change)))}

if(end(USD_EUR)==Sys.Date()){
print("USD_EUR.RDS FILE IS UP TO DATE !!!")
}else{
USDEUR_2 <-USDEUR[!(index(USDEUR)%in%index(USD_EUR))]
a <- as.numeric(coredata(USD_EUR[end(USD_EUR)]$Price))
b <- as.numeric(coredata(USDEUR_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDEUR_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDEUR_2))
USD_EUR <- rbind(USD_EUR,USDEUR_NEW)


#Saving and Uploading to Drive
file.remove("USD_EUR.rds")
saveRDS(USD_EUR,file="USD_EUR.rds")
drive_rm("USD_EUR.rds")
drive_upload("USD_EUR.rds",path=as_id(drive_get("Data")))}
