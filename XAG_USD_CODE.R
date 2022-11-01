#Getting XAG_USD FX from Drive
drive_download("XAG_USD.rds",overwrite = TRUE)
XAG_USD <- readRDS("XAG_USD.rds")
if(!sum(as.numeric(is.na(XAG_USD$Price)))==0){XAG_USD$Price <- impute_AR1_Gaussian(as.numeric(coredata(XAG_USD$Price)))}
if(!sum(as.numeric(is.na(XAG_USD$Change)))==0){XAG_USD$Change<- impute_AR1_Gaussian(as.numeric(coredata(XAG_USD$Change)))}

if(end(XAG_USD)==Sys.Date()){
print("XAG_USD.RDS FILE IS UP TO DATE !!!")
}else{
s <- menu(c("YES","NO"),title="\nIf you downloaded and inserted XAG_USD data from INVESTING press YES\n ")
if(s == 2){
browseURL("https://www.investing.com/currencies/xag-usd-historical-data")
J <- menu(c("YES","NO"),title="\nDid you insert XAG_USD data from INVESTING?\n ")
}
if(s ==1||J==1){
c <- read.csv("XAG_USD Historical Data.csv",stringsAsFactors=F)
XAGUSD_2 <- xts(x =data.frame(Price=as.numeric(gsub(",","",c$Price))),order.by=mdy(c$ï..Date))
XAGUSD_2 <-XAGUSD_2[!(index(XAGUSD_2)%in%index(XAG_USD))]
a <- as.numeric(coredata(XAG_USD[end(XAG_USD)]$Price))
b <- as.numeric(coredata(XAGUSD_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
XAGUSD_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(XAGUSD_2))
XAG_USD <- rbind(XAG_USD,XAGUSD_NEW)

#Saving and Uploading to Drive
file.remove("XAG_USD.rds")
saveRDS(XAG_USD,file="XAG_USD.rds")
drive_rm("XAG_USD.rds")
drive_upload("XAG_USD.rds",path=as_id(drive_get("Data")))}}
