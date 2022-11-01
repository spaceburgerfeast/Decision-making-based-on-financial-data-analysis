#Checking the availabiliry of the necessary packages and installing the absent
#packages
a <- installed.packages()
a <-a[,1]
necessery_packages <- c("tidyquant","RCurl","googledrive","lubridate","stringr","quantmod","imputeFin")

for(i in necessery_packages)
{
if(!is.element(i,a))
{
install.packages(i)
}
library(i,character.only=T)
}

#Checking the availability of necessary inputs to the program
if(!is.element("Data",list.files()))
{
print("Obtaining the necessary inputs")
dir.create("Data")
setwd(str_c(getwd(),"/Data"))
data_names <- c("bisttum.csv","USD_TRY.csv","XAU_USD.csv","XU100.rds","BIST 100 Historical Data.csv")
for(i in data_names)
{
drive_download(i,overwrite = TRUE)
}

k1 <- read.csv("USD_TRY.csv",stringsAsFactors=F)
k1$Date <- as.Date(k1$Date)
getFX("USD/TRY",from=k1$Date[1])
if(length(USDTRY)==1)
{
print("USD_TRY.csv file is up to date!")

}else{
getFX("USD/TRY",from=k1$Date[1]+days(1))
ind1 <- index(USDTRY)[order(seq(length(USDTRY)),decreasing=T)]
cdat1 <- coredata(USDTRY)[order(seq(length(USDTRY)),decreasing=T)]
ch <- 100*((cdat1/c(cdat1[-1],k1$Price[1]))-1)
USD_TRY <- data.frame(Date=c(ind1,k1$Date),Price=c(cdat1,k1$Price),Change=c(ch,k1$Change))
#IMPUTATION
if(sum(as.numeric(is.na(USD_TRY)))!=0){
USD_TRY <- impute_AR1_Gaussian(USD_TRY)}
write.csv(USD_TRY,str_c(getwd(),"/USD_TRY.csv"))
drive_rm("USD_TRY.csv")
drive_upload("USD_TRY.csv",path=as_id(drive_get("Data")))
}

k2 <- read.csv("XAU_USD.csv",stringsAsFactors=F)
k2$Date <- as.Date(k2$Date)
getFX("XAU/USD",from=k2$Date[1])
if(length(XAUUSD)==1)
{
print("XAU_USD.csv file is up to date!")

}else{
getFX("XAU/USD",from=k2$Date[1]+days(1))
ind1 <- index(XAUUSD)[order(seq(length(XAUUSD)),decreasing=T)]
cdat1 <- coredata(XAUUSD)[order(seq(length(XAUUSD)),decreasing=T)]
ch <- 100*((cdat1/c(cdat1[-1],k2$Price[1]))-1)
XAU_USD <- data.frame(Date=c(ind1,k2$Date),Price=c(cdat1,k2$Price),Change=c(ch,k2$Change))
#IMPUTATION
if(sum(as.numeric(is.na(XAU_USD)))!=0){
XAU_USD <- impute_AR1_Gaussian(XAU_USD)}
write.csv(XAU_USD,str_c(getwd(),"/XAU_USD.csv"))
drive_rm("XAU_USD.csv")
drive_upload("XAU_USD.csv",path=as_id(drive_get("Data")))
}

#BIST symbols are loading
bst <- read.csv("bisttum.csv",stringsAsFactors=F)
bst <- bst$Code
bst <- str_c(bst,".IS")
bst <- gsub(" ","",bst)

#Downloading stock prices in R
#data_content <- drive_ls(as_id(drive_get("Data")))
#data_content <- data_content$name

for(i in 1:length(bst)){
#if(!is.element(str_c(bst[i],".rds"),data_content)){
assign(bst[i],1)
tryCatch({
getSymbols(bst[i],verbose = TRUE,from="1950-01-01")
if(get(bst[i])==1){
bst<-bst[-i]
next}
if(sum(as.numeric(is.na(get(bst[i]))))!=0){
tmp <- impute_AR1_Gaussian(get(bst[i]))
assign(bst[i],tmp)}
#ASELS.IS CORRECTION
if(bst[i]=="ASELS.IS"){
for(k in 1:4)
{ASELS.IS[,k][index(ASELS.IS)<as.Date("2020-07-23")]<-ASELS.IS[,k][index(ASELS.IS)<as.Date("2020-07-23")]/2 }
}

saveRDS(get(bst[i]),file=str_c(bst[i],".rds"))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})

#drive_upload(str_c(bst[i],".rds"),path=as_id(drive_get("Data")))
#}else{
#drive_download(str_c(bst[i],".rds"))
#assign("bst_tmp",readRDS(str_c(bst[i],".rds")))}
}
#If the data file is there!!
}else{
print("DATA FILE IS THERE. CHECKING FOR UPDATE")
setwd(str_c(getwd(),"/Data"))
k1 <- read.csv("USD_TRY.csv",stringsAsFactors=F)
k1$Date <- as.Date(k1$Date)
getFX("USD/TRY",from=k1$Date[1])
if(length(USDTRY)==1)
{
print("USD_TRY.csv file is up to date!")

}else{
getFX("USD/TRY",from=k1$Date[1]+days(1))
ind1 <- index(USDTRY)[order(seq(length(USDTRY)),decreasing=T)]
cdat1 <- coredata(USDTRY)[order(seq(length(USDTRY)),decreasing=T)]
ch <- 100*((cdat1/c(cdat1[-1],k1$Price[1]))-1)
USD_TRY <- data.frame(Date=c(ind1,k1$Date),Price=c(cdat1,k1$Price),Change=c(ch,k1$Change))
#IMPUTATION
if(sum(as.numeric(is.na(USD_TRY)))!=0){
USD_TRY <- impute_AR1_Gaussian(USD_TRY)}
file.remove("USD_TRY.csv")
write.csv(USD_TRY,str_c(getwd(),"/USD_TRY.csv"))
drive_rm("USD_TRY.csv")
drive_upload("USD_TRY.csv",path=as_id(drive_get("Data")))
}
#------------------------------------------------------------
k2 <- read.csv("XAU_USD.csv",stringsAsFactors=F)
k2$Date <- as.Date(k2$Date)
getFX("XAU/USD",from=k2$Date[1])
if(length(XAUUSD)==1)
{
print("XAU_USD.csv file is up to date!")

}else{
getFX("XAU/USD",from=k2$Date[1]+days(1))
ind1 <- index(XAUUSD)[order(seq(length(XAUUSD)),decreasing=T)]
cdat1 <- coredata(XAUUSD)[order(seq(length(XAUUSD)),decreasing=T)]
ch <- 100*((cdat1/c(cdat1[-1],k2$Price[1]))-1)
XAU_USD <- data.frame(Date=c(ind1,k2$Date),Price=c(cdat1,k2$Price),Change=c(ch,k2$Change))
#IMPUTATION
if(sum(as.numeric(is.na(XAU_USD)))!=0){
XAU_USD <- impute_AR1_Gaussian(XAU_USD)}
file.remove("XAU_USD.csv")
write.csv(XAU_USD,str_c(getwd(),"/XAU_USD.csv"))
drive_rm("XAU_USD.csv")
drive_upload("XAU_USD.csv",path=as_id(drive_get("Data")))
}


#Checking for stock data update
m <- menu(c("Yes", "No"), title="Do you want to check for the updates of stock data?")
if(m==1){
#BIST symbols are loading
bst <- read.csv("bisttum.csv",stringsAsFactors=F)
bst <- bst$Code
bst <- str_c(bst,".IS")
bst <- gsub(" ","",bst)
k <- length(bst)
for(ii in 1:k){
temp_xts <-1
tryCatch({
temp_xts <- readRDS(str_c(bst[ii],".rds"))
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
if(temp_xts==1){
bst <- bst[-ii]
next}
assign(bst[ii],1)
tryCatch({
getSymbols(bst[ii],from=end(temp_xts)+1)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
if(!get(bst[ii])==1){
united <- rbind(temp_xts,get(bst[ii]))
united<- united[ ! duplicated( index(united), fromLast = TRUE ),  ]
assign(bst[ii],united)}

if(get(bst[ii])==1){
assign(bst[ii],temp_xts)
print("The stock data is up to date!!")
break}
}
}




}
XAU_USD <- read.csv("XAU_USD.CSV",stringsAsFactors=F)
XAU_USD <- XAU_USD[,2:4]
XAU_USD <- xts(x = data.frame(Price=as.numeric(XAU_USD$Price),Change=as.numeric(XAU_USD$Change)),order.by=as.Date(XAU_USD$Date))

USD_TRY <- read.csv("USD_TRY.CSV",stringsAsFactors=F)
USD_TRY <- USD_TRY[,2:4]
USD_TRY <- xts(x = data.frame(Price=as.numeric(USD_TRY$Price),Change=as.numeric(USD_TRY$Change)),order.by=as.Date(USD_TRY$Date))

#XU100 DATA HAS BEEN GETTING READY

#The old data
#XU100 <- read.csv("XU100.CSV",stringsAsFactors=F)
XU100 <- readRDS("XU100.rds")
#XU100 <- XU100[,2:8]
#XU100 <- xts(x = data.frame(Price=as.numeric(XU100$Price),Open=as.numeric(XU100$Open),High=as.numeric(XU100$High),Low=as.numeric(XU100$Low),Volume=as.numeric(XU100$Volume),Change=as.numeric(XU100$Change)),order.by=as.Date(XU100$Date))

#The new data
drive_download("BIST 100 Historical Data.csv",overwrite = TRUE)
b<-read.csv("BIST 100 Historical Data.csv",stringsAsFactors=F)
b$Vol. <-as.character(b$Vol.)
b$Change.. <-as.character(b$Change..)
for(i in 1:length(b$Vol.)){
if(grepl("B",b$Vol.[i])){b$Vol.[i]<-as.numeric(gsub("B","",b$Vol.[i]))*1000000000}
if(grepl("M",b$Vol.[i])){b$Vol.[i]<-as.numeric(gsub("M","",b$Vol.[i]))*1000000}
if(grepl("K",b$Vol.[i])){b$Vol.[i]<-as.numeric(gsub("K","",b$Vol.[i]))*1000}
}
c<- xts(x=data.frame(Price=as.numeric(gsub(",","",as.character(b$Price))),
Open=as.numeric(gsub(",","",as.character(b$Open))),
High=as.numeric(gsub(",","",as.character(b$High))),
Low=as.numeric(gsub(",","",as.character(b$Low))),
Volume=as.numeric(b$Vol.),
Change=as.numeric(gsub("%","",b$Change..))),
order.by=mdy(b$ï..Date)
)
#Uniting, removing duplicates and imputing
XU100<-rbind(XU100,c)
XU100<- XU100[ ! duplicated( index(XU100), fromLast = TRUE ),  ]
XU100 <- impute_AR1_Gaussian(XU100)

#Removing,Saving and Uploading XU100.RDS
file.remove("XU100.rds")
saveRDS(XU100,file="XU100.rds")
drive_rm("XU100.rds")
drive_upload("XU100.rds",path=as_id(drive_get("Data")))

XU100_1 <- XU100[index(USD_TRY)]
USD_TRY_1 <- USD_TRY[index(XU100)]
XU100_1$Price <- coredata(XU100_1$Price)/USD_TRY_1$Price
XU100_1$Open <- coredata(XU100_1$Open)/USD_TRY_1$Price
XU100_1$High <- coredata(XU100_1$High)/USD_TRY_1$Price
XU100_1$Low <- coredata(XU100_1$Low)/USD_TRY_1$Price
XU100_1$Volume <- coredata(XU100_1$Volume)/USD_TRY_1$Price 
XU100_1$Change <- coredata(XU100_1$Change) - USD_TRY_1$Change

XU100_USD <- XU100_1

XU100_2 <- XU100_USD[index(XAU_USD)]
XAU_USD_1<- XAU_USD[index(XU100_USD)]
XU100_2$Price <- coredata(XU100_2$Price)/XAU_USD_1$Price
XU100_2$Open <- coredata(XU100_2$Open)/XAU_USD_1$Price
XU100_2$High <- coredata(XU100_2$High)/XAU_USD_1$Price
XU100_2$Low <- coredata(XU100_2$Low)/XAU_USD_1$Price 
XU100_2$Volume <- coredata(XU100_2$Volume)/XAU_USD_1$Price
XU100_2$Change <- coredata(XU100_2$Change) - XAU_USD_1$Change

XU100_XAU <- XU100_2

chartSeries(XU100_XAU$Price, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")


#GETTING STOCK DATA FROM DATA FOLDER
for(i in 1:length(bst)){
if(nrow(get(bst[i]))==1){bst<-bst[-i]}
}
bst_usd <- c()
bst_xau <- c()
bst_try <- c()
for(i in 1:length(bst)){
if(!exists(bst[i])){
next}
a <-1
a <- readRDS(str_c(bst[i],".rds"))
if(a==1){
next}
#ADDING CHANGE TO STOCK DATA
a1 <- as.numeric(coredata(a[,4]))[-1]
a2 <- as.numeric(coredata(a[,4]))[-length(coredata(a[,4]))]
r <- ((a1/a2)-1)*100
r <- c((r[1]+r[2])/2,r)
m <- xts(x = data.frame(Change=r), order.by=index(a))
x <- merge(a,m)
bst_try <- c(bst_try,str_sub(bst[i],start=1,end=str_length(bst[i])-3))
assign(str_c(str_sub(bst[i],start=1,end=str_length(bst[i])-3),"_TRY"),x)

bst_tmp <- x[index(USD_TRY)]
usd_try_tmp<- USD_TRY[index(x)]

bst_tmp[,1]<-coredata(bst_tmp[,1])/usd_try_tmp$Price
bst_tmp[,2]<-coredata(bst_tmp[,2])/usd_try_tmp$Price
bst_tmp[,3]<-coredata(bst_tmp[,3])/usd_try_tmp$Price
bst_tmp[,4]<-coredata(bst_tmp[,4])/usd_try_tmp$Price
bst_tmp[,5]<-coredata(bst_tmp[,5])/usd_try_tmp$Price
bst_tmp[,7]<-coredata(bst_tmp[,7])-usd_try_tmp$Change
bst_usd <- c(bst_usd,str_c(str_sub(bst[i],start=1,end=str_length(bst[i])-3),"_USD")) 
assign(bst_usd[length(bst_usd)],bst_tmp)
bst_tmp <- bst_tmp[index(XAU_USD)]
xau_usd_tmp<- XAU_USD[index(bst_tmp)]

bst_tmp[,1]<-coredata(bst_tmp[,1])/xau_usd_tmp$Price
bst_tmp[,2]<-coredata(bst_tmp[,2])/xau_usd_tmp$Price
bst_tmp[,3]<-coredata(bst_tmp[,3])/xau_usd_tmp$Price
bst_tmp[,4]<-coredata(bst_tmp[,4])/xau_usd_tmp$Price
bst_tmp[,5]<-coredata(bst_tmp[,5])/xau_usd_tmp$Price
bst_tmp[,7]<-coredata(bst_tmp[,7])-xau_usd_tmp$Change
bst_xau <- c(bst_xau,str_c(str_sub(bst[i],start=1,end=str_length(bst[i])-3),"_XAU"))
assign(bst_xau[length(bst_xau)],bst_tmp)

}
