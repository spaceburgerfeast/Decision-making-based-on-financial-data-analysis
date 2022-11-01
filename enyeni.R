a <- installed.packages()
a <-a[,1]
necessery_packages <- c("tidyquant","ggpubr","gridExtra","RCurl","googledrive","lubridate","stringr","quantmod","imputeFin")

for(i in necessery_packages)
{
if(!is.element(i,a))
{
install.packages(i)
}
library(i,character.only=T)
}

#Getting USD/TRY FX from OANDA
getFX("USD/TRY")
#Deleting the weekend rows
Days <- weekdays(index(USDTRY))
USDTRY <- USDTRY[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_TRY FX from Drive
drive_download("USD_TRY.rds",overwrite = TRUE)
USD_TRY  <- readRDS("USD_TRY.rds")
if(!sum(as.numeric(is.na(USD_TRY$Price)))==0){USD_TRY$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_TRY$Price)))}
if(!sum(as.numeric(is.na(USD_TRY$Change)))==0){USD_TRY$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_TRY$Change)))}


#Binding USD_TRY and USDTRY
m <- end(USD_TRY)+1
for(i in 1:3){
if(weekdays(m) == "Cumartesi"){m <- m+1}
else if(weekdays(m) == "Pazar"){m <- m+1}
else if(weekdays(m) == "Saturday"){m <- m+1}
else if(weekdays(m) == "Sunday"){m <- m+1}
else{break}
} 
if((nrow(USDTRY[m])==0||weekdays(Sys.Date())=="Cumartesi"||weekdays(Sys.Date())=="Pazar"||weekdays(Sys.Date())=="Saturday"||weekdays(Sys.Date())=="Sunday")){
print("USD_TRY.RDS FILE IS UP TO DATE !!!")}else{
s <- menu(c("Get from Oanda","Get from INVESTING"),title="Do you want to bind USD/TRY data from OANDA OR INVESTING?")
if(s ==1){
USDTRY <-USDTRY[str_c(as.character(m),"/")]
a <- as.numeric(coredata(USD_TRY[end(USD_TRY)]$Price))
b <- as.numeric(coredata(USDTRY[str_c(as.character(m),"/")]))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDTRY_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDTRY))
USD_TRY <- rbind(USD_TRY,USDTRY_NEW)}
if(s == 2){
z <- read.csv("USD_TRY Historical Data.csv",stringsAsFactors=F)
USDTRY_2 <- xts(x =data.frame(Price=as.numeric(z$Price)),order.by=mdy(z$ï..Date))
USDTRY_2 <-USDTRY_2[str_c(as.character(m),"/")]
a <- as.numeric(coredata(USD_TRY[end(USD_TRY)]$Price))
b <- as.numeric(coredata(USDTRY_2[str_c(as.character(m),"/")]))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDTRY_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDTRY_2))
USD_TRY <- rbind(USD_TRY,USDTRY_NEW)
} 

#Saving and Uploading to Drive
file.remove("USD_TRY.rds")
saveRDS(USD_TRY,file="USD_TRY.rds")
drive_rm("USD_TRY.rds")
drive_upload("USD_TRY.rds",path=as_id(drive_get("Data")))}

#------------------------------------------------------------------------------

#Getting USD/XAU FX from OANDA
getFX("USD/XAU")
#Deleting the weekend rows
Days <- weekdays(index(USDXAU))
USDXAU <- USDXAU[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_XAU FX from Drive
drive_download("USD_XAU.rds",overwrite = TRUE)
USD_XAU <- readRDS("USD_XAU.rds")
if(!sum(as.numeric(is.na(USD_XAU$Price)))==0){USD_XAU$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_XAU$Price)))}
if(!sum(as.numeric(is.na(USD_XAU$Change)))==0){USD_XAU$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_XAU$Change)))}


#Binding USD_XAU and USDXAU
m <- end(USD_XAU)+1
for(i in 1:3){
if(weekdays(m) == "Cumartesi"){m <- m+1}
else if(weekdays(m) == "Pazar"){m <- m+1}
else if(weekdays(m) == "Saturday"){m <- m+1}
else if(weekdays(m) == "Sunday"){m <- m+1}
else{break}
} 
if((nrow(USDXAU[m])==0||weekdays(Sys.Date())=="Cumartesi"||weekdays(Sys.Date())=="Pazar"||weekdays(Sys.Date())=="Saturday"||weekdays(Sys.Date())=="Sunday")){
print("USD_XAU.RDS FILE IS UP TO DATE !!!")
}else{
if((as.numeric(coredata(USDXAU[end(USDXAU)]))<10)){
q <- menu(c("Continue(Downloaded and Moved)", "No"), title="USD_XAU data from OANDA is not tidy. Please download and move the data from investing manually")
if(q==1){
z <- read.csv("XAU_USD Historical Data.csv",stringsAsFactors=F)
USDXAU_2 <- xts(x =data.frame(Price=as.numeric(gsub(",","",z$Price))),order.by=mdy(z$ï..Date))
USDXAU_2 <-USDXAU_2[str_c(as.character(m),"/")]
a <- as.numeric(coredata(USD_XAU[end(USD_XAU)]$Price))
b <- as.numeric(coredata(USDXAU_2[str_c(as.character(m),"/")]))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDXAU_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDXAU_2))
USD_XAU <- rbind(USD_XAU,USDXAU_NEW) 
}
}else{
USDXAU <-USDXAU[str_c(as.character(m),"/")]
a <- as.numeric(coredata(USD_XAU[end(USD_XAU)]$Price))
b <- as.numeric(coredata(USDXAU[str_c(as.character(end(USD_XAU)+1),"/")]))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDXAU_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDXAU))
USD_XAU <- rbind(USD_XAU,USDXAU_NEW) }

#Saving and Uploading to Drive
file.remove("USD_XAU.rds")
saveRDS(USD_XAU,file="USD_XAU.rds")
drive_rm("USD_XAU.rds")
drive_upload("USD_XAU.rds",path=as_id(drive_get("Data")))}


#-----------------------------------------------------------------------------

#BIST symbols are loading
drive_download("bisttum.csv",overwrite = TRUE)
bst <- read.csv("bisttum.csv",stringsAsFactors=F)
bst <- bst$Code
bst <- str_c(bst,".IS")
tickers <- gsub(" ","",bst)
tickers_2 <- gsub(" ","",bst)

#BIST symbols are downloading
tickers <- getSymbols(tickers,verbose = TRUE,from="1987-01-01")
k <-c()

for(i in 1:length(tickers)){
#Eliminating erroneous components
if(!exists(tickers[i])||nrow(get(tickers[i]))<5 || is.na(as.numeric(coredata(get(tickers[i])[,4][end(get(tickers[i]))-1])))){
k <- c(k,-i)
}
if("ASELS.IS"== tickers[i]){
ASELS.IS[,4][index(ASELS.IS)<as.Date("2020-07-23")]<-ASELS.IS[,4][index(ASELS.IS)<as.Date("2020-07-23")]/2 }
if("DZGYO.IS"== tickers[i]){
DZGYO.IS[,4][index(DZGYO.IS)<as.Date("2020-08-07")]<-DZGYO.IS[,4][index(DZGYO.IS)<as.Date("2020-08-07")]/2 }
if("ALGYO.IS"== tickers[i]){
ALGYO.IS[,4][index(ALGYO.IS)<as.Date("2012-06-18")]<-ALGYO.IS[,4][index(ALGYO.IS)<as.Date("2012-06-18")]/6 }
if("ALKIM.IS"== tickers[i]){
ALKIM.IS[,4][index(ALKIM.IS)<as.Date("2020-04-24")]<-ALKIM.IS[,4][index(ALKIM.IS)<as.Date("2020-04-24")]/5 }
if("AVHOL.IS"== tickers[i]){
AVHOL.IS[,4][index(AVHOL.IS)<as.Date("2018-06-28")]<-AVHOL.IS[,4][index(AVHOL.IS)<as.Date("2018-06-28")]/2.5 }
if("BAKAB.IS"== tickers[i]){
BAKAB.IS[,4][index(BAKAB.IS)<as.Date("2002-12-19")]<-BAKAB.IS[,4][index(BAKAB.IS)<as.Date("2002-12-19")]/3 }
if("CEOEM.IS"== tickers[i]){
CEOEM.IS[,4][index(CEOEM.IS)<as.Date("2020-04-15")]<-CEOEM.IS[,4][index(CEOEM.IS)<as.Date("2020-04-15")]/3 }
if("IDEAS.IS"== tickers[i]){
IDEAS.IS[,4][index(IDEAS.IS)<as.Date("2019-11-04")]<-IDEAS.IS[,4][index(IDEAS.IS)<as.Date("2019-11-04")]/1.5 }

}
tickers <- tickers_2[k]




#Gold, usd and try based variables are getting ready
bst_try<-c()
for(i in 1:length(tickers)){
bst_try[i] <- str_c(str_sub(tickers[i],start=1,end=str_length(tickers[i])-3),"_TRY")}

for(i in 1:length(bst_try)){
a <- as.numeric(coredata(get(tickers[i])[,4]))
for(ii in 2:length(a))
{
w <- FALSE
m <- as.numeric(coredata(a[ii]))
if(is.na(m)){next}
n <- as.numeric(coredata(a[ii-1]))
if(is.na(n)){next}
k<- abs(((m-n)/n)*100)
w <- (k > 180)
if(w)
{
a[ii] <- as.numeric(a[ii-1])

}
}
b <- a[-1]
r <- ((a[-1]/a[-length(a)])-1)*100
j<-xts(x=data.frame(Price=b,Change=r),order.by=index(get(tickers[i]))[-1])
if(!sum(as.numeric(is.na(j$Price)))==0){j$Price <- impute_AR1_Gaussian(as.numeric(coredata(j$Price)))}
if(!sum(as.numeric(is.na(j$Change)))==0){j$Change<- impute_AR1_Gaussian(as.numeric(coredata(j$Change)))}

assign(bst_try[i],j)
}



bst_usd<-c()
for(i in 1:length(tickers)){
bst_usd[i] <- str_c(str_sub(tickers[i],start=1,end=str_length(tickers[i])-3),"_USD")}

for(i in 1:length(bst_usd)){
bst_tmp<-get(bst_try[i])[index(USD_TRY)]
usd_tmp<-USD_TRY[index(get(bst_try[i]))]
bst_tmp$Price <- coredata(bst_tmp$Price)/usd_tmp$Price 
bst_tmp$Change <- (coredata(bst_tmp$Change)- usd_tmp$Change)
if(!sum(as.numeric(is.na(bst_tmp$Price)))==0){bst_tmp$Price <- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Price)))}
if(!sum(as.numeric(is.na(bst_tmp$Change)))==0){bst_tmp$Change<- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Change)))}
assign(bst_usd[i],bst_tmp)
}

bst_xau<-c()
for(i in 1:length(tickers)){
bst_xau[i] <- str_c(str_sub(tickers[i],start=1,end=str_length(tickers[i])-3),"_XAU")}


for(i in 1:length(bst_xau)){
bst_tmp<-get(bst_usd[i])[index(USD_XAU)]
xau_tmp<-USD_XAU[index(get(bst_usd[i]))]
bst_tmp$Price <- coredata(bst_tmp$Price)/xau_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)-xau_tmp$Change
if(!sum(as.numeric(is.na(bst_tmp$Price)))==0){bst_tmp$Price <- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Price)))}
if(!sum(as.numeric(is.na(bst_tmp$Change)))==0){bst_tmp$Change<- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Change)))}
assign(bst_xau[i],bst_tmp)
}


#--------------------------------------------------------------------

#The old data
#XU100 <- read.csv("XU100.CSV",stringsAsFactors=F)
drive_download("XU100.rds",overwrite = TRUE)
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

XU100_2 <- XU100_USD[index(USD_XAU)]
XAU_USD_1<- USD_XAU[index(XU100_USD)]
XU100_2$Price <- coredata(XU100_2$Price)/XAU_USD_1$Price
XU100_2$Open <- coredata(XU100_2$Open)/XAU_USD_1$Price
XU100_2$High <- coredata(XU100_2$High)/XAU_USD_1$Price
XU100_2$Low <- coredata(XU100_2$Low)/XAU_USD_1$Price 
XU100_2$Volume <- coredata(XU100_2$Volume)/XAU_USD_1$Price
XU100_2$Change <- coredata(XU100_2$Change) - XAU_USD_1$Change

XU100_XAU <- XU100_2

# Live data
y <- getQuote(paste0("USD", "TRY", "=X"))
realt <- xts(x = data.frame(Price=y[1,2],Change=y[1,4]),order.by=as.Date(str_sub(as.character(y[1,1])),start=1,end=10))
if(end(USD_TRY)==as.Date(str_sub(as.character(y[1,1])),start=1,end=10)){
assign("USD_TRY",USD_TRY[-nrow(USD_TRY)])
assign("USD_TRY",rbind(USD_TRY,realt))
}else{assign("USD_TRY",rbind(USD_TRY,realt))}


a <- getQuote(tickers)

for(i in 1:length(bst_try)){

if(any(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3)) == str_sub(bst_try[i],start=1,end=(str_length(bst_try[i])-4)))){
k <- which(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3))==str_sub(bst_try[i],start=1,end=(str_length(bst_try[i])-4)))
realt <- xts(x = data.frame(Price=a[k,2],Change=a[k,4]),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))

if(end(get(bst_try[i]))==as.Date(str_sub(as.character(a[k,1])),start=1,end=10)){
assign(bst_try[i],get(bst_try[i])[-nrow(get(bst_try[i]))])
assign(bst_try[i],rbind(get(bst_try[i]),realt))
}else{assign(bst_try[i],rbind(get(bst_try[i]),realt))}

}}


chartSeries(XU100_XAU$Price, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")



