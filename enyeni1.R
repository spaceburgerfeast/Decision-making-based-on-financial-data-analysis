

#USER  DEFINED FUNCTIONS

#Simple moving average function
mysma <-function(j,dt,n){
p <- j$Price[dt]
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- log(as.numeric(coredata(b)))
f <- c()
d <- c()
for(i in 0:(length(b)-n)){
f <- c(f,mean(b[c((length(b)-n+1-i):(length(b)-i))]))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
autoplot(x)
}

#PRIME RETURN SERIES FUNCTION
RG <-function(M,ts_data,dfg,file_name){

u <- list()
primes <- c(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241)
n <- primes[seq(from=length(primes),to=1)]
p <- ts_data$Price[]
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- (as.numeric(coredata(b))-1)*100
n <- n[n<length(b)]
for(ii in 1:length(n)){
f <- c()
d <- c()
ss <- n[ii]
for(i in 0:(length(b)-ss)){
tp <- b[c((length(b)-ss+1-i):(length(b)-i))]
vv <- 1
lh <- length(tp)
for(cc in 1:length(tp)){vv <- vv*(1+(tp[cc]/100))}
f <- c(f,((vv^(1/lh))-1))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
if(ss==1){x <- x["2019/"]}
if(ss==2){x <- x["2019/"]}
if(ss==3){x <- x["2018/"]}
if(ss==5){x <- x["2018/"]}
if(ss==7){x <- x["2017/"]}
if(ss==11){x <- x["2016/"]}
if(ss==13){x <- x["2016/"]}
if(ss==17){x <- x["2015/"]}
if(ss==19){x <- x["2014/"]}
if(ss==23){x <- x["2013/"]}
if(ss==29){x <- x["2012/"]}
if(ss==31){x <- x["2011/"]}
df <- data.frame(Date=index(x),Price=as.numeric(coredata(x)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(str_c(M,as.character(ss)))+
geom_ma(n=dfg,show.legend=TRUE,color="green",linetype="solid")+
theme_dark(base_size =7)
assign(str_c("g",as.character(n[ii])),g)
u[[ii]] <- get(str_c("g",as.character(ss)))

}
res <- marrangeGrob(u, nrow = 1, ncol = 1)
# Export to a pdf file
file.remove(str_c(file_name,".pdf"))
ggexport(res, filename = str_c(file_name,".pdf"))
shell(str_c("C:/Users/vakka/Documents/",str_c(file_name,".pdf")))
}


#------------------------------------------------------------------------

#PRIME RETURN SERIES FUNCTION
R <-function(M,ts_data,W){

u <- list()
primes <- c(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,360,480,600,720,960,1200,1440,1680,1920,2160,2400)
n <- primes[seq(from=length(primes),to=1)]
p <- ts_data$Price
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- log(as.numeric(coredata(b)))
n <- n[n<length(b)]
for(ii in 1:length(n)){
f <- c()
d <- c()
ss <- n[ii]
for(i in 0:(length(b)-ss)){
f <- c(f,mean(b[c((length(b)-ss+1-i):(length(b)-i))]))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
if(ss==1){x <- x["2019/"]}
if(ss==2){x <- x["2017/"]}
if(ss==3){x <- x["2017/"]}
if(ss==4){x <- x["2017/"]}
if(ss==5){x <- x["2016/"]}
if(ss==6){x <- x["2016/"]}
if(ss==7){x <- x["2016/"]}
if(ss==8){x <- x["2016/"]}
if(ss==9){x <- x["2016/"]}
if(ss==10){x <- x["2015/"]}
if(ss==11){x <- x["2015/"]}
if(ss==12){x <- x["2015/"]}
if(ss==13){x <- x["2015/"]}
if(ss==14){x <- x["2014/"]}
if(ss==15){x <- x["2014/"]}
if(ss==16){x <- x["2014/"]}
if(ss==17){x <- x["2014/"]}
if(ss==18){x <- x["2014/"]}
if(ss==19){x <- x["2014/"]}
if(ss==20){x <- x["2013/"]}
if(ss==21){x <- x["2013/"]}
if(ss==22){x <- x["2013/"]}
if(ss==23){x <- x["2013/"]}
if(ss==24){x <- x["2013/"]}
if(ss==25){x <- x["2012/"]}
if(ss==26){x <- x["2012/"]}
if(ss==27){x <- x["2012/"]}
if(ss==28){x <- x["2012/"]}
if(ss==29){x <- x["2012/"]}
if(ss==30){x <- x["2012/"]}
if(ss==31){x <- x["2011/"]}
if(ss==32){x <- x["2011/"]}
if(ss==33){x <- x["2010/"]}
if(ss==34){x <- x["2010/"]}
if(ss==35){x <- x["2009/"]}
if(ss==36){x <- x["2009/"]}
if(ss==37){x <- x["2009/"]}
if(ss==38){x <- x["2008/"]}
if(ss==39){x <- x["2008/"]}
if(ss==40){x <- x["2007/"]}
if(ss==41){x <- x["2007/"]}
if(ss==42){x <- x["2007/"]}
if(ss==43){x <- x["2007/"]}
if(ss==44){x <- x["2006/"]}
if(ss==45){x <- x["2006/"]}
if(ss==46){x <- x["2006/"]}
if(ss==47){x <- x["2006/"]}
if(ss==48){x <- x["2006/"]}
if(ss==49){x <- x["2005/"]}
if(ss==49){x <- x["2005/"]}
if(ss==50){x <- x["2005/"]}
if(ss==51){x <- x["2005/"]}
if(ss==52){x <- x["2005/"]}
if(ss==53){x <- x["2005/"]}
if(ss==54){x <- x["2005/"]}
if(ss==55){x <- x["2005/"]}
if(ss==56){x <- x["2004/"]}
if(ss==57){x <- x["2004/"]}
if(ss==58){x <- x["2004/"]}
if(ss==59){x <- x["2004/"]}
df <- data.frame(Date=index(x),Price=as.numeric(coredata(x)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(str_c(M,as.character(ss)))+
geom_ma(n=W,show.legend=TRUE,color="green",linetype="solid")+
theme_dark(base_size =7)
assign(str_c("g",as.character(n[ii])),g)
u[[ii]] <- get(str_c("g",as.character(ss)))

}
res <- marrangeGrob(u, nrow = 1, ncol = 1)
# Export to a pdf file
file.remove("RETURN.pdf")
ggexport(res, filename = "RETURN.pdf")
#shell(str_c("C:/Users/vakka/Documents/",str_c(file_name,".pdf")))

}

#------------------------------------------------------------------------------
a <- installed.packages()
a <-a[,1]
necessery_packages <- c("tidyquant","rvest","ggpubr","gridExtra","RCurl","googledrive","lubridate","stringr","quantmod","imputeFin")

for(i in necessery_packages)
{
if(!is.element(i,a))
{
install.packages(i)
}
library(i,character.only=T)
}

#Getting USD/AUD FX from OANDA
#source("usd_aud.r",echo=T)


#Getting USD/EUR FX from OANDA
#source("usd_eur.r",echo=T)


#Getting USD/GBP FX from OANDA
#source("usd_gbp.r",echo=T)


#Getting USD/JPY FX from OANDA
#source("usd_jpy.r",echo=T)


#Getting USD/CHF FX from OANDA
#source("usd_chf.r",echo=T)


#Getting USD/TRY FX from OANDA
getFX("USD/TRY")
#Deleting the weekend rows
Days <- weekdays(index(USDTRY))
USDTRY <- USDTRY[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_TRY FX from Drive
drive_download("USD_TRY.rds",overwrite = TRUE)
USD_TRY  <- readRDS("USD_TRY.rds")
#if(!sum(as.numeric(is.na(USD_TRY$Price)))==0){USD_TRY$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_TRY$Price)))}
#if(!sum(as.numeric(is.na(USD_TRY$Change)))==0){USD_TRY$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_TRY$Change)))}

if(end(USD_TRY)==Sys.Date()){
print("USD_TRY.RDS FILE IS UP TO DATE !!!")
}else{
USDTRY_2 <-USDTRY[!(index(USDTRY)%in%index(USD_TRY))]
a <- as.numeric(coredata(USD_TRY[end(USD_TRY)]$Price))
b <- as.numeric(coredata(USDTRY_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDTRY_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDTRY_2))
USD_TRY <- rbind(USD_TRY,USDTRY_NEW)


#Saving and Uploading to Drive
#file.remove("USD_TRY.rds")
saveRDS(USD_TRY,file="USD_TRY.rds")
drive_rm("USD_TRY.rds")
drive_upload("USD_TRY.rds",path=as_id(drive_get("Data")))}

#------------------------------------------------------------------------------

#Getting USD/XAU FX from OANDA
#getFX("USD/XAU")
#Deleting the weekend rows
#Days <- weekdays(index(USDXAU))
#USDXAU <- USDXAU[!Days=="Cumartesi" & !Days=="Pazar" & !Days=="Saturday" & !Days=="Sunday"]

#Getting USD_XAU FX from Drive
drive_download("USD_XAU.rds",overwrite = TRUE)
USD_XAU <- readRDS("USD_XAU.rds")
if(!sum(as.numeric(is.na(USD_XAU$Price)))==0){USD_XAU$Price <- impute_AR1_Gaussian(as.numeric(coredata(USD_XAU$Price)))}
if(!sum(as.numeric(is.na(USD_XAU$Change)))==0){USD_XAU$Change<- impute_AR1_Gaussian(as.numeric(coredata(USD_XAU$Change)))}

if(end(USD_XAU)==Sys.Date()){
print("USD_XAU.RDS FILE IS UP TO DATE !!!")
}else{
s <- menu(c("YES","NO"),title="\nIf you downloaded and inserted USD_XAU data from INVESTING press YES\n ")
if(s == 2){
browseURL("https://www.investing.com/currencies/xau-usd-historical-data")
J <- menu(c("YES","NO"),title="\nDid you insert USD_XAU data from INVESTING?\n ")
}
if(s ==1||J==1){
c <- read.csv("XAU_USD Historical Data.csv",stringsAsFactors=F)
USDXAU_2 <- xts(x =data.frame(Price=as.numeric(gsub(",","",c$Price))),order.by=mdy(c$ï..Date))
USDXAU_2 <-USDXAU_2[!(index(USDXAU_2)%in%index(USD_XAU))]
a <- as.numeric(coredata(USD_XAU[end(USD_XAU)]$Price))
b <- as.numeric(coredata(USDXAU_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
USDXAU_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(USDXAU_2))
USD_XAU <- rbind(USD_XAU,USDXAU_NEW)

#Saving and Uploading to Drive
file.remove("USD_XAU.rds")
saveRDS(USD_XAU,file="USD_XAU.rds")
drive_rm("USD_XAU.rds")
drive_upload("USD_XAU.rds",path=as_id(drive_get("Data")))}}

source("XAG_USD_CODE.r",echo=T)
#-------------------------------------------------------------------------------

#Getting DXY from Drive


#-----------------------------------------------------------------------------
browseURL("https://www.investing.com/indices/ise-100-historical-data")
#BIST symbols are loading
drive_download("bisttum.csv",overwrite = TRUE)
bst <- read.csv("bisttum.csv",stringsAsFactors=F)
bst <- bst$Code
bst <- str_c(bst,".IS")
#bst <- bst[2]
tickers <- gsub(" ","",bst)
tickers_2 <- gsub(" ","",bst)
tickers <- tickers[-which(tickers=="SAFKR.IS")]
#BIST symbols are downloading
for(i in 1:length(tickers)){
getSymbols(tickers[i],from="2000-01-01")}

#Gold, usd and try based variables are getting ready
bst_try<-c()
for(j in 1:length(tickers)){
bst_try[j] <- str_c(str_sub(tickers[j],start=1,end=str_length(tickers[j])-3),"_TRY")}

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

#-------------------------------------------------------------------------------
# REAL TIME DATA
bst_try <- c("XU100_TRY",bst_try)
bst_xau <- c("XU100_XAU",bst_xau)
bst_usd <- c("XU100_USD",bst_usd)
tickers <- c("XU100.IS",tickers)
XU100_USD <- XU100_USD[,c(1,6)]
XU100_TRY <- XU100[,c(1,6)]
XU100_XAU <- XU100_XAU[,c(1,6)]
library(xml2)

a <- getQuote(tickers)

for(i in 1:length(bst_try)){

if(any(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3)) == str_sub(bst_try[i],start=1,end=(str_length(bst_try[i])-4)))){
k <- which(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3))==str_sub(bst_try[i],start=1,end=(str_length(bst_try[i])-4)))

if(end(get(bst_try[i]))==as.Date(str_sub(as.character(a[k,1])),start=1,end=10)){
assign(bst_try[i],get(bst_try[i])[-nrow(get(bst_try[i]))])
C <- ((a[k,2]/as.numeric(coredata(get(bst_try[i])$Price[end(get(bst_try[i]))])))-1)*100
realt <- xts(x = data.frame(Price=a[k,2],Change=C),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
assign(bst_try[i],rbind(get(bst_try[i]),realt))
}else{
C <- ((a[k,2]/as.numeric(coredata(get(bst_try[i])$Price[end(get(bst_try[i]))])))-1)*100
realt <- xts(x = data.frame(Price=a[k,2],Change=C),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
assign(bst_try[i],rbind(get(bst_try[i]),realt))}

}}


# Live data
y <- getQuote(paste0("USD", "TRY", "=X"))

if(end(USD_TRY)==as.Date(str_sub(as.character(y[1,1])),start=1,end=10)){
assign("USD_TRY",USD_TRY[-nrow(USD_TRY)])
M <- ((y[1,2]/as.numeric(coredata(USD_TRY$Price[end(USD_TRY)])))-1)*100
realt <- xts(x = data.frame(Price=y[1,2],Change=M),order.by=as.Date(str_sub(as.character(y[1,1])),start=1,end=10))
assign("USD_TRY",rbind(USD_TRY,realt))
}else{
M <- ((y[1,2]/as.numeric(coredata(USD_TRY$Price[end(USD_TRY)])))-1)*100
realt <- xts(x = data.frame(Price=y[1,2],Change=M),order.by=as.Date(str_sub(as.character(y[1,1])),start=1,end=10))
assign("USD_TRY",rbind(USD_TRY,realt))}


z <- read_html("https://www.investing.com/currencies/xau-usd-historical-data") %>% html_nodes("#last_last")%>%xml_contents()%>%html_text(trim=TRUE)%>%{gsub(",","",.)}%>%{as.numeric(.)}
if(Sys.Date()==end(USD_XAU)){
assign("USD_XAU",USD_XAU[-nrow(USD_XAU)])
M <- ((z/as.numeric(coredata(USD_XAU$Price[end(USD_XAU)])))-1)*100
realt <- xts(x = data.frame(Price=z,Change=M),order.by=Sys.Date())
assign("USD_XAU",rbind(USD_XAU,realt))
}else{
M <- ((z/as.numeric(coredata(USD_XAU$Price[end(USD_XAU)])))-1)*100
realt <- xts(x = data.frame(Price=z,Change=M),order.by=Sys.Date())
assign("USD_XAU",rbind(USD_XAU,realt))
}





for(i in 1:length(bst_usd)){

if(any(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3)) ==  str_sub(bst_usd[i],start=1,end=(str_length(bst_usd[i])-4)))){

k <- which(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3))==str_sub(bst_usd[i],start=1,end=(str_length(bst_usd[i])-4)))

if(end(get(bst_usd[i]))==as.Date(str_sub(as.character(a[k,1])),start=1,end=10)){
assign(bst_usd[i],get(bst_usd[i])[-nrow(get(bst_usd[i]))])
C <- (((a[k,2]/y[1,2])/as.numeric(coredata(get(bst_usd[i])$Price[end(get(bst_usd[i]))])))-1)*100
realt <- xts(x = data.frame(Price=(a[k,2]/y[1,2]),Change=C),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
assign(bst_usd[i],rbind(get(bst_usd[i]),realt))
}else{
C <- (((a[k,2]/y[1,2])/as.numeric(coredata(get(bst_usd[i])$Price[end(get(bst_usd[i]))])))-1)*100
realt <- xts(x = data.frame(Price=(a[k,2]/y[1,2]),Change=C),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
assign(bst_usd[i],rbind(get(bst_usd[i]),realt))}

}}





for(i in 1:length(bst_xau)){

if(any(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3)) ==  str_sub(bst_xau[i],start=1,end=(str_length(bst_xau[i])-4)))){

k <- which(str_sub(rownames(a),start=1,end=(str_length(rownames(a))-3))==str_sub(bst_xau[i],start=1,end=(str_length(bst_xau[i])-4)))

if(end(get(bst_xau[i]))==as.Date(str_sub(as.character(a[k,1])),start=1,end=10)){
assign(bst_xau[i],get(bst_xau[i])[-nrow(get(bst_xau[i]))])
C <- ((((a[k,2]/y[1,2])/z)/as.numeric(coredata(get(bst_xau[i])$Price[end(get(bst_xau[i]))])))-1)*100
realt <- xts(x = data.frame(Price=((a[k,2]/y[1,2])/z),Change=C),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
assign(bst_xau[i],rbind(get(bst_xau[i]),realt))
}else{
C <- ((((a[k,2]/y[1,2])/z)/as.numeric(coredata(get(bst_xau[i])$Price[end(get(bst_xau[i]))])))-1)*100
realt <- xts(x = data.frame(Price=((a[k,2]/y[1,2])/z),Change=C),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
assign(bst_xau[i],rbind(get(bst_xau[i]),realt))}

}}



chartSeries(XU100_XAU$Price, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")

#-------------------------------------------------------
#XAG BAZINDA

bst_xag<-c()
for(i in 1:length(tickers)){
bst_xag[i] <- str_c(str_sub(tickers[i],start=1,end=str_length(tickers[i])-3),"_XAG")}


for(i in 1:length(bst_xag)){
bst_tmp<-get(bst_usd[i])[index(XAG_USD)]
xag_tmp<-XAG_USD[index(get(bst_usd[i]))]
bst_tmp$Price <- coredata(bst_tmp$Price)/xag_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)-xag_tmp$Change
if(!sum(as.numeric(is.na(bst_tmp$Price)))==0){bst_tmp$Price <- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Price)))}
if(!sum(as.numeric(is.na(bst_tmp$Change)))==0){bst_tmp$Change<- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Change)))}
assign(bst_xag[i],bst_tmp)
}
#---------------------------------------------------------------
#XAU/XAG oraný

XAU_XAG<-c()
bst_tmp<-USD_XAU[index(XAG_USD)]
xag_tmp<-XAG_USD[index(USD_XAU)]
bst_tmp$Price <- coredata(bst_tmp$Price)/xag_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)-xag_tmp$Change
XAU_XAG<-bst_tmp


#---------------------------------------------------------------
#TRY_XAU

TRY_XAU<-c()
bst_tmp<-USD_XAU[index(USD_TRY)]
xag_tmp<-USD_TRY[index(USD_XAU)]
bst_tmp$Price <- coredata(bst_tmp$Price)*xag_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)+xag_tmp$Change
TRY_XAU<-bst_tmp

#---------------------------------------------------------------
#TRY_XAG


TRY_XAG<-c()
bst_tmp<-XAG_USD[index(USD_TRY)]
xag_tmp<-USD_TRY[index(XAG_USD)]
bst_tmp$Price <- coredata(bst_tmp$Price)*xag_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)+xag_tmp$Change
TRY_XAG<-bst_tmp


XU100_1 <- XU100[index(USD_TRY)]
USD_TRY_1 <- USD_TRY[index(XU100)]
XU100_1$Price <- coredata(XU100_1$Price)/USD_TRY_1$Price
XU100_1$Open <- coredata(XU100_1$Open)/USD_TRY_1$Price
XU100_1$High <- coredata(XU100_1$High)/USD_TRY_1$Price
XU100_1$Low <- coredata(XU100_1$Low)/USD_TRY_1$Price
XU100_1$Volume <- coredata(XU100_1$Volume)/USD_TRY_1$Price 
XU100_1$Change <- coredata(XU100_1$Change) - USD_TRY_1$Change

XU100_USD <- XU100_1[,c(1,6)]

XU100_2 <- XU100_USD[index(USD_XAU)]
XAU_USD_1<- USD_XAU[index(XU100_USD)]
XU100_2$Price <- coredata(XU100_2$Price)/XAU_USD_1$Price
XU100_2$Open <- coredata(XU100_2$Open)/XAU_USD_1$Price
XU100_2$High <- coredata(XU100_2$High)/XAU_USD_1$Price
XU100_2$Low <- coredata(XU100_2$Low)/XAU_USD_1$Price 
XU100_2$Volume <- coredata(XU100_2$Volume)/XAU_USD_1$Price
XU100_2$Change <- coredata(XU100_2$Change) - XAU_USD_1$Change

XU100_XAU <- XU100_2[,c(1,6)]
