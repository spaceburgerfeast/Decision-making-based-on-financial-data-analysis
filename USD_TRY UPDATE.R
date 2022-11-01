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
}




#Binding USD_TRY and USDTRY
m <- end(USD_TRY)+1
for(i in 1:3){
if(weekdays(m) == "Cumartesi"){m <- m+1}
else if(weekdays(m) == "Pazar"){m <- m+1}
else if(weekdays(m) == "Saturday"){m <- m+1}
else if(weekdays(m) == "Sunday"){m <- m+1}
else{break}
} 
#||weekdays(Sys.Date())=="Cumartesi"  ||weekdays(Sys.Date())=="Cumartesi"
if((nrow(USDTRY[m])==0||weekdays(Sys.Date())=="Pazar"||weekdays(Sys.Date())=="Saturday"||weekdays(Sys.Date())=="Sunday")){
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


#Binding USD_XAU and USDXAU
m <- end(USD_XAU)+1
for(i in 1:3){
if(weekdays(m) == "Cumartesi"){m <- m+1}
else if(weekdays(m) == "Pazar"){m <- m+1}
else if(weekdays(m) == "Saturday"){m <- m+1}
else if(weekdays(m) == "Sunday"){m <- m+1}
else{break}
} 
if((nrow(USDXAU[m])==0||weekdays(Sys.Date())=="Pazar"||weekdays(Sys.Date())=="Saturday"||weekdays(Sys.Date())=="Sunday")){
print("USD_XAU.RDS FILE IS UP TO DATE !!!")
}else{
if((as.numeric(coredata(USDXAU[end(USDXAU)]))<10)){
q <- menu(c("Continue(Downloaded and Moved)", "No"), title="USD_XAU data from OANDA is not tidy. Please download and move the data from investing manually")
if(q==1){
z <- read.csv("XAU_USD Historical Data.csv",stringsAsFactors=F)
USDXAU_2 <- xts(x =data.frame(Price=as.numeric(gsub(",","",z$Price))),order.by=mdy(z$ï..Date))
USDXAU_2 <-USDXAU_2[!(index(USDXAU_2)%in%index(USDXAU))]
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

