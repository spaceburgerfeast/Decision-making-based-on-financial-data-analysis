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
