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
#------------------------------------------------------------------------------

qqq <- read_html("https://www.investing.com/currencies/xag-usd-historical-data") %>% html_nodes("#last_last")%>%xml_contents()%>%html_text(trim=TRUE)%>%{gsub(",","",.)}%>%{as.numeric(.)}
if(Sys.Date()==end(XAG_USD)){
assign("XAG_USD",XAG_USD[-nrow(XAG_USD)])
M <- ((qqq/as.numeric(coredata(XAG_USD$Price[end(XAG_USD)])))-1)*100
realt <- xts(x = data.frame(Price=qqq,Change=M),order.by=Sys.Date())
assign("XAG_USD",rbind(XAG_USD,realt))
}else{
M <- ((qqq/as.numeric(coredata(XAG_USD$Price[end(XAG_USD)])))-1)*100
realt <- xts(x = data.frame(Price=qqq,Change=M),order.by=Sys.Date())
assign("XAG_USD",rbind(XAG_USD,realt))
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


#__________________________________________________________----
library(gridExtra)
library(ggpubr)

p <- read.csv("Pozisyonlarim.csv",stringsAsFactors=F)
p$Birim.Fiyat <- gsub(",",".",p$Birim.Fiyat)
p$Birim.Fiyat <- as.numeric(p$Birim.Fiyat)
p$Alis.Maliyeti <- gsub(",",".",p$Alis.Maliyeti)
p$Alis.Maliyeti <- as.numeric(p$Alis.Maliyeti)
p$Change <- ((p$Birim.Fiyat-p$Alis.Maliyeti)/p$Alis.Maliyeti)*100
ind <- order(p$Change,decreasing =T)
p$Sembol <- p$Sembol[ind]
p$Birim.Fiyat <- p$Birim.Fiyat[ind]
p$Alis.Maliyeti <- p$Alis.Maliyeti[ind]
p$Change <- p$Change[ind]

hisselerim_xau<-str_c(p$Sembol,"_XAU")
hisselerim_usd<-str_c(p$Sembol,"_USD")
hisselerim_try<-p$Sembol
names(p)[names(p) == "Degisim"]<- "Degisim (%)"
p


a <- p$Sembol
a<-a[-as.numeric(which(a =="ALMAD"))]
a<-a[-as.numeric(which(a =="TEKTU"))]
a<-a[-as.numeric(which(a =="VAKFNR"))]
a<-a[-as.numeric(which(a =="ISFIN"))]
a<-a[-as.numeric(which(a =="DJIST"))]
a<-a[-as.numeric(which(a =="AGESA"))]
a<-a[-as.numeric(which(a =="ATATP"))]
#a<-a[-as.numeric(which(a =="ZPLIB"))]
a<-a[-as.numeric(which(a =="USDTR"))]
a<-a[-as.numeric(which(a =="GLDTR"))]
low <- str_c(a,"_USD")

x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){


j <- get(low[i])$Change

#r <- coredata(j[-1])/j[-nrow(j)]
#r$Price <- log(as.numeric(coredata(r)))

df <- data.frame(Date=index(j["2020/"]),Change=as.numeric(coredata(j$Change["2020/"])))
g<-ggplot(df,aes(x=Date,y=Change))+
geom_line(linetype="dotted")+
ggtitle(low[i])+
theme_dark(base_size = 7)+
geom_ma(n=2,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=5,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=20,show.legend=TRUE,color="yellow",linetype="solid")+
geom_ma(n=60,show.legend=TRUE,color="black",linetype="solid")

assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd2.pdf")
#shell("C:/Users/vakka/Documents/sd2.pdf")

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


