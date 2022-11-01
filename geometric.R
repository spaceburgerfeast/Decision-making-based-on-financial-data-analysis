source("live.r",echo=T)
df <- data.frame(Date=index(XU100_USD$Price["2018/"]),Change=as.numeric(coredata(XU100_USD$Price["2018/"])))
ggplot(df,aes(x=Date,y=Change))+
geom_line()+
ggtitle("XU100")+
theme_dark(base_size =7)+
geom_ma(n=20,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=60,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=80,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=120,show.legend=TRUE,color="yellow",linetype="solid")+
geom_ma(n=240,show.legend=TRUE,color="black",linetype="solid")+
geom_ma(n=480,show.legend=TRUE,color="black",linetype="solid")

#--------------------------------------------------------
a <- as.numeric(coredata(TRY_XAU$Change))
b <- a[-1]-a[-length(a)]
c <- xts(b,order.by=index(TRY_XAU)[-1])
df <- data.frame(Date=index(c["2020/"]),Change=as.numeric(coredata(c["2020/"])))
ggplot(df,aes(x=Date,y=Change))+
geom_line()+
ggtitle("XU100")+
theme_dark(base_size =7)+
geom_ma(n=10,show.legend=TRUE,color="green",linetype="solid")#+
#geom_ma(n=60,show.legend=TRUE,color="red",linetype="solid")+
#geom_ma(n=80,show.legend=TRUE,color="red",linetype="solid")+
#geom_ma(n=120,show.legend=TRUE,color="yellow",linetype="solid")+
#geom_ma(n=240,show.legend=TRUE,color="black",linetype="solid")+
#geom_ma(n=480,show.legend=TRUE,color="black",linetype="solid")
#--------------------------------------------------------
a <- "TRY_XAU"
df <- data.frame(Date=index(get(a)["2019/"]),Change=as.numeric(coredata(get(a)$Change["2019/"])))
ggplot(df,aes(x=Date,y=Change))+
geom_line(linetype="dotted")+
ggtitle(a)+
theme_dark(base_size = 7)+
geom_ma(n=4,show.legend=TRUE,color="green",linetype="solid")+#2-5-20-60
geom_ma(n=9,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=19,show.legend=TRUE,color="yellow",linetype="solid")+
geom_ma(n=43,show.legend=TRUE,color="black",linetype="solid")
#-----------------------------------------------------

["2015/"]

#PRIME RETURN SERIES FUNCTION
RG <-function(M,ts_data,dfg,file_name){

u <- list()
primes <- c(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,360,480,600,720,960,1200,1440,1680,1920,2160,2400)
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
if(ss==1){x <- x["2018/"]}
if(ss==2){x <- x["2017/"]}
if(ss==3){x <- x["2016/"]}
if(ss==5){x <- x["2015/"]}
if(ss==7){x <- x["2015/"]}
if(ss==11){x <- x["2014/"]}
if(ss==13){x <- x["2014/"]}
if(ss==17){x <- x["2013/"]}
if(ss==19){x <- x["2013/"]}
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
#shell(str_c("C:/Users/vakka/Documents/",str_c("file_name",".pdf")))
}

RG("XU100_XAU",XU100_XAU,20,"RETURN")

------------------------------------------------------------------------

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
source("live.r",echo=T)

R("USD_TRY",USD_TRY,20)
R("XU100_USD",XU100_USD,20)

-----------------------------------------------------------------------

low <- tickers
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

---------------------------------------------------------
gr <- c("ADESE","KORDS","KARSN","SOKM","BIZIM","TEKTU","HATEK","CEMAS","MPARK","ATLAS","MGROS","ARSAN","PAGYO","ENKAI","ANSGR","ANHYT","EKGYO","DIRIT","AYES","ORMA","AKGUV","EDIP","HURGZ","KSTUR","BRMEN","NUGYO","OSTIM","NETAS","NTHOL","DOKTA","EUKYO","GOLTS","LIDFA","IHLAS","AVTUR","SNKRN","BEYAZ","DZGYO","TUCLK","AKSEN","PNSUT","EGCYH","CIMSA","BUCIM","SKTAS","AVGYO","TOASO","BRKSN","YKGYO","DOHOL","FLAP","SKBNK","RHEAG","USAK","MAKTK","CCOLA","EREGL","PRKME","IEYHO","MARTI","AKGRT","MEPET","YGYO","ETILR","ISYAT","MRDIN","YYAPI","AGHOL","METAL","AGYO","ENJSA","MEMSA","KUYAS","IHYAY","ALYAG","AFYON","AEFES","OZGYO","IPEKE","IHLGM","BAGFS","KRSTL","IHEVA","BTCIM","ISCTR","ATAGY","YKBNK","SAHOL","KOZAL","YGGYO","GSDHO","AYGAZ","ULKER","DEVA","ULAS","TSKB","GOODY","ARDYZ","AKFGY","AVISA","ISGYO","GARAN","VAKBN","TSPOR","TTKOM","ALBRK","KCHOL","TCELL","UMPAS","TKFEN","SEKFK","BJKAS","DOCO","THYAO","AKBNK","HALKB","TAVHL","RALYH","ADEL","PEGYO","EGCEY","TRCAS","IDGYO","SNGYO","PRZMA","SAMAT","YAYLA","ROYAL","METRO","KRTEK","AKENR","EGCYO","DOAS","EGSER","SILVR","TMPOL","ORGE","DGKLB","VKGYO","AKCNS","KERVN","INFO","FENER","MMCAS","NUHCM")
------------------------------------------------------------------------
u <- list()
f <- c()
d <- c()
p <- XU100_USD$Price
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- log(as.numeric(coredata(b)))
ss<-2
for(i in 0:(length(b)-ss)){
f <- c(f,mean(b[c((length(b)-ss+1-i):(length(b)-i))]))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
x <- x["2016/"]
df <- data.frame(Date=index(x),Price=as.numeric(coredata(x)))

for(ii in 1:240){
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(str_c(as.character(ii),as.character(ss)))+
geom_ma(n=ii+1,show.legend=TRUE,color="green",linetype="solid")+
theme_dark(base_size =7)
assign(str_c("g",as.character(ss)),g)
u[[ii]] <- get(str_c("g",as.character(ss)))}

res <- marrangeGrob(u, nrow = 1, ncol = 1)
# Export to a pdf file
file.remove("RETURN.pdf")
ggexport(res, filename = "RETURN.pdf")
---------------------------------------------------------------------------
low <- str_c(p$Sembol[-1],"_USD")#gsub("XAU","USD",gsub("_SX","",SX160$Symbols))
setwd("C:/Users/vakka/Documents/rts")
for(asd in 1:length(low)){
u <- list()
f <- c()
d <- c()
p <- get(low[asd])$Price
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- log(as.numeric(coredata(b)))
ss<-1
for(i in 0:(length(b)-ss)){
f <- c(f,mean(b[c((length(b)-ss+1-i):(length(b)-i))]))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
x <- x["2018/"]
df <- data.frame(Date=index(x),Price=as.numeric(coredata(x)))
a <-  c(1,2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61)
indx <- 1
for(ii in a){
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(str_c(low[asd],as.character(a[indx])))+
geom_ma(n=ii+1,show.legend=TRUE,color="green",linetype="solid")+
theme_dark(base_size =7)
assign(str_c("g",as.character(ss)),g)
u[[indx]] <- get(str_c("g",as.character(ss)))
indx <- indx+1}

res <- marrangeGrob(u, nrow = 1, ncol = 1)
# Export to a pdf file
file.remove(str_c(as.character(asd),"RETURN.pdf"))
ggexport(res, filename = str_c(as.character(asd),"RETURN.pdf"))

}
setwd("C:/Users/vakka/Documents")






ggplot(v,aes(x=Temperature,y=Without.Finger))+
geom_point(color="green")+
geom_point(y=v$With.Finger)+
ggtitle("Voltage Variation With and Without Finger")+
#scale_y_continuous(breaks = seq(0.590,0.621,by=0.003))+
#scale_x_continuous(breaks = seq(22.9,26.9,by=0.1))

v$Ortam <- ma(v$MAX, 2)


ggplot() +
 geom_point(data = v, aes(x = Nesne, y = X, color = "Difference")) +
 geom_line(data = v, aes(x = Nesne, y = X, color = "Difference")) +
 #geom_point(data = v, aes(x = Nesne, y = MIN, color = "MIN")) +
 #geom_line(data = v, aes(x = Nesne, y = MIN, color = "MIN")) +
 scale_x_continuous(breaks = seq(30,40,by=1))+
 scale_y_continuous(breaks = seq(0.8,1.2,by=0.1))+
 theme_bw(base_size = 11)+
 theme(legend.position="bottom",axis.text.y = element_text(colour="black"))+
 labs(x = "Temperature of The Object °C  ",y = "Voltage mV")+
 ggtitle('Voltage Difference Between MAX and MIN Value mV')+
 geom_vline(xintercept = 35, color="gray",linetype="dashed")+
   
geom_line(data = v, aes(x = Saat, y = Bos, color = "No Stimulus"),linetype = "dashed") +
   geom_point(data = v, aes(x = Saat, y = El.Tepki, color = "Hand Stimulus"),size=2) +
   #geom_hline(yintercept = mean(v$El.Tepki), color="gray",linetype="dashed")+
   geom_line(data = v, aes(x = Saat, y = El.Tepki, color = "Hand Stimulus"),linetype = "dashed") +
   xlab('Temperature °C / Time') +
   ylab('Voltage Values (mV)')+
   ggtitle("Voltage Variation Against Hand Stimulus")+
   scale_y_continuous(breaks = seq(0.620,0.629,by=0.001))+
   scale_x_discrete(breaks = v$Saat,label=str_c(str_c(as.character(v$Ortam),"°C"),v$Saat,sep="-"))+
   #theme(legend.position = c(.92, .60))+
   theme_bw(base_size = 8)+
   theme(legend.position="bottom",axis.text.y = element_text(colour="black"))
------------------------------------------------------------------------------
ggplot(data = v, aes(x = Saat, y = Bos),size=2) +
 geom_point() +
 
 #geom_hline(yintercept = mean(v$Bos), color="gray",linetype="dashed")+
   #geom_line(data = v, aes(x = Saat, y = Bos, color = "No Stimulus"),linetype = "dashed") +
   #geom_point(data = v, aes(x = Saat, y = El.Tepki, color = "Hand Stimulus"),size=2) +
   #geom_hline(yintercept = mean(v$El.Tepki), color="gray",linetype="dashed")+
   #geom_line(data = v, aes(x = Saat, y = El.Tepki, color = "Hand Stimulus"),linetype = "dashed") +
geom_ma(n=2,color="green",linetype="solid")+
geom_ma(n=3,color="red",linetype="solid")+
geom_ma(n=4,color="blue",linetype="solid")+   
xlab('Temperature °C / Time') +
   ylab('Voltage Values (mV)')+
   ggtitle("Voltage Variation Against Hand Stimulus")+
   scale_y_continuous(breaks = seq(0.620,0.629,by=0.001))+
   scale_x_discrete(breaks = v$Saat,label=str_c(str_c(as.character(v$Ortam),"°C"),v$Saat,sep="-"))+
   #theme(legend.position = c(.92, .60))+
   theme_bw(base_size = 8)+
   theme(legend.position="bottom",axis.text.y = element_text(colour="black"))


-----------------------------------------------------------------------

,label=str_c(as.character(v$Ortam),v$Saat,sep=" - ")
----------------------------------------------------
data <- data.frame(
  x=v$Ortam,
  y=(v$El.Tepki-v$Bos)*1000)

ggplot(data, aes(x=x, y=y))+
geom_point()+
geom_line()+
xlab("Temperature") +
ylab("Voltage Difference mV")+
scale_x_continuous(breaks = seq(23,27.5,by=0.1))

ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=1, yend=y), color="grey") +
  geom_point( color="orange", size=4) +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")+
scale_x_discrete(breaks = seq(23,27.5,by=0.1))
-----------------------------------------------------------


ggplot(data = v, aes(x = Ortam, y = Bos)) + 
#geom_line(data = v, aes(x = Ortam, y = Bos))+
geom_ma(n=2,color="green",linetype="solid")+
geom_ma(n=3,color="red",linetype="solid")+
geom_ma(n=4,color="blue",linetype="solid")+
geom_point()+
scale_y_continuous(breaks = seq(0.620,0.623,by=0.001))+
scale_x_continuous(breaks = seq(23.1,24.6,by=0.1))+
ggtitle("No Stimulus (Dots) - 2 Points MA (Green) - 3 Points MA (Red) - 4 Points MA (Blue)")+
xlab("Temperature °C") +
  ylab("Voltage Level V")
--------------------------------------------------------------------
d <- data.frame(x = v$Nesne[-11],y=1000*(v$MAX[-1]-v$MAX[-11]))
ggplot(data = d, aes(x = x, y = y)) +
geom_point()+geom_line(color="red")+
geom_ma(n=3,color="green",linetype="solid")+
ggtitle("Voltage Change Against Per Unit Temperature Change For MAXIMUM (V2-V1/T2-T1)")+
labs(x = "Object Temperature °C",y="Change Per Unit Temperature mV")+
scale_x_continuous(breaks = seq(30,39,by=1),labels=c("31-30","32-31","33-32","34-33","35-34","36-35","37-36","38-37","39-38","40-39"))+
scale_y_continuous(breaks = seq(-0.40,0.50,by=0.05))+
geom_vline(xintercept = 35, color="red",linetype="dashed")+
geom_hline(yintercept = 0, color="black",linetype="solid")+
theme_bw(base_size = 11)+
theme(legend.position="bottom",axis.text.y = element_text(colour="black"))



