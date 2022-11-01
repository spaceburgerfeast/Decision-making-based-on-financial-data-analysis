library("Amelia")
library(tidyquant)
library(tidyverse)
library(lubridate)
library(xts)
library(PerformanceAnalytics)
bst <- read.csv("bisttum.csv",stringsAsFactors=F)
bst <- bst$Code
bst <- str_c(bst,".IS")
bst <- gsub(" ","",bst)

 
tickers <- bst
tickers <- getSymbols(tickers,verbose = TRUE,from="1995-01-01")

prtfy <- c()
prtfy_usd <-c()
prtfy_xau <-c()
return <- c()
return_usd <-c()
return_xau <-c()
mean <-c()
mean_usd <-c()
mean_xau <-c()
variance <-c()
variance_usd <-c()
variance_xau <-c()
deviation <-c()
deviation_usd <-c()
deviation_xau <-c()
skewness <-c()
skewness_usd <-c()
skewness_xau <-c()
kurtosis <-c()
kurtosis_usd <-c()
kurtosis_xau <-c()




ons <- 31.1034807

usd <- read.csv("USD_TRY Historical Data.csv",stringsAsFactors=FALSE)
assign("usd",na.omit(usd))
dat_usd <- mdy(usd$ï..Date)

xau <- read.csv("XAU_USD Historical Data.csv",stringsAsFactors=FALSE)
assign("xau",na.omit(xau))
dat_xau <-mdy(xau$ï..Date)

dat_usd_xau <- intersect(dat_usd,dat_xau)

#Checking and elimination part if there is an xts 
#deprived of sufficient number of elements

len<-length(tickers)
x<-1
for(iii in 1:len)
{
a <- get(tickers[x])
b <- a[,4]
if(length(b)<100)
{
tickers<- tickers[-x]
len <- len-1
x <- x-1
}
x <- x+1
}

for(i in 1:length(tickers)) {
assign(tickers[i],na.omit(get(tickers[i])))
prtfy[i]<-substr(tickers[i],1,nchar(tickers[i])-3)
prtfy_usd[i] <- str_c(prtfy[i],"_USD")
prtfy_xau[i] <- str_c(prtfy[i],"_XAU")

return[i] <- str_c(prtfy[i],"_R")
return_usd[i] <- str_c(prtfy[i],"_R_USD")
return_xau[i] <- str_c(prtfy[i],"_R_XAU")
mean[i] <- str_c(prtfy[i],"_MEAN")
mean_usd[i] <- str_c(prtfy[i],"_MEAN_USD")
mean_xau[i] <- str_c(prtfy[i],"_MEAN_XAU")
variance[i] <- str_c(prtfy[i],"_VAR")
variance_usd[i] <- str_c(prtfy[i],"_VAR_USD")
variance_xau[i] <- str_c(prtfy[i],"_VAR_XAU")
deviation[i] <- str_c(prtfy[i],"_DEV")
deviation_usd[i] <- str_c(prtfy[i],"_DEV_USD")
deviation_xau[i] <- str_c(prtfy[i],"_DEV_XAU")
skewness[i] <- str_c(prtfy[i],"_SKEW")
skewness_usd[i] <- str_c(prtfy[i],"_SKEW_USD")
skewness_xau[i] <- str_c(prtfy[i],"_SKEW_XAU")
kurtosis[i] <- str_c(prtfy[i],"_KURT")
kurtosis_usd[i] <- str_c(prtfy[i],"_KURT_USD")
kurtosis_xau[i] <- str_c(prtfy[i],"_KURT_XAU")

a <- get(tickers[i])
b <- a[,4]

for(ii in 2:length(b))
{
m <- as.numeric(coredata(b[ii]))
n <- as.numeric(coredata(b[ii-1]))
k<- abs(((m-n)/n)*100)
j <- (k > 150)
if(j)
{
b[ii] <- as.numeric(b[ii-1])
}
}
assign(prtfy[i],b)

dat_ind_usd <- intersect(index(b),dat_usd)
usd_ind <- match(dat_ind_usd,dat_usd)
ind_ind <- match(dat_ind_usd,index(b))
c <- b[ind_ind] / usd$Price[usd_ind]

for(ii in 2:length(c))
{
m <- as.numeric(coredata(c[ii]))
n <- as.numeric(coredata(c[ii-1]))
k<- abs(((m-n)/n)*100)
j <- (k > 150)
if(j)
{
c[ii] <- as.numeric(c[ii-1])
}
}

assign(prtfy_usd[i],c)

dat_ind_usd_xau <- intersect(index(b),dat_usd_xau)
usd_ind_2 <- match(dat_ind_usd_xau,dat_usd)
ind_ind_2 <- match(dat_ind_usd_xau,index(b))
usd_xau_ind <- match(dat_ind_usd_xau,dat_xau)
d <- (b[ind_ind_2] / usd$Price[usd_ind_2])/(as.numeric(gsub(",","",xau$Price[usd_xau_ind]))/ons)

for(ii in 2:length(d))
{
m <- as.numeric(coredata(d[ii]))
n <- as.numeric(coredata(d[ii-1]))
k<- abs(((m-n)/n)*100)
j <- (k > 150)
if(j)
{
d[ii] <- as.numeric(d[ii-1])
}
}

assign(prtfy_xau[i],d)
}



for(i in 1:length(prtfy))
{
p <-1
y_try <- get(prtfy[i])
L_try <- length(y_try)
sq_try <- -seq(from=-L_try,to=-1,by=p)
r_try <- (( 1/ (coredata(y_try[sq_try[-1]])/y_try[sq_try[-length(sq_try)]]) )-1)*100
a_max <- max(r_try)
b_min <- min(r_try)
while(a_max>100)
{
r_try[which(r_try==a_max)]<-0
a_max <- max(r_try)
}
while(b_min<(-100))
{
r_try[which(r_try==b_min)]<-0
b_min <- min(r_try)
}

assign(return[i],r_try)
m_try <- mean(as.numeric(r_try))
assign(mean[i],m_try)
v_try <- var(as.numeric(r_try))
assign(variance[i],v_try)
d_try <- sd(as.numeric(r_try))
assign(deviation[i],d_try)
s_try <- skewness(as.numeric(r_try))
assign(skewness[i],s_try)
k_try <- kurtosis(as.numeric(r_try),method="moment")
assign(kurtosis[i],k_try)

y_usd <- get(prtfy_usd[i])
L_usd <- length(y_usd)
sq_usd <- -seq(from=-L_usd,to=-1,by=p)
r_usd <- (( 1/ (coredata(y_usd[sq_usd[-1]])/y_usd[sq_usd[-length(sq_usd)]]) )-1)*100
a_max <- max(r_usd)
b_min <- min(r_usd)
while(a_max>100)
{
r_usd[which(r_usd==a_max)]<-0
a_max <- max(r_usd)
}
while(b_min<(-100))
{
r_usd[which(r_usd==b_min)]<-0
b_min <- min(r_usd)
}


assign(return_usd[i],r_usd)
m_usd <- mean(as.numeric(r_usd))
assign(mean_usd[i],m_usd)
v_usd <- var(as.numeric(r_usd))
assign(variance_usd[i],v_usd)
d_usd <- sd(as.numeric(r_usd))
assign(deviation_usd[i],d_usd)
s_usd <- skewness(as.numeric(r_usd))
assign(skewness_usd[i],s_usd)
k_usd <- kurtosis(as.numeric(r_usd),method="moment")
assign(kurtosis_usd[i],k_usd)


y_xau <- get(prtfy_xau[i])
L_xau <- length(y_xau)
sq_xau <- -seq(from=-L_xau,to=-1,by=p)
r_xau <- (( 1/ (coredata(y_xau[sq_xau[-1]])/y_xau[sq_xau[-length(sq_xau)]]) )-1)*100
a_max <- max(r_xau)
b_min <- min(r_xau)
while(a_max>100)
{
r_xau[which(r_xau==a_max)]<-0
a_max <- max(r_xau)
}
while(b_min<(-100))
{
r_xau[which(r_xau==b_min)]<-0
b_min <- min(r_xau)
}

assign(return_xau[i],r_xau)
m_xau <- mean(as.numeric(r_xau))
assign(mean_xau[i],m_xau)
v_xau <- var(as.numeric(r_xau))
assign(variance_xau[i],v_xau)
d_xau <- sd(as.numeric(r_xau))
assign(deviation_xau[i],d_xau)
s_xau <- skewness(as.numeric(r_xau))
assign(skewness_xau[i],s_xau)
k_xau <- kurtosis(as.numeric(r_xau),method="moment")
assign(kurtosis_xau[i],k_xau)
}

ikili_xau <- c()
pearson_xau <- c()
for(i in 1:length(mean_xau))
{
ikili_xau[i] <- mean_xau[i]
pearson_xau[i] <- get(mean_xau[i])
}
mean_dat_xau <- data.frame(hisse = ikili_xau, mean = pearson_xau)
mean_dat_xau <- data.frame(hisse = mean_dat_xau$hisse[order(mean_dat_xau$mean,decreasing=T)], mean= mean_dat_xau$mean[order(mean_dat_xau$mean,decreasing=T)])

ikili_xau <- c()
pearson_xau <- c()
for(i in 1:length(variance_xau))
{
ikili_xau[i] <- variance_xau[i]
pearson_xau[i] <- get(variance_xau[i])
}
variance_dat_xau <- data.frame(hisse = ikili_xau, variance = pearson_xau)
variance_dat_xau <- data.frame(hisse = variance_dat_xau$hisse[order(variance_dat_xau$variance,decreasing=T)], variance= variance_dat_xau$variance[order(variance_dat_xau$variance,decreasing=T)])

ikili_xau <- c()
pearson_xau <- c()
for(i in 1:length(deviation_xau))
{
ikili_xau[i] <- deviation_xau[i]
pearson_xau[i] <- get(deviation_xau[i])
}
deviation_dat_xau <- data.frame(hisse = ikili_xau, deviation = pearson_xau)
deviation_dat_xau <- data.frame(hisse = deviation_dat_xau$hisse[order(deviation_dat_xau$deviation,decreasing=T)], deviation= deviation_dat_xau$deviation[order(deviation_dat_xau$deviation,decreasing=T)])

ikili_xau <- c()
pearson_xau <- c()
for(i in 1:length(skewness_xau))
{
ikili_xau[i] <- skewness_xau[i]
pearson_xau[i] <- get(skewness_xau[i])
}
skewness_dat_xau <- data.frame(hisse = ikili_xau, skewness = pearson_xau)
skewness_dat_xau <- data.frame(hisse = skewness_dat_xau$hisse[order(skewness_dat_xau$skewness,decreasing=T)], skewness= skewness_dat_xau$skewness[order(skewness_dat_xau$skewness,decreasing=T)])

ikili_xau <- c()
pearson_xau <- c()
for(i in 1:length(kurtosis_xau))
{
ikili_xau[i] <- kurtosis_xau[i]
pearson_xau[i] <- get(kurtosis_xau[i])
}
kurtosis_dat_xau <- data.frame(hisse = ikili_xau, kurtosis = pearson_xau)
kurtosis_dat_xau <- data.frame(hisse = kurtosis_dat_xau$hisse[order(kurtosis_dat_xau$kurtosis,decreasing=T)], kurtosis = kurtosis_dat_xau$kurtosis[order(kurtosis_dat_xau$kurtosis,decreasing=T)])


#Current Portfolio Analisis

library(gridExtra)
library(ggpubr)

p <- read.csv("Pozisyonlarým.csv",stringsAsFactors=F)
p$Birim.Fiyat <- gsub(",",".",p$Birim.Fiyat)
p$Birim.Fiyat <- as.numeric(p$Birim.Fiyat)
p$Alýþ.Maliyeti <- gsub(",",".",p$Alýþ.Maliyeti)
p$Alýþ.Maliyeti <- as.numeric(p$Alýþ.Maliyeti)
p$Deðiþim <- ((p$Birim.Fiyat-p$Alýþ.Maliyeti)/p$Alýþ.Maliyeti)*100
ind <- order(p$Deðiþim,decreasing =T)
p$Sembol <- p$Sembol[ind]
p$Birim.Fiyat <- p$Birim.Fiyat[ind]
p$Alýþ.Maliyeti <- p$Alýþ.Maliyeti[ind]
p$Deðiþim <- p$Deðiþim[ind]
p$Sembol <- str_c(p$Sembol,"_XAU")
hisselerim <- p$Sembol
#hisselerim <- intersect(hisselerim,prtfy_xau)

v<-c()
for(i in 1:length(mean_xau))
{
v[i]<-get(mean_xau[i])
}
v <- order(v,decreasing=T)
prtfy_azalan <- prtfy_xau[v]
x <- list()
k <- seq(1:length(prtfy_azalan))
for(i in 1:length(prtfy_azalan))
{
h <- get(prtfy_azalan[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(prtfy_azalan[i])
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}

res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "altýn_bazlý_mean.pdf")

source("change.r",echo=T)
source("son_bir_yýl.R",echo=T)
