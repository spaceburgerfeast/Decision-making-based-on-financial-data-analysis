#Current Portfolio Analisis

library(gridExtra)
library(ggpubr)

p <- read.csv("Pozisyonlarým.csv",stringsAsFactors=F)
p$Birim.Fiyat <- gsub(",",".",p$Birim.Fiyat)
p$Birim.Fiyat <- as.numeric(p$Birim.Fiyat)
p$Alýþ.Maliyeti <- gsub(",",".",p$Alýþ.Maliyeti)
p$Alýþ.Maliyeti <- as.numeric(p$Alýþ.Maliyeti)
p$Change <- ((p$Birim.Fiyat-p$Alýþ.Maliyeti)/p$Alýþ.Maliyeti)*100
ind <- order(p$Change,decreasing =T)
p$Sembol <- p$Sembol[ind]
p$Birim.Fiyat <- p$Birim.Fiyat[ind]
p$Alýþ.Maliyeti <- p$Alýþ.Maliyeti[ind]
p$Change <- p$Change[ind]

hisselerim_xau<-str_c(p$Sembol,"_XAU")
hisselerim_usd<-str_c(p$Sembol,"_USD")
hisselerim_try<-p$Sembol



x <- list()
k <- seq(1:length(hisselerim_xau))
l <- seq(1:length(hisselerim_usd))
m <- seq(1:length(hisselerim_try))
for(i in 1:length(hisselerim_xau))
{
h <- get(hisselerim_try[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(hisselerim_try[i])
assign(str_c("g",as.character(m[i])),g)
x[[3*i-2]] <- get(str_c("g",as.character(m[i])))


h <- get(hisselerim_usd[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(hisselerim_usd[i])
assign(str_c("g",as.character(l[i])),g)
x[[3*i-1]] <- get(str_c("g",as.character(l[i])))

h <- get(hisselerim_xau[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(hisselerim_xau[i])
assign(str_c("g",as.character(k[i])),g)
x[[3*i]] <- get(str_c("g",as.character(k[i])))

}

res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "try_usd_xau.pdf")

names(p)[names(p) == "Deðiþim"]<- "Deðiþim (%)"
p

