#Son bir yýlýn verileri

library(gridExtra)
library(ggpubr)


hisselerim_xau<-prtfy_xau
hisselerim_usd<-prtfy_usd
hisselerim_try<-prtfy



x <- list()
k <- seq(1:length(hisselerim_xau))
l <- seq(1:length(hisselerim_usd))
m <- seq(1:length(hisselerim_try))
for(i in 1:length(hisselerim_xau))
{
h <- get(hisselerim_try[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
df<-df[-seq(1:(nrow(df)-240)),]
g <- ggplot(df,aes(x=Date,y=Price))+geom_line(colour="blue")+ggtitle(hisselerim_try[i])+theme_light()
assign(str_c("g",as.character(m[i])),g)
x[[3*i-2]] <- get(str_c("g",as.character(m[i])))


h <- get(hisselerim_usd[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
df<-df[-seq(1:(nrow(df)-240)),]
g <- ggplot(df,aes(x=Date,y=Price))+geom_line(colour="green")+ggtitle(hisselerim_usd[i])+theme_light()
assign(str_c("g",as.character(l[i])),g)
x[[3*i-1]] <- get(str_c("g",as.character(l[i])))

h <- get(hisselerim_xau[i])
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h)))
df<-df[-seq(1:(nrow(df)-240)),]
g <- ggplot(df,aes(x=Date,y=Price))+geom_line(colour="red")+ggtitle(hisselerim_xau[i])+theme_light()
assign(str_c("g",as.character(k[i])),g)
x[[3*i]] <- get(str_c("g",as.character(k[i])))

}

res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "try_usd_xau_special.pdf")


