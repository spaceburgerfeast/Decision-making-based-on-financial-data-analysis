#------------------------------------------------------

x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){
a<-get(low[i])
}
for(i in 1:length(low)){
h <- get(low[i])["2019-11/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Price)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(low[i])+
geom_ma(n=20,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=60,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=120,show.legend=TRUE,color="blue",linetype="solid")+
geom_ma(n=240,show.legend=TRUE,color="black",linetype="solid")
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd3.pdf")

#------------------------------------------------------

x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){
a<-get(low[i])
}
for(i in 1:length(low)){
h <- get(low[i])["2002/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Price)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(low[i])+
geom_ma(n=120,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=240,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=480,show.legend=TRUE,color="blue",linetype="solid")+
geom_ma(n=960,show.legend=TRUE,color="black",linetype="solid")
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd4.pdf")