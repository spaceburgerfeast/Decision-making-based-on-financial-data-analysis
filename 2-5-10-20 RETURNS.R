low <- gsub("XAU","USD",gsub("_SX","",SX150$Symbols))

low <- str_c(p$Symbol,"_USD")

x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){


p <- get(low[i])$Price

r <- coredata(p[-1])/p[-nrow(p)]
r$Price <- log(as.numeric(coredata(r)))

df <- data.frame(Date=index(r$Price["2020/"]),Change=as.numeric(coredata(r$Price["2020/"])))
g<-ggplot(df,aes(x=Date,y=Change))+
geom_line(linetype="blank")+
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
