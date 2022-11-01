primes <- c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211)


p <- USD_TRY$Price

r <- coredata(p[-1])/p[-nrow(p)]
r$Price <- log(as.numeric(coredata(r)))
#colnames(r)[1] <- "Close"
#a <- EMA(Cl(r),n = 200)
#colnames(r)[1] <- "Price"
#autoplot(a)

chartSeries(r$Price["2018/"], theme="white", TA="addEMA(50, col='black')")

#df <- data.frame(Date=index(r$Price["2020-07/"]),Change=as.numeric(coredata(r$Price["2020-07/"])))
#ggplot(df,aes(x=Date,y=Change))+geom_line()+geom_ma(n=29,show.legend=TRUE,color="blue")+geom_ma(n=19,show.legend=TRUE,color="red")+geom_ma(n=11,show.legend=TRUE,color="green")+geom_ma(n=5,show.legend=TRUE,color="orange")

df <- data.frame(Date=index(r$Price["2020/"]),Change=as.numeric(coredata(r$Price["2020/"])))
ggplot(df,aes(x=Date,y=Change))+
geom_line(linetype="blank")+
theme_dark(base_size = 7)+
geom_ma(n=2,show.legend=TRUE,color="green",linetype="solid",size=0.70)+
geom_ma(n=5,show.legend=TRUE,color="red",linetype="solid",size=0.75)+
geom_ma(n=20,show.legend=TRUE,color="yellow",linetype="solid",size=0.80)+
geom_ma(n=60,show.legend=TRUE,color="black",linetype="solid",size=0.95)


Z <- function(data,n) {
x <- list()
k <- seq(1:n)
for(i in 1:n){
p <- data$Price

r <- coredata(p[-1])/p[-nrow(p)]
r$Price <- log(as.numeric(coredata(r)))
df <- data.frame(Date=index(r$Price["2020/"]),Change=as.numeric(coredata(r$Price["2020/"])))
g <- ggplot(df,aes(x=Date,y=Change))+ggtitle(as.character(i))+geom_line(linetype="blank")+geom_ma(n=i,show.legend=TRUE,color="green",linetype="solid",size=0.1)

assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd1.pdf")

}


+geom_ma(n=13,show.legend=TRUE,color="brown",linetype="solid",size=1)+geom_ma(n=11,show.legend=TRUE,color="blue",linetype="solid",size=0.75)+geom_ma(n=7,show.legend=TRUE,color="red",linetype="solid",size=0.5)+geom_ma(n=5,show.legend=TRUE,color="magenta",linetype="solid",size=0.25)
r2 <- coredata(r[-1])/r[-nrow(r)]
r3 <- coredata(r2[-1])/r2[-nrow(r2)]
r4 <- coredata(r3[-1])/r3[-nrow(r3)]
r5 <- coredata(r4[-1])/r4[-nrow(r4)]
r6 <- coredata(r5[-1])/r5[-nrow(r5)]
r7 <- coredata(r6[-1])/r6[-nrow(r6)]
r8 <- coredata(r7[-1])/r7[-nrow(r7)]
r9 <- coredata(r8[-1])/r8[-nrow(r8)]
webpage <- 
results <- webpage %>% html_nodes("#last_last")%>%xml_contents()%>%html_text(trim=TRUE)
 read_html("https://www.investing.com/currencies/xau-usd-historical-data") %>% html_nodes("#last_last")%>%xml_contents()%>%html_text(trim=TRUE)%>%{gsub(",","",.)}%>%{as.numeric(.)}


 
r$Price <- log(as.numeric(coredata(r)))
r2$Price <- log(as.numeric(coredata(r2)))
r3$Price <- log(as.numeric(coredata(r3)))
r4$Price <- log(as.numeric(coredata(r4)))
r5$Price <- log(as.numeric(coredata(r5)))
r6$Price <- log(as.numeric(coredata(r6)))
r7$Price <- log(as.numeric(coredata(r7)))
r8$Price <- log(as.numeric(coredata(r8)))
r9$Price <- log(as.numeric(coredata(r9)))


chartSeries(r2$Price["2013/"], theme="white", TA="addEMA(29, col='black')")
chartSeries(r3$Price["2013/"], theme="white", TA="addEMA(29, col='black')")
chartSeries(r4$Price["2013/"], theme="white", TA="addEMA(29, col='black')")
chartSeries(r9$Price["2013/"], theme="white", TA="addEMA(29, col='black')")



df <- data.frame(Change = as.numeric(coredata(r2$Price["2013/"])))
ggplot(data=df,aes(Change))+geom_density()