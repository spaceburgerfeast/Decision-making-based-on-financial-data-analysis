# Return series of an asset


a <- ODAS_XAU
n <- seq(500)
x <- list()

for(i in 1:length(n))
{
p <- n[i]

df <- data.frame(Date=index(a),Price=as.numeric(coredata(a)))

n_day_index_1 <- -seq(from=-nrow(df),to=-1,by=p)
n_day_index_2 <- n_day_index_1[-1]
n_day_index_1 <- n_day_index_1[-length(n_day_index_1)]
new_date <- df$Date[n_day_index_1]
n_day_return <- 100*((df$Price[n_day_index_1]/df$Price[n_day_index_2])-1)
return_xau_n <- data.frame(Date=new_date,Return=n_day_return)

g <- ggplot(return_xau_n,aes(x=Date,y=Return))+
     geom_line()+
     labs(title="ODAS",subtitle=str_c(as.character(n[i]),"-Day Return"))
assign(str_c("g",as.character(n[i])),g)
x[[i]] <- get(str_c("g",as.character(n[i])))

}

res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "Return_Series.pdf")
