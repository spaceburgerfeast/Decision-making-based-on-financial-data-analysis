# Return series of an asset


a <- CEMAS_USD
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

n_day_return_of_return <- 100*((return_xau_n$Return[-nrow(return_xau_n)]/return_xau_n$Return[-1])-1)
new_date_2 <- new_date[-length(new_date)]
return_of_return_xau_n <- data.frame(Date=new_date_2,Return=n_day_return_of_return)


g <- ggplot(return_xau_n,aes(x=Date,y=Return))+
     geom_line()+
     labs(title="ODAS",subtitle=str_c(as.character(n[2*i-1]),"-Day Return"))
assign(str_c("g",as.character(n[2*i-1])),g)
x[[2*i-1]] <- get(str_c("g",as.character(n[2*i-1])))

gg <- ggplot(return_of_return_xau_n,aes(x=Date,y=Return))+
     geom_line()+
     labs(title="ODAS",subtitle=str_c(as.character(n[2*i]),"-Day Return 2"))
assign(str_c("gg",as.character(n[2*i])),gg)
x[[2*i]] <- get(str_c("gg",as.character(n[2*i])))


}

res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "Return_Series.pdf")
