x <- list()
k <- seq(1:length(bst_xau))
for(i in 1:length(bst_xau)){
h <- get(bst_xau[i])["2020/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Price)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(bst_xau[i])+geom_ma(ma_fun=SMA,n=50)+geom_ma(ma_fun=SMA,n=200)+geom_ma(ma_fun=SMA,n=30)
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "last_graphs.pdf")