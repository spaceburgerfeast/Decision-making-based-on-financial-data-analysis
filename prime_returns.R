x <- list()
k <- seq(1:length(bst_xau))
for(i in 1:length(bst_xau)){
h <- get(bst_xau[i])["2020/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Price)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(bst_xau[i])
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "last_graphs_sonbir.pdf")

#RETURNS ON PRIME BASES
R <-function(ts_data){
u <- list()
primes <- c(3,139,211)
n <- primes
p <- ts_data$Price
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- log(as.numeric(coredata(b)))
n <- n[n<length(b)]
f <- c()
d <- c()
for(ii in 1:length(n)){
ss <- n[ii]
for(i in 0:(length(b)-ss)){
f <- c(f,mean(b[c((length(b)-ss+1-i):(length(b)-i))]))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
df <- data.frame(Date=index(x),Price=as.numeric(coredata(x)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle("NATEN_XAU")
assign(str_c("g",as.character(n[ii])),g)
u[[ii]] <- get(str_c("g",as.character(ss)))

}
res <- marrangeGrob(u, nrow = 1, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "RETURN.pdf")

}

primes <- c(2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211)

