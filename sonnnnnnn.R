#p <- KRDMD_XAU$Price

#b <- coredata(p[-1])/p[-nrow(p)]
#b$Price <- log(as.numeric(coredata(b)))

p <- IZTAR_XAU$Price
b <- coredata([-1])/p[-nrow(p)]
b$Price <- log(as.numeric(coredata(b)))
b <- as.numeric(coredata(b))
n <- 50 #length(b)
r <- c()
for(i in 1:n){
r[i] <-  mean(b[(length(b)-i+1):length(b)])
}
r <- r[seq(from=length(r),to=1)]
plot(r,type='l')


#Simple moving average function
mysma <-function(n,j){
p <- j$Price
a <- index(p)
b <- coredata(p[-1])/p[-nrow(p)]
b <- log(as.numeric(coredata(b)))
f <- c()
d <- c()
for(i in 0:(length(b)-n)){
f <- c(f,mean(b[c((length(b)-n+1-i):(length(b)-i))]))
d <- c(d,a[(length(b)-i)+1])
}
x <- xts(x = data.frame(SMA=f),order.by=as.Date(d))
autoplot(x)
}
seq(from=length(SMA),to=1)
