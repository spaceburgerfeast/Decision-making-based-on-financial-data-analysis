#Correlations in terms of TRY
correlation <- c()
n<-1
for(i in 1:(length(return)-1))
{
a <- get(return[i])
for(k in (i+1):length(return))
{
correlation[n] <-str_c(prtfy[i],prtfy[k],"TRY",sep="_")
b <- get(return[k])
a1 <- a[index(b)]
b1 <- b[index(a)]
c <- cor(as.numeric(a1),as.numeric(b1))
assign(correlation[n],c)
n <- n+1
}
}

ikili <- c()
pearson <- c()
for(i in 1:length(correlation))
{
ikili[i] <- correlation[i]
pearson[i] <- get(correlation[i])
}
c_dat <- data.frame(hisse = ikili, c = pearson)

#Correlations in terms of USD

correlation_usd <- c()
n<-1
for(i in 1:(length(return_usd)-1))
{
a <- get(return_usd[i])
for(k in (i+1):length(return_usd))
{
correlation_usd[n] <-str_c(prtfy[i],prtfy[k],"USD",sep="_")
b <- get(return_usd[k])
a1 <- a[index(b)]
b1 <- b[index(a)]
c <- cor(as.numeric(a1),as.numeric(b1))
assign(correlation_usd[n],c)
n <- n+1
}
}

ikili_usd <- c()
pearson_usd <- c()
for(i in 1:length(correlation_usd))
{
ikili_usd[i] <- correlation_usd[i]
pearson_usd[i] <- get(correlation_usd[i])
}
c_dat_usd <- data.frame(hisse = ikili_usd, c = pearson_usd)

#Correlations in terms of XAU

correlation_xau <- c()
n<-1
for(i in 1:(length(return_xau)-1))
{
a <- get(return_xau[i])
for(k in (i+1):length(return_xau))
{
correlation_xau[n] <-str_c(prtfy[i],prtfy[k],"XAU",sep="_")
b <- get(return_xau[k])
a1 <- a[index(b)]
b1 <- b[index(a)]
c <- cor(as.numeric(a1),as.numeric(b1))
assign(correlation_xau[n],c)
n <- n+1
}
}

ikili_xau <- c()
pearson_xau <- c()
for(i in 1:length(correlation_xau))
{
ikili_xau[i] <- correlation_xau[i]
pearson_xau[i] <- get(correlation_xau[i])
}
c_dat_xau <- data.frame(hisse = ikili_xau, c = pearson_xau)

