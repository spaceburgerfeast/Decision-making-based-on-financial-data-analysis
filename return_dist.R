# Distributions of returns

a <- KRDMD_XAU
Price<-as.numeric(coredata(a))

n_day_dist <-c()
i <-1
n<- 2
while(length(Price)>(n+i))
{
n_day_dist[i] <- ((Price[n+i]/Price[i])-1)*100
i<-i+1
}

df <- data.frame(Index=seq(i-1),Pool=n_day_dist)
ggplot(df,aes(x=Pool))+geom_density()

