Last <- c()
Symbols <- c()
n <- 5
for(i in 1:length(bst_xau))
{
p <- get(bst_xau[i])$Price
r <- coredata(p[-1])/p[-nrow(p)]
r$Price <- log(as.numeric(coredata(r)))
colnames(r) <- "Change"
if(n+2 > length(r)){next}
Last[i]<-  mean(as.numeric(coredata(r))[(length(r)-n+1):length(r)])
Symbols[i] <- bst_xau[i]
}p <- USD_TRY$Price



