for(i in 1:length(bst_try)){
for(ii in 1:nrow(get(bst_try[i])))
{
a <- is.na(as.numeric(get(bst_try[i])$Change[ii]))
aa<- is.na(as.numeric(get(bst_try[i])$Price[ii]))

if(a&(!aa)){
b <- get(bst_try[i])
k <- b$Price[ii]
x <- b$Price[ii-1]
b$Change[ii] <- ((as.numeric(k)/as.numeric(x))-1)*100
}
}
}