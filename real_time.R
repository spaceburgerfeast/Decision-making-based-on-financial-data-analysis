for(i in 1:length(bst_try)){

if(any(rownames(a)== str_c(str_sub(bst_xau[i],start=1,end=(str_length(bst_xau[i])-4)),".IS"))){
k <- which(rownames(a)==str_c(str_sub(bst_xau[i],start=1,end=(str_length(bst_xau[i])-4)),".IS"))
realt <- xts(x = data.frame(Price=a[k,2],Change=a[k,4]),order.by=as.Date(str_sub(as.character(a[k,1])),start=1,end=10))
#XAU PRICE
if(end(USD_XAU)==as.Date(str_sub(as.character(a[k,1])),start=1,end=10)){
realt_xau <- realt
realt_xau$Price <- coredata(realt$Price)/USD_XAU$Price[end(USD_XAU)]
realt_xau$Change <- coredata(realt$Change)-USD_XAU$Change[end(USD_XAU)]}

if(end(get(bst_xau[i]))==as.Date(str_sub(as.character(a[k,1])),start=1,end=10)){
assign(bst_xau[i],get(bst_xau[i])[-nrow(get(bst_xau[i]))])
assign(bst_xau[i],rbind(get(bst_xau[i]),realt_xau))
}else{assign(bst_xau[i],rbind(get(bst_xau[i]),realt_xau))}

}}