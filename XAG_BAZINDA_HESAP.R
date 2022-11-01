bst_xag<-c()
for(i in 1:length(tickers)){
bst_xag[i] <- str_c(str_sub(tickers[i],start=1,end=str_length(tickers[i])-3),"_XAG")}


for(i in 1:length(bst_xag)){
bst_tmp<-get(bst_usd[i])[index(XAG_USD)]
xag_tmp<-XAG_USD[index(get(bst_usd[i]))]
bst_tmp$Price <- coredata(bst_tmp$Price)/xag_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)-xag_tmp$Change
if(!sum(as.numeric(is.na(bst_tmp$Price)))==0){bst_tmp$Price <- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Price)))}
if(!sum(as.numeric(is.na(bst_tmp$Change)))==0){bst_tmp$Change<- impute_AR1_Gaussian(as.numeric(coredata(bst_tmp$Change)))}
assign(bst_xag[i],bst_tmp)
}

TRY_XAG<-c()
bst_tmp<-XAG_USD[index(USD_TRY)]
xag_tmp<-USD_TRY[index(XAG_USD)]
bst_tmp$Price <- coredata(bst_tmp$Price)*xag_tmp$Price 
bst_tmp$Change <- coredata(bst_tmp$Change)+xag_tmp$Change
TRY_XAG<-bst_tmp

df <- data.frame(Date=index(DXY_XAU$Price["2015/"]),Change=as.numeric(coredata(DXY_XAU$Price["2015/"])))
ggplot(df,aes(x=Date,y=Change))+
geom_line()+
ggtitle("XU100")+
theme_dark(base_size =7)+
geom_ma(n=20,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=60,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=80,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=120,show.legend=TRUE,color="yellow",linetype="solid")+
geom_ma(n=240,show.legend=TRUE,color="black",linetype="solid")+
geom_ma(n=480,show.legend=TRUE,color="black",linetype="solid")