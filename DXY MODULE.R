drive_download("DXY.rds",overwrite = TRUE)
DXY <- readRDS("DXY.rds")
if(!sum(as.numeric(is.na(DXY$Price)))==0){DXY$Price <- impute_AR1_Gaussian(as.numeric(coredata(DXY$Price)))}
if(!sum(as.numeric(is.na(DXY$Change)))==0){DXY$Change<- impute_AR1_Gaussian(as.numeric(coredata(DXY$Change)))}


if(end(DXY)==Sys.Date()){
print("DXY.RDS FILE IS UP TO DATE !!!")
}else{
s <- menu(c("YES","NO"),title="\nIf you downloaded and inserted DXY data from INVESTING press YES\n ")
if(s == 2){
browseURL("https://www.investing.com/indices/usdollar-historical-data")
J <- menu(c("YES","NO"),title="\nDid you inser DXY data from INVESTING?\n ")
}
if(s ==1||J==1){
z <- read.csv("US Dollar Index Historical Data.csv",stringsAsFactors=F)
DXY_2 <- xts(x =data.frame(Price=z$Price),order.by=mdy(z$ï..Date))
DXY_2 <-DXY_2[!(index(DXY_2)%in%index(DXY))]
a <- as.numeric(coredata(DXY[end(DXY)]$Price))
b <- as.numeric(coredata(DXY_2))
c <- c(a,b)
r <- 100*((c[-1]/c[-length(c)])-1)
DXY_NEW <- xts(x=data.frame(Price=b,Change =r),order.by=index(DXY_2))
DXY <- rbind(DXY,DXY_NEW) 

#Saving and Uploading to Drive
file.remove("DXY.rds")
saveRDS(DXY,file="DXY.rds")
drive_rm("DXY.rds")
drive_upload("DXY.rds",path=as_id(drive_get("Data")))

}}


dx <- read_html("https://www.investing.com/indices/usdollar-historical-data") %>% html_nodes("#last_last")%>%xml_contents()%>%html_text(trim=TRUE)%>%{gsub(",","",.)}%>%{as.numeric(.)}
if(Sys.Date()==end(DXY)){
assign("DXY",DXY[-nrow(DXY)])
M <- ((dx/as.numeric(coredata(DXY$Price[end(DXY)])))-1)*100
realt <- xts(x = data.frame(Price=dx,Change=M),order.by=Sys.Date())
assign("DXY",rbind(DXY,realt))
}else{
M <- ((dx/as.numeric(coredata(DXY$Price[end(DXY)])))-1)*100
realt <- xts(x = data.frame(Price=dx,Change=M),order.by=Sys.Date())
assign("DXY",rbind(DXY,realt))
}

