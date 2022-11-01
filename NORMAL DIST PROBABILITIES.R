normalp <- function(data,n)
{

p <- data$Change["2013/"]
b1 <- as.numeric(coredata(p))
if(!sum(is.na(b1))==0){b1 <- impute_AR1_Gaussian(b1)}
mb1 <- mean(b1)
sdb1 <- sd(b1)
z <- c("b1")
m <- c("mb1")
s <- c("sdb1")

a <- c()

for(i in 2:n){
for(ii in 1:(length(b1)-i)){
a[ii] <- sum(b1[c(ii:(i+ii-1))])
}
assign(str_c("b",as.character(i)),a)
z <- c(z,str_c("b",as.character(i)))
assign(str_c("m",str_c("b",as.character(i))),mean(a))
m <- c(m,str_c("m",str_c("b",as.character(i))))
assign(str_c("sd",str_c("b",as.character(i))),sd(a))
s <- c(s,str_c("sd",str_c("b",as.character(i))))
a <- c()
}
h<- c()
Y <- c()
for(j in 1:n){
if(sum(b1[c((length(b1)-j+1):length(b1))])>=0){
h[j]<- 1-pnorm(sum(b1[c((length(b1)-j+1):length(b1))]),get(m[j]),get(s[j]))
Y[j]<- (sum(b1[c((length(b1)-j+1):length(b1))])-get(m[j]))/get(s[j])
}else{
h[j]<- pnorm(sum(b1[c((length(b1)-j+1):length(b1))]),get(m[j]),get(s[j]))
Y[j]<- (sum(b1[c((length(b1)-j+1):length(b1))])-get(m[j]))/get(s[j])
}
}
x <- data.frame(Prob=h,SD=Y)
x
}

tt <- c()
for(q in 1:length(bst_usd)){
n <- 150
p <- as.numeric(coredata(get(bst_usd[q])$Change["2003/"]))
b1 <- na.omit(p)
a <- c()
mb1 <- mean(b1)
sd1 <- sd(b1)
z <- c("b1")
m <- c("mb1")
s<- c("sd1")
sxt <- c()
for(i in 2:n){
if(n ==1){break}
for(ii in 1:(length(b1)-i+1)){
a[ii] <- sum(b1[c(ii:(i+ii-1))])
}
assign(str_c("b",as.character(i)),a)
z <- c(z,str_c("b",as.character(i)))
assign(str_c("m",str_c("b",as.character(i))),mean(a))
m <- c(m,str_c("m",str_c("b",as.character(i))))
assign(str_c("sd",str_c("b",as.character(i))),sd(a))
s <- c(s,str_c("sd",str_c("b",as.character(i))))

a <- c()
}

sx <-c()
for(i in 1:n){
sx <- c(sx,(get(z[i])[length(get(z[i]))]-get(m[i]))/get(s[i]))
}
assign(str_c(bst_usd[q],"_SX"),sx)
tt <- c(tt,str_c(bst_usd[q],"_SX"))
}
for(mm in 1:n){
kk <-c()
for(m in 1:length(tt)){
kk <- c(kk,get(tt[m])[mm])
}
x <- data.frame(Symbols=tt,Value=kk)
y <- na.omit(x)
ind <- order(y$Value)
l <- data.frame(Symbols=y$Symbols[ind],Value=y$Value[ind])
assign(str_c("SX",as.character(mm)),l)
}

low <- c()
for(i in 1:n){
low[i]<-gsub("USD","TRY",str_sub(get(str_c("SX",as.character(i)))[3,1],start=1,end=str_length(get(str_c("SX",as.character(i)))[3,1])-3))
}
x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){
h <- get(low[i])["2020/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Price)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(low[i])
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd.pdf")







low <- gsub("XAU","USD",gsub("_SX","",SX240$Symbols))



x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){
a<-get(low[i])
if(nrow(a)<240){low<-low[-i]}}
for(i in 1:length(low)){
h <- get(low[i])["2019/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Price)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(low[i])+
geom_ma(n=60,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=120,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=240,show.legend=TRUE,color="blue",linetype="solid")+
geom_ma(n=480,show.legend=TRUE,color="black",linetype="solid")
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd1.pdf")
________________________________________________________________--
source("live.r",echo=T)
low <- gsub("XAU","USD",gsub("_SX","",SX160$Symbols))

x <- list()
k <- seq(1:length(low))
for(i in 1:length(low)){
h <- get(low[i])$Change["2019-11/"]
df <- data.frame(Date=index(h),Price=as.numeric(coredata(h$Change)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line(linetype = "dashed")+ggtitle(low[i])+
theme_dark(base_size = 2.5)+
geom_ma(n=4,show.legend=TRUE,color="red",linetype="solid")+
geom_ma(n=9,show.legend=TRUE,color="green",linetype="solid")+
geom_ma(n=19,show.legend=TRUE,color="blue",linetype="solid")+
geom_ma(n=43,show.legend=TRUE,color="orange",linetype="solid")
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}
res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "sd.pdf")

-------------------------------------------------------------------------------
source("live.r",echo=T)
tt <- c()
bst_usd1 <- bst_usd
for(i in 1:length(bst_usd1)){
a<-get(bst_usd1[i])
if(nrow(a)<240){bst_usd1<-bst_usd1[-i]}}

for(q in 1:length(bst_usd1)){
n <- 240
p <- as.numeric(coredata(get(bst_usd1[q])$Change["2012/"]))
b1 <- na.omit(p)
a <- c()
mb1 <- mean(b1)
sd1 <- sd(b1)
z <- c("b1")
m <- c("mb1")
s<- c("sd1")
sxt <- c()
for(i in 2:n){
if(n ==1){break}
for(ii in 1:(length(b1)-i+1)){
tp <- b1[c(ii:(i+ii-1))]
vv <- 1
for(cc in 1:length(tp)){vv <- vv*(1+(tp[cc]/100))}
a[ii] <- vv-1
}
assign(str_c("b",as.character(i)),a)
z <- c(z,str_c("b",as.character(i)))
assign(str_c("m",str_c("b",as.character(i))),mean(a))
m <- c(m,str_c("m",str_c("b",as.character(i))))
assign(str_c("sd",str_c("b",as.character(i))),sd(a))
s <- c(s,str_c("sd",str_c("b",as.character(i))))

a <- c()
}

sx <-c()
for(i in 1:n){
sx <- c(sx,(get(z[i])[length(get(z[i]))]-get(m[i]))/get(s[i]))
}
assign(str_c(bst_usd1[q],"_SX"),sx)
tt <- c(tt,str_c(bst_usd1[q],"_SX"))
}
for(mm in 1:n){
kk <-c()
for(m in 1:length(tt)){
kk <- c(kk,get(tt[m])[mm])
}
x <- data.frame(Symbols=tt,Value=kk)
y <- na.omit(x)
ind <- order(y$Value)
l <- data.frame(Symbols=y$Symbols[ind],Value=y$Value[ind])
assign(str_c("SX",as.character(mm)),l)
}


	