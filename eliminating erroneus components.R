k <-c()

for(i in 1:length(tickers)){
#Eliminating erroneous components
if(!exists(tickers[i])||nrow(get(tickers[i]))<5 || is.na(as.numeric(coredata(get(tickers[i])[,4][end(get(tickers[i]))-0])))){
k <- c(k,-i)
}
if("DZGYO.IS"== tickers[i]){
DZGYO.IS[,4][index(DZGYO.IS)<as.Date("2020-08-07")]<-DZGYO.IS[,4][index(DZGYO.IS)<as.Date("2020-08-07")]/2 }
if("ALGYO.IS"== tickers[i]){
ALGYO.IS[,4][index(ALGYO.IS)<as.Date("2012-06-18")]<-ALGYO.IS[,4][index(ALGYO.IS)<as.Date("2012-06-18")]/6 }
if("ALKIM.IS"== tickers[i]){
ALKIM.IS[,4][index(ALKIM.IS)<as.Date("2020-04-24")]<-ALKIM.IS[,4][index(ALKIM.IS)<as.Date("2020-04-24")]/5 }
if("AVHOL.IS"== tickers[i]){
AVHOL.IS[,4][index(AVHOL.IS)<as.Date("2018-06-28")]<-AVHOL.IS[,4][index(AVHOL.IS)<as.Date("2018-06-28")]/2.5 }
if("BAKAB.IS"== tickers[i]){
BAKAB.IS[,4][index(BAKAB.IS)<as.Date("2002-12-19")]<-BAKAB.IS[,4][index(BAKAB.IS)<as.Date("2002-12-19")]/3 }
if("CEOEM.IS"== tickers[i]){
CEOEM.IS[,4][index(CEOEM.IS)<as.Date("2020-04-15")]<-CEOEM.IS[,4][index(CEOEM.IS)<as.Date("2020-04-15")]/3 }
if("IDEAS.IS"== tickers[i]){
IDEAS.IS[,4][index(IDEAS.IS)<as.Date("2019-11-04")]<-IDEAS.IS[,4][index(IDEAS.IS)<as.Date("2019-11-04")]/1.5 }
if("KLGYO.IS"== tickers[i]){
KLGYO.IS[,4][index(KLGYO.IS)<as.Date("2020-11-09")]<-KLGYO.IS[,4][index(KLGYO.IS)<as.Date("2020-11-09")]/3.9 }

}
tickers <- tickers_2[k]
#tickers <- c("BAGFS.IS")
