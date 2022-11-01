#source("new.r",echo=T)

XU100_1 <- XU100[index(USD_TRY)]
USD_TRY_1 <- USD_TRY[index(XU100)]
XU100_1$Price <- coredata(XU100_1$Price)/USD_TRY_1$Price
XU100_1$Open <- coredata(XU100_1$Open)/USD_TRY_1$Price
XU100_1$High <- coredata(XU100_1$High)/USD_TRY_1$Price
XU100_1$Low <- coredata(XU100_1$Low)/USD_TRY_1$Price
XU100_1$Volume <- coredata(XU100_1$Volume)/USD_TRY_1$Price 
XU100_1$Change <- coredata(XU100_1$Change) - USD_TRY_1$Change

XU100_USD <- XU100_1

XU100_2 <- XU100_USD[index(XAU_USD)]
XAU_USD_1<- XAU_USD[index(XU100_USD)]
XU100_2$Price <- coredata(XU100_2$Price)/XAU_USD_1$Price
XU100_2$Open <- coredata(XU100_2$Open)/XAU_USD_1$Price
XU100_2$High <- coredata(XU100_2$High)/XAU_USD_1$Price
XU100_2$Low <- coredata(XU100_2$Low)/XAU_USD_1$Price 
XU100_2$Volume <- coredata(XU100_2$Volume)/XAU_USD_1$Price
XU100_2$Change <- coredata(XU100_2$Change) - XAU_USD_1$Change

XU100_XAU <- XU100_2

#lineChart(XU100_XAU, line.type = 'h', theme = 'white')
#barChart(XU100_XAU, bar.type = 'hlc', TA = NULL)
#candleChart(XU100_XAU, TA=NULL, subset = '2020')

#chartSeries(XU100_XAU$Price, theme="white", TA="addEMA(50, col='black');addEMA(200, col='blue')")