d <- data.frame(t=(c("Ocak","Þubat","Mart","Nisan","Mayýs","Haziran","Temmuz","Aðustos","Eylül","Ekim","Kasým","Aralýk")),e=(c("January","February","March","April","May","June","July","August","September","October","November","December")))

as.character(d$e[which(d$t == str_sub(format(index(ASELS_TRY[is.na(ASELS_TRY$Price)]), format="%B %d, %Y"),start=1,end=(str_length(format(index(ASELS_TRY[is.na(ASELS_TRY$Price)]), format="%B %d, %Y"))-9)))])

for(i in 1:length(bst_try))
{
f <-str_c(as.character(d$e[which(d$t == str_sub(format(index(get(bst_try[i])[is.na(get(bst_try[i])$Price)]), format="%B %d, %Y"),start=1,end=(str_length(format(index(get(bst_try[i])[is.na(get(bst_try[i])$Price)]), format="%B %d, %Y"))-9)))]),str_sub(format(index(get(bst_try[i])[is.na(get(bst_try[i])$Price)]), format="%B %d, %Y"),start=(str_length(format(index(get(bst_try[i])[is.na(get(bst_try[i])$Price)]), format="%B %d, %Y"))-8),end=(str_length(format(index(get(bst_try[i])[is.na(get(bst_try[i])$Price)]), format="%B %d, %Y")))))
x <- read_html("https://www.investing.com/equities/tupras-historical-data") %>% html_nodes("tbody tr")%>%xml_contents()%>%html_text(trim=TRUE)
a <-x[which(x==str_c(str_sub(f,start=1,end=3),str_sub(f,start=(str_length(f)-8),end=str_length(f))))+2]
}

