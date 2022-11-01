x <- list()
k <- seq(1:length(bst_xau))

for(i in 1:length(bst_xau)){
asels_tmp <- AKSUE_XAU$Price[index(get(bst_xau[i]))]
bst_xau_tmp <- get(bst_xau[i])$Price[index(AKSUE_XAU)]
tt1 <- coredata(asels_tmp["2002/"])/bst_xau_tmp["2002/"]
df <- data.frame(Date=index(tt1),Price=as.numeric(coredata(tt1)))
g <- ggplot(df,aes(x=Date,y=Price))+geom_line()+ggtitle(str_c("AKSUE_XAU_",bst_xau[i]))
assign(str_c("g",as.character(k[i])),g)
x[[i]] <- get(str_c("g",as.character(k[i])))
}

res <- marrangeGrob(x, nrow = 2, ncol = 1)
# Export to a pdf file
ggexport(res, filename = "asels_frac.pdf")