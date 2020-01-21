cfb<-read.csv("CFB2018complete (1).csv")
head(cfb)
tail(cfb)

plot(cfb$z_lysagarin,cfb$Zsagarin, xlab="last years performance", ylab="this years performance")


plot(cfb$Fravg, cfb$Zsagarin, xlab="average star rating of freshman", ylab="this years performance", main="freshman class and performance")

#lm(yaxis ~xaxis)-simple linear regression
lastyear<- lm(cfb$Zsagarin ~ cfb$z_lysagarin)
frclass<-lm(cfb$Zsagarin ~ cfb$Fravg)
#multiple linear regression
multiple<- lm(cfb$Zsagarin ~ cfb$z_lysagarin + cfb$Fravg)

summary(lastyear)
summary(frclass)
summary(multiple)
