library(ggplot2)
library(cowplot)
#library(graphics)
#library(xlsx)
library(openxlsx)
library(qqplotr)
library(scales)
library(grid)
library(anytime)
library(lubridate) 
library(tseries) 
library(timeSeries)
library(forecast)
library(tictoc)
library(xts)
library(quantmod)
library(rugarch)
library(readxl)
library(nortest)
library(PerformanceAnalytics)
library(quantmod)
library(rugarch)
library(car)
library(FinTS)
library(xlsx)

options(digits=4)
#-----------------Download data-------------------------------------------------------###

getSymbols("BZ=F", from="2015-01-01", to=Sys.Date(),auto.assign = TRUE)
head(ES=F)

getSymbols("BZ",from="2015-11-02",to="2020-04-10",src="yahoo")## brent
head(BR)
end(BR)
plot(BR)
wri
writexl::write_xlsx(BR2, "SILVER22.xlsx")
write.csv(SILVER22, file = "silver.csv")

writexl::write_xlsx(BR2, "bren.xlsx")
write.csv(BR2, file = "bren.csv")
library(rio)
x <- import("BR.csv")
convert("SILVER22.xlsx", "Silver.csv")
convert("covidData.xlsx", "covid.csv")
convert("BRENT.xlsx", "brent2020.csv")
convert("WHEAT.xlsx", "wheat2020.csv")
convert("WTI.xlsx", "wti2020.csv")
  convert("BR2.df", "brent.csv")

getSymbols("BZ=F",from="2015-11-02",to="2020-21-10",src="yahoo")## crude oil
head(CL)
end(CL)
plot(CL)
CL1<-as.data.frame(CL)
writexl::write_xlsx(CL1, "crude_oil.xlsx")

getSymbols("ZG",from="2015-11-02",to="2020-04-10",src="yahoo")## Gold
head(ZG)
end(ZG)
plot(ZG)
ZG1<-as.data.frame(ZG)
writexl::write_xlsx(ZG1, "Gold.xlsx")

wri
getSymbols("BZ",from="2015-11-02",to="2020-04-10",src="yahoo")## Gold
head(GC)
end(GC)
plot(GC)
GC1<-as.data.frame(GC)
writexl::write_xlsx(GC1, "Gold.xlsx")

getSymbols("HG",from="2015-11-02",to="2020-04-10",src="yahoo")## Copper
head(HG)
end(HG)
plot(HG)
HG1<-as.data.frame(HG)
writexl::write_xlsx(franceDta, "Dta1.xlsx")

# extract adjusted closing prices
IBM = IBM[, "IBM.Adjusted", drop=F]
head(IBM)
BR1 <- as.data.frame(BR)
dat <- row.names(BR1)
BR2 <- cbind(dat,BR1)
View(BR2)

# plot prices
plot(IBM)
##################################################################################################################
#################################################################################################################
#                                                COVID19 Analysis                                                 #
##################################################################################################################
##################################################################################################################
plot(covid_anal$wti_price,covid_anal$cov19_fran_cases)
abline(lm(covid_anal$wti_price~covid_anal$cov19_fran_cases))

# correlation matrix
library(GGally)
library(ggrepel)
library(ggpubr)
ggcorr(covid_anal,
       label = TRUE)
label_alpha = TRUE
#----------Cameroon covid impact
p1 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date, y = cov19_camer_cases), col= 2, size= 1)+
  theme_classic()

p2 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date , y = brent_price),col= 3, size= 1)+
  theme_classic()

p3 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date, y = cov19_camer_cases), col= 2, size= 1)+
  theme_classic()

p4 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date , y = wti_price),col= 3, size= 1)+
  theme_classic()

p5 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date, y = cov19_camer_cases), col= 2, size= 1)+
  theme_classic()

p6 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date , y = silver_price),col= 3, size= 1)+
  theme_classic()

p<- ggpubr::ggarrange(p1, p2,p3,p4,p5,p6 ,ncol= 2,nrow=3)

print(p)


##----------------------------------------France covid impact-----------------------------------------------##

t1 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date, y = cov19_fran_cases), col= 2, size= 1)+
  theme_classic()

t2 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date , y = brent_price),col= 3, size= 1)+
  theme_classic()

t3 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date, y = cov19_fran_cases), col= 2, size= 1)+
  theme_classic()

t4 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date , y = wti_price),col= 3, size= 1)+
  theme_classic()

t5 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date, y = cov19_fran_cases), col= 2, size= 1)+
  theme_classic()

t6 <- ggplot(data = covid_anal)+
  geom_line(aes(x = date , y = silver_price),col= 3, size= 1)+
  theme_classic()

t<- ggpubr::ggarrange(t1, t2,t3,t4,t5,t6 ,ncol= 2,nrow=3)

print(t)

#-----------------Granger Causality--------------------------------------------------------##
# granger causality
library(lmtest)
grangertest(silver_price ~ cov19_camer_cases, order = 1, data = covid_anal)
grangertest(wti_price ~ cov19_fran_cases, order = 1, data = covid_anal)
grangertest(wti_price ~ Camer_death, order = 1, data = covid_anal)
grangertest(wti_price ~ Fran_death, order = 1, data = covid_anal)
