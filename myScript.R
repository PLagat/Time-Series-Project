#TIME SERIES ANALYSIS
#RELATIONSHIP BETWEEN GDP GROWTH RATE AND UNEMPLOYMENT IN KENYA
#FROM YEAR 1991 TO 2019
?read_csv
problems(read_csv)
warnings()
.libPaths()

library(readr)
KENYA <- read_csv("D:/YOUTH UNEMPLOYMENT RATE.csv",show_col_types = FALSE)

View(KENYA)
head(KENYA)
tail(KENYA)
regression<-lm(KENYA$UnemploymentRate~KENYA$GDPgrowth)
summary(regression)
adf.test(residuals(regression))

library(data.table)
setnames(mydata,"UnemploymentRate","UnemploymentRate")

str(KENYA$DATE)
str(KENYA$UnemploymentRate)
str(KENYA$GDPgrowth)
X<- as.Date(KENYA$DATE)
head(X)
year <- as.numeric(format(X,'%Y'))
month <- as.numeric(format(X,'%m'))
day <- as.numeric(format(X,'%d'))
format(X, "%m/%d/%Y")
class(X)
months(X)
quarters(X)
class(KENYA$year)
mydata <- cbind(,year)

#VISUALIZING DATA
plot(KENYA$UnemploymentRate,type = "l",main= "Youth Unemployment Rate in Kenya",xlab = "Year",ylab = "Youth Unemployment Rate",lwd=2,col="blue", xlim=c(2005,2010))
par(mfrow=c(1,2))
plot(KENYA$GDPgrowth ,type = "l",main= "GDP Growth Rate in Kenya",xlab = "Year",ylab = "GDP Growth Rate",lwd=2, col=2, xlim=c(2005,2010))

KENYA$UnemploymentRate<- ts(data=KENYA$UnemploymentRate,frequency = 1, start=c(1991,1),end=c(2019,1))
unemprate <- window(KENYA$UnemploymentRate, start=c(1991,1),end=c(2019,1))
KENYA$GDPgrowth<- ts(data= KENYA$GDPgrowth, frequency=1, start=c(1991,1),end=c(2019,1))
GDPgrowth<- window(KENYA$GDPgrowth,start=c(1991,1),end=c(2019,1))
plot.ts(KENYA$GDPgrowth,KENYA$UnemploymentRate)

plot.ts(KENYA$UnemploymentRate)
plot.ts(KENYA$GDPgrowth)
unempGDP = cbind(KENYA$GDPgrowth,KENYA$UnemploymentRate)
class(unempGDP)
plot(unempGDP, main="Multiple time series plot",col=c("blue"), lty=1:2)
#plot(unempGDP.ts, plot.type="single", main="ANNUAL PLOT ON GDP AND UNEMPLOYMENT RATE", ylab="Adjusted close price", col=c("blue", "red"), lty=1:2) 
#legend(1995, 45, legend=c("Unemployment Rate","GDP"), col=c("blue", "red"),lty=1:2)

class(unemprate)
class(GDPgrowth)
start(unemprate)
frequency(unemprate)
end(unemprate)
time(GDPgrowth)




#CHECKING STATIONARITY
diffunemprate<- diff(KENYA$UnemploymentRate ,differences=2)
diffGDPgrowth<- diff(KENYA$GDPgrowth,differences = 1)
plot.ts(diffGDPgrowth, main="Differenced GDP growth rate", col=2, xlab="Year")
plot.ts(diffunemprate, main="Differenced Unemployment Rate", col=4, xlab="Year")
par(mfrow=c(1,2))

diffmodel <- lm(diffunemprate~diffGDPgrowth)
summary(diffmodel)

acf(KENYA$UnemploymentRate,main="Correlogram for Unemployment Rate in Kenya")
pacf(KENYA$UnemploymentRate,main="Partial Correlogram for Unemployment Rate in Kenya")
acf(diffunemprate, main="Correlogram for Unemployment Rate")
pacf(diffunemprate, main="Partial Correlogram for Unemployment Rate")



acf(KENYA$GDPgrowth,main="Correlogram for GDP Growth")
pacf(KENYA$GDPgrowth,main="Partial Correlogram for GDP Growth ")
acf(diffGDPgrowth,main="Correlogram for GDP Growth")
pacf(diffGDPgrowth,main="Partial Correlogram for GDP Growth")



library(tseries)  
library(zoo)

adf.test(KENYA$UnemploymentRate) #fail to reject, p=0.679 data is non-stationary
adf.test(diffunemprate) #reject null, diff=2, p-value=0.0199

adf.test(KENYA$GDPgrowth) # fail to reject null, p=0.05587, data is non-stationary
adf.test(diffGDPgrowth) #reject null, diff=1, p-value=0.01

Box.test(KENYA$UnemploymentRate, lag = 10, type = c("Ljung-Box"))
Box.test(diffunemprate, lag = 10, type = c("Ljung-Box"))
Box.test(KENYA$GDPgrowth, lag = 10, type = c("Ljung-Box"))
Box.test(diffGDPgrowth, lag = 10, type = c("Ljung-Box"))

install.packages("forecast")
library(forecast)
model1<-auto.arima(KENYA$UnemploymentRate)
summary(model1)
model2<- auto.arima(KENYA$GDPgrowth)
summary(model2)

library(lmtest)
coeftest(model2)

#checking for serial correlation
library(car)

summary(regression)
durbinWatsonTest(regression)
#p-value= 0, DW=0.927313 hence reject null, there is positive serial correlation of order 1

Box.test(resid(regression),lag= 12,type = c("Ljung-Box")) 
#reject null, we have joint significance upto lag 1

#correctting for serial correlation
lag_GDPgrowth<-lag(KENYA$GDPgrowth,k=1)
lag_unemprate<-lag(KENYA$UnemploymentRate,k=1)
regression2<- lm(KENYA$UnemploymentRate~lag_unemprate+KENYA$GDPgrowth)         
summary(regression2)

durbinWatsonTest(regression2)
#fail to reject null, p=0.798, DW=2.559977 there is non-autocorrelation of order 1
Box.test(resid(regression2),lag= 1,type = c("Ljung-Box")) 

install.packages("prais")
library(prais)
pw <- prais_winsten(KENYA$UnemploymentRate~KENYA$GDPgrowth, data=KENYA)
summary(pw)

Box.test(resid(pw),lag=10,type=c("Ljung-Box"))

library(sandwich)
newey <- NeweyWest(lm(KENYA$UnemploymentRate~KENYA$GDPgrowth),lag=2)
summary(newey)

library(lmtest)
coeftest(regression,vcov=newey)




#RUNNING VAR MODEL
Data<- window(ts.union(KENYA$UnemploymentRate,KENYA$GDPgrowth),start = c(1991,1), end = c(2019, 1), frequency = 1)
View(Data)
install.packages("vars")
library(vars)

VARselect(y=Data,lag.max=8, type = "const")       
##VARSelect allows you to select the optimal number of lags


var<- VAR(y=Data, p=1, type = c("const"), ic = "SC")
summary(var)

causality(var,cause="KENYA$UnemploymentRate",vcov=vcovHC(var))

###GRANGER CAUSALITY TEST -- USE ZOO PACKAGE
install.packages("zoo")
library(zoo)
grangertest(KENYA$UnemploymentRate~KENYA$GDPgrowth,order=1)

grangertest(KENYA$GDPgrowth~KENYA$UnemploymentRate,order=1)


#IMPULSE RESPONSE FUNCTIONS
# Calculate the IRF
ir1 <- irf(var, impulse = "KENYA.UnemploymentRate", response = "KENYA.GDPgrowth", n.ahead = 10)
# Plot the IRF
plot(ir1)
ir1 <- irf()


# Calculate the IRF
ir2 <- irf(var, impulse = "KENYA.GDPgrowth", response = "KENYA.UnemploymentRate", n.ahead = 20)
# Plot the IRF
plot(ir2)



# Calculate the variance decomposition
FEVD1 <- fevd(var, n.ahead = 10)
FEVD1
