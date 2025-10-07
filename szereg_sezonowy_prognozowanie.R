if(!require(openxlsx)) install.packages("openxlsx")
if(!require("xts")) install.packages("xts")
if(!require("dygraphs")) install.packages("dygraphs")
if(!require("dplyr")) install.packages("dplyr")
if(!require("forecast")) install.packages("forecast")
if(!require("tseries")) install.packages("tseries")
if(!require("lmtest")) install.packages("lmtest")
if(!require("urca")) install.packages("urca")

library(urca)
library(tseries)
library(dplyr)
library(openxlsx)
library("xts")
library(dygraphs)
library(forecast)
library(lmtest)
dev.off()  # Zamyka aktualne urządzenie graficzne i resetuje ustawienia
source("funs/testdf.R")
rm(list = ls())
setwd("C:/Users/Mariusz/OneDrive/Pulpit/Studia6/Szeregi czasowe/Projekt")
pkb <- read.xlsx("C:/Users/Mariusz/OneDrive/Pulpit/Studia6/Szeregi czasowe/Projekt/sezonowe.xlsx")

pkb$Date <- as.Date(paste(pkb$Year,
                          pkb$Month, pkb$Day, sep = "-"))

pkb <- select(pkb, Date, Value)

pkb.ts <- ts(data = pkb$Value, frequency = 4, 
                     start = c(1995, 1))

pkb.xts <- as.xts(pkb.ts)


plot(pkb.ts)
names(pkb.xts)[1] <- "pkb"
pkb_sample.xts <- window(pkb.xts, end = c(2022,4))
tail(pkb_sample.xts)
plot(pkb_sample.xts)
pkb_sample.xts$lpkb <- log(pkb_sample.xts$pkb)
plot(pkb_sample.xts$lpkb)
#Obejrzyjmy korelogramy
par(mar = rep(2, 4)) # Set the margin on all sides to 2

par(mfrow = c(2, 1))

acf(pkb_sample.xts$lpkb,  lag.max = 12,
    xlim=c(1,12),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(pkb_sample.xts$lpkb, lag.max=12,
     xlim=c(1,12),
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))
tsdisplay(pkb_sample.xts$dlpkb)

pkb_sample.xts$dlpkb <- diff.xts(pkb_sample.xts$lpkb, lag = 1)
plot(pkb_sample.xts$dlpkb)

#korelogramy różnic regularnych
par(mfrow = c(2, 1))

acf(pkb_sample.xts$dlpkb,  lag.max = 12,
    xlim=c(1,12),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(pkb_sample.xts$dlpkb, lag.max=12,
     xlim=c(1,12),
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))
tsdisplay(pkb_sample.xts$dlpkb)
#######różnice sezonowe
pkb_sample.xts$d4lpkb <- diff.xts(pkb_sample.xts$lpkb, lag = 4)
par(mfrow = c(2, 1))

acf(pkb_sample.xts$d4lpkb,  lag.max = 12,
    xlim=c(1,12),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(pkb_sample.xts$d4lpkb, lag.max=12,
     xlim=c(1,12),
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1)
tsdisplay(pkb_sample.xts$d4dlpkb)
plot(pkb_sample.xts$d4dlpkb, main = "")
pkb_sample.xts$d4dlpkb <- diff.xts(pkb_sample.xts$dlpkb, lag = 4)
##################################
pkb_sample.xts$lag4lpkb <- lag.xts(pkb_sample.xts$lpkb, k = 4)
plot(pkb_sample.xts$d4lpkb)
###### Formalne testowanie #######
testdf(variable = pkb_sample.xts$d4dlpkb ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 3)
testdf(variable = pkb_sample.xts$dlpkb ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 3)
testdf(variable = pkb_sample.xts$d4lpkb ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 3)


####KPSS
kpss.test <- ur.kpss(pkb_sample.xts$d4dlpkb, type = c("mu"))
summary(kpss.test)
kpss.test <- ur.kpss(pkb_sample.xts$dlpkb, type = c("mu"))
summary(kpss.test)


model1=lm(dlpkb~0+lag4lpkb, data=pkb_sample.xts)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1
summary(model1)
adf.test(residuals(model1))
pkb_sample.xts$lag4dlpkb <- lag.xts(pkb_sample.xts$d4lpkb, k=1)

model2=lm(dlpkb~0+lag4lpkb+lag4dlpkb, data=pkb_sample.xts)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1


pkb_sample.xts$lag4d2pkb <- lag.xts(pkb_sample.xts$d4lpkb, k=2)

model3=lm(dlpkb~0+lag4lpkb+lag4dlpkb+lag4d2pkb, data=pkb_sample.xts)
summary(model3)
bg1 <- bgtest(model3, order = 1)
bg1

pkb_sample.xts$lag4d3pkb <- lag.xts(pkb_sample.xts$d4lpkb, k=3)

model4=lm(dlpkb~0+lag4lpkb+lag4dlpkb+lag4d2pkb+lag4d3pkb, data=pkb_sample.xts)
summary(model4)
bg1 <- bgtest(model4, order = 1)
bg1

pkb_sample.xts$lag4d4pkb <- lag.xts(pkb_sample.xts$d4lpkb, k=4)

model5=lm(dlpkb~0+lag4lpkb+lag4dlpkb+lag4d2pkb+lag4d3pkb+lag4d4pkb, data=pkb_sample.xts)
summary(model5)
bg1 <- bgtest(model5, order = 1)
bg1

pkb_sample.xts$lag4d5pkb <- lag.xts(pkb_sample.xts$d4lpkb, k=5)

model6=lm(dlpkb~0+lag4lpkb+lag4dlpkb+lag4d2pkb+lag4d3pkb+lag4d4pkb+lag4d5pkb, data=pkb_sample.xts)
summary(model6)
bg1 <- bgtest(model6, order = 1)
bg1

pkb_sample.xts$lag4d6pkb <- lag.xts(pkb_sample.xts$d4lpkb, k=6)

model7=lm(dlpkb~0+lag4lpkb+lag4dlpkb+lag4d2pkb+lag4d3pkb+lag4d4pkb+lag4d5pkb+lag4d6pkb, data=pkb_sample.xts)
summary(model7)
bg1 <- bgtest(model7, order = 1)
bg1


######## szereg stacjonarny d=1, D = 1

Box.test(pkb_sample.xts$d4dlpkb, type = "Ljung-Box", lag = 12)

# Test Boxa-Pierce
Box.test(pkb_sample.xts$d4dlpkb, type = "Box-Pierce", lag = 12)


##### wypustki
par(mfrow = c(2, 1))
acf(pkb_sample.xts$d4dlpkb,  lag.max = 12, 
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(pkb_sample.xts$d4dlpkb, lag.max=12,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

tsdisplay(pkb_sample.xts$d4dlpkb)

###### identyfikuje P = 1, Q = 1
nobs <- length(pkb_sample.xts$lpkb)

#######ARIMA (0,1,0)(1,1,1)
arima010111 <- arima(pkb_sample.xts$lpkb,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # częstotliwość danych (4 dla danych kwartalnych)
                                     period = 4),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010111
coeftest(arima010111)



####### kolejna ARIMA (0,1,0)(1,1,0)

arima010110 <- arima(pkb_sample.xts$lpkb,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 0),
                                     # częstotliwość danych (4 dla danych kwartalnych)
                                     period = 4),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010110
coeftest(arima010110)

#########

par(mfrow = c(2, 1))
acf(resid(arima010110), lag.max = 12,
    ylim = c(-0.4, 0.4),xlim=c(2,12), lwd = 4, col = "red")
pacf(resid(arima010110), lag.max = 12,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))


#wydają się białym szumem

#### test ilorazu wiarygodności

teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010110)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )


############ ARIMA (0,1,0)(0,1,0)

arima010010 <- arima(pkb_sample.xts$lpkb,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 0),
                                     # częstotliwość danych (4 dla danych kwartalnych)
                                     period = 4),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010010
coeftest(arima010010)
######test LR

teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010010)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )

#########

AIC(arima010111, arima010110)
BIC(arima010111, arima010110)


par(mfrow = c(2, 1))
acf(resid(arima010110), lag.max = 12,
    ylim = c(-0.4, 0.4), xlim=c(2,12), lwd = 4, col = "red")
pacf(resid(arima010110), lag.max = 12,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

tsdisplay(resid(arima010110))


Box.test(resid(arima010110), type = "Ljung-Box", lag = 8)
Box.test(resid(arima010110), type = "Box-Pierce", lag = 8)
jarque.bera.test(resid(arima010110))
############ Prognoza

arima010110 <- arima(pkb_sample.xts$lpkb,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 0),
                                     # częstotliwość danych (4 dla danych kwartalnych)
                                     period = 4),
)

forecast <- predict(arima010110, n.ahead = 4)

str(forecast)

length(pkb.xts$lpkb)
pkb.xts$lpkb <- log(pkb.xts$pkb)


par(mfrow = c(1,2))
ts.plot(pkb.xts[, 2],
        main = "", xlim = c(26,31), ylim = c(12.8,13.4))

abline(v = 28.5, lty = 2, col = "black")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col = "red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col = "red", lty = 3)

##########
ts.plot(pkb.xts[, 2],
        main = "", xlim = c(15,31), ylim = c(12,13.4))

abline(v = 28.5, lty = 2, col = "black")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col = "red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col = "red", lty = 3)
par(mfrow = c(1,1))

########

values <- window(pkb.xts$lpkb, start =("2023-01-01"))

pkb_forecast <- data.frame(forecast = forecast$pred,
                           values)

pkb_forecast$mae <- abs(pkb_forecast$lpkb -
                          pkb_forecast$forecast)
pkb_forecast$mse <- (pkb_forecast$lpkb -
                       pkb_forecast$forecast)^2
pkb_forecast$mape <- abs((pkb_forecast$lpkb -
                            pkb_forecast$forecast) /
                           pkb_forecast$lpkb)
pkb_forecast$amape <- abs((pkb_forecast$lpkb -
                             pkb_forecast$forecast) /
                            (pkb_forecast$lpkb +
                               pkb_forecast$forecast))

colMeans(pkb_forecast[, 3:6])
options(scipen = 5)
round(colMeans(pkb_forecast[, 3:6]), 3)




################## HOLT WINTERS ##############



pkb.HW.add <- HoltWinters(pkb_sample.xts$lpkb,
                          seasonal = "additive")

pkb.HW.mult <- HoltWinters(pkb_sample.xts$lpkb,
                          seasonal = "multiplicative")

pkb.HW.add

pkb.HW.mult


pkb.HW.add.forecast <- predict(pkb.HW.add,
                                  n.ahead = 4,
                                  prediction.interval = TRUE)

pkb.HW.mult.forecast <- predict(pkb.HW.mult,
                               n.ahead = 4,
                               prediction.interval = TRUE)


pkb.ts <- as.ts(pkb.xts$lpkb)

plot(pkb.ts, main = "", xlim = c(26,31), ylim = c(12,13.7))
lines(pkb.HW.add.forecast[, 1], col = "blue") # prognoza
lines(pkb.HW.add.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(pkb.HW.add.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 28.5, lty = 2, col = "black") # dodajemy pionową linię referencyjną

plot(pkb.ts, main = "")
lines(pkb.HW.mult.forecast[, 1], col = "blue") # prognoza
lines(pkb.HW.mult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(pkb.HW.mult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 28.5, lty = 2, col = "black") # dodajemy pionową linię referencyjną


values <- window(pkb.xts$lpkb, start =("2023-01-01"))

pkb_forecast <- data.frame(forecast = pkb.HW.add.forecast[,1],
                           values)

pkb_forecast$mae <- abs(pkb_forecast$lpkb -
                          pkb_forecast$forecast)
pkb_forecast$mse <- (pkb_forecast$lpkb -
                       pkb_forecast$forecast)^2
pkb_forecast$mape <- abs((pkb_forecast$lpkb -
                            pkb_forecast$forecast) /
                           pkb_forecast$lpkb)
pkb_forecast$amape <- abs((pkb_forecast$lpkb -
                             pkb_forecast$forecast) /
                            (pkb_forecast$lpkb +
                               pkb_forecast$forecast))
colMeans(pkb_forecast[, 3:6])
options(scipen = 5)
round(colMeans(pkb_forecast[, 3:6]), 4)
