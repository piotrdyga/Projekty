if(!require(openxlsx)) install.packages("openxlsx")
if(!require("xts")) install.packages("xts")
if(!require("dygraphs")) install.packages("dygraphs")
if(!require("dplyr")) install.packages("dplyr")
if(!require("forecast")) install.packages("forecast")
if(!require("tseries")) install.packages("forecast")
library(tseries)
library(dplyr)
library(openxlsx)
library("xts")
library(dygraphs)
library(forecast)

rm(list = ls())


setwd("C:/Users/Mariusz/OneDrive/Pulpit/Studia6/Szeregi czasowe/Projekt")

niesezonowe <- read.xlsx("C:/Users/Mariusz/OneDrive/Pulpit/Studia6/Szeregi czasowe/Projekt/Piotr_Dyga_niesezonowe.xlsx")

niesezonowe$Date <- as.Date(paste(niesezonowe$Year, niesezonowe$Month, niesezonowe$DAY, sep = "-"))

niesezonowe <- select(niesezonowe, Date, Value)
head(niesezonowe)
tail(niesezonowe)
niesezonowe.ts <- ts(data = niesezonowe$Value, frequency = 12, 
                 start = c(1995, 5))
niesezonowe.ts.train <-
  window(niesezonowe.ts, end = c(2023, 12))

niesezonowe.ts.test <-
  window(niesezonowe.ts, start = c(2024,1))

niesezonowe.HW <- HoltWinters(niesezonowe.ts.train,
                              gamma = FALSE)
plot(niesezonowe.HW)

niesezonowe.HW

niesezonowe.HW.forecast <- predict(niesezonowe.HW,
                                        n.ahead = 3,
                                        prediction.interval = TRUE)

plot(window(niesezonowe.ts, start = c(2022,12)))
lines(niesezonowe.HW.forecast[, 1], col = "blue") # prognoza
lines(niesezonowe.HW.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(niesezonowe.HW.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2024, lty = 2) # dodajemy pionową linię referencyjną

# Wyodrębnienie wartości wygładzonych (in-sample)
fitted_values <- niesezonowe.HW$fitted[,1]
# Wyodrębnienie prognoz (out-of-sample)
forecast_values <- niesezonowe.HW.forecast[,1]
# Połączenie wartości in-sample i out-of-sample w jedną serię czasową
combined_series <- ts(c(fitted_values, forecast_values), 
                      start = start(fitted_values), 
                      frequency = frequency(niesezonowe.ts))

combined_series <- ts.union(niesezonowe.ts, combined_series)

colnames(combined_series) <- c("Values", "Holt")

combined_series = as.xts(combined_series)

index(combined_series)

library(zoo)

# Sprawdzenie dat w combined_series
dates <- as.yearmon(index(combined_series))
dates
# Przypisanie 1 dla okresu od stycznia 2024
sample_period <- ifelse(dates >= as.yearmon("2024-01"), 1, 0)

combined_series$sample_period <- sample_period

combined_series$mae <- abs(combined_series$Holt-combined_series$Values)
combined_series$mse <- (combined_series$Holt-combined_series$Values)^2
combined_series$mape <- abs((combined_series$Holt-combined_series$Values)/combined_series$Values)
combined_series$amape <- abs((combined_series$Holt-combined_series$Values)/(combined_series$Holt+combined_series$Values))

aggregate(combined_series[, 4:7],
          by = list(combined_series$sample_period),
          FUN = function(x) mean(x, na.rm = T))
##########################################
plot(niesezonowe.HW)

niesezonowe.ts.train <-
  window(niesezonowe.ts, end = c(2023, 12))

niesezonowe.ts.test <-
  window(niesezonowe.ts, start = c(2024,1))

niesezonowe.HW.add <- HoltWinters(niesezonowe.ts.train, seasonal = "additive")
niesezonowe.HW.add$alpha
niesezonowe.HW.add$beta

niesezonowe.HW.mult <-HoltWinters(niesezonowe.ts.train, seasonal = "multiplicative")
niesezonowe.HW.mult$alpha
niesezonowe.HW.mult$beta

plot(niesezonowe.HW.add)
plot(niesezonowe.HW.mult)

niesezonowe.HW.add.forecast <- predict(niesezonowe.HW.add,
                                       n.ahead = 3,
                                       prediction.interval = TRUE)

niesezonowe.HW.mult.forecast <- predict(niesezonowe.HW.mult,
                                       n.ahead = 3,
                                       prediction.interval = TRUE)

plot(window(niesezonowe.ts, start = c(2022,12)))
lines(niesezonowe.HW.add.forecast[, 1], col = "blue") # prognoza
lines(niesezonowe.HW.add.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(niesezonowe.HW.add.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2024, lty = 2) # dodajemy pionową linię referencyjną

plot(window(niesezonowe.ts, start = c(2022,12)))
lines(niesezonowe.HW.mult.forecast[, 1], col = "blue") # prognoza
lines(niesezonowe.HW.mult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(niesezonowe.HW.mult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2024, lty = 2) # dodajemy pionową linię referencyjną

length(niesezonowe.ts)
length(niesezonowe.HW.add$fitted[,1])
length(niesezonowe.HW.mult$fitted[,1])


length(niesezonowe.ts)

niesezonowe.summary <- window(niesezonowe.HW$fitted[,1], end = 347, extend = TRUE)
window(niesezonowe.summary, start = c(2024,1)) <- niesezonowe.HW.add.forecast[,1]
sample_period <- ts(if_else(index(niesezonowe.summary) < "2024.000",0,1))
names(niesezonowe.summary) <- c("Holt")







niesezonowe.summary$date <- index(niesezonowe.ts)

niesezonowe.summary$sample_period <- sample_period
niesezonowe.summary$Value <- niesezonowe.ts
head(niesezonowe.summary)

index(niesezonowe.summary)
podsumowanie <- data.frame(niesezonowe.HW.add$fitted[,1])
names(podsumowanie) <- c("Holt")
podsumowanie$Holt <- niesezonowe.HW.add$fitted[,1]
podsumowanie <- rbind(podsumowanie, data.frame(Holt = niesezonowe.HW.add.forecast[,1]))
# Konwersja dopasowanych wartości na data.frame
podsumowanie <- data.frame(Holt = as.numeric(niesezonowe.HW.add$fitted[,1]))

# Łączenie dopasowanych wartości z prognozowanymi
podsumowanie <- rbind(podsumowanie, data.frame(Holt = niesezonowe.HW.add.forecast[,1]))
# Liczba obserwacji
n <- nrow(podsumowanie)
# Stworzenie zmiennej sample_period z domyślną wartością 0
podsumowanie$sample_period <- 0

# Ustawienie wartości 1 dla trzech ostatnich obserwacji
podsumowanie$sample_period[(n-2):n] <- 1
# Usunięcie dwóch pierwszych obserwacji z niesezonowe.ts
niesezonowe.ts.trimmed <- niesezonowe.ts[-c(1, 2)]

podsumowanie$Value <- as.numeric(niesezonowe.ts.trimmed)
# Wyświetlenie wyników
print(podsumowanie)


# Wyświetlenie wyników
View(podsumowanie)

# Wyświetlenie wyników
View(podsumowanie)


library(xts)
niesezonowe.summary = as.xts(niesezonowe.summary)
colnames(niesezonowe.summary) <- "Value"
niesezonowe.summary$sample_period <- 0

#########################################
niesezonowe$lValue <- log(niesezonowe$Value)

niesezonowe <- select(niesezonowe, Date, Value, lValue)



# Znalezienie optymalnej wartości lambda
#lambda <- BoxCox.lambda(niesezonowe$Value)
#lambda
# Przeprowadzenie transformacji Boxa-Coxa z optymalną wartością lambda
#niesezonowe_transformed <- BoxCox(niesezonowe, lambda_optimal)

#niesezonowe.xts <- xts(niesezonowe[,1:3],
#                       niesezonowe$Date)
tail(niesezonowe.xts)

#niesezonowe_sample.xts <- window(niesezonowe.xts, end = as.Date("2023-12-31"))

#head(niesezonowe.xts)

niesezonowe = ts(data = niesezonowe$Value, frequency = 12, 
                 start = c(1995, 5), end = c(2023,12))

dniesezonowe <- diff(niesezonowe)

lniesezonowe <- log(niesezonowe)

dlniesezonowe <- diff(lniesezonowe)

lambda <- BoxCox.lambda(niesezonowe)
lambda

#Wykresy logarytmu i różnic logarytmu
par(mfrow = c(4, 1))
plot(lniesezonowe, type = "l", main = "l(S&P 500)")
plot(dlniesezonowe, type = "l", main = "dl(S&P 500)")
plot(niesezonowe, type = "l", main = "(S&P 500)")
plot(dniesezonowe, type = "l", main = "d(S&P 500)")
tsdisplay(lniesezonowe, lag.max = 24)
tsdisplay(dniesezonowe, lag.max = 24)

par(mfrow = c(2, 1))
plot(dniesezonowe, type = "l", main = "d(S&P 500)")
plot(dlniesezonowe, type = "l", main = "dl(S&P 500)")
#test autokorelacji
library(lmtest)
library(urca)
plot(lniesezonowe)
df.test.lniesezonowe <- ur.df(niesezonowe, type = c("none"), lags = 0)
summary(df.test.niesezonowe)

resid.lniesezonowe <- df.test.lniesezonowe@testreg$residuals
bgtest(resid.lniesezonowe~1, order = 1)

#testujemy autokorelacje
testdf(variable = niesezonowe, ADF_type = "c", ADF_max_order = 7, BG_max_order = 6)
#wyniki pokazały że trzeba dodać jedno opóźnienie żeby isterpretować wyniki te
testdf(variable = dniesezonowe, ADF_type = "c", ADF_max_order = 7, BG_max_order = 6)
#test kpss
kpss.test <- ur.kpss(dniesezonowe, type = c("mu"))
summary(kpss.test)
#test Ljung Box
Box.test(dniesezonowe, type = "Ljung-Box", lag = 24)
#test Box-Pears
Box.test(dniesezonowe, type = "Box-Pierce", lag = 24)

tsdisplay(dniesezonowe, lag.max = 24)

## ARIMA
arima111 <- arima(dniesezonowe,
             order = c(1,0,1))
coeftest(arima111)
arima110 <- arima(dniesezonowe,
                  order = c(1,0,0))
coeftest(arima110)
coeftest(arima011)
arima011 <- arima(dniesezonowe,
                  order = c(0,0,1))
arima010 <- arima(dniesezonowe,
                  order = c(0,0,0))
AIC(arima111, arima110, arima011, arima010)
BIC(arima111, arima110, arima011, arima010)
teststat <- teststat<- 2*(as.numeric(logLik(arima011))-as.numeric(logLik(arima010)))
teststat
pchisq(teststat, df = 1, lower.tail = FALSE)
#Na tej podstawie chce wybrać model 011
#testowanie białego szumu
par(mfrow = c(1, 2))
Acf(resid(arima010), lag.max = 24,
    ylim = c(-1, 1),
    xlim=c(1,24),
    lwd = 4, col = "red")
Pacf(resid(arima010), lag.max =24 ,
     lwd = 4, col = "red")

par(mfrow = c(1, 1))
### formalne testy
Box.test(resid(arima010), type = "Ljung-Box", lag = 24)
Box.test(resid(arima010), type = "Box-Pierce", lag = 24)


jarque.bera.test(residuals(arima010))

arima.best.AIC <- auto.arima(dniesezonowe,
                             d = 0,             # parameter d w modelu ARIMA
                             max.p = 4,         # p = maksymalna wartosc
                             max.q = 4,         # q = maksymalna wartosc
                             max.order = 8,     # suma p+q
                             start.p = 0,       # Wartosc startowa dla p
                             start.q = 0,       # Wartosc startowa dla q
                             ic = "aic",        # Wybor modelu na podstawie kryterium informcyjne
                             stepwise = FALSE,  # jezeli FALSE rozwaza wszystkie modeli
                             allowdrift = TRUE, # model zawiera stalą
                             trace = TRUE)      # wyswietlenie rozwazonych modeli


coeftest(arima.best.AIC)

