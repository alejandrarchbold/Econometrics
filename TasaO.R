rm(list = ls())

library(plyr)
library(readxl)
library(RcppRoll)
library(reshape2)
library(tidyverse)
library(lubridate)
library(forecast)
library(polynom)
library(urca)
library(Rssa)
library(lmtest)
library(tseries)
library(seasonal)

### Datos

info <- read_excel("~/TO.xlsx")
info$mes1 <- ymd(info$mes1)

### Gráficos

# Gráfico General

info %>% ggplot(aes(x = mes1, y = ocup)) + geom_line(size = 0.5, color = "#A52A2A") +
  labs(title = "Tasa de Ocupación", 
       subtitle = "Total Nacional",
       x = "Tiempo",
       y = "Porcentaje  (%)", 
       caption = "Fuente: Dane") +
  theme_bw() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %y")

# Gráfico cambio mensual

info %>% mutate(o_ocup = ocup - lag(ocup)) %>% ggplot(aes(x = mes1, y = o_ocup)) + geom_line(size = 1, color = "blue") +
  labs(title = "Cambio mensual en la tasa de ocupación", 
       subtitle = "Total Nacional",
       x = "Tiempo",
       y = "Porcentaje  (%)", 
       caption = "Fuente: Dane") +
  theme_bw() +
  scale_x_date(date_breaks = "12 months", date_labels = "%b %y")

# Gráfico ACF y PACF

ggtsdisplay(ts(info$ocup, start = c(2001,1), frequency = 12), main = "Tasa de ocupación")

# diferenciado cambio mensual

ggtsdisplay(diff(ts(info$ocup, start = c(2001,1), frequency = 12)), main = "Cambio mensual en la tasa de ocupación")


timeseries <- ts(info$ocup, frequency = 12)
plot(timeseries)
diffts <- diff(timeseries)
plot(diffts)
difftscomponent <- stats::decompose(diffts)
adjusted_diffts <- diffts - difftscomponent$seasonal
acf(adjusted_diffts)
pacf(adjusted_diffts)

ggtsdisplay(timeseries, main = "Tasa de Ocupación")
ggtsdisplay(diffts, main = "Cambio mensual de la Tasa de Ocupación")
ggtsdisplay(adjusted_diffts, main = "Cambio de la tasa de ocupación sin el componente estacional")


summary(m1 <- Arima(adjusted_diffts, order = c(1,1,0), include.mean = F))# AR(1))
coeftest(m1)

f1 <- forecast(m1, h = 10)
plot(f1)



summary(ur.df(timeseries, type = "trend", lags = 25, selectlags = "BIC"))
summary(ur.df(timeseries, type = "drift", lags = 25, selectlags = "BIC"))
summary(ur.df(timeseries, type = "none", lags = 25, selectlags = "BIC"))


summary(ur.df(adjusted_diffts, type = "trend", lags = 25, selectlags = "BIC"))
summary(ur.df(adjusted_diffts, type = "drift", lags = 25, selectlags = "BIC"))
summary(ur.df(adjusted_diffts, type = "none", lags = 25, selectlags = "BIC"))

ts(info$ocup, start =  c(2001,1), frequency = 12) %>% stats::decompose(type="multiplicative") %>% 
  autoplot() + 
  labs(title = "Tasa de Ocupación", 
       subtitle = "Descomposición",
       x = "Tiempo",
       y = " ") + 
  theme_bw()


