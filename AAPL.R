rm(list = ls())
# Stock Generalized AutoRegressive Conditional Heteroscedasticity
# Libraries
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)

# AAPL daily
getSymbols("AAPL",
           from = "2005-01-01",
           to = "2020-07-02")
chartSeries(AAPL["2019-12"])
chartSeries(AAPL)

# Daily Returns
return <- CalculateReturns(AAPL$AAPL.Close)
return <- return[-1]
hist(return)
chart.Histogram(return,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return, theme = 'white')

# Annualized volatility 
sd(return)
sqrt(252) * sd(return["2019"])
chart.RollingPerformance(R = return["2008::2020"],
                         width = 52,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Apple Inc. Monthly Rolling Volatility")

#1 .sGARCH Model With Constant Mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m <- ugarchfit(data = return, spec = s)
plot(m, which = 'all')
f <- ugarchforecast(fitORspec = m,
                    n.ahead = 20)

# Plot to see if volatility will dec/inc
plot(fitted(f))
plot(sigma(f))

# Application example - portfolio allocation 
v <- sqrt(252) * sigma(m)
# w shows the rate that we can increase in riskier assets
# The percentage being divided by v is the volatility percentage 
# that we aim to shoot for.
w <- 0.05/v
plot(merge(v, w),
     multi.panel = T)
# To calculate the amount of riskier assets to allocate -> tail(w) then take 
# the last decimal and multiply it by the portfolio total

# 2. GARCH with sstd
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
plot(m, which = 'all')

# 3. GJR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
plot(m, which = 'all')

# 4. AR(1) JR-GARCH
s <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
plot(m, which = 'all')

# 5. GJR-GARCH in mean
s <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm = T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
plot(m, which = 'all')

# Simulation
s <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m <- ugarchfit(data = return, spec = s)
plot(m, which = 'all')
sfinal <- s
setfixed(sfinal) <- as.list(coef(m))

f2008 <- ugarchforecast(data = return["/2008-12"],
                        fitORspec = sfinal,
                        n.ahead = 252)
f2019 <- ugarchforecast(data = return["/2019-12"],
                        fitORspec = sfinal,
                        n.ahead = 252)
par(mfrow = c(1,1))
plot(sigma(f2008))
plot(sigma(f2019))

sim <- ugarchpath(spec = sfinal,
                  m.sim = 3,
                  n.sim = 1*252,
                  rseed = 123)
plot.zoo(fitted(sim))
plot.zoo(sigma(sim))
p <- 364.11*apply(fitted(sim), 2, 'cumsum') + 364.11
matplot(p, type = "l", lwd = 3)
