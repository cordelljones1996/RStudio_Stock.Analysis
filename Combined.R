rm(list = ls())
# Stock Generalized AutoRegressive Conditional Heteroscedasticity
# Libraries
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(rugarch)

#DIA
# DIA daily
getSymbols("DIA",
           from = "2005-01-01",
           to = "2020-07-02")
chartSeries(DIA["2019-12"])
chartSeries(DIA)

# Daily Returns
return.dia <- CalculateReturns(DIA$DIA.Close)
return.dia <- return.dia[-1]
hist(return.dia)
chart.Histogram(return.dia,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return.dia, theme = 'white')

# Annualized volatility 
sd(return.dia)
sqrt(252) * sd(return.dia["2019"])
chart.RollingPerformance(R = return.dia["2008::2020"],
                         width = 52,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Dow Industiral Average's Monthly Rolling Volatility")

#1 .sGARCH Model With Constant Mean
s.dia <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m.dia <- ugarchfit(data = return.dia, spec = s.dia)
plot(m.dia, which = 'all')
f.dia <- ugarchforecast(fitORspec = m.dia,
                    n.ahead = 20)

# Plot to see if volatility will dec/inc
plot(fitted(f.dia))
plot(sigma(f.dia))

# Application example - portoflio allocation 
v.dia <- sqrt(252) * sigma(m.dia)
# w shows the rate that we can increase in riskier assets
# The percentage being divided by v is the volatility percentage 
# that we aim to shoot for.
w.dia <- 0.05/v.dia
plot(merge(v.dia, w.dia),
     multi.panel = T)
# To calculate the amount of riskier assets to allocate -> tail(w) then take 
# the last decimal and multiply it by the portfolio total

# 2. GARCH with sstd
s.dia <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m.dia <- ugarchfit(data = return.dia, spec = s.dia)
plot(m.dia, which = 'all')

# 3. GJR-GARCH
s.dia <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.dia <- ugarchfit(data = return.dia, spec = s.dia)
plot(m.dia, which = 'all')

# 4. AR(1) JR-GARCH
s.dia <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.dia <- ugarchfit(data = return.dia, spec = s.dia)
plot(m.dia, which = 'all')

# 5. GJR-GARCH in mean
s.dia <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm = T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.dia <- ugarchfit(data = return.dia, spec = s.dia)
plot(m.dia, which = 'all')

# Model 3 showed to have the lowest information criteria 
# Simulation
s.dia <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.dia <- ugarchfit(data = return.dia, spec = s.dia)
plot(m.dia, which = 'all')
sfinal.dia <- s.dia
setfixed(sfinal.dia) <- as.list(coef(m.dia))

f2008.dia <- ugarchforecast(data = return.dia["/2008-12"],
                        fitORspec = sfinal.dia,
                        n.ahead = 252)
f2019.dia <- ugarchforecast(data = return.dia["/2019-12"],
                        fitORspec = sfinal.dia,
                        n.ahead = 252)
par(mfrow = c(1,1))
plot(sigma(f2008.dia))
plot(sigma(f2019.dia))

sim.dia <- ugarchpath(spec = sfinal.dia,
                  m.sim = 3,
                  n.sim = 1*252,
                  rseed = 123)
plot.zoo(fitted(sim.dia))
plot.zoo(sigma(sim.dia))
p.dia <- 3575300*apply(fitted(sim.dia), 2, 'cumsum') + 3575300
matplot(p.dia, type = "l", lwd = 3)

#AAPL
# AAPL daily
getSymbols("AAPL",
           from = "2005-01-01",
           to = "2020-07-02")
chartSeries(AAPL["2019-12"])
chartSeries(AAPL)

# Daily Returns
return.aapl <- CalculateReturns(AAPL$AAPL.Close)
return.aapl <- return.aapl[-1]
hist(return.aapl)
chart.Histogram(return.aapl,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return.aapl, theme = 'white')

# Annualized volatility 
sd(return.aapl)
sqrt(252) * sd(return.aapl["2019"])
chart.RollingPerformance(R = return.aapl["2008::2020"],
                         width = 52,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Apple Inc. Monthly Rolling Volatility")

#1 .sGARCH Model With Constant Mean
s.aapl <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m.aapl <- ugarchfit(data = return.aapl, spec = s.aapl)
plot(m.aapl, which = 'all')
f.aapl <- ugarchforecast(fitORspec = m.aapl,
                    n.ahead = 20)

# Plot to see if volatility will dec/inc
plot(fitted(f.aapl))
plot(sigma(f.aapl))

# Application example - portfolio allocation 
v.aapl <- sqrt(252) * sigma(m.aapl)
# w shows the rate that we can increase in riskier assets
# The percentage being divided by v is the volatility percentage 
# that we aim to shoot for.
w.aapl <- 0.05/v.aapl
plot(merge(v.aapl, w.aapl),
     multi.panel = T)
# To calculate the amount of riskier assets to allocate -> tail(w) then take 
# the last decimal and multiply it by the portfolio total

# 2. GARCH with sstd
s.aapl <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m.aapl <- ugarchfit(data = return.aapl, spec = s.aapl)
plot(m.aapl, which = 'all')

# 3. GJR-GARCH
s.aapl <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.aapl <- ugarchfit(data = return.aapl, spec = s.aapl)
plot(m.aapl, which = 'all')

# 4. AR(1) JR-GARCH
s.aapl <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.aapl <- ugarchfit(data = return.aapl, spec = s.aapl)
plot(m.aapl, which = 'all')

# 5. GJR-GARCH in mean
s.aapl <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm = T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.aapl <- ugarchfit(data = return.aapl, spec = s.aapl)
plot(m.aapl, which = 'all')

# Model 3 showed to have the lowest information criteria 
# Simulation
s.aapl <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.aapl <- ugarchfit(data = return.aapl, spec = s.aapl)
plot(m.aapl, which = 'all')
sfinal.aapl <- s.aapl
setfixed(sfinal.aapl) <- as.list(coef(m.aapl))

f2008.aapl <- ugarchforecast(data = return.aapl["/2008-12"],
                        fitORspec = sfinal.aapl,
                        n.ahead = 252)
f2019.aapl <- ugarchforecast(data = return.aapl["/2019-12"],
                        fitORspec = sfinal.aapl,
                        n.ahead = 252)
par(mfrow = c(1,1))
plot(sigma(f2008.aapl))
plot(sigma(f2019.aapl))

sim.aapl <- ugarchpath(spec = sfinal.aapl,
                  m.sim = 3,
                  n.sim = 1*252,
                  rseed = 123)
plot.zoo(fitted(sim.aapl))
plot.zoo(sigma(sim.aapl))
p.aapl <- 364.11*apply(fitted(sim.aapl), 2, 'cumsum') + 364.11
matplot(p.aapl, type = "l", lwd = 3)

#AMZN
# AMZN daily
getSymbols("AMZN",
           from = "2005-01-01",
           to = "2020-07-02")
chartSeries(AMZN["2019-12"])
chartSeries(AMZN)

# Daily Returns
return.amzn <- CalculateReturns(AMZN$AMZN.Close)
return.amzn <- return.amzn[-1]
hist(return.amzn)
chart.Histogram(return.amzn,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return.amzn, theme = 'white')

# Annualized volatility 
sd(return.amzn)
sqrt(252) * sd(return.amzn["2019"])
chart.RollingPerformance(R = return.amzn["2008::2020"],
                         width = 52,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Amazon Inc. Monthly Rolling Volatility")

#1 .sGARCH Model With Constant Mean
s.amzn <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m.amzn <- ugarchfit(data = return.amzn, spec = s.amzn)
plot(m.amzn, which = 'all')
f.amzn <- ugarchforecast(fitORspec = m.amzn,
                    n.ahead = 20)

# Plot to see if volatility will dec/inc
plot(fitted(f.amzn))
plot(sigma(f.amzn))

# Application example - portoflio allocation 
v.amzn <- sqrt(252) * sigma(m.amzn)
# w shows the rate that we can increase in riskier assets
# The percentage being divided by v is the volatility percentage 
# that we aim to shoot for.
w.amzn <- 0.05/v.amzn
plot(merge(v.amzn, w.amzn),
     multi.panel = T)
# To calculate the amount of riskier assets to allocate -> tail(w) then take 
# the last decimal and multiply it by the portfolio total

# 2. GARCH with sstd
s.amzn <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m.amzn <- ugarchfit(data = return.amzn, spec = s.amzn)
plot(m.amzn, which = 'all')

# 3. GJR-GARCH
s.amzn <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.amzn <- ugarchfit(data = return.amzn, spec = s.amzn)
plot(m.amzn, which = 'all')

# 4. AR(1) JR-GARCH
s.amzn <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.amzn <- ugarchfit(data = return.amzn, spec = s.amzn)
plot(m.amzn, which = 'all')

# 5. GJR-GARCH in mean
s.amzn <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm = T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.amzn <- ugarchfit(data = return.amzn, spec = s.amzn)
plot(m.amzn, which = 'all')

# Model 3 showed to have the lowest information criteria 
# Simulation
s.amzn <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.amzn <- ugarchfit(data = return.amzn, spec = s.amzn)
plot(m.amzn, which = 'all')
sfinal.amzn <- s.amzn
setfixed(sfinal.amzn) <- as.list(coef(m.amzn))

f2008.amzn <- ugarchforecast(data = return.amzn["/2008-12"],
                        fitORspec = sfinal.amzn,
                        n.ahead = 252)
f2019.amzn <- ugarchforecast(data = return.amzn["/2019-12"],
                        fitORspec = sfinal.amzn,
                        n.ahead = 252)
par(mfrow = c(1,1))
plot(sigma(f2008.amzn))
plot(sigma(f2019.amzn))

sim.amzn <- ugarchpath(spec = sfinal.amzn,
                  m.sim = 3,
                  n.sim = 1*252,
                  rseed = 123)
plot.zoo(fitted(sim.amzn))
plot.zoo(sigma(sim.amzn))
p.amzn <- 2878.70*apply(fitted(sim.amzn), 2, 'cumsum') + 2878.70
matplot(p.amzn, type = "l", lwd = 3)

#TSLA
# TSLA daily
getSymbols("TSLA",
           from = "2005-01-01",
           to = "2020-07-02")
chartSeries(TSLA["2019-12"])
chartSeries(TSLA)

# Daily Returns
return.tsla <- CalculateReturns(TSLA$TSLA.Close)
return.tsla <- return.tsla[-1]
hist(return.tsla)
chart.Histogram(return.tsla,
                methods = c('add.density', 'add.normal'),
                colorset = c('blue', 'green', 'red'))
chartSeries(return.tsla, theme = 'white')

# Annualized volatility 
sd(return.tsla)
sqrt(252) * sd(return.tsla["2019"])
chart.RollingPerformance(R = return.tsla["2008::2020"],
                         width = 52,
                         FUN = "sd.annualized",
                         scale = 252,
                         main = "Tesla Inc. Monthly Rolling Volatility")

#1 .sGARCH Model With Constant Mean
s.tsla <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'norm')
m.tsla <- ugarchfit(data = return.tsla, spec = s.tsla)
plot(m.tsla, which = 'all')
f.tsla <- ugarchforecast(fitORspec = m.tsla,
                    n.ahead = 20)

# Plot to see if volatility will dec/inc
plot(fitted(f.tsla))
plot(sigma(f.tsla))

# Application example - portoflio allocation 
v.tsla <- sqrt(252) * sigma(m.tsla)
# w shows the rate that we can increase in riskier assets
# The percentage being divided by v is the volatility percentage 
# that we aim to shoot for.
w.tsla <- 0.05/v.tsla
plot(merge(v.tsla, w.tsla),
     multi.panel = T)
# To calculate the amount of riskier assets to allocate -> tail(w) then take 
# the last decimal and multiply it by the portfolio total

# 2. GARCH with sstd
s.tsla <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "sGARCH"),
                distribution.model = 'sstd')
m.tsla <- ugarchfit(data = return.tsla, spec = s.tsla)
plot(m.tsla, which = 'all')

# 3. GJR-GARCH
s.tsla <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.tsla <- ugarchfit(data = return.tsla, spec = s.tsla)
plot(m.tsla, which = 'all')

# 4. AR(1) JR-GARCH
s.tsla <- ugarchspec(mean.model = list(armaOrder = c(1,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.tsla <- ugarchfit(data = return.tsla, spec = s.tsla)
plot(m.tsla, which = 'all')

# 5. GJR-GARCH in mean
s.tsla <- ugarchspec(mean.model = list(armaOrder = c(0,0),
                                  archm = T,
                                  archpow = 2),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.tsla <- ugarchfit(data = return.tsla, spec = s.tsla)
plot(m.tsla, which = 'all')

# Model 3 showed to have the lowest information criteria 
# Simulation
s.tsla <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                variance.model = list(model = "gjrGARCH"),
                distribution.model = 'sstd')
m.tsla <- ugarchfit(data = return.tsla, spec = s.tsla)
plot(m.tsla, which = 'all')
sfinal.tsla <- s.tsla
setfixed(sfinal.tsla) <- as.list(coef(m.tsla))

f2010.tsla <- ugarchforecast(data = return.tsla["/2010-12"],
                        fitORspec = sfinal.tsla,
                        n.ahead = 252)
f2019.tsla <- ugarchforecast(data = return.tsla["/2019-12"],
                        fitORspec = sfinal.tsla,
                        n.ahead = 252)
par(mfrow = c(1,1))
plot(sigma(f2010.tsla))
plot(sigma(f2019.tsla))

sim.tsla <- ugarchpath(spec = sfinal.tsla,
                  m.sim = 3,
                  n.sim = 1*252,
                  rseed = 123)
plot.zoo(fitted(sim.tsla))
plot.zoo(sigma(sim.tsla))
p.tsla <- 1119.63*apply(fitted(sim.tsla), 2, 'cumsum') + 1119.63
matplot(p.tsla, type = "l", lwd = 3)
