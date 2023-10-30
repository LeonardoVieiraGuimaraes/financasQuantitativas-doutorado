
# Gaussian white noite

set.seed(123)
y = rnorm(250)
ts.plot(y, main="Processo de ruido branco Gaussiano", xlab="Tempo", ylab="y(t)", col="blue", lwd=2)
abline(h=0)

plot(y, main="Gaussian White Noise Process", type="l", xlab="time", ylab="y(t)", col="blue", lwd=2)
abline(h=0)

# log returns Gaussian white noise
set.seed(123)
y = rnorm(60, mean=0.01, sd=0.05)
ts.plot(y,main="Processo GWN para log-retornos mensais", xlab="time",ylab="r(t)", col="blue", lwd=2, type="p") 
abline(h=c(0, -0.05, 0.05), lwd=2, lty=c("solid", "dotted", "dotted"), col=c("black", "red", "red"))

# Tendencia deterministica
set.seed(123)
e = rnorm(250)
y.dt = 0.1*seq(1,250) + e
ts.plot(y.dt, lwd=2, col="blue", main="Tendencia deterministica + ruido")
abline(a=0, b=0.1)

# Random walk
set.seed(321)
e = rnorm(250)
y.rw = cumsum(e)
ts.plot(y.rw, lwd=2, col="blue", main="Passeio aleatorio")
abline(h=0)

##################################
# MA(1) simulacao
set.seed(12345)
ma1.model = list(ma=0.9)
mu = 1
set.seed(123)
ma1.sim = mu + arima.sim(model=ma1.model, n=250)

# Para alterar a distribuicao de epsilon
ma1.sim = mu + arima.sim(model=ma1.model, n=250, innov=rnorm(250,mean=0,sd=0.1))

# FAC
ma1.acf = ARMAacf(ar=0, ma=0.9, lag.max=10)
ma1.acf

# 2 gráficos
par(mfrow=c(2,1))
ts.plot(ma1.sim, main="Processo MA(1): mu=1, theta=0.9", xlab="time", ylab="y(t)", col="blue", lwd=2)
abline(h=c(0,1))
plot(0:10, ma1.acf, type="h", col="blue", lwd=2, main="FAC para MA(1): theta=0.9", xlab="lag", ylab="FAC");
abline(h=0)
par(mfrow=c(1,1)
##################################


##################################
# AR(1) simulacao
ar1.model = list(ar=0.9)
mu = 1
set.seed(123)
ar1.sim = mu + arima.sim(model=ar1.model,n=250)
ts.plot(ar1.sim, main="Processo AR(1): mu=1, phi=0.9", xlab="time", ylab="y(t)", col="blue", lwd=2)

bacf = acf(ar1.sim)
plot(bacf)
bacf

ar1.acf = ARMAacf(ar=0.9, ma=0, lag.max=20)
plot(0:20, ar1.acf, type="h", col="blue", lwd=2, main="FAC para AR(1): phi=0.9", xlab="lag", ylab="FAC");
abline(h=0)

##################################

##################################
# ARMA(1, 1)
arma11.model = list(ar=c(0.9), ma=c(0.9)))
mu = 1
set.seed(123)
arma11.sim = mu + arima.sim(model=arma11.model, n=250)
ar1.acf = ARMAacf(ar=0.9, ma=0.9, lag.max=10)
##################################

##################################
# Passeio aleatorio com tendencia
# Random walk
set.seed(321)
e = rnorm(250)
y.rw = cumsum(e)
ts.plot(y.rw, lwd=2, col="blue", main="Passeio aleatorio")
abline(h=0)

# Random walk with drift
mu = rep(0.1, length(e))
y.rwd = cumsum(mu) + cumsum(e)
ts.plot(y.rwd, lwd=2, col="blue", main="Passeio aleatorio com tendência")
abline(h=0)
##################################



##################################
# Teste de I(0)
library("tseries");
data = suppressWarnings(get.hist.quote("^BVSP", start = "2000-01-01", end = "2020-08-01"));
data = data[, "Close"];
for (i in 2 : length(data)) { if (is.na(data[i])) data[i] = data[i-1]}
returns = diff(data)/data[-length(data)];


adf.test(returns, alternative = "stationary")

i1 = cumsum(returns);
i2 = cumsum(i1);
i3 = cumsum(i2);
##################################

##################################
# Cointegracao

library("tseries");
petr4 = suppressWarnings(get.hist.quote("PETR4.SA", start = "2013-01-01", end = "2017-01-01"));
petr4 = petr4[, "Close"];
petr3 = suppressWarnings(get.hist.quote("PETR3.SA", start = "2013-01-01", end = "2017-01-01"));
petr3 = petr3[, "Close"];
for (i in 2 : length(petr4)) { if (is.na(petr4[i])) petr4[i] = petr4[i-1]}
for (i in 2 : length(petr3)) { if (is.na(petr3[i])) petr3[i] = petr3[i-1]}

adf.test(petr4, alternative = "stationary");
petr4.i0 = diff(petr4);
adf.test(petr4.i0, alternative = "stationary");

adf.test(petr3, alternative = "stationary");
petr3.i0 = diff(petr3);
adf.test(petr3.i0, alternative = "stationary");

diferenca = petr3 - petr4;
adf.test(diferenca, alternative = "stationary");

plot(petr4, type="l", col="red", ylim = c(-2, 25))
abline(h = 0, col="black")
lines(petr3, col="blue")
lines(diferenca, col="green")
##################################


##################################
# GARCH 
install.packages("rugarch")
library("tseries");
data = get.hist.quote("^BVSP", start = "2000-01-01", end = "2017-01-01");
data = data[, "Close"];
for (i in 2 : length(data)) { if (is.na(data[i])) data[i] = data[i-1]}
returns = diff(data)/data[-length(data)];

library("rugarch");
spec <- ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 
                   mean.model     = list(armaOrder = c(1, 1)));

garch <- ugarchfit(spec = spec, data = returns, solver.control = list(trace=0));

options(scipen=999)

# Coefficients
garch@fit$coef

# Sigma condicional
garch@fit$sigma

# Serie z
garch@fit$z

# Mais informacao
garch

plot(data, type="l");
plot(returns, type="l");
plot(garch@fit$sigma, type="l");
##################################
