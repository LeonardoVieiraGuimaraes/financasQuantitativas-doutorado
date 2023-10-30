# Dados

mu.A = 0.175
sig.A = 0.258
sig2.A = sig.A^2
mu.B = 0.055
sig.B = 0.115
sig2.B = sig.B^2
rho.AB = -0.164
sig.AB = rho.AB*sig.A*sig.B


# Portfolio A e r_f 
rf = 0.03

# RFR + asset A
w.A = seq(from=0, to=1.4, by=0.1)
mu.p.A = rf + w.A*(mu.A - rf)
sig.p.A = w.A*sig.A

# RFR + asset B
w.B = seq(from=0, to=1.4, by=0.1)
mu.p.B = rf + w.B*(mu.B - rf)
sig.p.B = w.B*sig.B

# plot portfolios of T-Bills and assets A and B
plot(sig.p.A, mu.p.A, type="b", col="black", ylim=c(0, max(mu.p.A)),
     xlim=c(0, max(sig.p.A, sig.p.B)), pch=16,
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
points(sig.p.B, mu.p.B, type="b", col="blue", pch=16)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4)
text(x=sig.B, y=mu.B, labels="Asset B", pos=1)
text(x=0, y=rf, labels=expression(r[f]), pos=2)


# Calculo do portfolio tangencial:
top = (mu.A - rf)*sig2.B - (mu.B - rf)*sig.AB
bot = (mu.A - rf)*sig2.B + (mu.B - rf)*sig2.A -
      (mu.A - rf + mu.B - rf)*sig.AB
w.A.T = top/bot
w.B.T = 1 - w.A.T
w.A.T
w.B.T
mu.T = w.A.T*mu.A + w.B.T*mu.B
sig2.T = w.A.T^2 * sig2.A + w.B.T^2 * sig2.B +
             2*w.A.T*w.B.T*sig.AB
sig.T = sqrt(sig2.T)
mu.T
sig.T


# Portfolios
# RFR + asset A
w.A = seq(from=0, to=1.4, by=0.1)
mu.p.A = rf + w.A*(mu.A - rf)
sig.p.A = w.A*sig.A
# RFR + asset B
w.B = seq(from=0, to=1.4, by=0.1)
mu.p.B = rf + w.B*(mu.B - rf)
sig.p.B = w.B*sig.B
# RFR + asset B
w.T = seq(from=0, to=1.8, by=0.1)
mu.p.T = rf + w.T*(mu.T - rf)
sig.p.T = w.T*sig.T
# A + B
w.A = seq(from=-0.4, to=1.4, by=0.1)
w.B = 1 - w.A
mu.p = w.A * mu.A + w.B * mu.B
sig2.p = w.A^2 * sig2.A + w.B^2 * sig2.B + 2 * w.A * w.B * sig.AB
sig.p = sqrt(sig2.p)

# plot portfolios of T-Bills and assets A and B
plot(sig.p.A, mu.p.A, type="b", col="black", ylim=c(0, max(mu.p.A)),
     xlim=c(0, max(sig.p.A, sig.p.B, sig.T)), pch=16,
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
points(sig.p.B, mu.p.B, type="b", col="blue", pch=16)
points(sig.p,   mu.p,   type="b", col="red", pch=16)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4)
text(x=sig.B, y=mu.B, labels="Asset B", pos=1)
text(x=0, y=rf, labels=expression(r[f]), pos=2)


# plot portfolios of T-Bills and assets A and B
plot(sig.p.A, mu.p.A, type="b", col="black", ylim=c(0, max(mu.p.A)),
     xlim=c(0, max(sig.p.A, sig.p.B, sig.T)), pch=16,
     xlab=expression(sigma[p]), ylab=expression(mu[p]))
points(sig.p.B, mu.p.B, type="b", col="blue", pch=16)
points(sig.p.T, mu.p.T, type="b", col="#00AA00", pch=16)
points(sig.p,   mu.p,   type="b", col="red", pch=16)
text(x=sig.A, y=mu.A, labels="Asset A", pos=4)
text(x=sig.B, y=mu.B, labels="Asset B", pos=1)
text(x=sig.T-0.08, y=mu.T, labels="Portfolio tangencial", pos=1)
text(x=0, y=rf, labels=expression(r[f]), pos=2)

