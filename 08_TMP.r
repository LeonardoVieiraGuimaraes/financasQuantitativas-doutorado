
mu.A = 0.175
sig.A = 0.258
sig2.A = sig.A^2
mu.B = 0.055
sig.B = 0.115
sig2.B = sig.B^2
rho.AB = -0.164
sig.AB = rho.AB*sig.A*sig.B

# Exemplo portfolio equally weighted
w.A = 0.5
w.B = 0.5
mu.p1 = w.A*mu.A + w.B*mu.B
sig2.p1 = w.A^2 * sig2.A + w.B^2 * sig2.B + 2*w.A*w.B*sig.AB
sig.p1 = sqrt(sig2.p1)
mu.p1
sig2.p1
sig.p1

# Exemplo long/short
w.A = 1.5
w.B = -0.5
mu.p2 = w.A*mu.A + w.B*mu.B
sig2.p2 = w.A^2 * sig2.A + w.B^2 * sig2.B + 2*w.A*w.B*sig.AB
sig.p2 = sqrt(sig2.p2)
mu.p2
sig2.p2
sig.p2

# Portfolios eficientes com 2 ativos

w.A = seq(from=-0.4, to=1.4, by=0.1)
w.B = 1 - w.A
mu.p = w.A * mu.A + w.B * mu.B
sig2.p = w.A^2 * sig2.A + w.B^2 * sig2.B + 2 * w.A * w.B * sig.AB
sig.p = sqrt(sig2.p)
plot(sig.p, mu.p, type="b", pch=16,
       ylim=c(0, max(mu.p)), xlim=c(0, max(sig.p)),
       xlab=expression(sigma[p]), ylab=expression(mu[p]),
       col=c(rep("red", 6), rep("green", 13)))
text(x=sig.A, y=mu.A, labels="Asset A", pos=4)
text(x=sig.B, y=mu.B, labels="Asset B", pos=4)

# Variancia mínima global
wA.min = (sig2.B - sig.AB)/(sig2.A + sig2.B - 2*sig.AB)
wB.min = 1 - wA.min
wA.min
wB.min
mu.p.min = wA.min*mu.A + wB.min*mu.B
sig2.p.min = wA.min^2 * sig2.A + wB.min^2 * sig2.B + 2*wA.min*wB.min*sig.AB
sig.p.min = sqrt(sig2.p.min)
mu.p.min
sig.p.min

# Dados de 3 ativos com notação matricial

asset.names <- c("A", "B", "C")
mu.vec = c(0.0427, 0.0015, 0.0285)
names(mu.vec) = asset.names
sigma.mat = matrix(c(0.0100, 0.0018, 0.0011,
                       0.0018, 0.0109, 0.0026,
                       0.0011, 0.0026, 0.0199),
                     nrow=3, ncol=3)
dimnames(sigma.mat) = list(asset.names, asset.names)
mu.vec
sigma.mat

# igualmente distribuido
w.vec = rep(1,3) / 3
names(w.vec) = asset.names
mu.p.w = crossprod(w.vec, mu.vec)
sig2.p.w = t(w.vec) %*% sigma.mat %*% w.vec
sig.p.w = sqrt(sig2.p.w)
mu.p.w
sig.p.w

# Min global
topo.mat  = cbind(2*sigma.mat, rep(1, 3));
fundo.vec = c(rep(1, 3), 0);
A.mat = rbind(topo.mat, fundo.vec);
b.vec = c(rep(0, 3), 1);
z.mat = solve(A.mat) %*% b.vec;
w.vec = z.mat[1:3,1];
w.vec

mu.gmin = as.numeric(crossprod(w.vec, mu.vec))
mu.gmin

sig2.gmin = as.numeric(t(w.vec) %*% sigma.mat %*% w.vec);
sig.gmin = sqrt(sig2.gmin)
sig2.gmin
sig.gmin

# Portfolio igual a A
topo.mat  = cbind(2*sigma.mat, mu.vec, rep(1, 3))
meio.vec  = c(mu.vec, 0, 0)
fundo.vec = c(rep(1, 3), 0, 0)
A.mat = rbind(topo.mat, meio.vec, fundo.vec)
bA.vec = c(rep(0, 3), mu.vec["A"], 1)
# Sistema de equações
z.mat = solve(A.mat) %*% bA.vec
wA.vec = z.mat[1:3,]
wA.vec

mu.pA = as.numeric(crossprod(wA.vec, mu.vec))
mu.pA
sig2.pA = as.numeric(t(wA.vec) %*% sigma.mat %*% wA.vec)
sig.pA = sqrt(sig2.pA)
sig2.pA
sig.pA

# Portfolio igual a C
bC.vec = c(rep(0, 3), mu.vec["C"], 1)
z.mat = solve(A.mat) %*% bC.vec
wC.vec = z.mat[1:3,]
wC.vec
mu.pC = as.numeric(crossprod(wC.vec, mu.vec))
sig2.pC = as.numeric(t(wC.vec) %*% sigma.mat %*% wC.vec)
sig.pC = sqrt(sig2.pC)
mu.pC
sig.pC


#################################################
# Resolvendo Markowitz com desigualdades:
# Exemplo que não está nos slides
# Usa a biblioteca de gráficos que disponibilizei

#####
## Igual aos slides:
source("graficos.r");
loadPackage("quantmod");
loadPackage("quadprog");

ativos <- c("ABEV3.SA", "BBAS3.SA", "BBDC3.SA", "CMIG4.SA", "EMBR3.SA", "ITSA4.SA", "ITUB4.SA", "PETR4.SA", "USIM5.SA", "VALE3.SA");
ativos2 <- c("ABEV3", "BBAS3", "BBDC3", "CMIG4", "EMBR3", "ITSA4", "ITUB4", "PETR4", "USIM5", "VALE3");

getSymbols(ativos, src='yahoo');

returnsList = list();
for (i in 1 : length(ativos)) {
    returnsList[[i]] = periodReturn(eval(parse(text = ativos[i])), period='monthly', subset='2017::2019');
}
returns.df = as.data.frame(returnsList);
colnames(returns.df) = ativos;

mu <- matrix(colMeans(returns.df), nrow=1)
Sigma <- cov(returns.df);

# quadprog
Dmat = Sigma;                  # Q do modelo
dvec = rep(0, length(ativos)); # c do modelo
Amat = matrix(c(rep(1, length(ativos)),
                mu, 
                diag(length(ativos))),
              ncol = length(ativos), 
              byrow = TRUE);
bvec = c(1, 0, rep(0, length(ativos)));
meq  = 1;

sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
print(sol);

ggOptions = getGGOptions();
ggOptions$saveGraphics = 0;
ggOptions$height = 6;
ggOptions$width = 10;
ggOptions$titleSize = 20;
ggOptions$axisTitleSize = 17;
ggOptions$axisLabelSize = 13;
ggOptions$yTitle = "Proporção";
ggOptions$barChart = 1;
ggOptions$percentageInYAxis = 1;
ggOptions$removeXAxisSpace  = 0;
ggOptions$barFill = "blue";

ret = 0.02;
bvec = c(1, ret, rep(0, length(ativos)));
sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
ggOptions$imageName = "FiguraMARK-5a.pdf";
ggOptions$title = sprintf("Alocação com retorno esperado mínimo de %.1f%%", 100*ret);
print(ativos2);
print(sol$solution);
plotGraph(sol$solution, xValues = ativos2, ggOptions = ggOptions);
vari = t(sol$solution) %*% Sigma %*% sol$solution;
devi = sqrt(vari);
printf("Retorno esperado: %8.5f, Desvio: %8.5f, Vari %8.5f, Value %8.5f\n", crossprod(sol$solution, t(mu)), devi, vari, sol$value);

ret = 0.03;
bvec = c(1, ret, rep(0, length(ativos)));
sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
ggOptions$imageName = "FiguraMARK-5b.pdf";
ggOptions$title = sprintf("Alocação com retorno esperado mínimo de %.1f%%", 100*ret);
plotGraph(sol$solution, xValues = ativos2, ggOptions = ggOptions);
vari = t(sol$solution) %*% Sigma %*% sol$solution;
devi = sqrt(vari);
printf("Retorno esperado: %8.5f, Desvio: %8.5f, Vari %8.5f, Value %8.5f\n", crossprod(sol$solution, t(mu)), devi, vari, sol$value);

print(crossprod(sol$solution, t(mu)));
print(t(sol$solution) %*% Sigma %*% sol$solution);
print(sqrt(vari));


ret = -1;
bvec = c(1, ret, rep(0, length(ativos)));
sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
ggOptions$imageName = "FiguraMARK-5c.pdf";
ggOptions$title = sprintf("Alocação com retorno esperado mínimo de %.1f%%", 100*ret);
plotGraph(sol$solution, xValues = ativos2, ggOptions = ggOptions);
vari = t(sol$solution) %*% Sigma %*% sol$solution;
devi = sqrt(vari);
printf("Retorno esperado: %8.5f, Desvio: %8.5f, Vari %8.5f, Value %8.5f\n", crossprod(sol$solution, t(mu)), devi, vari, sol$value);


################
## Exemplo Markowitz com desigualdades dos slides

library("quadprog");
library("quantmod");
ativos = c("PETR4.SA", "VALE3.SA", "ITUB4.SA", "USIM5.SA", "CMIG4.SA", 
            "BBAS3.SA", "ABEV3.SA", "ITSA4.SA", "EMBR3.SA", "BBDC3.SA");
getSymbols(ativos, src='yahoo');

head(PETR4.SA)

returnsList = list();
for (i in 1 : length(ativos)) {
     returnsList[[i]] = periodReturn(eval(parse(text = ativos[i])), period='monthly', subset='2017::2019');
}
returns.df = as.data.frame(returnsList);
colnames(returns.df) = ativos;

mu = matrix(colMeans(returns.df), nrow=1)
colnames(mu) = ativos;
Sigma = cov(returns.df);
mu

Dmat = Sigma;
dvec = rep(0, length(ativos)); 
Amat = matrix(c(rep(1, length(ativos)),
                    mu, 
                    diag(length(ativos))
                   ),
                  ncol = length(ativos), 
                  byrow = TRUE);
bvec = c(1, 0, rep(0, length(ativos)));
meq  = 1;

sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
sol

retorno = crossprod(sol$solution, t(mu))
retorno
variancia = t(sol$solution) %*% Sigma %*% sol$solution;
variancia
sqrt(variancia);

# Retorno 2%
bvec = c(1, 0.02, rep(0, length(ativos)));
sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
sol
retorno = crossprod(sol$solution, t(mu))
retorno
variancia = t(sol$solution) %*% Sigma %*% sol$solution;
variancia
sqrt(variancia);

# Retorno 3%
bvec = c(1, 0.03, rep(0, length(ativos)));
sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
sol
retorno = crossprod(sol$solution, t(mu))
retorno
variancia = t(sol$solution) %*% Sigma %*% sol$solution;
variancia
sqrt(variancia);






################
### Utilizando dados disponibilizados

library(RCurl);
library(quadprog);
printf <- function(...) invisible(cat(sprintf(...)))

# Vamos precisar da funcao que converte uma taxa anual para diaria
annualToDaily <- function(r) { return((r + 1)^(1/252) - 1); }
dailyToAnnual <- function(r) { return((r + 1)^(252) - 1); }


# Baixar dados diarios
library(RCurl);
dataURL = "http://www.dcc.ufmg.br/~arbex/portfolios/IBOV.csv";
data = getURL(dataURL, ssl.verifypeer=0L, followlocation=1L);
prices = read.csv(text = data, header = TRUE, sep = ",", stringsAsFactors = FALSE);

# Isolar dados de 2017 ate o 1o dia de 2018
beg = which(prices[,1] > 20170000)[1];
end = which(prices[,1] > 20200000)[1];
prices = prices[beg:end,];

# Eliminar empresas que possuem zeros
prices = prices[, colSums(prices == 0) == 0];

# Tirar Datas, IBOV e BOVA11
assets = prices[,4:ncol(prices)];

# Calcular matriz de retornos
returns = apply(assets, 2, function(x){diff(x)/x[-length(x)]} ); 

# Calcular vetor de retornos esperados e matriz de covariancias
numAtivos = ncol(returns);
mu = matrix(colMeans(returns), nrow=1)
colnames(mu) = colnames(returns);
Sigma = cov(returns);


# Criar variaveis de entrada para o modelo quadratico
# Matriz Amat são as restrições: 
# A primeira linha inclui a restricao de soma w = 1, 
# A segunda inclui Retorno esperado do portfolio >= target,
# as demais linhas são cada w_i >= 0.
#
# O vetor bvec são os lados direitos das restrições
#
# meq diz quantas restricoes são igualdade. No pacote quadprog, as 
# igualdades sempre devem vir primeiro. Como meq = 1, a primeira linha
# de Amat é uma igualdade e as demais desigualdades do tipo >=
Dmat = Sigma; 
dvec = rep(0, numAtivos); 
Amat = matrix(c(rep(1, numAtivos), mu, diag(numAtivos)),
              ncol = numAtivos, 
              byrow = TRUE);
bvec = c(1, 0, rep(0, numAtivos));
meq  = 1;

# Funcao pra, dado um target return, resolver o Markowitz sem shorting
# Tambem imprime os valores e plota os pesos
solveMarkowitzAndPlot <- function(targetReturn) {
    bvec[2] = annualToDaily(targetReturn);
    sol <- solve.QP(Dmat = Dmat, dvec = dvec, Amat=t(Amat), bvec=bvec, meq=meq);
    sol
    # Calculo do retorno e variancia
    retorno = crossprod(sol$solution, t(mu))
    variancia = t(sol$solution) %*% Sigma %*% sol$solution;
    # Removendo da solucao os pesos muito baixos, quase zero
    names(sol$solution) = colnames(assets);
    sol$solution = sol$solution[sol$solution >= 10e-4];
    barplot(sol$solution, cex.names=0.7);
    #print(sol$solution);
    printf("Anual : Ret. min: %9.3f%%, mu: %9.3f%%, Stdev: %9.3f%%\n", targetReturn*100, dailyToAnnual(retorno)*100, sqrt(252)*sqrt(variancia)*100);
    printf("Diario: Ret. min: %9.5f%%, mu: %9.5f%%, Stdev: %9.5f%%\n", annualToDaily(targetReturn)*100, retorno*100, sqrt(variancia)*100);
}

# Portfolio com retorno mínimo de 0%
solveMarkowitzAndPlot(0);
solveMarkowitzAndPlot(0.20);
solveMarkowitzAndPlot(0.30);
solveMarkowitzAndPlot(0.50);

solveMarkowitzAndPlot(5);
solveMarkowitzAndPlot(7.44);
solveMarkowitzAndPlot(7.45);


