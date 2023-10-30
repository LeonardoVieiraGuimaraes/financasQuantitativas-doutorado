# Calcular o VaR IBOV, dados historicos de 2017

# Evita imprimir notacao cientifica
options(scipen=999);
# Definicao de printf
printf <- function(...) invisible(cat(sprintf(...)))

# Baixar os dados
library(RCurl)
data <- getURL("http://www.dcc.ufmg.br/~arbex/portfolios/IBOV.csv", ssl.verifypeer=0L, followlocation=1L);
prices = read.csv(text = data, header = TRUE, sep = ",", stringsAsFactors = FALSE);

# Isolar dados de 2017 ate o 1o dia de 2018
begin = which(prices[,1] > 20170000)[1];
end   = which(prices[,1] > 20180000)[1];
ibov = prices$IBOV[begin:end];

# Calculo dos retornos
returns = diff(ibov) / ibov[-(length(ibov))];

# Primeiro o VaR historico, assumindo distribuição normal
rmu = mean(returns);
rsd = sd  (returns);
VaR5 = qnorm(p = 0.05, mean = rmu, sd = rsd)
printf("Media = %.5f, SD = %.5f, VaR 5%% = %.5f\n", rmu, rsd, VaR5);
# Preço do índice caso o retorno seja igual ao do VaR:
precoVaRHistorico = ibov[length(ibov)]*(1 + VaR5);


# Simulação de Monte Carlo, usando o Constant Expected Return model com com epsilon ~ N(0, 1)
simulacoes = numeric(10000);
for (i in 1 : length(simulacoes)) {
  simulacaoRetorno = rmu + rsd*rnorm(1);
  simulacoes[i]  = ibov[length(ibov)] * (1 + simulacaoRetorno);
}
simulacoes = sort(simulacoes);
indiceVaR  = as.integer(length(simulacoes) * 0.05);
precoVaRSimulado = simulacoes[indiceVaR];




printf("Ultimo valor       : %.2f\n", ibov[length(ibov)]);
printf("Valor VaR histórico: %.2f\n", precoVaRHistorico);
printf("Valor VaR simulado : %.2f\n", precoVaRSimulado);


