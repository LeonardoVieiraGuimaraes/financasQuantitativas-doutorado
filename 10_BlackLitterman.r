# Mostrar

# https://www.betterment.com/
# https://www.betterment.com/resources/portfolio-optimization/

# https://www.wealthfront.com/
# https://www.wealthfront.com/methodology
# https://research.wealthfront.com/whitepapers/investment-methodology/

#
#

printf <- function(...) invisible(cat(sprintf(...)));

markowitz <- function(returns, sigma, minReturn) {
    N = length(returns);
    topo  = cbind(2*sigma, returns, rep(1, N));
    meio  = c(returns, 0, 0)
    fundo = c(rep(1, N), 0, 0)
    A     = rbind(topo, meio, fundo);
    b     = c(rep(0, N), minReturn, 1);

    z = solve(A) %*% b;
    w = z[1:N,]
    return(w);
}

# 430 Luenberger
markowitzUnconstrained <- function(riskAversionCoefficient, sigma, returns) {
    return(solve(riskAversionCoefficient * sigma) %*% returns);
}

assets    = c("US Bonds", "Intl Bonds", "US Large Growth", "US Large Value", "US Small Growth", 
              "US Small Value", "Intl Dev Equity", "Intl Emerg Equity");
N = length(assets);

marketCap = c(0.1934, 0.2613, 0.1209, 0.1209, 0.0134, 0.0134, 0.2418, 0.0349);
names(marketCap) = assets;

histReturns = c(0.0315, 0.0175, -0.0639, -0.0286, -0.0675, -0.0054, -0.0675, -0.0526);
names(histReturns) = assets;

weightedAverage =  sum(marketCap * histReturns);

sigma = matrix(c(0.001005,  0.001328, -0.000579, -0.000675,  0.000121,  0.000128, -0.000445, -0.000437, 
                 0.001328,  0.007277, -0.001307, -0.000610, -0.002237, -0.000989,  0.001442, -0.001535, 
                -0.000579, -0.001307,  0.059852,  0.027588,  0.063497,  0.023036,  0.032967,  0.048039, 
                -0.000675, -0.000610,  0.027588,  0.029609,  0.026572,  0.021465,  0.020697,  0.029854, 
                 0.000121, -0.002237,  0.063497,  0.026572,  0.102488,  0.042744,  0.039943,  0.065994, 
                 0.000128, -0.000989,  0.023036,  0.021465,  0.042744,  0.032056,  0.019881,  0.032235, 
                -0.000445,  0.001442,  0.032967,  0.020697,  0.039943,  0.019881,  0.028355,  0.035064, 
                -0.000437, -0.001535,  0.048039,  0.029854,  0.065994,  0.032235,  0.035064,  0.079958),
                ncol = N, byrow = 1);
colnames(sigma) = assets;
rownames(sigma) = assets;

riskAversionCoefficient = 3.07;

impliedExcessReturn = riskAversionCoefficient * sigma %*% marketCap;


# Markowitz
print(markowitzUnconstrained(riskAversionCoefficient, sigma, histReturns));

print(markowitzUnconstrained(riskAversionCoefficient, sigma, impliedExcessReturn));

# Black Litterman inputs

# Matrix with K views
Q =  c(0.0525, 0.0025, 0.02);
K = length(Q);

# Which assets are influences by each views
# Equal weights scheme
P = matrix(c( 0, 0,   0,    0,   0,    0, 1, 0,
             -1, 1,   0,    0,   0,    0, 0, 0,
              0, 0, 0.5, -0.5, 0.5, -0.5, 0, 0), 
           ncol = N, byrow = 1)

# Market capitalisation weighted scheme
P = matrix(c( 0, 0,   0,    0,   0,    0, 1, 0,
             -1, 1,   0,    0,   0,    0, 0, 0,
              0, 0, 0.9, -0.9, 0.1, -0.1, 0, 0), 
           ncol = N, byrow = 1)

omega = matrix(rep(0, K*K), ncol = K);

# One would expect the equilibrium returns to be less volatile than historical returns
# tau purpose is to reduce general volatility
tau = 0.025;

for (i in 1 : K) {
    omega[i,i] = P[i,] %*% sigma %*% P[i,] * tau;
}


# Black Litterman (should be equal to Table 6)
BL1 = solve(tau * sigma) + (t(P) %*% solve(omega) %*% P);
BL1 = solve(BL1);

BL2 = solve(tau * sigma) %*% impliedExcessReturn + (t(P) %*% solve(omega) %*% Q);

E = BL1 %*% BL2;

print(markowitzUnconstrained(riskAversionCoefficient, sigma, E));
