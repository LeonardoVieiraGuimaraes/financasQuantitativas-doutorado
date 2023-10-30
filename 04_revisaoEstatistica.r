# Codigos dos slides da media geometrica

numMedias = 100000;
medias = numeric(numMedias);
for (i in 1 : numMedias) {
    medias[i] = mean(runif(1000));
}
plot = hist(medias, freq = FALSE, breaks = 100);

xfit = seq(min(medias), max(medias), length = 40) 
yfit = dnorm(xfit, mean = mean(medias), sd = sd(medias));
lines(xfit, yfit, col = "black", lwd = 2);


# Alternativamente:
#plot = hist(medias);
#xfit = seq(min(medias), max(medias), length = 40) 
#yfit = dnorm(xfit, mean = mean(medias), sd = sd(medias));
#yfit = yfit * diff(plot$mids[1:2]) * length(medias);
#lines(xfit, yfit, col = "black", lwd = 2);
## Diferença entre 2 centros quaisquer de duas barras
## consecutivas do histograma --> diff(plot$mids[1:2])


# Media geometrica

jogoAM <- function(w) {
  avg = (5/6 * (1-w)) + (1/6 * (1+10*w)) - 1;
}

jogoGM <- function(w) {
  tim = (1 - w)^(5/6) * (1 + 10*w)^(1/6) - 1;
}

data = seq(0, 1, by=0.01);
am = jogoAM(data);
gm = jogoGM(data);

plot(data, am, type = "l", ylim = c(-1, 1), col = "blue");
lines(data, gm, col = "red")
abline(a = 0, b = 0)






