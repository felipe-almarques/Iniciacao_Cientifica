### GArch
dados_garch <- garch_sim(1500, alpha=c(0.005, 0.08, .003), beta = 0.85)
ts.plot(dados_garch)
head(dados_garch)
tail(dados_garch)
length(dados_garch)

### SV
beta <- mean(dados_garch)
dados_sv <- sv_sim(1500, beta, phi=0.95, sigma2 = 0.5)
ts.plot(dados_sv)
head(dados_sv)
tail(dados_sv)
length(dados_sv)

