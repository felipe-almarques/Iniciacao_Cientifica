# Função para estimação dos parâmetros de um SV via máxima verossimilhança construída através do filtro de kalman.
# A função está baseada no detalhamento teórico presente no artigo "A note on Stochastic Volatility models"
# m = 2 (duas normais na mistura)

# Params iniciais: (alpha, phi, sigmaw, sigma1, sigma2, mu1, mu2)

n <- 100
y <- rnorm(100)
h <-  P <- epsilon <- Sigma <- k <- rep(0, n)
params_ini <- c(.2, .9, .3, .3, .2, .7, .5)
alpha <- params_ini[1]; phi <- params_ini[1]; sigmaw <- params_ini[3]
sigma1 <- params_ini[1]; sigma2 <- params_ini[1]; mu1 <- params_ini[6]
mu2 <- params_ini[7]

for (t in 1:100) {
  
}
