# infant mortality

HPLLInfMort <- function(par, dx, lx, x){
  theta_1 = par[1]
  theta_2 = par[2]
  theta_3 = par[3]
  qxHP1   = theta_1 ^ ((x + theta_2) ^ (theta_3))
  qxHP    = qxHP1 / (1 + qxHP1)
  pxHP    = 1 - qxHP
  -sum((lx - dx) * log(pxHP) + dx * log(qxHP))
}

fitHP1 = optim(c(1e-6, 1e-6, 1e-6), HPLLInfMort, 
               dx = Slovenia_2019_males$dx[1:5], 
               lx = Slovenia_2019_males$lx[1:5], x = Slovenia_2019_males$Age[1:5])
fitHP2 = optim(c(1e-2, 1e-2, 1e-2), HPLLInfMort, 
               dx = Slovenia_2019_males$dx[1:5], 
               lx = Slovenia_2019_males$lx[1:5], x = Slovenia_2019_males$Age[1:5])
fitHP1$value; fitHP2$value

# [1] 2486.598
# [1] 2211.316

PH.lnqx <- function(par, grid) {
  th1 = par[1]
  th2 = par[2]
  th3 = par[3]
  qx1 = th1^((grid + th2)^th3)
  log(qx1 / (1 + qx1))
}
g_male <- 
  ggplot(Slovenia_2019_males, aes(Age, log(qx))) + 
  geom_point(col = KULbg) + 
  geom_line(stat = "identity", col = KULbg) + 
  theme_bw() +
  ggtitle("Slovenia - males, 2019") + 
  labs(y = bquote(ln(q[x])))
df_HP <- data.frame(
  x = 0:110,
  HP1 = PH.lnqx(fitHP1$par, 0:110),
  HP2 = PH.lnqx(fitHP2$par, 0:110)
)
g_male <- g_male + geom_line(data = df_HP, 
                             aes(x, HP1), col = "red") + 
  geom_line(data = df_HP, aes(x, HP2), 
            col = "purple")
g_male

theta_1HPInf = fitHP2$par[1]
theta_2HPInf = fitHP2$par[2]
theta_3HPInf = fitHP2$par[3]


# accident hump

HPLLAccid <- function(par, dx, lx, x) {
  theta_4 = par[1]
  theta_5 = par[2]
  theta_6 = par[3]
  qxHP1 = theta_4 * exp(-theta_5 * 
                          (log(x) - log(theta_6)) ^ 2)
  qxHP  = qxHP1 / (1 + qxHP1)
  pxHP  = 1 - qxHP
  -sum((lx - dx) * log(pxHP) + dx * log(qxHP))
}
par.init = c(0.0001, 10, 18)
par.init = c(0.01, 5, 18)

fit1 = optim(par.init,
             HPLLAccid,
             dx = Slovenia_2019_males$dx[16:25],
             lx = Slovenia_2019_males$lx[16:25],
             x = Slovenia_2019_males$Age[16:25])
(theta_4HPAcc = fit1$par[1])
## [1] 0.001786852
(theta_5HPAcc = fit1$par[2])
## [1] 1.434572
(theta_6HPAcc = fit1$par[3])
## [1] 53.83771

df_HPAcc <- data.frame(
  x = 0:110,
  qxHP1 = theta_4HPAcc * exp(-theta_5HPAcc * 
                               (log(0:110) - log(theta_6HPAcc)) ^ 2)
)
df_HPAcc$qxHP <- df_HPAcc$qxHP1 / (1 + df_HPAcc$qxHP1)
g_male <- g_male + geom_line(data = df_HPAcc, 
                             aes(x, log(qxHP)), 
                             col = "green") +
  ylim(-12, 0)
g_male


# adult mortality

HPLLAdult <- function(par,dx,lx,x){
  theta_7 = par[1]
  theta_8 = par[2]
  qxHP1 = theta_7 * theta_8 ^ x
  qxHP  = qxHP1 / (1 + qxHP1)
  pxHP  = 1 - qxHP
  -sum((lx - dx) * log(pxHP) + dx * log(qxHP))
}
par.init = c(0.01,1)

fit1 = optim(par.init,
             HPLLAdult,
             dx = Slovenia_2019_males$dx[25:99],
             lx = Slovenia_2019_males$lx[25:99],
             x = Slovenia_2019_males$Age[25:99])
(theta_7HPAd = fit1$par[1])
## [1] 2.07285e-05
(theta_8HPAd = fit1$par[2])
## [1] 1.106483

df_HPAd <- data.frame(
  x = 0:110,
  qxHP1 = theta_7HPAd * theta_8HPAd ^ (0:110)
)
df_HPAd$qxHP <- df_HPAd$qxHP1 / (1 + df_HPAd$qxHP1)
g_male <- g_male + geom_line(data = df_HPAd, 
                             aes(x, log(qxHP)), 
                             col = "blue") +
  ylim(-12, 0)
g_male


# all together

HPLL <- function(par,dx,lx,x){
  theta_1 = par[1]
  theta_2 = par[2]
  theta_3 = par[3]
  theta_4 = par[4]
  theta_5 = par[5]
  theta_6 = par[6]
  theta_7 = par[7]
  theta_8 = par[8]
  qxHP1 = theta_1 ^ ((x + theta_2) ^ (theta_3)) + 
    theta_4 * exp(-theta_5 * (log(x) - log(theta_6)) ^ 2) + 
    theta_7 * theta_8 ^ x
  qxHP  = qxHP1 / (1 + qxHP1)
  pxHP  = 1 - qxHP
  -sum((lx - dx) * log(pxHP) + dx * log(qxHP))
}
par.init = c(theta_1HPInf,theta_2HPInf,theta_3HPInf,
             theta_4HPAcc,theta_5HPAcc,theta_6HPAcc,
             theta_7HPAd,theta_8HPAd)

fit1 = optim(par.init,
             HPLL,
             dx = Slovenia_2019_males$dx[1:99],
             lx = Slovenia_2019_males$lx[1:99],
             x = Slovenia_2019_males$Age[1:99])
theta_1HP = fit1$par[1]
theta_2HP = fit1$par[2]
theta_3HP = fit1$par[3]
theta_4HP = fit1$par[4]
theta_5HP = fit1$par[5]
theta_6HP = fit1$par[6]
theta_7HP = fit1$par[7]
theta_8HP = fit1$par[8]

df_HP <- data.frame(
  x = 0:110,
  qxHP1 = theta_1HP ^ ((0:110 + theta_2HP) ^ (theta_3HP)) + 
    theta_4HP * exp(-theta_5HP *(log(0:110) - log(theta_6HP)) ^ 2) + 
    theta_7HP * theta_8HP ^ (0:110)
)
df_HP$qxHP <- df_HP$qxHP1 / (1 + df_HP$qxHP1)
g_male <- g_male + geom_line(data = df_HP, 
                             aes(x, log(qxHP)), 
                             col = "red") +
  ylim(-12, 0)
g_male


# simulating future lifetimes

age = 0
x   = 0:110
p   = 1 / (1 + theta_1HP ^ ((x + theta_2HP) ^ 
                              (theta_3HP)) +
             theta_4HP *
             exp(-theta_5HP * (log(x) - 
                                 log(theta_6HP)) ^ 2) + 
             theta_7HP * (theta_8HP ^ x))
n   = 110 - age
# pmat[1+i] is i_p_age
pmat = cumprod(p[(age + 1):length(p)])


nsim     = 100000
age_death = numeric(nsim)
u        = runif(nsim, 0, 1)
t1       = numeric(nsim)
t2       = numeric(nsim)

for(j in 1:nsim) {
  if (u[j] > pmat[1]) {
    t1[j] = 0
  }
  else{
    i = 1
    flag = FALSE
    while (i <= (n - 1) & !flag) {
      if (pmat[i] >= u[j] && u[j] > pmat[i + 1]) {
        t1[j] = i
        flag = TRUE
      }
      i = i + 1
    }
  }
  t = t1[j]
  if (t == 0) {
    t2[j] = log(u[j]) / (log(p[t + 1]))
  }
  if (t > 0) {
    t2[j] = (log(u[j]) - log(pmat[t])) / 
      log(p[t + age + 1])
  }
  age_death[j] = age + t1[j] + t2[j]
}

# group the simulated lifetimes in integer classes and plot
d_sim <- data.frame(
  x = age:110,
  sim = as.numeric(table(factor(floor(age_death), 
                                levels = age:110)))
)

# and plot
g_sim_HP <- ggplot(d_sim) + 
  geom_step(aes(x, sim)) + 
  theme_bw() + ggtitle("Slovenia - males, 2019") + 
  labs(y = bquote(d[x]))

# now we add the theoretical results, using H&P law
l = nsim * c(1, pmat)
d = -diff(l)

d_theo <- data.frame(
  x = age:110,
  theo = d
)

g_sim_HP <- g_sim_HP + 
  geom_step(data = d_theo, aes(x, theo), 
            color = "red")

g_sim_HP <- g_sim_HP + 
  geom_step(data = Slovenia_2019_males, 
            aes(Age, dx), 
            color = "blue")

g_sim_HP
