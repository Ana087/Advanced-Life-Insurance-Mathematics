f <- function(par, x, px) {
  th5 = par[2]
  th3 = par[1]
  th6 = par[3]
  sum((log(th5) + (th3 - 1) * (th3)^x * log(th6) - log(px))^2)
}
# initial values
par.init = c(1.1049542, 0.9999124, 0.9998060)
# resulting parameters
(Ballegeer = optim(par.init, f, x = as.numeric(Slovenia_2019_males$Age[41:92]), lower = c(1, -Inf, -Inf), upper = c(Inf, 1, 1),
                   method = "L-BFGS-B", px = 1 - Slovenia_2019_males$qx[41:92])$par)
## 1.1061009 0.9998189 0.9998067
theta_3B = Ballegeer[1]
theta_5B = Ballegeer[2]
theta_6B = Ballegeer[3]
# tranform parameters Ballegeer to Makeham parameters original parametrization
th1B = -log(Ballegeer[2])
th2B = -log(Ballegeer[3]) * log(Ballegeer[1])
th3B = Ballegeer[1]
c(th1B, th2B, th3B)


g_male <- 
  ggplot(Slovenia_2019_males, aes(Age, log(qx))) + 
  geom_point(col = KULbg) + 
  geom_line(stat = "identity", col = KULbg) + 
  theme_bw() +
  ggtitle("Slovenia - males, 2019") + 
  labs(y = bquote(ln(q[x])))
df_Ball <- data.frame(
  Age = 40:91, 
  Makeham = log(1 - theta_5B * theta_6B ^ 
                  ((theta_3B - 1) * 
                     theta_3B ^ (40:91)))
)
g_male <- g_male + geom_line(data = df_Ball, 
                             aes(Age, Makeham), 
                             col = "red", lwd = 1.2) 
g_male



# binonial log likelihood

BinLL <- function(par, x, dx, lx) {
  px = exp(-(par[1] + par[3] * par[2]^x)) 
  -sum((lx - dx) * log(px) + dx * log(1 - px))
}
par.init = c(8.758055e-05, 1.104954, 2.036360e-05)
fit1     = optim(par.init, BinLL, x = as.numeric(Slovenia_2019_males$Age[41:92]), lx = as.numeric(Slovenia_2019_males$lx[41:92]), 
                 dx = as.numeric(Slovenia_2019_males$dx[41:92]), 
                 control = list(reltol = 1e-10))
theta_1DV = fit1$par[1] ; theta_3DV = fit1$par[2] ; theta_7DV = fit1$par[3]
theta_1M  = theta_1DV
theta_2M  = theta_7DV * log(theta_3DV) / (theta_3DV - 1)
theta_3M  = theta_3DV
c(theta_1M, theta_2M, theta_3M)
## 0.0000929862 0.0000206520 1.1049288016

g_male <- 
  ggplot(Slovenia_2019_males, aes(Age, log(qx))) + 
  geom_point(col = KULbg) + 
  geom_line(stat = "identity", col = KULbg) + 
  theme_bw() +
  ggtitle("Slovenia - males, 2019") + 
  labs(y = bquote(ln(q[x])))
df_DV <- data.frame(
  Age = 40:91, 
  Makeham = log(1 - exp(-(theta_1DV + 
                            theta_7DV * theta_3DV ^ (40:91))))
)
g_male <- g_male + geom_line(data = df_DV, 
                             aes(Age, Makeham), 
                             col = "red", lwd = 1.2) 
g_male


# simulating future lifetimes with makeham

age  = 50
nsim = 100000
v1   = rexp(nsim, 1)
v2   = rexp(nsim, 1)
t1   = (log(theta_2M * theta_3M ^ age +
              v1 * log(theta_3M)) -
          log(theta_2M)) / log(theta_3M) - age
t2   = v2 / theta_1M
t    = pmin(t1, t2)
age_death    = t + age


# group the simulated lifetimes in integer 
#        classes 
d_sim <- data.frame(
  x = age:110,
  sim = as.numeric(table(
    factor(floor(age_death), 
           levels = age:110)))
)
# and plot
g_sim_Makeham <- ggplot(d_sim) + 
  geom_step(aes(x, sim)) + 
  theme_bw() + 
  ggtitle("Slovenia - males, 2019") + 
  labs(y = bquote(d[x]))
g_sim_Makeham

# now we add the theoretical results, 
# using Makeham's law
grid = age:110
tgrid = grid - age
p = exp(-theta_1M * tgrid - 
          theta_2M * theta_3M ^ (age) * 
          (theta_3M ^ tgrid - 1) / log(theta_3M))
l = nsim * p
d = c(-diff(l), 0) # add zero to match length of grid
d_theo <- data.frame(
  x = age:110,
  theo = d
)
g_sim_Makeham <- g_sim_Makeham + 
  geom_step(data = d_theo, 
            aes(x, theo), 
            color = "red")
g_sim_Makeham


g_sim_Makeham <- g_sim_Makeham + 
  geom_step(data = Slovenia_2019_males, 
            aes(Age, dx), 
            color = "blue")
g_sim_Makeham
