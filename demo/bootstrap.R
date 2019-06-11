true_mean = 2
true_sd = 2
n = 100
obs = rnorm(n,true_mean, true_sd)

hist(obs)
mean(obs)
var(obs)

estimated_mean= mean(obs);estimated_mean
estimated_var = (1/n)*sum((obs-estimated_mean)^2);estimated_var

#Bootstrap
B = 10000
u1 = rep(0, B)
u2 = rep(0, B)
for (i in 1:B) {
  resample = sample(obs, replace = TRUE)
  # resample = rnorm(n,true_mean, true_sd)
  u1[i] = mean(resample)
  u2[i] = var(resample)
}
par(mfrow=c(1,2))
hist(u1)
hist(u2)
mean(u1)
mean(u2)

zscore = (mean(obs) - 2.1)/(sqrt(var(obs)/n));zscore
p_value = 2*(1-pnorm(zscore));p_value
#set a =0.05
a = 0.05
p_value < a
#reject the null
