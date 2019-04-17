library(MASS)
Z = rbinom(1000 ,size = 1,prob = 0.5) + 1
X = matrix(rep(0, 2000), ncol=2)
mu1  = c(0,0)
sigma1 = diag(2)
mu2  = c(1,0)
sigma2 = matrix(c(1,0,0,2) ,2,2, byrow = T)

for (i in 1:1000) {
  if (Z[i] == 1) {
    obsev = mvrnorm(1, mu1, sigma1)
    X[i,1] = obsev[1]
    X[i,2] = obsev[2]
  }else if(Z[i] == 2){
    obsev = mvrnorm(1, mu2, sigma2)
    X[i,1] = obsev[1]
    X[i,2] = obsev[2]
  }
}
plot(X, col = Z+1)

output1 = kmeans(X, centers = 2)
centers = output1$centers
plot(X, col = output1$cluster+1)

library(mclust)
output2 = Mclust(X, G = 2)
output2$parameters$mean
plot(X, col = output2$classification+1)
