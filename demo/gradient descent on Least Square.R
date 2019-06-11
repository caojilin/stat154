set.seed(11)
x <- matrix(rnorm(400), ncol = 4)
y <- rnorm(100)
m <- length(y)

X<-cbind(rep(1, 100), x)
theta<-rep(0,5)

gradDescent<-function(X, y, theta, alpha, num_iters){
  m <- length(y)
  J_hist <- rep(0, num_iters)
  T_hist = matrix(rep(0, num_iters * ncol(X)), ncol=ncol(X))
  for(i in 1:num_iters){

    theta <- theta - alpha*(1/m)*(t(X)%*%(X%*%theta - y))
    
    J_hist[i]  <- mean((X %*% theta - y)^2)
    T_hist[i,] = theta
  }
  # for a R function to return two values, we need to use a list to store them:
  results<-list(theta, J_hist,T_hist)
  return(results)
}

alpha <- 0.1
num_iters <- 150
results <- gradDescent(X, y, theta, alpha, num_iters)
theta <- results[[1]]
cost_hist <- results[[2]]
print(tail(cost_hist))
print(theta)

plot(1:num_iters, cost_hist, type = 'l')

lmod = lm(y ~ X[,c(2,3,4,5)])
lmod$coefficients
#MSE
mean(lmod$residuals^2)

