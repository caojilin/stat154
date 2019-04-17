library(ggplot2)

colMax <- function(data) apply(data, 1, max)

peaks <-  function(x, y) return(3 * (1-x)^2 * exp(-(x^2) - (y+1)^2) -
  10 * (x/5 - x^3 - y^5) * exp(-x^2 - y^2) -
  1/3 * exp(-(x+1)^2 - y^2))

axisx <- seq(-3,3,by=0.05)
axisy <- seq(-3,3,by=0.05)

grid <- expand.grid(x=axisx, y=axisy)
grid$z <- with(grid, peaks(x,y))

# plot peaks function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = z)) +
  scale_fill_gradientn(colours = terrain.colors(10))

# generate our data
set.seed(123456)
B <- 3
n <- 500
p <- 2
X <- matrix(2 * B * runif(n*p) - B, nrow=n)
rho <- 0.2
Y <- peaks(X[, 1], X[, 2]) + rho * rnorm(n) 
data <- data.frame(cbind(X, Y))
colnames(data) <- c('X1', 'X2', 'Y')

# train and test split
test_size <- n / 5
test_ind <- sample(n, size = test_size)
dataTest <- data[test_ind, ]
dataTrain <- data[-test_ind, ]


ggplot(dataTrain) +
  geom_point(aes(x = X1, y = X2, color=Y), size = 1.2) + 
  scale_colour_gradientn(colours = terrain.colors(10)) +
  coord_fixed(ratio = 1)


## run linear model
fitlm <- lm(Y~X1+X2, dataTrain)
summary(fitlm)
lmfun <- function(x, y) return(fitlm$coefficients[1] + x*fitlm$coefficients[2] + y*fitlm$coefficients[3])
# MSE on test
mean((predict(fitlm, dataTest) - as.matrix(dataTest['Y']))^2)
grid$zlm <- with(grid, lmfun(x,y))
# plot lm function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = zlm)) +
  scale_fill_gradientn(colours = terrain.colors(10))

## run ridge regression
library(glmnet)
fitRidge <- cv.glmnet(as.matrix(dataTrain[c('X1', 'X2')]), as.matrix(dataTrain['Y']), family = "gaussian", alpha=0)
ridgefun <- function(x, y) return(coef(fitRidge)[1] + x*coef(fitRidge)[2] + y*coef(fitRidge)[3])
# MSE on test
mean((predict(fitRidge, newx=as.matrix(dataTest[c('X1', 'X2')]))- as.matrix(dataTest['Y']))^2)
grid$zridge <- with(grid, ridgefun(x,y))
# plot ridge function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = zridge)) +
  scale_fill_gradientn(colours = terrain.colors(10))

## fit kernel ridge regression with Gaussian kernel
## try to define a function for kernel ridge regression so we can try many sigmas
vecKernel <- function(sigma) {
  # form the kernel matrix on training
  trainDist <- as.matrix(dist(cbind(dataTrain[, 1:2]), upper=TRUE))
  K <- exp(-trainDist^2/(2*sigma^2))
  
  # lambda is regularization
  lambda <- 0.01
  h <- solve(K+lambda*diag(nrow(dataTrain)), dataTrain[, 3])
  kernelridgefun <- function(x, y) {
    Knew <- exp(-((dataTrain[, 1]-x)^2 + (dataTrain[, 2]-y)^2)/(2*sigma^2))
    return(sum(Knew * h))
  }
  vecKernelridgefun <- Vectorize(kernelridgefun)
  return(vecKernelridgefun)
}

grid$zkernelridge005 <- with(grid, vecKernel(0.05)(x,y))
# plot kernel ridge function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = zkernelridge005)) +
  scale_fill_gradientn(colours = terrain.colors(10))

grid$zkernelridge05 <- with(grid, vecKernel(0.5)(x,y))
# plot kernel ridge function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = zkernelridge05)) +
  scale_fill_gradientn(colours = terrain.colors(10))

grid$zkernelridge03 <- with(grid, vecKernel(0.3)(x,y))
# plot kernel ridge function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = zkernelridge03)) +
  scale_fill_gradientn(colours = terrain.colors(10))

grid$zkernelridge5 <- with(grid, vecKernel(5)(x,y))
# plot kernel ridge function on a grid 
ggplot(grid, aes(x, y)) +
  geom_raster(aes(fill = zkernelridge5)) +
  scale_fill_gradientn(colours = terrain.colors(10))

