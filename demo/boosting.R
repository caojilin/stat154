library(gbm)
library(MASS)
library(glmnet)
library(spatstat)
#boosting
set.seed(1)
train = sample (1: nrow(Boston ), nrow(Boston )/2)
boston.test=Boston[-train ,"medv"]
boost.boston = gbm(medv ~ ., data=Boston[train,], distribution="gaussian", n.trees = 5000,
                   interaction.depth = 4)
summary (boost.boston)

plot(boost.boston ,i="rm")
plot(boost.boston ,i="lstat")

yhat.boost = predict(boost.boston ,newdata=Boston[-train ,],
                    n.trees =5000)
mean((yhat.boost -boston.test)^2)

# linear regression
lm.boston = lm(medv ~., data=Boston[train,])
yhat.lm = predict(lm.boston, Boston[-train,])
mean((yhat.lm -boston.test)^2)

#lasso and ridge
training = Boston[train,-14]
# feature_classes <- sapply(names(training),function(x){class(training[[x]])})
# categorical_feats <- names(feature_classes[feature_classes == "integer"])
# training[categorical_feats] = factor(training[categorical_feats])
training = dummify(training)
training.label = Boston[train, "medv"]
testing = dummify(Boston[-train,])

fit.ridge= cv.glmnet(training, training.label, alpha=0, family="gaussian",type.measure = "mse")
fit.ridge = glmnet(training, training.label, alpha=0, family="gaussian",lambda = 0.7604016)

y_hat = predict(fit.ridge, training)

mean((y_hat - training.label)^2)
