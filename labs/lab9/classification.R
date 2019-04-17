
library(ISLR)
library(ggplot2)
colnames(Default)
dim(Default)
summary(Default)
default_numeric <- rep(0, nrow(Default))
default_numeric[Default$default == 'Yes'] <- 1
Default$default_num <- default_numeric
logis <- glm(default_num ~ balance+student+income,family = "binomial", data = Default)
summary(logis)
preds = logis$fitted.values > 0.5
mean(preds == default_numeric)

library(e1071)
# scaled = as.data.frame(scale(Default[-c(1,2,5)]))
# scaled$default_num = default_numeric
# scaled$student = Default$student
model = svm(default_num ~ balance+student+income, type="C-classification",data=Default)
preds = fitted(model)
mean(preds == default_numeric)

library(randomForest)
fit <- randomForest(factor(default_num) ~ balance+student+income,
                    data=Default,
                    ntree=30)
preds <- predict(fit, Default)
mean(preds == default_numeric)

library(MASS)

fit = lda(default_num ~ balance+student+income,data=Default)
preds <- predict(fit, Default)
mean(preds$class == default_numeric)
