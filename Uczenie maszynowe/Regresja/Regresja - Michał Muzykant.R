library (tidyverse)
library(mlr3verse)
library(xgboost)
library(corrplot)
library(simputation) 
library(corrgram)
library(missForest)
library(mice)
en.train <- read.csv("./data/train_energy.csv")


en.test <- read.csv("./data/test_energy.csv")

en.train$wday <- factor(en.train$wday) %>% as.numeric()
en.train$time <- factor(en.train$time) %>% as.numeric()

# uzupełnienie brakujących wartości przy pomocy biblioteki "mice"
imputed_data <-  mice(en.train, method="cart")


en.train <- complete(imputed_data) 





imputed_data2 <-  mice(en.test, method="cart")





en.test <- complete(imputed_data2) 


summary(en.train)

corrplot(cor(en.train[, 1:31]), tl.cex = 0.7)

corrgram (en.train, order = TRUE , lower.panel=panel.conf)


en.train <- en.train %>% select(-time,-id,-r1,-r2, -visibility,-h1
                                ,-t6)

en.test <- en.test %>% select(-h1,t6)



task.en <- TaskRegr$new(id = "en", backend = en.train, target = "energy")


set.seed(10)
lrn.rn <- lrn("regr.ranger", id = "rn", mtry = 3, num.trees = 1000)

lrn.xg <- lrn("regr.xgboost", id = "xg", eta = 0.01, nrounds = 500)

summary(en.train)

lrn.rn$train(task = task.en)

lrn.xg$train(task = task.en)

summary(en.train)

pred.rn <- lrn.rn$predict_newdata(en.test)



pred.rn$response


pred.df <- data.frame(id = en.test$id, energy = pred.rn$response)


summary(en.train)
write.csv(pred.df, "./data/solution_enjoy30.csv", row.names = FALSE)









