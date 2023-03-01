library (tidyverse)
library(mlr3verse)
library(corrplot)
library(corrgram)


wn.train <- read.csv("./data/train_wine.csv", stringsAsFactors = TRUE)

ggplot(wn.train, aes(x=class, fill = class)) +
  geom_bar(stat="count") +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  labs(y="Num of Observations", x="Wine Quality") +
  labs(title= "Distribution of Red Wine Quality Ratings")
# przeważają wina średnie "good"(2624) i "medium"(1433)







wn.train$class <- factor(wn.train$class) %>% as.numeric()
wn.train$color <- factor(wn.train$color) %>% as.numeric()
wn.train$condition <- factor(wn.train$condition) %>% as.numeric()
wn.train$vineyard <- factor(wn.train$vineyard) %>% as.numeric()
wn.train$chlor.class <- factor(wn.train$chlor.class) %>% as.numeric()


corrplot(cor(wn.train[, 1:21]), order = "hclust", tl.cex = 0.7)



corrplot(cor(wn.train[, 1:21]), tl.cex = 0.7)
summary(wn.train)


corrgram (wn.train, order = TRUE , lower.panel=panel.conf)
#Silna korelacja dodatnia z:
# - total.sulfur.dioxide i free.sulfur.dioxide; 
# -fixed.acidity i density 
# -density i residual sugar

# Silna korelacja ujemna z:
#- density i alcohol

wn.train <- wn.train %>% select(class,alcohol,volatile.acidity,
                                density,sulphates,pH,chlorides,residual.sugar,total.sulfur.dioxide,fixed.acidity,color
)


# skorzystamy z miary impurity algorytmu random forest
lrn.rn0 <- lrn("classif.ranger", importance = "impurity")

# filtrowanie cechy "importance"
filter.rn <- flt("importance", learner = lrn.rn0)



task.wn0 <- TaskClassif$new(id = "wn", backend = wn.train, 
                            target = "class")

set.seed(80)
filter.rn$calculate(task.wn0)


filter.rn$scores
autoplot(filter.rn) + theme(axis.text.x = element_text(angle = 90))
# zmienna Alcohol,volatile acidity i są wyraźnie ważniejsze od pozostałych zmiennych

# wszystkie zmienne, które nie uzyskały wyższego wskaźnika niż zmienna "id" uznałem za nieważne 



wn.train$free.sulfur.dioxide
#może dodać fixed.acidity, sulfur.taste, acid.sulfur


# można wyrzucić PH citrix.acid,fixed,acidity,residual.sugar,sulphates,total.sulfur.dioxide
# wyrzucam citric.acid
# dane testoe
wn.test <- read.csv("./data/test_wine.csv", stringsAsFactors = TRUE)






task.wn <- TaskClassif$new(id = "wn", backend = wn.train, 
                           target = "class")
set.seed(10)
lrn.rn <- lrn("classif.ranger", id = "rn", mtry = 3, num.trees = 1000)


lrn.rn$train(task = task.wn)

summary(wn.train)

pred.rn <- lrn.rn$predict_newdata(wn.test)


pred.rn$response
pred.df <- data.frame(id = wn.test$id, class = pred.rn$response)



write.csv(pred.df, "./data/solution_rp110.csv", row.names = FALSE)


table(pred.df)


