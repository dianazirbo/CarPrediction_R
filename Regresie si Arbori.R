library(tidyverse)
library(modelr)
library(scatterplot3d)
library(olsrr)

car<-read.csv("C:/Users/Diana/Desktop/UBB/AN 3 SEM 2/Big Data/Proiect2/carpriceedited.csv")
car <- select(car, turbo:price)


#
model<-lm(price~turbo+wheelbase+carlength+carwidth+carheight+curbweight+enginesize+boreratio+stroke+compressionratio+horsepower+peakrpm+citympg+highwaympg,
          data=car)
FWDfit.p<-ols_step_forward_p(model,penter=.07)
FWDfit.p



car %>%
  ggplot(aes(enginesize, price)) + geom_point() + geom_smooth()

car %>%
  ggplot(aes(curbweight, price)) + geom_point() + geom_smooth()

car %>%
  ggplot(aes(peakrpm, price)) + geom_point() + geom_smooth()

mod_price_enginesize <- lm(data = car, price ~ enginesize)    
summary(mod_price_enginesize)
confint(mod_price_enginesize)

grid_enginesize <- car %>%
  data_grid(enginesize = seq_range(enginesize, 60)) %>%
  add_predictions(mod_price_enginesize, "price")
ggplot(car, aes(enginesize, price)) + 
  geom_point() +  
  geom_line(data = grid_enginesize, color = "orange", size = 2)



mod_price_curbweight <- lm(data = car, price ~ curbweight)    
summary(mod_price_curbweight)
confint(mod_price_curbweight)

grid_curbweight <- car %>%
  data_grid(curbweight = seq_range(curbweight, 100)) %>%
  add_predictions(mod_price_curbweight, "price")
ggplot(car, aes(curbweight, price)) + 
  geom_point() +  
  geom_line(data = grid_curbweight, color = "orange", size = 2)


mod_price_peakrpm <- lm(data = car, price ~ peakrpm)    
summary(mod_price_peakrpm)
confint(mod_price_peakrpm)

grid_peakrpm <- car %>%
  data_grid(peakrpm = seq_range(peakrpm, 1000)) %>%
  add_predictions(mod_price_peakrpm, "price")
ggplot(car, aes(peakrpm, price)) + 
  geom_point() +  
  geom_line(data = grid_peakrpm, color = "orange", size = 2)


mod_price_all <- lm(data = car, price ~ enginesize + curbweight + peakrpm) 
summary(mod_price_all)

mod_price_enginesize_curbweight <- lm(data = car, price ~ enginesize + curbweight)
summary(mod_price_enginesize_curbweight)

mod_price_enginesize_peakrpm <- lm(data = car, price ~ enginesize + peakrpm)
summary(mod_price_enginesize_peakrpm)


s3d <- scatterplot3d(car$enginesize, car$curbweight, car$price, car$peakrpm, type="p") 
s3d$plane3d(mod_price_all, lty.box = "solid")

newvalueprice <- tibble(
  enginesize = 350,
  curbweight = 2910,
  peakrpm = 4750
)
predict(mod_price_all, newdata = newvalueprice, interval = "confidence")
predict(mod_price_all, newdata = newvalueprice, interval = "prediction")

mod_price_enginesize_curbweight_peakrpm_interaction <- lm(data = car, price ~ enginesize * curbweight * peakrpm)
summary(mod_price_enginesize_curbweight_peakrpm_interaction)



#



library(AmesHousing)
library(rsample)
library(tidyverse)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(ipred)

car<-read.csv("C:/Users/Diana/Desktop/UBB/AN 3 SEM 2/Big Data/Proiect2/carpriceedited.csv")
car <- select(car, turbo:price)

set.seed(123)
car_split <- initial_split(car, prop = 0.7)
car_train <- training(car_split)
car_test <- testing(car_split)

car %>%
  ggplot (aes(price)) +
  geom_density()

m1 <- rpart(
  formula = price ~ ., 
  data = car_train,
  method = "anova"
)

m1
rpart.plot(m1)
plotcp(m1)
m1$cptable

m2 <- rpart(
  formula = price ~ ., 
  data = car_train,
  method = "anova",
  control = list(cp = 0, xval = 10)
)
m2
rpart.plot(m2)
plotcp(m2)
m2$cptable
abline(v = 4, lty = "dashed")



#cautam cele mai bune valori pentru parametri minsplit si maxdepth
hyper_grid <- expand.grid(
  minsplit = seq(5, 20, 1),
  maxdepth = seq(8, 15, 1)
)
head(hyper_grid)
models <- list()
for (i in 1:nrow(hyper_grid)) {
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  models[[i]] <- rpart(
    formula = price ~. ,
    data = car_train,
    method = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
get_cp <- function(x) {
  min <- which.min(x$cptable[,"xerror"])
  cp <- x$cptable[min, "CP"]
}
get_min_error <- function(x) {
  min <- which.min(x$cptable[, "xerror"])
  xerror <- x$cptable[min, "xerror"]
}


mutated_grid <- hyper_grid %>%
  mutate(
    cp = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  )  
mutated_grid %>%
  arrange(error) %>%
  top_n(-5, wt=error)

optimal_tree <- rpart(
  formula = price ~ .,
  data = car_train,
  method = "anova",
  control = list(minsplit = 12, maxdepth = 9, cp = 0.1092)
)

pred <- predict(m1, newdata = car_test)
RMSE(pred = pred, obs = car_test$price)
optimal_tree
rpart.plot(optimal_tree)


#
library(ipred)
set.seed(123)
bagged_m1 <- bagging(
  formula = price ~ .,
  data = car_train, 
  coob = TRUE
)
bagged_m1

pred <- predict(bagged_m1, newdata = car_test)
RMSE(pred = pred, obs = car_test$price)

ntree <- 10:30
rmse <- vector(mode = "numeric", length = length(ntree))
for (i in seq_along(ntree)) {
  set.seed(123)
  model <- bagging(
    formula = price ~ .,
    data = car_train,
    coob = TRUE,
    nbagg = ntree[i]
  )
  rmse[i] = model$err
}
plot(ntree, rmse, type ="l", lwd=2)
abline(v=25, col = "red", lty="dashed")

bagged_best <- bagging(
  formula = price ~ .,
  data = car_train, 
  coob = TRUE,
  nbagg = 52,
  ns = 2200
)


pred <- predict(bagged_best, newdata = car_test)
RMSE(pred = pred, obs = car_test$price)

