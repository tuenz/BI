install.packages("readxl")
library(readxl)

dat <- read_excel(path = "CarPrediction.xlsx")
?read_excel
dat
dat[12:18]1
sapply(dat, class)

cars <- dat[,-1]
cars

find_na <- function(data){
  sum =0
  for(i in 1:nrow(data)){
    for (j in 1:ncol(data))
    {
    sum<-sum+is.na(cars[i,j])
    }
  }
  print(sum)
}

find_na(cars)

hist(cars$price, freq=F)
lines(density(cars$price))

install.packages("psych")
correl <- round(cor(cars), 2)
correl[,ncol(correl)]

cars_clean <- cars[,c(5:6,8,10:11,13, 15:17)]
psych::pairs.panels(cars_clean)

cars_sc <- scale(cars_clean)
cars_sc
class(cars_sc)


train <- 1:140
test <- 141:(nrow(cars_sc))

model <- lm(price ~ horsepow-er+enginesize+carwidth+carlength+curbweight+cylindernumber, data = as.data.frame(cars_sc[train,]))
summary(model)
pred <- predict(model, cars_clean[test,])
cor(pred, cars_clean$price[test])

install.packages("usdm")
install.packages("car")
library(car)
car::vif(model)

model <- lm(price ~ horsepower+enginesize+carwidth, data = as.data.frame(cars_sc[train,]))
summary(model)
pred <- predict(model, cars_clean[test,])
cor(pred, cars_clean$price[test])
car::vif(model)

model2 <- lm(price ~ horsepower+enginesize+carwidth+0, data = as.data.frame(cars_sc[train,]))
summary(model2)
pred <- predict(model2, cars_clean[test,])
cor(pred, cars_clean$price[test])
car::vif(model2)

anova(model2,model)

car::qqPlot(model2, simulate = TRUE)








