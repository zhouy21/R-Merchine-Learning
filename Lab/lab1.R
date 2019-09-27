# Prepare environment
# install.packages("dplyr")
# install.packages("tidyr")
library(tidyr)
library(dplyr)
library(tidyverse) 
library(ggplot2) 
library(ggthemes) 
library(scales) 
theme_set(theme_bw())

# Loading the dataset
dd <- read_tsv("Lab/dataset/cogo-train.tsv")
str(dd)


### Describing the data ==================================================
## 1. What fraction of users use each browser? 
cogo_browsers <- dd %>% 
  summarise(
    p_chrome = mean(browser1), 
    p_firefox = mean(browser2), 
    p_safari = mean(browser3))

cogo_browsers

# rearrange the dataset: transform cols to rows
cogo_browsers <- cogo_browsers %>% 
  pivot_longer(
    c(p_chrome, p_firefox, p_safari),
    names_to="browser",
    values_to="p")

cogo_browsers

# plot the distribution of browsers using gglot2:
ggplot(cogo_browsers, aes(browser, p)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Browser") + 
  scale_y_continuous("% Cogo Users", label=percent_format())


## 2. What fraction of users use each device? 
cogo_devices <- dd %>% 
  summarise( 
    p_android = mean(device_type1), 
    p_apple = mean(device_type2), 
    p_other_mobile = mean(device_type3), 
    p_desktop = mean(device_type4)) %>% 
  pivot_longer(
    c(p_android,p_apple,p_other_mobile,p_desktop), 
    names_to = "device",
    values_to = "p")

ggplot(cogo_devices, aes(device, p)) + 
  geom_bar(stat="identity") + 
  scale_x_discrete("Device") + 
  scale_y_continuous("% Cogo Users", label=percent_format())

## 3. What fraction of users use more than one browser? 
# create new variables: 
dd %<>% 
  mutate(num_browsers = browser1 + browser2 + browser3) %>% 
  mutate(two_or_more_browsers = num_browsers > 1)

dd %>%
  summarise(mean(two_or_more_browsers))

## 4. What is the distribution of email open-rates? 
# plot a histogram
ggplot(dd, aes(p_open)) + 
  geom_histogram()

# plot a cumulative distribution
ggplot(dd, aes(p_open)) + 
  scale_y_continuous("Pr[p_open < Y]") + 
  scale_x_continuous("Open rate") + 
  stat_ecdf()


### Regression: ==========================================================
## 1. Creating irrelevant predictors: ir
dd %>% mutate(
  ir0=runif(n(),min=0,max=10000),
  ir1=(as.integer(user_id/10)%%10),
  ir2=(as.integer(user_id/10)%%100),
  ir3=(as.integer(user_id/10)%%1000),
  ir4=runif(n(),min=0,max=10000),
  ir5=runif(n(),min=0,max=10000),
  ir6=runif(n(),min=0,max=10000),
  ir7=runif(n(),min=0,max=10000),
  ir8=runif(n(),min=0,max=10000),
  ir9=runif(n(),min=0,max=10000)
  )

## 2. Train and test datasets
set.seed(810)

# split data into two sets:
# assign 100K random rows to the test set 
test_index <- sample(nrow(dd), 100000) 

# now split 
dd.test <- dd[test_index,] 
dd.train <- dd[-test_index,]


## 3. Linear regression:==================================================
str(dd)
f1 <- as.formula(p_open ~ browser1 + browser2 + browser3) 
f2 <- as.formula(p_open ~ num_browsers + age + gender)  

# extract the true response value
y.train <- dd.train$p_open 
y.test <- dd.test$p_open

# 1.Create Linear regression based on "dd.train"
fit.lm1 <- lm(f1, dd.train)

# 2.Predict open rate
yhat.train.lm1 <- predict(fit.lm1, dd.train) 

# 3.Calculate MSE
mse.train.lm1 <- mean((y.train - yhat.train.lm1)^2)

# 4.Test 
yhat.test.lm1 <- predict(fit.lm1, dd.test) 
mse.test.lm1 <- mean((y.test - yhat.test.lm1)^2)

# coefficent: coef()
coef(fit.lm1)


## use small training sample: =============================================
dd.train.small.sample.size <- 30 
dd.train.small.sample <- dd.train[sample(nrow(dd.train), dd.train.small.sample.size),]
y.train.small.sample <- dd.train.small.sample$p_open

fit.lm1.small <- lm(f1, dd.train.small.sample)
yhat.train.lm1.small <- predict(fit.lm1.small, dd.train.small.sample) 
mse.train.lm1.small <- mean((y.train.small.sample - yhat.train.lm1.small)^2)
mse.train.lm1.small # the MSE is smaller


## Importance of randomness: =============================================
# collect data with age>26
dd.train.nr <- dd.train[dd.train$age>26,] 
y.train.nr <- dd.train.nr$p_open

# create a random sample of the same size for comparison:
dd.train.r <- dd.train[sample(nrow(dd.train), nrow(dd.train.nr)),] 
y.train.r <- dd.train.r$p_open

# non-random data 
fit.lm2.nr <- lm(f2, dd.train.nr)

yhat.train.lm2.nr <- predict(fit.lm2.nr, dd.train.nr)  
mse.train.lm2.nr <- mean((y.train.nr - yhat.train.lm2.nr)^2)

yhat.test.lm2.nr <- predict(fit.lm2.nr, dd.test)   
mse.test.lm2.nr <- mean((y.test - yhat.test.lm2.nr)^2)

# random data
fit.lm2.r <- lm(f2, dd.train.r)

yhat.train.lm2.r <- predict(fit.lm2.r, dd.train.r)  
mse.train.lm2.r <- mean((y.train.r - yhat.train.lm2.r)^2)

yhat.test.lm2.r <- predict(fit.lm2.r, dd.test)  
mse.test.lm2.r <- mean((y.test - yhat.test.lm2.r)^2)

# Compare
mse.test.lm2.nr
mse.test.lm2.r





