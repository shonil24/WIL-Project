## https://stats.oecd.org/index.aspx?queryid=33940
## https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series

library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
library(caret) # to dummy encode

covid <- read_csv("C:/Users/winuser/Downloads/coronavirus.csv")
head(covid)

##### Tidying the COVID dataset before joining as per the need

# checking for total null values
sapply(covid, function(x) sum(is.na(x)))

# dropping not needed columns
covid <- select(covid, -matches("province|lat|long"))
head(covid)

# total days with negative numbers reported
count <- covid[covid$cases < 0, ]
count %>% summarise(total_negative_cases = n())

# checking format of negative cases with respect to types
covid %>% filter(date == "2020-10-08") %>% filter(country == "Angola")
covid %>% filter(date == "2020-08-28") %>% filter(country == "Angola")

### Dealing negative cases after tidying data
# Confirmed cases have negative values which means no of patients tested and found not confirmed so those values would be 0.
# Recovered cases have negative values which means no of patients tested again could be covid positive (confirmed)
# Death cases have negative values which means it could be possible that negative deaths are patients who are not dead are still with covid severe condition(confirmed)

# Changing data types to factor, dates and numeric
covid$date <- as.Date(covid$date)
covid$country <- as.factor(covid$country)
covid$type <- as.factor(covid$type)
covid[, 4] <- sapply(covid[, 4], as.numeric)

# checking the data types
sapply(covidW, class)

# Checking if factors are labeled correctly
levels(covid$country)
levels(covid$type)

#check structure and attributes
str(covid)

# checking attributes. 
attributes(covid)

## Since dates are used in covid dataset sub setting the dates into quarterly for GDP dataset
# sub setting data quarterly
covid$quarter <- quarters(covid$date)
head(covid)

#---- Wider format
# spreading the cases type and mutating active column
covidW <- covid %>% 
  select(country, type, cases, quarter) %>% 
  group_by(country, type, quarter) %>% 
  summarise(cases = sum(cases)) %>% 
  spread(key = type, value = cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  arrange(country) 

head(covidW)

# active cases are negative. This happens because the past active patients might have been recovered in present/future and so recovered counts are higher. 
# subtracting previous to current active cases.
covidW %>% 
  mutate(active = abs((active) - lag(active, default = first(country))))

# changing quarter to factor
covidW$quarter <- as.factor(covidW$quarter)

boxplot(covidW$confirmed)$out

co <- boxplot(covidW$confirmed)$out

covidW[which(covidW$confirmed %in% co),]

boxplot(which(covidW$confirmed %in% co))

cap <- function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(0.05, 0.25, 0.75, 0.95))
    iqr <- IQR(x[,i])
    x[x[,i] < quantiles[2] - 1.5*iqr, i] <- quantiles[1]
    x[x[,i] > quantiles[3] + 1.5*iqr, i] <- quantiles[4]
    }
  x}

cap <- function(x){
    quantiles <- quantile( x, c(0.05, 0.25, 0.75, 0.95))
    iqr <- IQR(x)
    x[x < quantiles[2] - 1.5*iqr] <- quantiles[1]
    x[x > quantiles[3] + 1.5*iqr] <- quantiles[4]
  x}

# Replacing extreme values with percentiles using cap function
covidL = cap(covidW$confirmed)

covidW

# checking outliers
boxplot(covidW$confirmed)

### Tidying the GDPWORLD dataset before joining as per the need

GDPWorld_un <- read_csv("C:/Users/winuser/Downloads/QNA_19102020172848076.csv")
head(GDPWorld_un)

# checking for total null values
sapply(GDPWorld_un, function(x) sum(is.na(x)))

# Dropping unnecessary columns including columns with null values
GDPWorld_un <- select(GDPWorld_un, matches("Country|MEASURE|TIME|Value", ignore.case = FALSE))
head(GDPWorld_un)

# converting column headers to lower case
names(GDPWorld_un) <- tolower(names(GDPWorld_un))
head(GDPWorld_un)

# filtering only 2020 related entries
GDPWorld_un <- GDPWorld_un %>% 
  filter(grepl("2020-",time)) %>% 
  arrange(country)

# now changing it into quarter format
GDPWorld_un$time <- substr(GDPWorld_un$time, 6, 7)
head(GDPWorld_un)

# changing the time column name to quarter
names(GDPWorld_un)[names(GDPWorld_un) == "time"] <- "quarter"
head(GDPWorld_un)

# Since there are 2 measures spreading the measure values
# GPSA = Economic growth rate compared to previous quarter
# GYSA = Economic growth rate compared to same quarter of the previous year
GDPWorld <- GDPWorld_un %>% 
  select(country, measure, quarter, value) %>% 
  group_by(country, quarter) %>% 
  spread(key = measure, value = value)

# Changing data types to factor, dates and numeric
GDPWorld$quarter <- as.factor(GDPWorldquarter)
GDPWorld$country <- as.factor(GDPWorld$country)
GDPWorld$GPSA <- as.numeric(GDPWorld$GPSA)
GDPWorld$GYSA <- as.numeric(GDPWorld$GYSA)

# checking the data types
sapply(GDPWorld, class)

# Checking if factors are labeled correctly
levels(GDPWorld$country)
levels(GDPWorld$quarter)

#check structure and attributes
str(GDPWorld)

# checking attributes. 
attributes(GDPWorld)

# checking the structure of both data frames
head(covidW)
head(GDPWorld)

### Merging the data sets
World <- inner_join(covidW, GDPWorld, by = c("country" = "country", "quarter" = "quarter"))
head(World) 

### Data sets are merged successfully

# detecting outliers. Found in 4 types of cases
boxplot(World$confirmed)

# detecting outliers in GPSA and GYSA. No outliers found
boxplot(World$GPSA)
boxplot(World$GYSA)

# Recommended transformation to correct for right skew include:
# Log transformation
# Natural log transformation
# Square root transformation
# Reciprocal transformation

# Normal histogram
hist(World$confirmed)

# Using log transformation to check histogram for normalization. 
# Since most rows are excluded transformation wouldn't affect the underlying structure
hist(log(World$confirmed))

hist(log10(World$confirmed))

hist(1/(World$confirmed))

hist(sqrt(World$confirmed))

# Normal histogram for measures. Normalization is not applied since it contains negative GDP values.
hist(World$GPSA)

## taking all combos
WorldF <- full_join(covidW, GDPWorld, by = c("country" = "country", "quarter" = "quarter"))
head(WorldF)

## If taken first dataset attributes they correlated and same goes to second dataset.
## Merging and checking columns between 2 data sets with each other doesn't work
World[sapply(World, is.numeric)] %>% 
  cor(method = "pearson", use = "complete.obs")

dmy <- dummyVars(" ~ .", data = World)
world <- data.frame(predict(dmy, newdata = World))
 
head(world)

# Sample the dataset. The return for this is row nos.
set.seed(1)
row.index <- sample(1:nrow(world), 0.8*nrow(world))
train = world[row.index, ]
test = world[-row.index, ]

X_test <- test[ , !(colnames(test) %in% c("GPSA"))]
y_test <- test$GPSA

X_train <- train[ , !(colnames(train) %in% c("GPSA"))]
y_train <- train$GPSA

# Let's make default model.
model1 = lm(GPSA~., data = train)
summary(model1)
par(mfrow=c(2,2))
plot(model1)

pred1 <- predict(model1, newdata = test)
RMSE(pred1, test$GPSA)
R2(pred1, test$GPSA)

#### Wont work. Lets do on GDP

# dmy1 <- dummyVars(" ~ .", data = GDPWorld)
# gdpW <- data.frame(predict(dmy1, newdata = GDPWorld))
# 
# set.seed(2)
# row.index1 <- sample(1:nrow(gdpW), 0.8*nrow(gdpW))
# train = gdpW[row.index1, ]
# test = gdpW[-row.index1, ]
# 
# X_test <- test[ , !(colnames(test) %in% c("GPSA"))]
# y_test <- test$GPSA
# 
# X_train <- train[ , !(colnames(train) %in% c("GPSA"))]
# y_train <- train$GPSA
# 
# #Let's make default model.
# model1 = lm(GPSA~., data = train)
# summary(model1)
# par(mfrow=c(2,2))
# plot(model1)
# 
# model2 = lm(GPSA ~ country.India + country.Indonesia + quarterQ2 + GYSA, data = train)
# summary(model2)
# par(mfrow=c(2,2))
# plot(model2)
# 
# pred1 <- predict(model2, newdata = test)
# rmse <- sqrt(sum((exp(pred1) - test$GPSA)^2)/length(test$GPSA))
# c(RMSE = rmse, R2=summary(model2)$r.squared)
# 
# ## rmse will deviate 7.89 from actual
# par(mfrow=c(1,1))
# plot(test$GPSA, exp(pred1))

##------------------------------------

# set.seed(1)
# row.index <- sample(1:nrow(World), 0.8*nrow(World))
# train = World[row.index, ]
# test = World[-row.index, ]
# 
# X_test <- test[ , !(colnames(test) %in% c("GPSA"))]
# y_test <- test$GPSA
# 
# X_train <- train[ , !(colnames(train) %in% c("GPSA"))]
# y_train <- train$GPSA
# 
# knnModelW <- train(GPSA ~ ., data = train, method = "knn")
# WorldPre <- predict(knnModelW, test) 

# would only work on classification
# confusionMatrix(
#   as.factor(WorldPre), 
#   as.factor(test$GPSA)
#   )$overall['Accuracy']

# error1 <- (WorldPre - test$GPSA)
# RMSE_Model <- sqrt(mean(error1^2))

# # (a) RMSE
# RMSE(WorldPre, test$GPSA)
# # (b) R-square
# R2(WorldPre, test$GPSA)
# 
# summary(knnModelW)
