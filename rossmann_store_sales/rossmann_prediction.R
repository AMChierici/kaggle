
require(data.table)
require(ggplot2)

train <- fread('data/train.csv')
test <- fread('data/test.csv')
store <- fread('data/store.csv')

# Explore -----------------------------------------------------------------

summary(train)
# is date a character?
str(train)
# convert to data
train[, Date:=as.Date(Date, "%Y-%m-%d")]
summary(train)

# convert for test too
summary(test)
test[, Date:=as.Date(Date, "%Y-%m-%d")]
summary(test)
# test includes only sales from 1/8/2015 to 17/9/2015 (out of sample test)

# check if there is a pattern in the same period for the past years
checkpattern <- train[Date>=as.Date("2013-08-01", "%Y-%m-%d")
                      & Date<=as.Date("2013-09-17", "%Y-%m-%d")
                      | Date>=as.Date("2014-08-01", "%Y-%m-%d")
                      & Date<=as.Date("2014-09-17", "%Y-%m-%d"), ]
checkpattern <- checkpattern[, sum(Sales), by=Date]
checkpattern[, Year:=year(Date)]
# date already sorted, define time (in days from 01-01)
checkpattern[, time:=as.numeric(Date - as.Date(paste0(Year, "-01-01"), "%Y-%m-%d"))]
setnames(checkpattern, 'V1', 'Sales')
qplot(time, Sales, data=checkpattern, color=as.factor(Year), geom='line')

# let's see overall trends
train[, Year:=year(Date)]
train[, Week:=week(Date)]
train[, CalDay:=as.numeric(substr(Date, 9, 10))]
# Define time in days since beginning of year
train[, TimeBY:=as.numeric(Date - as.Date(paste0(Year, "-01-01"), "%Y-%m-%d"))]
# Abs time
train[, AbsTime:=time(Date)]
# let's see daily sale per year
summ <- train[, sum(Sales), by=Date]
summ[, Year:=year(Date)]
setnames(summ, 'V1', 'Sales')
summ[, time:=as.numeric(Date - as.Date(paste0(Year, "-01-01"), "%Y-%m-%d"))]
qplot(time, Sales, data=summ, color=as.factor(Year), geom='line')

train[TimeBY>310 & TimeBY<=320]
#looks like there is an effect of promo and week no. and day no. are not aligned.

# let's see by exposure measure - open store and weekly average. Check if we have to aggr by other dims such as Yr, week, day, etc.
summ_sales <- train[, sum(Sales), by=Date]
setnames(summ_sales, 'V1', 'Sales')
setkey(summ_sales, Date)
summ_stores <- train[, sum(Open), by=Date]
setnames(summ_stores, 'V1', 'StoresOpen')
setkey(summ_stores, Date)
summ <- summ_stores[summ_sales]
summ[, Year:=year(Date)]
summ[, time:=as.numeric(Date - as.Date(paste0(Year, "-01-01"), "%Y-%m-%d"))]
summ[, AdjSales:=Sales/StoresOpen]
qplot(time, AdjSales, data=summ, color=as.factor(Year), geom='line')
qplot(time, AdjSales, data=summ, color=as.factor(Year), geom='line', xlim=c(259,212))
# chech phase, how to make the time series on the same phase

summ <- train[ , sum(Sales), by=list(Year, Week)]
setnames(summ, 'V1', 'Sales')
qplot(Week, Sales, data=summ, color=as.factor(Year), geom='line')
qplot(TimeBY, Sales, data=train[Store==1 & Sales!=0], color=as.factor(Year), geom='line', xlim=c(300,400))

# check store data
store <- fread("data/store.csv")
length(unique(c(train$Store, test$Store)))
sum(!(unique(train$Store) %in% unique(test$Store)))
# there are 259 stores in Train that are not in test
sum(!(unique(test$Store) %in% unique(train$Store)))
# all stores in test are in train.
summary(store)
summary(store[Promo2==1]) #OK, this means that NAs in Promo2SinceWeek, Promo2SinceYear, PromoInterval are when Promo is OFF.


# Data munging -------------------------------------------------------------------
# deal with missing or NAs
# not much rationale on competition distance an open since, just use the median
store[is.na(CompetitionDistance), CompetitionDistance:=median(store$CompetitionDistance, na.rm=TRUE)]
store[is.na(CompetitionOpenSinceMonth), CompetitionOpenSinceMonth:=median(store$CompetitionOpenSinceMonth, na.rm=TRUE)]
store[is.na(CompetitionOpenSinceYear), CompetitionOpenSinceYear:=median(store$CompetitionOpenSinceYear, na.rm=TRUE)]
# promo2 variables treated as factors, any ordering should not really matter. Though one thing to ecplore in feature engineering is calculate some adjustment factors...
store[Promo2==0, Promo2SinceWeek:="NoPromo2"]
store[Promo2==0, Promo2SinceYear:="NoPromo2"]
store[Promo2==0, PromoInterval:="NoPromo2"]
store[, Promo2SinceWeek:=as.factor(as.character(Promo2SinceWeek))]
store[, Promo2SinceYear:=as.factor(as.character(Promo2SinceYear))]
store[, PromoInterval:=as.factor(PromoInterval)]
setkey(store, Store)
setkey(train, Store)
setkey(test, Store)
sum(!(train$Store %in% store$Store))
train <- store[train]
sum(!(test$Store %in% store$Store))
test <- store[test]

#feature engineering
train[, Year:=year(Date)]
train[, Week:=week(Date)]
train[, CalDay:=as.numeric(substr(Date, 9, 10))]
# Define time in days since beginning of year
train[, TimeBY:=as.numeric(Date - as.Date(paste0(Year, "-01-01"), "%Y-%m-%d"))]
# Abs time
train[, AbsTime:=time(Date)]

# Train -------------------------------------------------------------------
# There are three approaches that can be tried: 1) a time series model(one model per store?); 2) Algo that trys to abstract everything (RF); 3) Blend both. I will start with 2).



# Test --------------------------------------------------------------------


# Optimise ----------------------------------------------------------------


# Train all and submit ----------------------------------------------------


