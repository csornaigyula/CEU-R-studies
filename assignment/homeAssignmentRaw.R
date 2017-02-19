#If you decide to write an R Markdown document, 
#then include at least

#some exploratory data analysis on the available variables,
#feature engineering and enriching the flights dataset 
#(eg weekday or grouped hour of the day),
#a model predicting if a flight will be 
#late by more than 15 minutes at the destination.

rm(list=ls())
data(package = 'nycflights13')
library(nycflights13)
library(pander)
str(flights)
pander(summary(flights))
dim(flights)
#exploring why we have NAs 

#in departure delay
dim(subset(flights, flights$dep_delay == 0))
#we do have 0 values, so NA is unknown
dim(subset(flights, flights$arr_delay == 0))


#we will lose 9430 rows with dropping NAs, which in the case of this
#dataset size is an acceptable loss
lostRows <-dim(flights)[1] - dim(subset
    (flights, !is.na(flights$dep_time) &
             !is.na(flights$dep_delay) &
             !is.na(flights$arr_delay) &
             !is.na(flights$dep_time) &
             !is.na(flights$arr_time)
           )
)[1]

df <- subset (flights, !is.na(flights$dep_time) &
    !is.na(flights$dep_delay) &
    !is.na(flights$arr_delay) &
    !is.na(flights$dep_time) &
    !is.na(flights$arr_time))

head(df$tailnum)
head(df$carrier)
head(df$origin)
head(df$dest)
df$tailnumf <- as.factor(df$tailnum)
df$carrierf <- as.factor(df$carrier)
df$originf <- as.factor(df$origin)
df$destf <- as.factor(df$dest)
#it is highly possible, that tailnum will not be used 
#effectively for prediction, as the factor count is too high

dfm <- df
dfm$tailnum <- NULL
dfm$carrier <- NULL
dfm$dest <- NULL
dfm$origin <- NULL
dfm$tailnumf <- NULL

#year will not have any predictive power, as all data is from 2013
summary(dfm$year)
dfm$year <- NULL
?strptime
time_format <- "%Y-%m-%d %H:%M:%S"
dfm$weekday <- as.factor(format(strptime(dfm$time_hour, format=time_format),"%A"))

summary(subset(dfm, dfm$dep_time > 959))
summary(subset(dfm, dfm$dep_time < 959))
dfm$deph <- dfm$dep_time %/%100
dfm$depm <- dfm$dep_time%%100

dfm$dep_time <- NULL
dfm$time_hour <- NULL

dfm$sch_ah <- dfm$sched_arr_time%/%100
dfm$sch_am <- dfm$sched_arr_time%%100
dfm$sched_arr_time <- NULL
#hour and minute already has this info
dfm$sched_dep_time <- NULL
dfm$arrh <- dfm$arr_time%/%100
dfm$arrm <- dfm$arr_time%%100
dfm$arr_time <- NULL

#dep_delay
summary(dfm$dep_delay)

#dfm$schdeph <- dfm$sched_dep_time %/%100
#summary(dfm$hour - dfm$schdeph)
#?flights
#dfm$schdeph <- NULL

##charts here
library(ggplot2)
ggplot(dfm)+aes(x=arr_delay)+
  geom_histogram(bins=50, fill='indianred')+
  labs(
    x="Distribution of arrival delay minute values - bins=50",
    y="Count on logarythmic scale",
    title="Histogram of arrival delays"
  )+
  theme_bw()

ggplot(dfm)+aes(x=dep_delay)+
  geom_histogram(bins=50, fill='dodgerblue3')+
  labs(
    x="Distribution of departure delay minute values - bins=50",
    y="Count on logarythmic scale",
    title="Histogram of departure delays"
  )+
  scale_y_log10()+
  theme_bw()

mypalette <- 'Dark2'
ggplot(dfm)+aes(x=dfm$air_time, y=dfm$arr_delay, col=weekday)+
  geom_point()+
  labs(
    x="Air time",
    y="Delays on the departure side",
    title="Dependency between air time and delay on departure side - given weekdays"
  )+
  geom_smooth(methd="lm", col='black')+
  scale_fill_brewer(mypalette)+
  theme_bw()


summary(dfm$arr_delay)
ncolor <- 8
cuts<- split(dfm$arr_delay, cut_number(dfm$arr_delay, ncolor))
str(cuts)
for (i in 1:ncolor) {
  dfm$adcat[sapply(cuts[i], is.element, el = dfm$arr_delay) ]<-i
}
cuts
summary(dfm$adcat)
dfm$adcatf <- as.factor(dfm$adcat)
ggplot(dfm) + geom_tile(aes(x=hour, y=weekday, fill=adcat)) + 
  labs(x="Hour of the day", y="Day of the week",
       title="Flight delay heatmap")+
  scale_fill_distiller(palette = "Spectral")+
  theme_bw()

dfm$adcatf<- NULL
dfm$adcat <- NULL


dfm$carrierfN <- as.numeric(dfm$carrierf)
dfm$originfN <- as.numeric(dfm$originf)
dfm$destfN <- as.numeric(dfm$destf)
dfm$wdN <- as.numeric(dfm$weekday)
dfm$carrierf <-NULL
dfm$originf <- NULL
dfm$destf <- NULL
dfm$weekday <- NULL
dfm$isMT15 <- ifelse(dfm$arr_delay > 15,1,0)
dfm$arr_delay <- NULL

dfm$rnd <-runif(dim(dfm[1])) 
dfm <- dfm[order(dfm$rnd),]
train <- dfm[0:round((dim(dfm[,1]))*0.7),]
test <- dfm[(round((dim(dfm[,1]))*0.7)+1):(dim(dfm)[1]),]
dfm$rnd <-NULL
#summary(train)
library(class)
fit2 <- knn(train[,1:18], test[,1:18], train$isMT15, k = 2)

pander(table(test$isMT15,fit2))

fit10 <- knn(train[,1:18], test[,1:18], train$isMT15, k = 10)
pander(table(test$isMT15,fit10))

?knn

table(test$isMT15,fit10)[1,1]

str(test)
str(fit10)
