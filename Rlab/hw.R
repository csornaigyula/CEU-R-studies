library(ggplot2)
str(mtcars)
#number of carb
ggplot(mtcars)+aes(x=carb)+geom_bar()

#horsepower by carb
rm(list=ls())
getwd()
df <- mtcars
#boxplot
ggplot(df)+aes(x=carb, y=hp)+geom_boxplot()

#barplot
ggplot(df)+aes(x=carb/gear)+geom_bar()
#hp wg per carb
summary(df)
ggplot(df)+aes(x=wt, y=hp, size=carb)+geom_point()

#with trendline
?geom_smooth
ggplot(df)+aes(x=wt, y=hp, size=carb)+
  geom_point()+
  geom_smooth(method = 'lm', se = FALSE )

library(hflights)
library(data.table)
#number of flights to LAX

dt <- data.table(hflights)
dt[Dest=='LAX',.(laxsum = .N)]

#shortest flight per weekday
str(dt)
dt[,dayOfWeek := weekdays(
  #after as.Date it becomes real date
  as.Date(
    #concatenante, this looks like a date
    paste(Year, Month, DayofMonth, sep= "-")
  ))]
dt[DayOfWeek!=6&DayOfWeek!=7 , list(minTime = min(ActualElapsedTime, na.rm = TRUE)),by=list(DOW= dayOfWeek)]

summary(d)
#average delay per dest, origin
dt[, list(avgDelay = mean(ArrDelay, na.rm = TRUE)), by=list(Origin,Dest)]

dys <- dt[, list(
  avgArrDelay = mean(ArrDelay, na.rm = TRUE),
  avgDepDelay = mean(DepDelay, na.rm = TRUE)), by=list(Dest)]

library(ggrepel)
ggplot(dys)+
  aes(x=avgDepDelay, y=avgArrDelay)+
  geom_point() +
  geom_text_repel(aes(label=Dest))

#cancelled flight ratio
dys <- dt[, list(
  cancelPCT = sum(Cancelled)*100/ .N), by=list(Dest)]

library(nycflights13)

nf <- data.table(flights)
str(nf)
#number of flights to LAX
nf[dest=='LAX',list(num = .N)]

#number of flights from lax to JFK
nf[dest=='LAX'&origin=='JFK',list(num = .N)]

#average delay from LAX to JFK
nf[dest=='LAX'&origin=='JFK',list(avgDelay = mean(dep_delay,  na.rm = TRUE))]

#which destination has lowest avg delay
delays<-nf[origin=='JFK',list(avgDelay = mean(dep_delay,  na.rm = TRUE)),by=.(dest)]
delays <- delays[order(delays$avgDelay)]
delaysp[1,]
min(delays$avgDelay)


#average delay per destination
ggplot(delays)+aes(x=dest, y=avgDelay)+geom_col()+geom_text_repel(aes(label=dest))

#distribution of delays
#average delay per destination
ggplot(delays)+aes(x=avgDelay)+geom_histogram(bins=20)

#new variable day of week
nf[,dayOfWeek := weekdays(
  as.Date(
    paste(year, month, day, sep= "-")
  ))]

nnf<-nf[,list(numberPerWD = .N),by=list(WD=dayOfWeek)]

#number of flights per weekday
ggplot(nnf)+aes(x=WD,y=numberPerWD)+geom_col()

#heatmap
hnnf<-nf[,list(numberPerHWD = .N),by=list(WD=dayOfWeek,HH=hour)]

colors <- colorRampPalette(c( "green", "yellow", "red"))(
  length(unique(hnnf$numberPerHWD))
)
hnnf$flightNoFactor <- factor(hnnf$numberPerHWD)
N <- as.numeric(nlevels(hnnf$flightNoFactor))

ggplot(hnnf)+
  geom_tile(aes(x=WD, y=HH, fill = flightNoFactor))+
  geom_text(aes(x=WD, y=HH, label = numberPerHWD),col='black')+
  scale_fill_manual(values=colors, breaks=levels(hnnf$flightNoFactor)[seq(1, N, by=3)])+
  labs(x="Day of week", y="Hour of day", title="Average fligth number from JFK")
  