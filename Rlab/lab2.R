rm(list=ls())

# CHECK WORKING DIRECTORY

getwd()
df <- read.csv('http://bit.ly/BudapestBI-R-csv')
str(df)

 #TODO 5th responder's weight
weigth5th <- df[5,5]
weigth5th

 #TODO what is minimum weight
minWeight <- min(df[,5])
minWeight

#TODO whai is the minimum weight 
males <- subset(df,sex=='m')
minWeightsMales <- min(males[,5])
minWeightsMales
#hint: 1liner
min(subset(df,sex=='m')$weightLb)

#TODO what is the weight of the tallest man
tallestStats <- (subset(males, heightIn == max(males[,4])))
dim(tallestStats)
tallestStats$weightLb
(subset(males, heightIn == max(males[,4])))$weightLb

#hint: rowindex
which(df$heightIn == tallestStats$heightIn)

df$height <- df$heightIn*2.54
df$weight <- df$weightLb * 0.45
df$bmi <- df$weight/(df$height/100)^2

gyulaBMI <- 80/(177/100)^2

plot(df$ageYear,df$bmi, col='darkorchid4')
abline(lm(df$bmi~df$ageYear), col='indianred')

fit <- lm(df$bmi~df$ageYear)
summary(fit)


#Summary stats better
summary(df)
install.packages("pander")
library(pander)
pander(summary(df))

#TASK: what is avg BMI per age
df$year <- round(df$ageYear)
agga <- aggregate(bmi~year,FUN=mean, data=df)
plot(round(agga$year),agga$bmi, col='darkorchid4')

#NEW STUFF histogram
hist(df$bmi)
abline(v = c(18.5,25), col='orangered1')

#boxplot
# box: 1-3Q, 95%, outliers
boxplot(df$bmi)
boxplot(bmi~sex,df)

#beanplot
#mixture of boxplot and histogram
install.packages("beanplot")
library(beanplot)
beanplot(df$bmi ~ df$sex)

par(mfrow = c(1,2))


str(df)
table(df$sex)
pie(table(df$sex))
#frequency
barplot(table(df$sex))
dotchart(table(df$sex))

#coolest game changer
pairs(df, col='dodgerblue3')

##########################################################
##GGPLOT
##########################################################
#install.packages('ggplot2')
library(ggplot2)
#install.packages('lazyeval')
library(lazyeval)
#built in dataset for example
?diamonds
#pairs(diamonds)
#histogram with ggplot
ggplot(diamonds, aes(x = cut)) + geom_bar()
p <- ggplot(diamonds, aes(x = cut)) 
pbar <- p+geom_bar()
pbar <- pbar + theme_bw()
pbar

#1 basic var histogram transformations
ggplot(diamonds, aes(x = cut)) + 
#new fill
  geom_bar(colour='yellow',fill='firebrick') +
# flip axes
  coord_flip() +
# different scale
  scale_y_reverse()

#2d classification in 2d
ggplot(diamonds, aes(x = carat, y=price)) +
  geom_point()

#3d classification in 2d
ggplot(diamonds, aes(x = carat, y=price,
                     color=cut)) +
  geom_point()

#4d classification in 2d
ggplot(diamonds, aes(x = carat, y=price,
                     color=cut,
                     shape=color)) +
  geom_point()
#warning comes, ggplot by default has 6 shapes only, let's flip and 
ggplot(diamonds, aes(x = carat, y=price,
                     color=color,
                     shape=cut)) +
  geom_point()

#let's generate a new plot for every single category in clarity
#5 vars on 1 plot
ggplot(diamonds, aes(x = carat, y=price,
                     color=color,
                     shape=cut)) +
  geom_point() +
  facet_wrap(~clarity)

#add theme
ggplot(diamonds, aes(x = carat, y=price,
                     color=color,
                     shape=cut)) +
  geom_point() +
  facet_wrap(~clarity) + 
  theme_bw()


#histogram on price
ggplot(diamonds,aes(x=price)) +
  geom_histogram(binwidth = 100, colour='deepskyblue2',fill='aliceblue')+
  theme_dark()
  
#extra
  ggplot(diamonds,aes(x=price)) +
    geom_histogram(binwidth = 100, colour='deepskyblue2',fill='aliceblue')+
    facet_wrap(~cut) +
    theme_dark()
#kernel density +cut --> same to 1 chart
  ggplot(diamonds,aes(x=price, fill=cut)) + 
    geom_density(alpha=0.3)
#heatmap on cut and color
ggplot(diamonds,aes(x=cut, y=color)) +
    geom_bin2d()
#heatmap - although this does not work without trafo
ggplot(diamonds,aes(x=cut, y=color, fill=price))+
  geom_tile()
#scatterplot on x,y,z
ggplot(diamonds,aes(x=x, y=y, 
                    color=z)) +
  geom_point()+
  theme_bw()


#slow: how we can fix: how many points in one area
install.packages('hexbin')
library(hexbin)
ggplot(diamonds,aes(x=x, y=y)) + geom_hex()

#fit model carat and price
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point() + 
  geom_smooth()


#fit model carat and price - different models
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE)

#fit model carat and price - different models, different cuts, different fits
ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() + 
  geom_smooth(method='lm', se=FALSE)

ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point() + 
  geom_smooth()
#warning! not the same, one fit only
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) + 
  geom_smooth()
#2 models on same chart
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) + 
  geom_smooth()+
  geom_smooth(method='lm')

#orgasm
ggplot(diamonds, aes(x=carat, y=price)) +
  geom_point(aes(color=cut)) + 
  geom_smooth()+
  geom_smooth(method='lm')+
  facet_wrap(~color)


p <- ggplot(diamonds, aes(x=carat, y=price, color=cut)) +
  geom_point()
#install.packages('ggthemes')
library(ggthemes)
p + theme_economist() 
#Error in loadNamespace(i, c(lib.loc, .libPaths()), versionCheck = vI[[i]]) : 
#  namespace 'ggplot2' 2.1.0 is already loaded, but >= 2.2.0 is required
#In addition: Warning message:
#  package 'ggthemes' was built under R version 3.3.2 
#Error: package or namespace load failed for 'ggthemes'

#install.packages('GGally')
library(GGally)

ggpairs(diamonds)

#multiorgasm
install.packages('pairsD3')
library(pairsD3)




#DATA GRABBING
#install.packages('XML')
library(XML)
df <- readHTMLTable(readLines('https://en.wikipedia.org/wiki/FTSE_100_Index'),
                              which=2,header=TRUE, stringsAsFactors=FALSE)
str(df)
dim(df)
names(df)
names(df)[4] 
names(df)[4] <- 'CAP'
str(df)

#Employee column
#check which company has lowest, highest
#banking sector

df$emp <- as.numeric(gsub(",", "", df$Employees))
library(ggplot2)
df$emp
range(df$emp)

#minmax in banking sector
range((subset(df, Sector='Banking'))$emp)
aggregate(emp~Sector, FUN=min, data=df)


#visualize the employees and cap per sector
ggplot(df,aes(x=emp, y=CAP, color=Sector))+
  geom_point()

#visula companies in banking 
#by market and employees
banks <- subset(df, Sector == 'Banking')
ggplot(banks, aes(x=emp, y=CAP)) +
  geom_text(aes(label=Company))

install.packages('ggrepel')
library(ggrepel)
ggplot(df, aes(x=emp, y=CAP)) +
  geom_text_repel(aes(label=Company))

install.packages('data.table')
install.packages('hflights')
library(data.table)
library(hflights)

str(hflights)
dt <- data.table(hflights)
str(dt)
#lists only first some and last sone, not all 200k lines
dt
dt[DayOfWeek ==5 & Month==3]
#dt[i,j, by=....]
dt[DayOfWeek ==5 & Month==3, ActualElapsedTime]
dt[DayOfWeek ==5 & Month==3 & Cancelled==FALSE, list(ActualElapsedTime,AirTime)]
dt[DayOfWeek ==5 & Month==3 & Cancelled==FALSE, 
   mean(ActualElapsedTime, na.rm = TRUE),
   by=Origin]
#average time to LA
dt[Dest=='LAX', 
   mean(ActualElapsedTime, na.rm = TRUE),by=Origin]

#average time to any
dt[, 
   mean(ActualElapsedTime, na.rm = TRUE),by=Dest]

dt[, 
   list(
     avgElapsedTime = mean(ActualElapsedTime, na.rm = TRUE),
     minElapsedTime = min(ActualElapsedTime, na.rm = TRUE),
     maxElapsedTime = max(ActualElapsedTime, na.rm = TRUE),
     stdevElapsedTime = sd(ActualElapsedTime, na.rm = TRUE)
     ),by=list(Destination = Dest)]

dt[, 
   .(
     avgElapsedTime = mean(ActualElapsedTime, na.rm = TRUE),
     minElapsedTime = min(ActualElapsedTime, na.rm = TRUE),
     maxElapsedTime = max(ActualElapsedTime, na.rm = TRUE),
     stdevElapsedTime = sd(ActualElapsedTime, na.rm = TRUE)
   ),by=.(Destination = Dest)]

#number of cancelled flights to LAX
str(dt)

dt[Dest == 'LAX' & Cancelled == 1,.(cnt = sum(Cancelled))]
dt[Dest == 'LAX' & Cancelled == 1,.N]
#sum of miles travelled to LAX
dt[Dest == 'LAX' & Cancelled != 1,.(sumMiles = sum(Distance, na.rm = TRUE))]
dt[, distanceKM := Distance *1.6]
dt[Dest == 'LAX' & Cancelled != 1,.(sumKM = sum(distanceKM, na.rm = TRUE))]
#percentage of flights cancelled by destination
dt[ Cancelled == 1,.(cnt = sum(Cancelled)),by=Dest]
dt[ Cancelled == 1,.(cnt = sum(Cancelled)),by=.(Dest,Origin)]

dt[ ,.(pctCancelled = sum(Cancelled*100) / .N)  ,by=Dest]
dt[ ,.(pctCancelled = mean(Cancelled*100))  ,by=Dest]
dt[, .(sum(Cancelled==1) *100/ .N), by=Dest]


#plot departure and arrival delays
ggplot(dt,aes(x=ArrDelay, y=DepDelay))+geom_point()
ggplot(dt,aes(x=ArrDelay, y=DepDelay))+geom_hex()

#by dest
ggplot(dt,aes(x=ArrDelay, y=DepDelay, color=Dest))+geom_point()

dly <- dt[,.( avgAD = mean(ArrDelay, na.rm = TRUE), 
              avgDD = mean(DepDelay, na.rm = TRUE)), by=Dest]
dly

ggplot(dly,aes(x=avgAD, y=avgDD))+
  geom_point() +
  geom_text_repel(aes(label=Dest))



  

#visualize avg flight time per dest
avf <- dt[ Cancelled != 1,.(avgFlight = mean(AirTime, na.rm = TRUE)),by=Dest]
ggplot(avf,aes(x=Dest, y=avgFlight))+
  geom_col() + 
  geom_text_repel(aes(label=Dest))

ggplot(avf,aes(x=Dest, y=avgFlight))+
  geom_bar(stat = 'identity')

##heatmap
install.packages('nycflights13')
library(nycflights13)
str(flights)

dt <- data.table(flights)
str(dt)
weekdays(Sys.Date())
dt[,dayOfWeek := weekdays(
  #after as.Date it becomes real date
  as.Date(
  #concatenante, this looks like a date
  paste(year, month, day, sep= "-")
))]
str(dt)


dta <- dt[,.N, by=.(dayOfWeek,hour)]

ggplot(dta, aes(x=hour, y=dayOfWeek, fill= N))+
  geom_tile()
#data story


#long and wide tables
install.packages('reshape2')
library(reshape2)
#this is the numeric representation of heatmap!
dcast(dta,dayOfWeek ~ hour)
#here wa are losing info
dtw <-dcast(dta,hour ~ dayOfWeek)
#back to long format
melt(dtw,id.vars = 'hour')



