rm(list=ls())
#Transform the mtcars dataset to a new data.table object called dt
library(data.table)
dt <-data.table(mtcars)
str(dt)

#Count the number of cars with less than 4 gears
dt[gear <4 ,.(countLT4G=.N )]
#15


#Count the number of cars with more than 4 gears and less than 90 horsepower
dt[gear > 4 & hp<90 ,.(count2= .N )]
dim(subset(mtcars, mtcars$gear > 4 & mtcars$hp < 90))
#There is no such car

#What's the average weight of cars with 4 gears?
dt[gear==4, .(avgWT = mean(wt))]
#avgWT = 2.616667

#What's the weight of the car with the best fuel consumption?
#the higher the miles per gallon the best the consumption is
dt[mpg == max(dt$mpg)]$wt
#1.835

#Plot the distribution of the number of cylinders
ggplot(dt)+aes(x=cyl)+geom_histogram()

#Plot the distribution of the number of cylinders grouped by carburetors
dt2 <- dt[,.(cylNo = .N) ,by=.(carb)]
ggplot(dt2)+aes(x=carb, y=cylNo)+geom_col()

#Plot the average weight of cars grouped by the number of cylinders
dt3 <- dt[,.(avgWT = mean(wt)),by=.(cyl=cyl)]
ggplot(dt3)+aes(x=cyl, y=avgWT)+geom_col()

#Plot the distribution of the performance of the cars (horsepower) per number of cylinders
ggplot(dt)+aes(x=cyl,y=hp)+geom_point()

#Install and load the ISLR package and use its Auto dataset for the below exercises
install.packages('ISLR')
library(ISLR)
df <- Auto
str(df)
#Plot the weight and horsepower of cars
ggplot(df)+aes(x=weight, y=horsepower)+geom_point()

#Add a linear trend line to the above plot
ggplot(df)+aes(x=weight, y=horsepower)+geom_point()+geom_smooth(method='lm')

#Fit a linear model using the weight of cars to predict acceleration
fit <- lm(acceleration ~ weight, data = df)
summary(fit)

#What's the estimated acceleration of a car with weight = 3?
ggplot(df)+aes(x=weight, y=acceleration)+geom_point()+geom_smooth(method='lm')
p <- predict(fit, newdata = data.frame(weight = 3000))
p
#15.51098

#Filter for cars from America (1) and Europe (2) and 
#store the results in a new object called auto (mind the lower case letters)
auto <- subset(df,origin==1 | origin==2)
summary(auto$origin)

#Remove the name column
auto$name <- NULL

#Apply k-means or hierarchical clustering on this dataset to split the 
#observations into 3 groups, and show the number of observations in the clusters
summary(auto)
library(class)
dim(auto)


auto$of <-as.factor(auto$origin)
auto$origin <-NULL
auto$cy <- as.factor(auto$cylinders)
auto$cylinders <- NULL
auto$yr <-as.factor(auto$years)
auto$rnd <- NULL

adata <-auto

#dissimilarity matrix can only be the input
dm <- dist(auto[, 1:8])
str(dm)
summary(dm)
?hclust
hc <- hclust(dm)
plot(hc)
rect.hclust(hc, k = 3, border = 'red')
library(cluster)
auto <- subset(df,origin==1 | origin==2)
summary(auto$origin)

#Remove the name column
auto$name <- NULL
clusplot(auto[, 1:7], cn, color = TRUE, shade = TRUE, labels = 2)

cn <- cutree(hc, k=3)
str(cn)
tcn <- table(cn)
tbl <- data.table(tcn)
ggplot(tbl)+aes(x=tbl$cn, y=tbl$N)+geom_col()+
  labs(x="Cluster items", y="Number of elements in the hyerarchical cluster", 
       title="Clustering output with cut at level 3")

#Bonus points: Build and visualize a decision tree to tell if a car was made in America or Europe, 
#show the confusion matrix, do the same with k-NN
auto2 <- subset(df,origin==1 | origin==2)
auto2$rnd <-runif(313) 
auto2 <- auto2[order(auto2$rnd),]
auto2$rnd <- NULL
auto2$name <- NULL
auto2$origin <-as.factor(auto2$origin)
train <- auto2[1:200,]
test <- auto2[201:313,]
library(rpart)
library(class)
#decision tree
ct <- rpart(origin ~ mpg +  cylinders + displacement + horsepower +weight + acceleration + year , data = train)
table(test$origin, predict(ct, newdata = test,type = 'class'))
library(party)
library(partykit)

plot(as.party(ct))

#k-nn
fknn <- knn(train[,1:7], test[,1:7], train$origin, k=3)
table(test$origin, fknn)
fknn2 <- knn(train[,1:7], test[,1:7], train$origin, k=5)
table(test$origin, fknn2)
