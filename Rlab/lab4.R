<<<<<<< HEAD
rm(list=ls())

# CHECK WORKING DIRECTORY

getwd()
df <- read.csv('http://bit.ly/BudapestBI-R-csv')
str(df)
## metric
df$heightCm <- df$heightIn*2.54  
df$weightKg <- df$weightLb * 0.45

str(df)
fit <- lm(weightKg ~ heightCm, data = df)

summary(fit)

plot(df$heightCm, df$weightKg)
abline(fit, col='red')
points(df$heightCm,predict(fit), col='red')

points(df$heightCm,predict(fit), col='red')

#residual visualization
plot(df$heightCm,df$weightKg)
abline(fit, col='red')
segments(df$heightCm, df$weightKg,
         df$heightCm, predict(fit),
         col='green')

#104 is height, what is the weight
predict(fit,newdata = data.frame(heightCm = 104))
predict(fit,newdata = data.frame(heightCm = 56))


plot(df$heightCm, df$weightKg, xlim=c(0,200), ylim=c(-100,100))
abline(fit, col='red')

#polynomial model
fit2 <- lm(weightKg ~ heightCm + I(heightCm^2), data=df)
fit2p <- lm(weightKg ~ poly(weightKg,2, raw=TRUE), data=df)
predict(fit2,newdata = data.frame(heightCm = 56))

library(ggplot2)
ggplot(df, aes(x=heightCm, y=weightKg))+geom_point()+
  geom_smooth(method='lm', formula = y ~ poly(x,5))


df  <- read.csv('http://bit.ly/math_and_shoes')
df

fits <- lm(math ~ size, data=df)
summary (fits)

ggplot(df, aes(x=size, y=math))+
  geom_point()+
  geom_smooth(method='lm', col='indianred')+
  geom_smooth(method='lm', formula= y~poly(x,5), col='cyan')
#the trick is the secret x variable, which is age

fitsx <- lm(math ~ size + x, data=df)
summary (fitsx)

#COOL 3D
install.packages('scatterplot3d')
library(scatterplot3d)
p <- scatterplot3d(df[,c('size', 'math', 'x')])
install.packages('rgl')
library(rgl)
plot3d(df$x, df$size, df$math, col='red')

residuals(lm(math~x, df))
residuals(lm(size~x, df))

#how do they correlate
cor(residuals(lm(math~x, df)),
    residuals(lm(size~x, df)))

install.packages('psych')
library(psych)
partial.r(df,2:3,4) #correlation of 2 and 3, with controlling for 4
str(iris)
ifit <- lm(Sepal.Width ~ Sepal.Length, data=iris)
summary(ifit)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)

pairs(iris, col='dodgerblue3')

ifit2 <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)
summary(ifit2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=iris$Species))+
  geom_point()+
  geom_smooth(method='lm')

#DIFFERENT!
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width ))+
  geom_point(aes(col=iris$Species))+
  geom_smooth(method='lm')

#difference explained
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width ))+
  geom_point(aes(col=iris$Species))+
  geom_smooth(aes(col=iris$Species), method='lm')+
  geom_smooth(method='lm', col='black')
#########################################################
#clustering - unsupervised
#01 - distance matrix first, eucledian distance
dm <- dist(iris[,1:4])
str(dm)

summary(dm)
#02 - cluster
hc <- hclust(dm)
plot(hc)

#cut the tree, 3 clusters
rect.hclust(hc,k=3, border='red')
rect.hclust(hc,k=10, border='orange')

#no viz
cn <- cutree(hc, k=3)
cutree(hc, k=10)

#confusion matrix
table(iris$Species,cn)

install.packages('NbClust')
library(NbClust)

#optimal number of clusters
NbClust(iris[,1:4], method = 'complete')

#k-means with k=3
kc <- kmeans(iris[,1:4],3)
kc$cluster

#compare the 2 methods
table(cn,kc$cluster)

##############################################
# supervised -> classify to species

#train and test df
#iris is sorted, let's randomize
iris$rnd <-runif(150) 
i2 <- iris[order(iris$rnd),]

train <- i2[1:100,] #first 100
test <- i2[101:150,]#last 50
###
#k-nearest neighbors
#install.packages('class')
library(class)
fit <- knn(train[,1:4], test[,1:4], train$Species)
#fit <- knn(train[,1:4], test[,1:4], train$Species, k=3)

table(test$Species,fit)

###
#decision tree
#install.packages("rpart")
library(rpart)
ct <- rpart(Species ~ .,data=train)
plot(ct)
text(ct)

#scoring of training dataset
score_train <- predict(ct)
score_train
#labeling the test set based on predicted score
score_test <- predict(ct, newdata = test, type='class')
score_test

#confusion matrix
table(test$Species,score_test)

#hyperparams
ct2 <- rpart(Species ~ .,data=train, minsplit=3)
plot(ct2)
text(ct2)

score_train2 <- predict(ct2)
score_train2
#labeling the test set based on predicted score
score_test2 <- predict(ct2, newdata = test, type='class')
score_test2

#confusion matrix
table(test$Species,score_test2)

#cooler partitioning trees
install.packages('party')
library(party)
install.packages('partykit')
library(partykit)

plot(as.party(ct))

#
df <- read.csv('http://bit.ly/BudapestBI-R-csv')
#classify gender
summary(df)
df$rnd <-runif(237) 
df2 <- df[order(df$rnd),]
train <- df2[0:150,]
test <- df2[151:237,]
fit <- knn(train[,3:5], test[,3:5], train$sex, k = 2)


table(test$sex,fit)

#official
ct <- rpart(sex ~ heightIn + weightLb, data=df2, minsplit=1)
plot(ct);text(ct)
table(df2$sex,predict(ct, type='class'))
#error: no test set and train set
seed(7)
rnd <- runif(nrow(df))
df3 <- df[order(rnd),]

train <- df3[1:100,]
test <- df3[101:237,]
ct <- rpart(sex ~ heightIn + weightLb, data=train, minsplit=1)
plot(as.party(ct))


predict(ct)#will return the training labels, that is bundled with the model


###################################################
#machine learning with PCA - principal component analysis
#install.packages('jpeg')
library(jpeg)
download.file('http://bit.ly/BudapestBI-R-img', 'image.jpg')

img <- readJPEG("C:\\Users\\csorn\\Documents\\edu\\ceu\\RTrack\\image.jpg")
str(img)
h <- dim(img)[1]
w <- dim(img)[2]

#rgb is columns, rows are pixel index
img2d <- matrix(img,nrow=h*w, ncol=3)
str(img2d)
head(img2d)

pca <- prcomp(img2d)

str(pca)
summary(pca)
#PC1 explains 96% of the variation and we csn drop the original PC2 and PC3
str(pca$x)
#compress 3 columns to 1, transform back to 2d
str(matrix(pca$x[,1],h))

image(matrix(pca$x[,1],h))
image(matrix(pca$x[,2],h))
image(matrix(pca$x[,3],h))

#MDS = multidimensional scaling, PCA with 2 columns

#LOOKUP GITHUB

library(h2o)
?h2o.init
#we run it on our own computer
h2o.init()
library(hflights)
#upload
as.h2o(hflights,'hflights')

#get access to the data
hflight.hex <- as.h2o(hflights,'hflights')
str(hflight.hex)
#--> only 10 rows cached in R, the rest is stored in H2O

hflight.hex$FlightNum <- as.factor(hflight.hex$FlightNum)

for(v in c('Month','DayofMonth','DayOfWeek','DepTime','ArrTime')){
  hflight.hex[,v] <-as.factor(hflight.hex[,v])
}
str(hflight.hex)

#task: h2o --> cancelled
# Month, DayofMonth, DayOfWeek, hour of the day

h2o.shutdown()
Y
=======
rm(list=ls())

# CHECK WORKING DIRECTORY

getwd()
df <- read.csv('http://bit.ly/BudapestBI-R-csv')
str(df)
## metric
df$heightCm <- df$heightIn*2.54  
df$weightKg <- df$weightLb * 0.45

str(df)
fit <- lm(weightKg ~ heightCm, data = df)

summary(fit)

plot(df$heightCm, df$weightKg)
abline(fit, col='red')
points(df$heightCm,predict(fit), col='red')

points(df$heightCm,predict(fit), col='red')

#residual visualization
plot(df$heightCm,df$weightKg)
abline(fit, col='red')
segments(df$heightCm, df$weightKg,
         df$heightCm, predict(fit),
         col='green')

#104 is height, what is the weight
predict(fit,newdata = data.frame(heightCm = 104))
predict(fit,newdata = data.frame(heightCm = 56))


plot(df$heightCm, df$weightKg, xlim=c(0,200), ylim=c(-100,100))
abline(fit, col='red')

#polynomial model
fit2 <- lm(weightKg ~ heightCm + I(heightCm^2), data=df)
fit2p <- lm(weightKg ~ poly(weightKg,2, raw=TRUE), data=df)
predict(fit2,newdata = data.frame(heightCm = 56))

library(ggplot2)
ggplot(df, aes(x=heightCm, y=weightKg))+geom_point()+
  geom_smooth(method='lm', formula = y ~ poly(x,5))


df  <- read.csv('http://bit.ly/math_and_shoes')
df

fits <- lm(math ~ size, data=df)
summary (fits)

ggplot(df, aes(x=size, y=math))+
  geom_point()+
  geom_smooth(method='lm', col='indianred')+
  geom_smooth(method='lm', formula= y~poly(x,5), col='cyan')
#the trick is the secret x variable, which is age

fitsx <- lm(math ~ size + x, data=df)
summary (fitsx)

#COOL 3D
install.packages('scatterplot3d')
library(scatterplot3d)
p <- scatterplot3d(df[,c('size', 'math', 'x')])
install.packages('rgl')
library(rgl)
plot3d(df$x, df$size, df$math, col='red')

residuals(lm(math~x, df))
residuals(lm(size~x, df))

#how do they correlate
cor(residuals(lm(math~x, df)),
    residuals(lm(size~x, df)))

install.packages('psych')
library(psych)
partial.r(df,2:3,4) #correlation of 2 and 3, with controlling for 4
str(iris)
ifit <- lm(Sepal.Width ~ Sepal.Length, data=iris)
summary(ifit)

plot(iris$Sepal.Length, iris$Sepal.Width, col=iris$Species)

pairs(iris, col='dodgerblue3')

ifit2 <- lm(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width, data=iris)
summary(ifit2)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, col=iris$Species))+
  geom_point()+
  geom_smooth(method='lm')

#DIFFERENT!
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width ))+
  geom_point(aes(col=iris$Species))+
  geom_smooth(method='lm')

#difference explained
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width ))+
  geom_point(aes(col=iris$Species))+
  geom_smooth(aes(col=iris$Species), method='lm')+
  geom_smooth(method='lm', col='black')
#########################################################
#clustering - unsupervised
#01 - distance matrix first, eucledian distance
dm <- dist(iris[,1:4])
str(dm)

summary(dm)
#02 - cluster
hc <- hclust(dm)
plot(hc)

#cut the tree, 3 clusters
rect.hclust(hc,k=3, border='red')
rect.hclust(hc,k=10, border='orange')

#no viz
cn <- cutree(hc, k=3)
cutree(hc, k=10)

#confusion matrix
table(iris$Species,cn)

install.packages('NbClust')
library(NbClust)

#optimal number of clusters
NbClust(iris[,1:4], method = 'complete')

#k-means with k=3
kc <- kmeans(iris[,1:4],3)
kc$cluster

#compare the 2 methods
table(cn,kc$cluster)

##############################################
# supervised -> classify to species

#train and test df
#iris is sorted, let's randomize
iris$rnd <-runif(150) 
i2 <- iris[order(iris$rnd),]

train <- i2[1:100,] #first 100
test <- i2[101:150,]#last 50
###
#k-nearest neighbors
#install.packages('class')
library(class)
fit <- knn(train[,1:4], test[,1:4], train$Species)
#fit <- knn(train[,1:4], test[,1:4], train$Species, k=3)

table(test$Species,fit)

###
#decision tree
#install.packages("rpart")
library(rpart)
ct <- rpart(Species ~ .,data=train)
plot(ct)
text(ct)

#scoring of training dataset
score_train <- predict(ct)
score_train
#labeling the test set based on predicted score
score_test <- predict(ct, newdata = test, type='class')
score_test

#confusion matrix
table(test$Species,score_test)

#hyperparams
ct2 <- rpart(Species ~ .,data=train, minsplit=3)
plot(ct2)
text(ct2)

score_train2 <- predict(ct2)
score_train2
#labeling the test set based on predicted score
score_test2 <- predict(ct2, newdata = test, type='class')
score_test2

#confusion matrix
table(test$Species,score_test2)

#cooler partitioning trees
install.packages('party')
library(party)
install.packages('partykit')
library(partykit)

plot(as.party(ct))







>>>>>>> 4ddd2897ef0b99464dcd3f56d10136f6f34700cd
