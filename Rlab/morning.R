rm(list=ls())
getwd()
df <- mtcars
str(mtcars)
#Plot the weight and horsepower of cars from the mtcars dataset (bundled with R, see ?mtcars)
#Add a linear trend line to the above plot
#Add a 3rd degree polynomial model to the above plot
ggplot(df)+aes(x=df$hp, y=df$wt)+geom_point()+geom_smooth(method='lm')+
  geom_smooth(method='lm' ,  formula=y~poly(x,3), col='blue')

#not here
segments(df$heightCm, df$weightKg,
         df$heightCm, predict(fit),
         col='green')

#Fit a linear model on hp to predict weight
#Estimate the weight based on the above model for a car with 98 horsepower
fit <- lm(wt ~ hp, data = df)
predict(fit, newdata = data.frame(hp = 98))
#Estimate the weight based on the above model for Lotus Europa
predict(fit, newdata = data.frame(hp = 113))

#What's the average fuel consumption?
?mtcars
avgConsLperKm <- 100/(mean(df$mpg)*1.6/3.79)
avgConsLperKm

#Build a linear model to describe fuel consumption based on the horsepower and weight
fit2  <- lm(mpg ~ hp + wt, data=df)

#Compute a new variable in the dataset for the ratio of wt and hp
df$whp_ratio <- df$wt / df$hp

#Plot the distribution of this new variable on a boxplot
ggplot(df)+aes(x=whp_ratio)+geom_histogram()

#Create an aggregated dataset on mtcars including the average hp and wt grouped by the number of gears
library(data.table)
df$name <- rownames(df)
dt <- data.table(df)
dt2 <- dt[, list(avgHP = mean(hp),avgWT=mean(wt)),by=list(gear)]

#Compute a new variable for fuel consumption using the "liters per 100 kilometers" unit based on mpg
dt$l_per100km <- 100/(df$mpg*1.6/3.79)



#Which car has the best fuel consumption?
dt3 <- dt[,.(minLp100 = min(l_per100km))]

#Compute wt2 to store the weight in kilograms based on wt
dt$wt2 <- dt$wt*0.45

#Apply k-means clustering on the dataset to split the observations into 3 groups
kc <- kmeans(df[,1:13],3)
kc$cluster


#Perform hierarchical clustering on the dataset and plot the dendogram
dm <- dist(df[, 1:13])
str(dm)
summary(dm)
hc <- hclust(dm)
plot(hc)
rect.hclust(hc, k = 3, border = 'red')
cn <- cutree(hc, k=3)

#Compare the cluster memberships returned by the hierarchical and k-means methods
table(cn, kc$cluster)


#Build a decision tree to tell if a car has automatic or manual transmission (hint: you might want to convert the number to factor first)
#Visualize the above decision tree
library(rpart)
ct <- rpart(am  ~ .,data=df)
plot(ct)
library(party)
library(partykit)
plot(as.party(ct))


Create a confusion matrix for the above model
Use the k-NN algorithm to fit a similar model and decide on the best number of neighbors to use
Did you use a training and validation dataset?
Visualize the (dis)similarity of cars using PCA or MDS
Load the weight.csv and build a model to classifying observation if BMI is above the normal threshold (25)