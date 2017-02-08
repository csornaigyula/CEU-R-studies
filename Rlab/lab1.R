rm(list=ls())

# CHECK WORKING DIRECTORY

getwd()

pie(c(80,20),col = c('deepskyblue3','lightsalmon'),
    labels = c(
        'cleaning data c. Krisz',
        'complaining about cleaning data'),
    init.angle = 0,
    main = 'Data science cool'
    )

#uniform distribution
?runif
runif(5, max=10)

set.seed(42)
runif(2)
f <- function(x) 2*x*1
f(x)

x <- seq(0,5, by=0.1)
f(x)

plot(x,f(x))

x <- seq(0,10, by=0.1)
g <- function(x) sin(x)
g(x)
plot(x,g(x), type='l')
?sin

h <- c(174, 170, 160)
w <- c(90, 80, 70)
cor(h,w)
lm(formula = w ~h )
plot(h,w)
abline(lm(w~h), col='indianred')

#real data munging
#df[row,column]
df <- read.csv('http://bit.ly/BudapestBI-R-csv')

#structure of the object
str(df)

#shows levels as well
df[1,1]
df[1,2]

#first row
df[1,]

#using indexes
plot(df[,5],df[,4], col='seagreen')
abline(lm(df[,4]~df[,5]), col='indianred')

#using names
plot(df$heightIn,df$weightLb, col='seagreen')
abline(lm(df$weightLb~df$heightIn), col='indianred')

df$height <- df$heightIn*2.54
df$weight <- df$weightLb * 0.45
df$bmi <- df$weightLb/(df$height/100)^2

#basic stats
min(df$bmi)
max(df$bmi)
range(df$bmi)
nrow(df)
ncol(df)
dim(df)

females <- subset(df,df$sex =='f')
males <- subset(df,df$sex =='m')

#stats by
aggregate(bmi~sex,FUN=mean, data=df)
?aggregate
t.test(height~sex,data=df)
