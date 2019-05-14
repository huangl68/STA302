# Hello everyone! Welcome to the fourth Rlab!



# Again, let's begin by stting up our directory

#Importing the data set
NYC <- read.csv("nyc.csv",header=TRUE,sep=',')
head(NYC)

#EDA means Exploratory Data Analysis, we won't dwelve profoundly into it
#but we will introduce basic tools of data visualization

#pairs function is a great way to get an overview of the data
pairs(Price~Food+Decor+Service,data=NYC,gap=0.4,
      cex.labels=1.5)

#summary gives you a breakdown of the variables quantiles
summary(NYC)

#hist produces an histogram of a variable
hist(NYC$Price)


model <- lm(Price~Food,data=NYC)
summary(model)
sum <- summary(model)
sum$coefficients #is a table containing the estimates

tvalue <- sum$coefficients[2,1]/sum$coefficients[2,2]
?dt
n <- nrow(NYC)
df <- n-2
tvalue
pt(tvalue,df,lower.tail=FALSE)


model <- lm(Food~Decor,data=NYC)
summary(model)
sum <- summary(model)
sum$coefficients #is a table containing the estimates

#Let's do Sheather problem1 !
boxo <- read.table('boxoffice.txt',head=TRUE)
head(boxo)
#Oups wrong data set 

boxo <- read.csv('playbill.csv')
head(boxo)
plot(CurrentWeek~LastWeek,data=boxo)

model <- summary(lm(CurrentWeek~LastWeek,data=boxo))

#1(a)
n <- nrow(boxo)
df <- n-2
tvalue <- qt(0.975,df=df)
lb <- model$coefficients[2,1]-tvalue*model$coefficients[2,2]
ub <- model$coefficients[2,1]+tvalue*model$coefficients[2,2]

#1(b)
tstat <- (model$coefficients[1,1]-10000)/model$coefficients[1,2]
pt(tstat,df=df)*2

