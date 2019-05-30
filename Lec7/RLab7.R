# R Lab 7

# Your test is netx Tuesday and that already was a long lecture
# This will be a short Rlab.

#set up working directory

#Download the data
data <- read.table('krafft.txt',head=TRUE)
head(data) 

#First let's take a long look at a typical MLR output
G2 <- as.integer(data$GROUP==2)
G3 <- as.integer(data$GROUP==3)
G4 <- as.integer(data$GROUP==4)
model <- lm(data$RA~data$VTINV+data$DIPINV+data$HEAT+data$KPOINT+G2+G3+G4)
summary(model)

par(mfrow=c(2,2))
plot(lm(model))

#That's cool but where's my food data set
#Importing the data set
NYC <- read.csv("nyc.csv",header=TRUE,sep=',')
head(NYC)


#Sometimes interactions can create collinearity
model <- lm(Price~Food*Decor*Service,data=NYC)
summary(model)

par(mfrow=c(2,2))
plot(lm(model))

NYC <- as.matrix(NYC[,c(3,4,5,6,7)])



