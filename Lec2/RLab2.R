# Hello everyone! Welcome to this second Rlab!
# I'm still experimenting on how to do these Rlab, now we'll try a 
# simple R code with comments!

# As you can see the symbole "#" is used for comment
# Everything that appears after the "#" is not compiled.


# Again, let's begin by stting up our directory
setwd("C:/Users/beaul/Dropbox/U of T/Year 4/STA302/Lecture2")


#Begin with last week little exemple
A <- c(48,56,58)
B <- c(44,46,51)
Response <- c(A,B)
Groups <- c(0,0,0,1,1,1)
?t.test
t.test(Response~Groups,alternative="greater",var.equal=FALSE)

#Let's download two data sets (with different extension)!
#Google it!
?read.table

NYC <- read.csv("nyc.csv",header=TRUE,sep=',')
head(NYC)

NYC <- read.csv("nyc.csv") #Look at the default option ?read.table
head(NYC)

NYC <- read.table("nyc.csv") #I've heard read.table is faster!
head(NYC)

Krafft <- read.table("krafft.txt") #Let's download a txt file
head(Krafft) #Heuuuuu we've got a problem!

Krafft <- read.table("krafft.txt",header=TRUE) #Let's download a txt file
head(Krafft) #Heuuuuu we've got a problem!

boxplot(NYC$Price~NYC$East)


t.test(NYC$Price~NYC$East,alternative="two.sided",var.equal=FALSE)

anova(lm(Price~East,data=NYC))

t.test(NYC$Price~NYC$East,alternative="two.sided",var.equal=TRUE)

#it would have been cleaner to define the formula once
#formula is a great function for supervised learning with R
#it defined the predictors and output variables for futur analysis

Form <- formula(NYC$Price~NYC$East)
t.test(Form)

#Second problem

head(Krafft)
Form <- formula(Krafft$RA~Krafft$GROUP)
boxplot(Form)

t.test(Form)#This is why we introduced ANOVA!

anova(lm(Form)) # DF Don't make any sense!

#Since groups are identify with numbers, R considers them to be a numerical variables
#It is important to understand the techniques we use to make sure you've used the 
#R functions correctly

Form <- formula(Krafft$RA~as.factor(Krafft$GROUP))
boxplot(Form)

t.test(Form)#This is why we introduced ANOVA!

anova(lm(Form))


