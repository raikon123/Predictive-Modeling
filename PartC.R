####### R introduction

# to quit q()

# assign a value to a variable, and display it
x <- c(3,5,7)           
y <- c(1,3,11)
x
y

# this will be the same with (...)
# R (command/object) is case sensitive! x and X are not the same variable
( X = c(3,5,7) )
( Y = c(1,3,11) )
ls()

# run command from script editor: highlight command and ctrl-R

# or source(*) to run a script
source("C:\\RichardXu\\myDocument\\ActuarialAndSOA\\MaryvilleU\\2017Fal\\Week1\\PartC-test.R")
# Note if you want to produce text output, you must use the print() or cat() function
source("C:\\RichardXu\\myDocument\\ActuarialAndSOA\\MaryvilleU\\2017Fal\\Week1\\PartC-testB.R")



# THIS is a comment line; R will not execute it


# Retrieve historical command (up/down arrow)


# list all objects in workspace
ls()


# remove a few of them, and check again
rm(X)
rm(Y)
ls()

# remove all object in current workspace! Be careful!
rm(list=ls())
ls()


# get help
help(ls)
?ls
apropos("log")


# use R as number calculator
4^2-3*2
pi
exp(-2.5)
sin(pi/4)
sqrt(3)
factorial(5)
choose(10,2)         # 10!/2!/8!=10*9/2=45


# vector and its operation
x
length(x)
sum(x)
prod(x)
cumsum(x)
diff(x)

(x+y)
(x-y)
(x*y)
(x/y)

# what if lengths of 2 objects are different?
y4 <-  c(1,3,11,19)
(x+y4)
(x-y4)
(x*y4)
(x/y4)

# what if I like to operate on certain elements
x
y4
x[1:2]+y4[3:4]



# vectorization, what is it?
ncase = 5000000	# 1E6 should be enough; not more than 1E8
x1 <- runif(ncase, 0,1)
x2 <- runif(ncase, 0,1)
x3 <- runif(ncase, 0,1)
x4 <- runif(ncase, 0,1)
mean(x1)
mean(x2)

system.time(x3 <- x1+x2)
mean(x3)

system.time(for(i in 1:ncase){x4[i] <- x1[i]+x2[i]})
mean(x4)




# generate data within R

x5 <- 1:7
x6 <- 12:8
(x7 <- rep(1:4, 2))
(x8 <- rep(1:4, c(2,1,2,1)))
rep(c(2,1,2,1), 2)
rep(c("ABC","DEF","GHI"),2)

seq(1,4)  	# same as 1:4
seq(1,4, by=0.5)
seq(1,4, length=6)

# what this command will do?
(x9 <- rep(c("A","AA","AAA","AAAA","AAAAA"), c(1,2,3,4,5)))


# you can combine rep, c, and seq



#==============Import data into R
?read.csv
args(read.csv)

RGA <- read.csv("C:\\RichardXu\\myDocument\\ActuarialAndSOA\\MaryvilleU\\2017Fal\\Week1\\RGA-revenue.csv")
head(RGA)
tail(RGA)
summary(RGA)


str(RGA)
attributes(RGA)


RGA$Year
RGA[,"Year"]
RGA[[1]]
RGA[["Year"]]


RGA[2,"Year"]
RGA[2,1]


RGA[RGA$Year>2007,]
RGA[RGA$Year>2007,2]


RGA[RGA$Year==2008,"X2008FC"] <- 2
RGA

RGA[RGA$Year==2008,"X2008FC"] <- 1
RGA


Year
attach(RGA)
Year
detach(RGA)


# use R built-in data
data(trees)
trees


#======================= build a model

?lm

md1 <- lm(Revenue ~ Year, data = RGA)
summary(md1)

md2 <- lm(Revenue ~ Year + X2008FC, data = RGA)
summary(md2)

anova(md1,md2)


# let's do a test, see how random number helps to improve the model
# seed for pseudo-random generator
# set seed will make your results repeatable, even it is random!
set.seed(101)
RGA$rnum <- rnorm(10)
RGA
md3 <- lm(Revenue ~ Year + rnum, data = RGA)

summary(md1)
summary(md3)

anova(md1,md3)



#################### test/validation/prediction

summary(md2)

fitted(md2)
coef(md2)
residuals(md2)

AIC(md1)
AIC(md2)

AIC(md1)
AIC(md3)

(RGA$pred <- predict(md2))
RGA$Revenue

datanew <- data.frame(Year=c(2012,2013),X2008FC=c(0,0),Revenue=c(9.481,10.318), rnum=c(0,0))
datanew
(datanew$pred <- predict(md2, datanew))

datanew <- data.frame(Year=c(2012,2013, 2014),X2008FC=c(0,0,0),Revenue=c(9.481,10.318,10.904), rnum=c(0,0,0))
datanew
(datanew$pred <- predict(md2, datanew))



#################### output /graphic

plot(md2)

par(mfrow=c(2,2))
plot(md2)

par(mfrow=c(1,1))


plot(RGA$Year, RGA$Revenue)
# abline(md2)  # will get warning
lines(RGA$Year, fitted(md2))

points(RGA$Year, fitted(md2), pch=20,col=20,cex=1.5)


#combine 2 dataframes
allRGA <- rbind(RGA, datanew)

plot(allRGA$Year, allRGA$Revenue)
# abline(md2)  # will get warning
lines(allRGA$Year,allRGA$pred)

points(allRGA$Year, allRGA$pred, pch=20,col=20,cex=1.5)

# write model results to file

write.csv(allRGA, "allRGAdata.csv")


# right click mouse on plot and copy or save
# or save in different format; for example PDF
pdf("Sample.pdf")
plot(allRGA$Year, allRGA$Revenue)
lines(allRGA$Year,allRGA$pred)
points(allRGA$Year, allRGA$pred, pch=20,col=20,cex=1.5)
dev.off()


$ some examples
example(plot)                        # Remember to press Enter when prompted.
example(barplot)                     # Some are better than others!
example(boxplot)
example(dotchart)
example(coplot)
example(hist)
example(fourfoldplot)
example(stars)
example(image)
example(contour)
example(filled.contour)
example(persp)


# some interesting graphics

#Scintillating grid illusion
library(animation)
vi.grid.illusion()

# Hermann grid illusion
vi.grid.illusion(type = "h", lwd = 22, nrow = 5, ncol = 5, col = "white")

# Lilac Chaser
ani.options(nmax = 20)
par(mar = c(1, 1, 1, 1))
vi.lilac.chaser()




