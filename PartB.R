# an simple example
x<-c(3,5,7)            # x,y coordinates
y<-c(1,3,11)

mNormal<-glm(y~x,family=gaussian)		# fit model

plot(x,y,ylim=c(0,12))				# plot points and lables
text(3,1.5,"x1")
text(5,2.5,"x2")
text(7,11.5,"x3")

abline(mNormal,col="red")


RGA <- read.csv("C:\\RichardXu\\myDocument\\ActuarialAndSOA\\MaryvilleU\\2017Fal\\Week1\\RGA-revenue.csv")
iModel2 <- lm(Revenue~Year+X2008FC, data=RGA)
summary(iModel2)

plot(iModel2)

======================
tpc <- read.csv("C:\\RichardXu\\myDocument\\ActuarialAndSOA\\MaryvilleU\\2017Fal\\Week1\\Third_party_claims.csv")
summary(tpc)
head(tpc)
tail(tpc)

tpc$X <- tpc$accidents/tpc$population
tpc$Y <- tpc$ki/tpc$population

# a model

m1tpc <- lm(Y~X,data=tpc)
summary(m1tpc)

# inlcude "sd" as categorical
m2tpc <- lm(Y~X+factor(sd),data=tpc)
summary(m2tpc)

m2tpc <- lm(Y~X+factor(sd)+pop_density,data=tpc)
summary(m2tpc)

# plot model and data
plot(tpc$X,tpc$Y)
abline(m2tpc,col='red')

#some checking, AIC, confidence level, etc
AIC(m1tpc)

confint(m1tpc)



#prediction
xpred <- seq(min(tpc$X),max(tpc$X),length=201)

ypred <- predict(m1tpc,newdata=data.frame(X=xpred),interval='confidence')
plot(tpc$X,tpc$Y)
abline(m1tpc,col='red')
lines(xpred,ypred[,2],lty=2)
lines(xpred,ypred[,3],lty=2)

ypred <- predict(m1tpc,newdata=data.frame(X=xpred),interval='prediction')
plot(tpc$X,tpc$Y)
abline(m1tpc,col='red')
lines(xpred,ypred[,2],lty=2)
lines(xpred,ypred[,3],lty=2)
