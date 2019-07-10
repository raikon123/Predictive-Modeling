### different purity function
par("mar"=c(3,2,2,1))

x <- seq(0, 1, by=0.01)

giniFun <- function(x) {2*x*(1-x)}
plot(x,giniFun(x), type="l", lwd=2, col="green", ylim=c(0,0.7))

infoFun <- function(x) {-x*log(x)-(1-x)*log(1-x)}
lines(x,infoFun(x), type="l", lwd=2, col="red")

lines(c(0,0.5,1),c(0,0.5,0),type="l", lwd=2, col="black")

legend(0.2,0.2, c("Gini","Entropy","Misclassification error"), lwd=c(2,2,3),col=c("green","red","black"))


####################### Titanic example ###############################

library(rpart)		# CART R version
library(rpart.plot)	# needed for titanic data and nice plot
data(ptitanic)		# Titanic data with passenger names and other details removed

head(ptitanic)
summary(ptitanic)
#pclass 	passenger class, unordered factor: 1st 2nd 3rd
#survived 	factor: died or survived
#sex 		unordered factor: male female
#age 		age in years, min 0.167 max 80.0
#sibsp 	number of siblings or spouses aboard, integer: 0...8
#parch 	number of parents or children aboard, integer: 0...6

tTree <- rpart(survived ~ ., data=ptitanic, cp=.02, method="class")
plot(tTree, uniform=T)
text(tTree, use.n = TRUE, all = TRUE, cex=.8, xpd=NA)

# nice plot
cols <- c("darkred", "green4")[tTree$frame$yval] # green if survived
prp(tTree, 
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    branch=.5,           # change angle of branch lines
    faclen=0,            # do not abbreviate factor levels
    trace=1,             # print the automatically calculated cex
    shadow.col="gray",   # shadows under the leaves
    branch.lty=3,        # draw branches using dotted lines
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col=cols, border.col=cols,   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)              # round the split box corners a tad



######################### iris example ##############################
data(iris)
head(iris)
summary(iris)

pairs(iris[1:4], col=iris$Species)
# look nicer
# 
pairs(iris[1:4], main = "Iris Data", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

# look at rpart and control
?rpart

# build a large tree with greedy algorithm
set.seed(100)
irisTree <- rpart(Species ~ ., data=iris, minsplit = 3, cp=0.0001)
plot(irisTree,uniform=T)
text(irisTree, use.n = TRUE, all = TRUE, cex=.7, xpd=NA)

# check the CP table
irisTree$cptable
plotcp(irisTree)

# prune back
irisTree2 <- prune(irisTree, cp=0.01)

plot(irisTree2,uniform=T)
text(irisTree2, use.n = TRUE, all = TRUE, cex=.7, xpd=NA)


cols <- c("darkred", "green4")[tTree2$frame$yval] # green if survived
prp(irisTree2, 
    extra=106,           # display prob of survival and percent of obs
    nn=TRUE,             # display the node numbers
    fallen.leaves=TRUE,  # put the leaves on the bottom of the page
    branch=.5,           # change angle of branch lines
    faclen=0,            # do not abbreviate factor levels
    trace=1,             # print the automatically calculated cex
    shadow.col="gray",   # shadows under the leaves
    branch.lty=3,        # draw branches using dotted lines
    split.cex=1.2,       # make the split text larger than the node text
    split.prefix="is ",  # put "is " before split text
    split.suffix="?",    # put "?" after split text
    col=cols, border.col=cols,   # green if survived
    split.box.col="lightgray",   # lightgray split boxes (default is white)
    split.border.col="darkgray", # darkgray border on split boxes
    split.round=.5)              # round the split box corners a tad

table(irisTree$y,irisTree$where)

table(irisTree2$y,irisTree2$where)


######################### CART insurance application #############################
# see results in class
####################################clustering


##### generate random variables
nPtPerCluster <- 100
myColor <- c("red","blue","green","grey","yellow","purple")

set.seed(100)
testData1 <- data.frame(x1<-rnorm(nPtPerCluster,0.0,0.3),x2<-rnorm(nPtPerCluster,0.0,0.3), group <- 1)
testData2 <- data.frame(x1<-rnorm(nPtPerCluster,1.6,0.3),x2<-rnorm(nPtPerCluster,0.0,0.3), group <- 2)
testData3 <- data.frame(x1<-rnorm(nPtPerCluster,0.8,0.3),x2<-rnorm(nPtPerCluster,1.4,0.3), group <- 3)
names(testData1) <- c("x1","x2","group")
names(testData2) <- c("x1","x2","group")
names(testData3) <- c("x1","x2","group")

# combine into a single data frame
inputData <- rbind(testData1,testData2,testData3)
summary(inputData)

# same color to display
par("mar"=c(3,2,2,1))
plot(inputData$x1, inputData$x2,xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=20)

#different colors to display
plot(inputData$x1, inputData$x2,xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=20,col=myColor[inputData$group])

################################# K-means ############################

##### initialize cluster
nCluster <- 3
inputData$curCluster <- NULL
inputData$preCluster <- NULL

index <- sample(1:(3*nPtPerCluster), nCluster, replace = FALSE)

for (i in 1:nCluster){
	inputData[index[i],"curCluster"] <- i
}
# plot only initial points
plot(inputData[inputData$curCluster>0,"x1"], inputData[inputData$curCluster>0,"x2"],xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=16,col=myColor[inputData[inputData$curCluster>0,"curCluster"]])

# overlap with original data
plot(inputData$x1, inputData$x2,xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=20,col=myColor[inputData$group])
points(inputData[inputData$curCluster>0,"x1"], inputData[inputData$curCluster>0,"x2"], col = "brown", pch = 8, cex=2)

##### clustering analysis
nMaxLoop <- 100

# distance to the 3 means, and initialization
distance <- data.frame(rep(0,3*nPtPerCluster))
for(k in 2:nCluster) distance <- cbind(distance,rep(0,3*nPtPerCluster))

curx1 <- inputData[,"x1"]
curx2 <- inputData[,"x2"]

for(i in 1:nMaxLoop){

	inputData$preCluster <- inputData$curCluster
	inputData$curCluster <- 0
	centroidX1 <- aggregate(x1~preCluster,data=inputData, FUN=mean)
	centroidX2 <- aggregate(x2~preCluster,data=inputData, FUN=mean)

	for(k in 1:nCluster){
		distance[[k]] <- sqrt((curx1-centroidX1[centroidX1$preCluster==k,2])^2+(curx2-centroidX2[centroidX2$preCluster==k,2])^2)
	}
	# find cluster id and plot
	inputData$curCluster <- max.col(-distance)
	plot(inputData$x1, inputData$x2,xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=20,col=myColor[inputData$curCluster])
	# plot cluster centers
	points(centroidX1[,2], centroidX2[,2], col = myColor[1:nCluster], pch = 8, cex=2)

	paste("Hit c and enter to continue...")
	for (n in 1:100){
		input <- readline()
		if(input == "c") break
	}
	# loop end critetion
	if(!any(is.na(inputData$preCluster))){
		if(all(inputData$preCluster==inputData$curCluster)) break
	}
}



### look at new clusters vs. original clusters
table(inputData$curCluster, inputData$group)



### calculate sume square (SS) of cluster distance

totalDist <- 0.0
for(k in 1:nCluster)	totalDist = totalDist+sum(distance[inputData$curCluster==k,k]*distance[inputData$curCluster==k,k])
totalDist

# results: k, dist, SS
# k=1, -		323.3045
# k=2, 208.9673	182.0913
# k=3, 110.6219	 53.9764
# k=4, 103.7818	 47.2299
# k=5,  94.7005	 39.8259
# k=6,  89.6230	 35.6399


# make a plot
k <- 1:6
#err <- c(208.9673, 110.6219, 103.7818, 94.70048, 89.62303)
ErrorSS <- c(323.3045,182.0913, 53.97644,47.2299,39.82594,35.63995)
plot(k,ErrorSS,ylim=c(0,350),type="p",pch=16, col="blue")
lines(k,ErrorSS,ylim=c(0,350),col="red")




##### build a model with R algorithm
mData <- inputData[,c("x1","x2")]
summary(mData)
mCluster <- kmeans(mData, 3)



##Compared to previous
plot(inputData$x1, inputData$x2,xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=20,col=mCluster$cluster)
windows()
par("mar"=c(3,2,2,1))
plot(inputData$x1, inputData$x2,xlim=c(-1.0,2.5),ylim=c(-1.0,2.5),pch=20,col=myColor[inputData$curCluster])

table(inputData$curCluster, inputData$group)
table(mCluster$cluster,inputData$group)

######## ok, job done!!!






#################### Hierarchical clustering ###################
# load packages
library(Amelia)
library(FactoMineR)

itr <- read.csv("C:\\RichardXu\\myDocument\\ActuarialAndSOA\\MaryvilleU\\2017Fall\\Week5\\DataSummary2013ForPM.csv")
summary(itr)
head(itr)
itr4model <- itr[,c(-1,-2)]
head(itr4model)

#set limits
setbound <- matrix(c(
1, 49,90,
2, 1,	1100,
3, 1,	120,
4, 0, 50,
5, 0, 72,
6, 0, 0.3,
7, 1,	1600.0,
8, 0.01, 7.0,
9, 5.0, 100.0,
10, 20.0,100.0,
11, 0.01,  18.0,
12, 0.1,  50.0,
13, 0, 95.0,
14, 0, 80.0,
15, 0, 90.0,
16, 0,  20.0,
17, 0,  45.0,
18, 0, 115.0,
19, 0, 100.0,
20, 0, 215.0,
21, 0, 3850.0,
22, 400.0, 102800.0,
23, 0.01,  1.0,
24, 0.01,  20,
25, 20.0, 80
), nrow=25, ncol=3, byrow=TRUE)

### test converge of imputation; 
### this will take long time to finish!!!
#itrImp200 <- amelia(itr4model, m=200, bounds=setbound)
# pre-calculated
load("C:\\RichardXu\\Rworkplace\\imputation.rdata")


# check if data is complete and get average data
myData1 <- itrImp200[[1]][["imp1"]]
nimp <- 100
nImp <- 1
for (i in 2:nimp){
	impi <- paste("imp",i,sep="");
	if (!is.null(itrImp200[[1]][[impi]])) {
		nImp = nImp+1
		myData1 <- myData1+itrImp200[[1]][[impi]]
	}
}
myData1 <- myData1/nImp
nImp

myData2 <- itrImp200[[1]][["imp101"]]
nimp <- 200
nImp <- 1
for (i in 102:nimp){
	impi <- paste("imp",i,sep="");
	if (!is.null(itrImp200[[1]][[impi]])) {
		nImp = nImp+1
		myData2 <- myData2+itrImp200[[1]][[impi]]
	}
}
myData2 <- myData2/nImp
nImp

# check the difference
diff <- myData1-myData2
for (i in 1:25) print(sqrt(sum(diff[,i]*diff[,i]))/sum(myData1[,i]))

#ok, now we use the average of the whole dataset 

myData3 <- itrImp200[[1]][["imp1"]]
nimp <- 200
nImp <- 1
for (i in 2:nimp){
	impi <- paste("imp",i,sep="");
	if (!is.null(itrImp200[[1]][[impi]])) {
		nImp = nImp+1
		myData3 <- myData3+itrImp200[[1]][[impi]]
	}
}
myData3 <- myData3/nImp
nImp
### this is the dataset we will use


#weights
wts <- c(2.0,rep(1.0,24))

# without weights
pca3 <- PCA(myData3)
# with weights
pca3 <- PCA(myData3, col.w=wts)

# clustering
myCluster3 <- HCPC(pca3,nb.clust=6)



# write out CSV file
write.csv(myCluster3$call$X,"cluster.csv")



# plot 5x5 PCA plots
ncp <- 5
par(mfrow=c(ncp-1,ncp-1),mar=c(0.5, 2, 2, 0))

for (m in seq(1,ncp-1)){  
	for (n in seq(2,ncp)){  
		if(m==1)
			plot(-10:10,-10:10,type="n", main =paste("Dim ",n))
		else
			plot(-6:6,-6:6,type="n")
		if(n==2)
			text(-6.0,0.5,label=paste("Dim ",m), srt=90, cex=1.4)
		if(m<n){
			points(myCluster3$call$X[,n], myCluster3$call$X[,m], col=myCluster3$call$X$clust, pch=19,cex=0.8)
		}
	}
}

# plot a large dim1 vs. dim2
windows()
plot(seq(-9,8,length.out=18),seq(-9,5,length.out = 18),type="n", main="Dim1 vs. Dim2")
points(myCluster3$call$X[,1], myCluster3$call$X[,2], col=myCluster3$call$X$clust, pch=19,cex=0.6)

#2D scatterplots
dimLimit <- 3
for(i in 2:dimLimit){    
	plot(myCluster3, axes=c(1,i), choice="map", draw.tree=FALSE, ind.names=TRUE, new.plot=TRUE)
}
#3D scatterplots
for(i in 2:dimLimit){    
	plot(myCluster3, axes=c(1,i), choice="3D.map", draw.tree=TRUE, ind.names=TRUE, new.plot=TRUE)
}

#plot interia gain bar chart
plot(myCluster3, choice="bar", new.plot=TRUE)

#clusters tree
plot(myCluster3, choice="tree", rect=TRUE, new.plot=TRUE)







