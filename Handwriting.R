usps<- read.csv("usps.csv")
indexes = sample(1:nrow(usps), size=0.2*nrow(usps))
xtest = usps[indexes,1:256]
xtrain = usps[-indexes,1:256]
ytest = usps[indexes,257]
ytrain = usps[-indexes,257]
y<-usps[,257]
xusps<-usps[,-257]
train<-cbind(xtrain,ytrain)
test<-cbind(xtest,ytest)

#PCA
library(ISLR)
set.seed(565)
pca.out = prcomp(xusps,scale=TRUE)
summary(pca.out)
pca.var=pca.out$sdev ^2
pve=pca.var/sum(pca.var)
plot((pve), xlab="Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", ylim=c(0,1), type="b")

#LDA
library (MASS)
#test error rtae
set.seed(565)
train<- cbind(xtrain,ytrain)
lda.fit=lda(ytrain ~ . ,data=train)
test<- cbind(xtest,ytest)
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
table(lda.class,ytest)
pred<-table(lda.class,ytest)
correct.rate<- sum(diag(pred))/sum(pred)
correct.rate
mean(lda.class != ytest)

#training error rate
set.seed(565)
x=usps[,1:256]
lda.fit=lda(y ~ . ,data=usps)
lda.pred=predict(lda.fit,usps)
lda.class=lda.pred$class
table(lda.class,y)
pred<-table(lda.class,y)
correct.rate<- sum(diag(pred))/sum(pred)
1-correct.rate

#LDA on PCS
PLDA<-cbind(pca.out$x[,1:16],y)

#PCs test error rate
set.seed(565)
indexes=sample(1:nrow(PLDA),size=0.2*nrow(PLDA))
xpctest=PLDA[indexes,1:16]
xpctrain=PLDA[-indexes,1:16]
ypctest=PLDA[indexes,17]
ypctrain=PLDA[-indexes,17]
pctrain<- cbind(xpctrain,ypctrain)
pctest<- cbind(xpctest,ypctest)
pctrain<-as.data.frame(pctrain)
pctest<-as.data.frame(pctest)
lda.fit=lda(ypctrain ~ . ,data=pctrain)
lda.pred=predict(lda.fit,pctest)
lda.class=lda.pred$class
table(lda.class,ypctest)
pred<-table(lda.class,ypctest)
correct.rate<- sum(diag(pred))/sum(pred)
correct.rate
mean(lda.class != ypctest)

#PCs training error rate
set.seed(565)
PLDA<-as.data.frame(PLDA)
xpc=PLDA[,1:16]
ypc=PLDA[,17]
lda.fit=lda(y ~ . ,data=PLDA)
lda.fit
lda.pred=predict(lda.fit,PLDA)
lda.class=lda.pred$class
table(lda.class,ypc)
pred<-table(lda.class,ypc)
correct.rate<- sum(diag(pred))/sum(pred)
correct.rate
mean(lda.class != ypc)

#KNN
data=read.csv("usps.csv")
y=data$y 
x=data[,1:256]
# steps: 
# (1) finding the optimal k by finding the last local minimum of test error rate for 10-fold cross validation by using PCA 
# (2) computing the training error rate for the optimal k for both PCA and full data.

# (1)
pca.out=prcomp(x,scale=T)
z=pca.out$x[,1:16]
zy=cbind(z,y)

dat<-zy[sample(nrow(zy)),]
folds <- cut(seq(1,nrow(dat)),breaks=10,labels=FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dat[testIndexes, ]
  trainData <- dat[-testIndexes, ]
}

xtrain=trainData[,1:16]
ytrain=trainData[,17]
xtest=testData[,1:16]
ytest=testData[,17]

# testing error for pca-10fold cv
library(class)
set.seed(565)
knnpc.pred1=knn(xtrain,xtest,ytrain,k=1)
table(knnpc.pred1,ytest)
mean(knnpc.pred1==ytest)

# repeat this part by replacing k with different values--> get different %(correct classification),
# testing error rate=1-%(correct classification)
# repeat this part by replacing seed number
# get the optimal k --> 6 or 7


# (2)

# training errror using PCA  @ k=7,8:

knnpc.pred7=knn(xtrain,xtrain,ytrain,k=7)
table(knnpc.pred7,ytrain)
mean(knnpc.pred7==ytrain)
#trainig error(PCA,k=7)=1-0.9636=0.0364 (minimum)

knnpc.pred8=knn(xtrain,xtrain,ytrain,k=8)
table(knnpc.pred8,ytrain)
mean(knnpc.pred8==ytrain)
#trainig error(PCA,k=8)=1- 0.9615201=0.0385 
# training errror using full data @ k=7,8
y=data$y 
x=data[,1:256]
dat<-data[sample(nrow(data)),]
folds <- cut(seq(1,nrow(dat)),breaks=10,labels=FALSE)
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- dat[testIndexes, ]
}

xtrain=trainData[,1:256]
ytrain=trainData[,257]
xtest=testData[,1:256]
ytest=testData[,257]

library(class)

knn.pred7=knn(xtrain,xtrain,ytrain,k=7)
table(knn.pred7,ytrain)
mean(knn.pred7==ytrain)

knn.pred8=knn(xtrain,xtrain,ytrain,k=8)
table(knn.pred8,ytrain)
mean(knn.pred8==ytrain)

#K-Means
y=data$y 
x=data[,1:256]
indexes = sample(1:nrow(data), size=0.2*nrow(data))
xtest = data[indexes,1:256]
xtrain = data[-indexes,1:256]
ytest = data[indexes,257]
ytrain = data[-indexes,257]
traindata=data.frame(x=xtrain,y=ytrain)
testdata=data.frame(x=xtest,y=ytest)
pca.out=prcomp(x,scale=T)
r=abs(pca.out$rotation)
z=pca.out$x[,1:16]

#km of all data
set.seed(565)
km.out=kmeans(x,10,nstart=20)
t=table(km.out$cluster,y)
t

#(making a table of 1 and 6)
data1<-data[(data[,257]==1),]
data6<-data[(data[,257]==6),]
new<-rbind(data1,data6)
y1.6<-new[,257]
x1.6<-new[,-257]
xy16=km.out=kmeans(x1.6,2,nstart=20)
data16<-table(km.out$cluster,y1.6)
data16

#rank 1
t1<-rbind(t[7,],t[6,],t[2,],t[3,],t[4,],t[9,],t[10,],t[1,],t[5,],t[8,])
colnames(t1)<-c(1:10)
t1
sum(diag(t1))/sum(t1)

#rank2
t2<-rbind(t[9,],t[6,],t[2,],t[3,],t[4,],t[7,],t[10,],t[1,],t[5,],t[8,])
colnames(t2)<-c(1:10)
t2
sum(diag(t2))/sum(t2)

#km of first 16 pc
set.seed(565)
kmpc.out=kmeans(z,10,nstart=20)
tpc=table(kmpc.out$cluster,y)  
tpc

# making a table of 1 and 6 under kmpc:
data01<-data[(data[,257]==1),]
data06<-data[(data[,257]==6),]
new0<-rbind(data01,data06)
y01.06<-new0[,257]
x01.06<-new0[,-257]
xy0106=km.out=kmeans(x01.06,2,nstart=20)
data0106<-table(km.out$cluster,y01.06)
data0106

#rank 1 of kmpc
tpc1<-rbind(tpc[4,],tpc[5,],tpc[8,],tpc[7,],tpc[6,],tpc[9,],tpc[2,],tpc[1,],tpc[10,],tpc[3,])
colnames(tpc1)<-c(1:10)
tpc1
sum(diag(tpc1))/sum(tpc1)

#rank2 of kmpc
tpc2=rbind(tpc[9,],tpc[5,],tpc[8,],tpc[7,],tpc[6,],tpc[4,],tpc[2,],tpc[1,],tpc[10,],tpc[3,])
colnames(tpc2)=c(1:10)
tpc2
sum(diag(tpc2))/sum(tpc2)

#EM
install.packages("EMCluster")
library(EMCluster)
set.seed(565)
emobj <- simple.init(xusps, nclass = 10)
emobj <- shortemcluster(xusps, emobj)
ret <- emcluster(xusps, emobj, assign.class = TRUE)
ret$llhdval
EMtable<-table(ret$class,y)
logL(xusps,emobj)
logL(xusps,ret)
#no stable result

#EM PCA
emobjpca <- simple.init(pca.out$x[,1:16], nclass = 10)
emobjpca <- shortemcluster(pca.out$x[,1:16], emobjpca)
retpca <- emcluster(pca.out$x[,1:16], emobjpca, assign.class = TRUE)
retpca$llhdval
EM<-table(retpca$class,y)
tem<- rbind(EM[9,],EM[6,],EM[10,],EM[2,],EM[7,],EM[1,],EM[5,],EM[3,],EM[8,],EM[4,])
tem
sum(diag(tem))/sum(tem)
#0.5861476

#Hi
hc.complete = hclust(dist(usps), method="complete")
hc.cluster<- cutree(hc.complete, 10)
hi<- table(hc.cluster,y)
table<- rbind(hi[6,],hi[4,],hi[7,],hi[5,],hi[2,],hi[10,],hi[1,],hi[3,],hi[8,],hi[9,])
table
sum(diag(table))/sum(table)
# 0.4113788

#Hi using PC
hcpc.out=hclust(dist(pca.out$x[,1:16]))
hcpc.cluster<- cutree(hcpc.out, 10)
hipc<- table(hcpc.cluster,y)
tablepc<- rbind(hipc[3,],hipc[1,],hipc[6,],hipc[2,],hipc[8,],hipc[4,],hi[9,],hipc[5,],hipc[7,],hipc[10,])
tablepc
sum(diag(tablepc))/sum(tablepc)
#0.3371602

#Spectral
install.packages("mlbench")
install.packages("GGally")
install.packages("network")
install.packages("sna")
install.packages("kernlab")

library(mlbench)
library(GGally)
library(network)
library(ggplot2)
library(kernlab)
library(mlbench)
library(kernlab)
install.packages("kernlab")
set.seed(565)
sc <- specc(as.matrix(xusps), centers=10)
sptable<-table(sc,y)

#Spectral PCA
sc <- specc(pca.out$x[,1:16], centers=10)
sptable<-table(sc,y)
