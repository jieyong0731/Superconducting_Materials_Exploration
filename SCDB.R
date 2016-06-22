library("R.matlab") #matlab interface for R
library("ggplot2") #plot package
library(caret) # machine learning for R
library(rattle) #tree plot

# Load orignal matlab data
a <- readMat("data/SCDB.mat")
names(a)
name <-as.character()
SCDB <-data.frame(dX=a$dX,Nv=a$Nv,dR=a$dR,Tc=a$Tc)
for (i in 1:nrow(SCDB)){
  value <- (a$grouped.str)[[i]][[1]][1,1]
 name[i] <- ifelse(is.null(value), NA,value)
}
SCDB <-cbind(Name= name,SCDB)
str(SCDB)
SCDB$Name <- as.character(SCDB$Name)
na <-sum(is.na(SCDB))
totalnum <- nrow(SCDB)
# remove NAs
SCDB <- SCDB[!(is.na(SCDB$Name))& !(is.na(SCDB$dX)),]
# save tidy data
write.table(SCDB, "data/neatdata.txt", sep="\t")
#make sure no NAs
sum(is.na(SCDB))


#histogram plot
par(mfrow=c(2,2))
hist(SCDB$Tc,breaks=200)
hist(SCDB$dR,breaks=200)
hist(SCDB$dX,breaks=200)
hist(SCDB$Nv,breaks=200)
dev.copy(png,'figures/histogram.png')
dev.off()
par(mfrow=c(1,1))


# pairs plot
str(SCDB)
str(cars)
pairs(SCDB[,2:5])
dev.copy(png,'figures/pairs.png')
dev.off()


# linear Model Building

library(caret)
model_lm <- train(Tc ~dX+Nv+dR, data=SCDB,method="lm")
finMod <-model_lm$finalModel
print(finMod)
rmse <- sqrt(mean((finMod$fitted.values-SCDB$Tc)^2))
rmse
# highly nonlinear.

#label Tc by high or low
m<- median(SCDB$Tc)
SCDB$label <-as.factor(ifelse(SCDB$Tc>m,"high","low"))

#glm
train_control <- trainControl(method="cv", number=10)
model_glm <- train(label~dX+Nv+dR,data=SCDB,trControl=train_control, method="glm",tuneLength = 9)
pred_glm<- predict(model_glm , SCDB)
Pglm <- postResample(pred_glm, SCDB$label)[1]
Pglm


#tree
train_control <- trainControl(method="cv", number=10)
model_tree <- train(label~dX+Nv+dR,data=SCDB,trControl=train_control, method="rpart",tuneLength = 9)
pred_tree<- predict(model_tree , SCDB)
Ptree <- postResample(pred_tree, SCDB$label)[1]
Ptree
# display the tree
fancyRpartPlot(model_tree$finalModel)
dev.copy(png,'figures/tree.png')
dev.off()
# show the importance of dX
hist(SCDB$dX[SCDB$label=="high"],breaks=200,col="red",xlab="dX",main="Histogram of dX")
hist(SCDB$dX[SCDB$label=="low"],breaks=200,add=TRUE,col="blue")
text(1,1200,"Red: Tc > 19K",col="red")
text(1,1000,"Blue: Tc < 19K",col="blue")
dev.copy(png,'figures/dX.png')
dev.off()

#knn
modelknn <- train(label~dX+Nv+dR, data=SCDB,method="knn",trControl=train_control,tuneLength = 9)
predknn <- predict(modelknn , SCDB)
Pknn <- postResample(predknn, SCDB$label)[1]
Pknn
# knn is better than decision tree and glm.

# sweet spots revisit, see Rmd file for details
plot(SCDB$dX, SCDB$dR,col= ifelse(SCDB$label=="high","red","black"), main="Quantum Structural Diagram", pch =16)

rect(-0.5,-0.9,0.2,0.1,col="green",density=0,lwd=2)
text(0,-0.5,"A",col="green",lwd=3)

rect(-1.2,1.9,-0.1,2.6,col="blue",density=0,lwd=2)
text(-0.5,3,"B",col="blue",lwd=3)


rect(-2.4,1.2,-0.3,1.65,col="red",density=0,lwd=2)
text(-2,0,"C:high Tc?",col="red",lwd=3)
abline(h=0)
abline(v=0)
dev.copy(png,'figures/diagram.png')
dev.off()




















