
#####################
#   Book Problems   #
#####################

##############################
#   Chapter 2: Question 10   #
##############################
            
rm(list = ls())

library(MASS)
myData = Boston
attach(myData)

# 10(a)
dim(myData)

# 10(b)
pairs(myData)

# 10(c)
x <- myData[1]
y <- myData[2 : 7]
z <- myData[8 : 14]

cor(x, y)
cor(x, z)

par(mfrow=c(1, 5))
plot(age, crim)
plot(dis, crim)
plot(rad, crim)
plot(tax, crim)
plot(ptratio, crim)

# 10(d)
summary(crim)
summary(tax)
summary(ptratio)

par(mfrow=c(1,3))
boxplot(crim, main="Crime Rates")
boxplot(tax, main="Tax Rates")
boxplot(ptratio, main="Pupil-teacher ratios")

# 10(e)
margin.table(table(crim,chas),2)

# 10(f)
median(ptratio)

# 10(g)
temp <- myData[order(medv),]
x <- sapply(myData,summary)
y <- temp[c(1,2),]

z <- as.data.frame(rbind(y,x))

z1 <- z[, 1:7]
z2 <- z[, 8:14]
z1
z2


# 10(h)
length(rm[rm > 7])
length(rm[rm > 8])

summary(myData[which(rm > 8),])


##############################
#   Chapter 3: Question 15   #
##############################

rm(list = ls())

library(MASS)
myData = Boston
attach(myData)

# 15(a)
par(mfrow=c(1, 4))
chas <- factor(chas, labels = c("N","Y"))

lm.zn = lm(crim~zn)
summary(lm.zn)
plot(lm.zn)

lm.indus = lm(crim~indus)
summary(lm.indus)
plot(lm.indus)

lm.chas = lm(crim~chas) 
summary(lm.chas)
plot(lm.chas)

lm.nox = lm(crim~nox)
summary(lm.nox)
plot(lm.nox)

lm.rm = lm(crim~rm)
summary(lm.rm)
plot(lm.rm)

lm.age = lm(crim~age)
summary(lm.age)
plot(lm.age)

lm.dis = lm(crim~dis)
summary(lm.dis)
plot(lm.dis)

lm.rad = lm(crim~rad)
summary(lm.rad)
plot(lm.rad)

lm.tax = lm(crim~tax)
summary(lm.tax)
plot(lm.tax)

lm.ptratio = lm(crim~ptratio)
summary(lm.ptratio)
plot(lm.ptratio)

lm.black = lm(crim~black)
summary(lm.black)
plot(lm.black)

lm.lstat = lm(crim~lstat)
summary(lm.lstat)
plot(lm.lstat)

lm.medv = lm(crim~medv)
summary(lm.medv)
plot(lm.medv)


# 15(b)
lm.fit = lm(crim~., data = myData)
summary(lm.fit)

# 15(c)
x = c(coefficients(lm.zn)[2], coefficients(lm.indus)[2], coefficients(lm.chas)[2], coefficients(lm.nox)[2],
      coefficients(lm.rm)[2], coefficients(lm.age)[2], coefficients(lm.dis)[2], coefficients(lm.rad)[2],
      coefficients(lm.tax)[2], coefficients(lm.ptratio)[2], coefficients(lm.black)[2], coefficients(lm.lstat)[2],
      coefficients(lm.medv)[2])
y = coefficients(lm.fit)[2:14]

plot(x, y)

# 15(d)
lm.zn = lm(crim~poly(zn,3))
summary(lm.zn)

lm.indus = lm(crim~poly(indus,3))
summary(lm.indus)

lm.nox = lm(crim~poly(nox,3))
summary(lm.nox)

lm.rm = lm(crim~poly(rm,3))
summary(lm.rm)

lm.age = lm(crim~poly(age,3))
summary(lm.age)

lm.dis = lm(crim~poly(dis,3))
summary(lm.dis)

lm.rad = lm(crim~poly(rad,3))
summary(lm.rad)

lm.tax = lm(crim~poly(tax,3))
summary(lm.tax)

lm.ptratio = lm(crim~poly(ptratio,3))
summary(lm.ptratio)

lm.black = lm(crim~poly(black,3))
summary(lm.black)

lm.lstat = lm(crim~poly(lstat,3))
summary(lm.lstat)

lm.medv = lm(crim~poly(medv,3))
summary(lm.medv)


##############################
#   Chapter 6: Question 9    #
##############################

rm(list = ls())

library(ISLR)
myData = College
attach(myData)

# 9(a)
set.seed(1)
train = sample(1:nrow(myData), nrow(myData)/2)
test = (-train)
myData.train = myData[train, ]
myData.test = myData[test, ]

# 9(b)
lm.fit = lm(Apps~., data = myData[train,])

lm.pred = predict(lm.fit, myData[test,])
val.errors = mean((myData$Apps[test] - lm.pred) ^ 2)
val.errors

# 9(c)
library(glmnet)

train.mat = model.matrix(Apps~., data=myData[train,])
test.mat = model.matrix(Apps~., data=myData[test,])
grid = 10 ^ seq(4, -2, length=100)

cv.out.ridge = cv.glmnet(train.mat, myData$Apps[train], alpha = 0, lambda=grid, thresh=1e-12)

bestIam.ridge = cv.out.ridge$lambda.min
bestIam.ridge

ridge.pred = predict(cv.out.ridge, s=bestIam.ridge, newx = test.mat)
ridge.errors = mean((myData$Apps[test] - ridge.pred)^2)
ridge.errors

# 9(d)
cv.out.lasso = cv.glmnet(train.mat, myData$Apps[train], alpha = 1, lambda=grid, thresh=1e-12)

bestIam.lasso = cv.out.lasso$lambda.min
bestIam.lasso

lasso.pred = predict(cv.out.lasso, s=bestIam.lasso, newx = test.mat)
lasso.errors = mean((myData$Apps[test] - lasso.pred)^2)
lasso.errors

lasso.mod = glmnet(model.matrix(Apps~., data=myData), myData[, "Apps"], alpha=1)
predict(lasso.mod, s=bestIam.lasso, type="coefficients")

# 9(e)
library(pls)

pcr.fit = pcr(Apps~., data = myData, subset = train, scale = TRUE, validation = "CV")
rvalidationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)

pcr.pred = predict(pcr.fit, myData[test,], ncomp = 16)
pcr.error = mean((myData$Apps[test] - pcr.pred)^2)
pcr.error

# 9(f)
pls.fit = plsr(Apps~., data = myData, subset = train, scale = TRUE, validation = "CV")
validationplot(pls.fit, val.type="MSEP")
summary(pls.fit)

pls.pred = predict(pls.fit, myData[test,], ncomp = 10)
pls.error = mean((myData$Apps[test] - pls.pred)^2)
pls.error

# 9(g)
test.avg = mean(myData.test[, "Apps"])
lm.test.r2 = 1 - mean((myData.test[, "Apps"] - lm.pred)^2) /mean((myData.test[, "Apps"] - test.avg)^2)
ridge.test.r2 = 1 - mean((myData.test[, "Apps"] - ridge.pred)^2) /mean((myData.test[, "Apps"] - test.avg)^2)
lasso.test.r2 = 1 - mean((myData.test[, "Apps"] - lasso.pred)^2) /mean((myData.test[, "Apps"] - test.avg)^2)
pcr.test.r2 = 1 - mean((myData.test[, "Apps"] - data.frame(pcr.pred))^2) /mean((myData.test[, "Apps"] - test.avg)^2)
pls.test.r2 = 1 - mean((myData.test[, "Apps"] - data.frame(pls.pred))^2) /mean((myData.test[, "Apps"] - test.avg)^2)
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), col="blue", names.arg=c("OLS", "Ridge", "Lasso", "PCR", "PLS"), main="Test R-squared")



##############################
#   Chapter 6: Question 11   #
##############################

rm(list = ls())

# 11(a)
set.seed(1)
library(MASS)
library(leaps)
library(glmnet)
library(pls)

myData = Boston
attach(myData)

# Best subset selection

predict.regsubsets = function(object, newdata, id, ...) {
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

k = 10
p = ncol(myData)-1
folds = sample(rep(1:k, length=nrow(myData)))
cv.errors = matrix(NA, k, ncol(myData)-1)
for (i in 1:k) {
  best.fit = regsubsets(crim~., data=Boston[folds!=i,], nvmax=p)
  for (j in 1:p) {
    pred = predict(best.fit, Boston[folds==i, ], id=j)
    cv.errors[i,j] = mean((Boston$crim[folds==i] - pred)^2)
  }
}
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, pch=19, type="b")
which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]


# Lasso
x = model.matrix(crim~.-1, data=myData)
y = crim
cv.lasso = cv.glmnet(x, y, type.measure="mse")
plot(cv.lasso)
coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])

# Ridge
x = model.matrix(crim~.-1, data=myData)
y = crim
cv.ridge = cv.glmnet(x, y, type.measure="mse", alpha=0)
plot(cv.ridge)
coef(cv.ridge)
sqrt(cv.ridge$cvm[cv.ridge$lambda == cv.ridge$lambda.1se])

# PCR
pcr.fit = pcr(crim~., data=myData, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")
summary(pcr.fit)


##############################
#   Chapter 4: Question 10   #
##############################

rm(list = ls())

library(ISLR)
myData = Weekly
attach(Weekly)

# 10(a)
summary(myData)
pairs(myData)
cor(myData[,-9])

# 10(b)
glm.fit = glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = myData, family = binomial)
summary(glm.fit)

# 10(c)
glm.probs = predict(glm.fit, type = "response")

glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, myData$Direction)

# 10(d)
train = (Year < 2009)
test = myData[!train,]

glm.fit = glm(Direction ~ Lag2, data = myData, family = binomial, subset = train)
glm.probs = predict(glm.fit, test, type = "response")

glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > .5] = "Up"

Dir = Direction[!train]
table(glm.pred, Dir)
mean(glm.pred == Dir)

# 10(g)

library(class)

train.X = as.matrix(Lag2[train])
test.X = as.matrix(Lag2[!train])
train.Direction = Direction[train]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)

table(knn.pred, Dir)
mean(knn.pred == Dir)

# 10(h)

# 10(i)

# Logistic regression - (Lag2:Lag1)
glm.fit = glm(Direction ~ Lag2:Lag1, data = Weekly, family = binomial, subset = train)

glm.probs = predict(glm.fit, test, type = "response") 

glm.pred = rep("Down", length(glm.probs))
glm.pred[glm.probs > .5] = "Up"

Dir = Direction[!train]
table(glm.pred, Dir)
mean(glm.pred == Dir)

# KNN - (k = 10)
knn.pred = knn(train.X, test.X, train.Direction, k=10)

table(knn.pred, Dir)
mean(knn.pred == Dir)

# KNN - (k = 100)
knn.pred = knn(train.X, test.X, train.Direction, k=100)

table(knn.pred, Dir)
mean(knn.pred == Dir)


##############################
#   Chapter 8: Question 8    #
##############################

rm(list = ls())

library(tree)
library(randomForest)
library(ISLR)
myData = Carseats
attach(myData)
set.seed(1)

# 8(a)
train = sample(dim(myData)[1], dim(myData)[1] / 2)
myData.train = myData[train, ]
myData.test = myData[-train, ]

# 8(b)
tree.myData = tree(Sales~., data=myData.train)
summary(tree.myData)
plot(tree.myData)
text(tree.myData, pretty=0)

pred.myData = predict(tree.myData, myData.test)
mean((myData.test$Sales - pred.myData)^2)

# 8(c)
cv.myData = cv.tree(tree.myData, FUN=prune.tree)
par(mfrow=c(1, 2))
plot(cv.myData$size, cv.myData$dev, type="b")
plot(cv.myData$k, cv.myData$dev, type="b")

pruned.myData = prune.tree(tree.myData, best=9)
par(mfrow=c(1, 1))
plot(pruned.myData)
text(pruned.myData, pretty=0)
pred.pruned = predict(pruned.myData, myData.test)
mean((myData.test$Sales - pred.pruned)^2)

# 8(d)
bag.myData = randomForest(Sales~., data=myData.train, mtry=10, ntree=500, importance=TRUE)
bag.pred = predict(bag.myData, myData.test)
mean((myData.test$Sales - bag.pred)^2)
importance(bag.myData)

# 8(e)
rf.myData = randomForest(Sales~., data=myData.train, mtry=5, ntree=500, importance=TRUE)
rf.pred = predict(rf.myData, myData.test)
mean((myData.test$Sales - rf.pred)^2)
importance(rf.myData)

##############################
#   Chapter 8: Question 11   #
##############################

rm(list = ls())

library(gbm)
library(ISLR)
myData = Caravan
attach(myData)
set.seed(1)

# 11(a)
train = 1:1000
myData$Purchase = ifelse(myData$Purchase == "Yes", 1, 0)

myData.train = myData[train, ]
myData.test = myData[-train, ]

# 11(b)
boost.myData = gbm(Purchase~., data=myData.train, n.trees=1000, shrinkage=0.01, distribution="bernoulli")
summary(boost.myData)

# 11(c)
boost.prob = predict(boost.myData, myData.test, n.trees=1000, type="response")
boost.pred = ifelse(boost.prob > 0.2, 1, 0)
table(myData.test$Purchase, boost.pred)

lm.myData = glm(Purchase~., data=myData.train, family=binomial)
lm.prob = predict(lm.myData, myData.test, type="response")
lm.pred = ifelse(lm.prob > 0.2, 1, 0)
table(myData.test$Purchase, lm.pred)


##############################
#   Problem 1: Beauty Pays   #
##############################

beauty_data = read.csv("C:/Users/Neerav Basant/Desktop/Summer/Predictive Modeling/Part 1/BeautyData.csv")
attach(beauty_data)

lm.fit = lm(CourseEvals~., data = beauty_data)

summary(lm.fit)



##########################################
#   Problem 2: Housing Price Structure   #
##########################################

Housing_Data = read.csv("C:/Users/Neerav Basant/Desktop/Summer/Predictive Modeling/Part 1/MidCity.csv")
attach(Housing_Data)

n = dim(Housing_Data)[1]

dn1 = rep(0,n)
dn1[Nbhd==1]=1

dn2 = rep(0,n)
dn2[Nbhd==2]=1

dn3 = rep(0,n)
dn3[Nbhd==3]=1

BR = rep(0,n)
BR[Brick=="Yes"]=1

Price = Price/1000
SqFt = SqFt/1000

MidCityModel = lm(Price~BR+dn2+dn3+SqFt+Offers+SqFt+Bedrooms+Bathrooms)

summary(MidCityModel)
confint(MidCityModel)

model2 = lm(Price~BR+dn2+dn3+SqFt+Offers+SqFt+Bedrooms+Bathrooms + BR:dn3)

summary(model2)
confint(model2)
confint(model2, level = 0.99)
