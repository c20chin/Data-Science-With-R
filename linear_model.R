setwd("../R_working_directory")
dat = read.csv("./AlbumSales.csv")
head(dat)
y = dat[,1]
x1 = dat[,2]
x2 = dat[,3]
x3 = dat[,4]

###===Topic 1. Linear Model

eq1 = as.formula(Sales~Adv) # Sales = a + b*Adv
output1 = lm(eq1, data = dat)
summary(output1)
names(summary(output1))

#coefficient   #beta
X=cbind(1,x1) 
inv.XX=solve((t(X)%*%X)) #solve(crossprod(X))
inv.XX %*% t(X) %*% y
beta.hat = output1$coeff
beta.hat
solve(crossprod(X))
inv.XX


#betahat
fitted=cbind(1,x1) %*% beta.hat
compare.fitted = cbind(fitted,output1$fitted.values)
head(compare.fitted)

# residule
rsd=dat$Sales-cbind(1,x1) %*% beta.hat
compare.resid = cbind(rsd, output1$residuals)
head(compare.resid)

#standard deviation
std.dev=sqrt(sum(rsd^2)/(nrow(rsd)-length(beta.hat)))
std.dev
summary(output1)$sigma
output1$sigma #Extract Residual Standard Deviation

solve((t(X) %*% X))
summary(output1)$cov.unscale

COV=solve((t(X) %*%X) * sum(rsd^2)/(nrow(rsd)-length(beta.hat)))
Std.Error=sqrt(diag(COV))
Std.Error
summary(output1)$coef

RMSE = sqrt(mean(rsd^2))
MAE = mean(abs(rsd))
MAPE = mean(abs(rsd/y))





###=== Topic 2. K-Fold CV
ID <- seq_len(length(y))
k = 10
fold = sample(rep(1:k, length.out = length(y)))

cv.errors = NULL
for (i in 1:k) {
  
  testset <- ID[fold==i]
  trainset <- ID[fold!=i]
  trainmodel <- lm(eq1, data=dat[trainset,])
  test.fit = predict(trainmodel, newdata= dat[testset,])
  test.errors=dat[testset,1]-test.fit
  
  cv.errors=c(cv.errors, test.errors)
  
}

sqrt(mean(cv.errors^2))
mean(abs(cv.errors))
mean(abs(cv.errors[order(as.integer(names(cv.errors)))]/y))