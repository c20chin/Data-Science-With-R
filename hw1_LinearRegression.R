setwd("../R_working_directory")
dat = read.csv("./AlbumSales.csv")
head(dat)
y = dat[,1]
x1 = dat[,2]
x2 = dat[,3]
x3 = dat[,4]


####====Homework====####

###===1.===###
eq1 = as.formula(Sales~Adv)
output1=lm(eq1,data = dat)

X = cbind(1,x1)
beta.hat= output1$coeff
fitted = X %*% beta.hat

rsd = dat$Sales - fitted
tot = dat$Sales - mean(dat$Sales)
ssres = sum(rsd^2)
sstot = sum(tot^2)
rsquared = 1 - ssres/sstot

#compare
summary(output1)$r.squared #[1] 0.3346481
rsquared                   #[1] 0.3346481



###===2.===###
eq2=as.formula(Sales~Adv+airplay)
output2 = lm(eq2,data=dat)
X2 = cbind(1,x1,x2)
fitted2 = X2 %*% output2$coeff

#eq2
rsd2 = dat$Sales - fitted2
RMSE_eq2 = sqrt(mean(rsd2^2))
MAE_eq2 = mean(abs(rsd2))
MAPE_eq2 = mean(abs(rsd2/y)) 

#eq1
RMSE = sqrt(mean(rsd^2))
MAE = mean(abs(rsd))
MAPE = mean(abs(rsd/y))

#compare
eq1e = c(RMSE,MAE,MAPE)
eq2e = c(RMSE_eq2,MAE_eq2,MAPE_eq2)
compare_eq = cbind(eq1e, eq2e)
compare_eq

#OUTCOME:
#            eq1e       eq2e
# [1,]   65.6606491   49.0116251
# [2,]   50.8685427   38.8883372
# [3,]    0.4539981    0.3167036

#CONCLUSION: eq2 is better, since its value in RSME, MAE, MAPE are lower


###===3.===###
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

sqrt(mean(cv.errors^2)) #RMSE
mean(abs(cv.errors)) #MAE
mean(abs(cv.errors[order(as.integer(names(cv.errors)))]/y)) #MAPE


# If we use mean(abs(cv.errors/y)) to calculate MAPE:
mean(abs(cv.errors/y)) #wrong MAPE
mean(abs(cv.errors[order(as.integer(names(cv.errors)))]/y)) #real MAPE

# EXPLAIN:
# Since cv.errors are not in right order, when divided by y,
# will be in wrong sequence

# SOLUTION:
mean(abs(cv.errors[order(as.integer(names(cv.errors)))]/y))
# use "order(as.integer())" to reorder the items in the sets