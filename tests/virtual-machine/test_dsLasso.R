rm(list=ls())
gc()
#load libraries
library(DSI)
library(DSOpal)
library(dsMTLClient)


#n=100; p=100; sp=0.7; type="regress"
createDataset=function(n, p, sp, type){
  w = matrix(0,nrow = p, ncol = 1)
  nonSp=as.integer(p*(1-sp))
  signatures=stats::rnorm(nonSp, mean=1, sd=0.1)
  w[1:nonSp]=signatures
  
  xx <- matrix(data=stats::rnorm(n*p),ncol=p)
  xx=apply(xx, 2, function(x)(x-mean(x))/sd(x))
  y=xx %*% w
  y=(y-mean(y))/sd(y)
  if(type=="regress"){
    yy <- y + stats::rnorm(n, sd = 0.5, mean = 0)
  } else if (type=="classify"){
    yy <- sign(y + stats::rnorm(n, sd = 0.5, mean = 0))
  }
  return(list(x=xx, y=yy, w=w))
}




##################################################################################################################
#login data
##################################################################################################################
builder <- DSI::newDSLoginBuilder()
builder$append(server="s1", url = "https://192.168.56.100:8443/", user = "administrator",
               password = "datashield_test&", driver = "OpalDriver", options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")
builder$append(server="s2", url = "https://192.168.56.100:8443/", user = "administrator",
               password = "datashield_test&", driver = "OpalDriver", options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")

logindata <- builder$build()
datasources <- DSI::datashield.login(logins = logindata, assign = TRUE)
datashield.symbols(datasources)



##################################################################################################################
#Regression
##################################################################################################################

##########################
#create and upload regression data
##########################
n=100; p=60; sp=0.7; type="regress"
data=createDataset(n, p, sp, type)
XX=list(data$x[1:50, ], data$x[51:100, ]); 
YY=list(data$y[1:50, , drop=F], data$y[51:100, , drop=F]); 
X="X"; Y="Y"

serverKey1=list(server=datasources[1], key="mannheim2022")
serverKey2=list(server=datasources[2], key="mannheim2022")
ds.setMyServerData(serverKey1, data=XX[[1]], symbol="X")
ds.setMyServerData(serverKey1, data=YY[[1]], symbol="Y")
ds.setMyServerData(serverKey2, data=XX[[2]], symbol="X")
ds.setMyServerData(serverKey2, data=YY[[2]], symbol="Y")
str(ds.getMyServerData(serverKey1, "Y"))
str(ds.getMyServerData(serverKey2, "Y"))

opts=list();opts$init=0; opts$maxIter=10; opts$tol=0.01; opts$ter=2;

##########################
#Tests for solvers
##########################
#use case 1: Lasso solver
m1=ds.LS_Lasso(X, Y, lam=0.1, C=0, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
#use case 2: elastic net solver
m2=ds.LS_Lasso(X, Y, lam=0.1, C=10, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m2$Obj, ylab="objective values", xlab="iteration")
#The number of non-zero coefficients
sum(m1$w!=0)
sum(m2$w!=0)
#compare the coefficient vecter between lasso and elastic model
plot(m1$w, m2$w, xlab="lasso model", ylab="elastic model")

##########################
#Tests for algorithm training
##########################
#use case 1: lambda sequence was estimated from data
fit1=ds.Lasso_Train(X=X, Y=Y, type="regress", nlambda=5, lam_ratio=0.5, C=0, opts=opts, 
                    datasources=datasources, nDigits=4)
#regularization tree
matplot(t(fit1$ws), type = "l", main="solution Path", xlab = "lambda", ylab = "coefficients")
#use case 2: use a given lambda
fit2=ds.Lasso_Train(X=X, Y=Y, type="regress", nlambda=5, lambda=0.05, C=0, opts=opts, 
                    datasources=datasources, nDigits=4)
plot(fit2$ws, xlab="index of coefficients", ylab="values")
#use case 3:lambda sequence was inputted from users
fit3=ds.Lasso_Train(X=X, Y=Y, type="regress", lambda=c(1,0.5,0.05), C=0, opts=opts, 
                    datasources=datasources, nDigits=4)
#regularization tree
matplot(t(fit3$ws), type = "l", main="solution Path", xlab = "lambda", ylab = "coefficients")

##########################
#Tests for cross-validation procedure
##########################
cvResult=ds.Lasso_CVInSite(X=X, Y=Y, type="regress", lam_ratio=0.5, nlambda=5, opts=opts, 
                           C=0, datasources=datasources, 
                           nDigits=4, nfolds=5)
boxplot(cvResult$mse_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="mean squared error")
#training with selected hype-parameter
fit=ds.Lasso_Train(X=X, Y=Y, type="regress", lambda=cvResult$lambda.min, nlambda=5, 
                   opts=opts, C=0, datasources=datasources, nDigits=4)
#coefficients
plot(fit$ws, xlab="index of coefficients", ylab="values")
##################################################################################################################





##################################################################################################################
#Classification
##################################################################################################################

##########################
#create data
##########################
n=100; p=60; sp=0.7; type="classify"
data=createDataset(n, p, sp, type)
XX=list(data$x[1:50, ], data$x[51:100, ]); 
YY=list(data$y[1:50, , drop=F], data$y[51:100, , drop=F]); 
X="X"; Y="Y"

serverKey1=list(server=datasources[1], key="mannheim2022")
serverKey2=list(server=datasources[2], key="mannheim2022")
ds.setMyServerData(serverKey1, data=XX[[1]], symbol="X")
ds.setMyServerData(serverKey1, data=YY[[1]], symbol="Y")
ds.setMyServerData(serverKey2, data=XX[[2]], symbol="X")
ds.setMyServerData(serverKey2, data=YY[[2]], symbol="Y")
str(ds.getMyServerData(serverKey1, "Y"))
str(ds.getMyServerData(serverKey2, "Y"))

opts=list();opts$init=0; opts$maxIter=10; opts$tol=0.01; opts$ter=2;

##########################
#Tests for solvers
##########################
#use case 1: Lasso solver
m1=ds.LR_Lasso(X, Y, lam=0.1, C=0, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
#use case 2: elastic net solver
m2=ds.LR_Lasso(X, Y, lam=0.1, C=10, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m2$Obj, ylab="objective values", xlab="iteration")
#The number of non-zero coefficients
sum(m1$w!=0)
sum(m2$w!=0)
#compare the coefficient vecter between lasso and elastic model
plot(m1$w, m2$w, xlab="lasso model", ylab="elastic model")

##########################
#Tests for algorithm training
##########################
#use case 1: lambda sequence was estimated from data
fit1=ds.Lasso_Train(X=X, Y=Y, type="classify", nlambda=5, lam_ratio=0.5, C=0, opts=opts, 
                    datasources=datasources, nDigits=4)
#regularization tree
matplot(t(fit1$ws), type = "l", main="solution Path", xlab = "lambda", ylab = "coefficients")
#use case 2: use a given lambda
fit2=ds.Lasso_Train(X=X, Y=Y, type="classify", nlambda=5, lambda=0.1, C=0, opts=opts, 
                    datasources=datasources, nDigits=4)
#plot the models
plot(fit2$ws, ylab="values")
#use case 3: lambda sequence was inputted from users
fit3=ds.Lasso_Train(X=X, Y=Y, type="classify", lambda=c(1,0.5,0.1), C=0, opts=opts, 
                    datasources=datasources, nDigits=4)
#plot regularization tree
matplot(t(fit3$ws), type = "l", main="solution Path", xlab = "lambda", ylab = "coefficients")

##########################
#Tests for In-site cross-validation procedure
##########################
cvResult=ds.Lasso_CVInSite(X=X, Y=Y, type="classify", lam_ratio=0.5, nlambda=5, opts=opts, 
                           C=0, datasources=datasources, nDigits=4, 
                           nfolds=5)
#plot CV results
boxplot(cvResult$mcr_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="missing classification rate")
fit=ds.Lasso_Train(X=X, Y=Y, type="classify", lambda=cvResult$lambda.min, opts=opts, 
                   C=0, nlambda = 5, datasources=datasources, nDigits=4)
#coefficients
str(fit$ws)
##################################################################################################################

DSI::datashield.logout(datasources)




n=100; p=60; sp=0.7; type="regress"
data=createDataset(n, p, sp, type)
XX=list(data$x[1:50, ], data$x[51:100, ]); YY=list(data$y[1:50, , drop=F], data$y[51:100, , drop=F]); 
X=XX[[1]]; Y=matrix(YY[[1]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsLasso_R_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server1/dsLasso_R_Y.rda")

X=XX[[2]]; Y=matrix(YY[[2]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsLasso_R_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server2/dsLasso_R_Y.rda")

n=100; p=60; sp=0.7; type="classify"
data=createDataset(n, p, sp, type)
XX=list(data$x[1:50, ], data$x[51:100, ]); YY=list(data$y[1:50, , drop=F], data$y[51:100, , drop=F]); 
X=XX[[1]]; Y=matrix(YY[[1]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsLasso_C_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server1/dsLasso_C_Y.rda")

X=XX[[2]]; Y=matrix(YY[[2]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsLasso_C_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server2/dsLasso_C_Y.rda")








