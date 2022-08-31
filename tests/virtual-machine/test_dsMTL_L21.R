rm(list=ls())
gc()
#load libraries
library(DSI)
library(DSOpal)
library(dsMTLClient)



#ns=c(100, 150, 200, 250, 300, 350, 400); p=200; sp=0.7; type="regress"
#shared signatures,heterogeneous signatures, random noises, data dimension
createDataset=function(ns, p, sp, type){
  W = matrix(0,nrow = p, ncol = length(ns))
  nonSp=as.integer(p*(1-sp))
  signatures=stats::rnorm(nonSp, mean=1, sd=0.1)
  W=apply(W, 2, function(x){x[1:nonSp]=sample(c(-1,1), size = length(signatures), 
                                              replace = TRUE)* signatures; return(x)})
  unit=as.integer((nrow(W)-nonSp)/length(ns))
  for(i in 1:length(ns)){
    start=p*(1-sp)+1 + unit*(i-1)
    W[start:(start+unit-1),i]=stats::rnorm(unit, mean=0, sd = 0.1)
  }
  
  X=list(); Y=list()
  for(i in 1:length(ns)){
    xx <- matrix(data=stats::rnorm(ns[i]*p),ncol=p, nrow = ns[i])
    xx=apply(xx, 2, function(x)(x-mean(x))/sd(x))
    y=xx %*% W[,i]
    y=(y-mean(y))/sd(y)
    if(type=="regress"){
      yy <- y + stats::rnorm(ns[i], sd = 0.5, mean = 0)
    } else if (type=="classify"){
      yy <- sign(y + stats::rnorm(ns[i], sd = 0.1, mean = 0))
    }
    X[[i]]=xx; Y[[i]]=yy
  }
  
  return(list(X=X, Y=Y, W=W))
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






##################################################################################################################
#Regression
##################################################################################################################

##########################
#create and upload data
##########################
data=createDataset(ns=c(50, 50), p=60, sp=0.7, type="regress")
XX=data$X; YY=data$Y
X="X"; Y="Y"

serverKey1=list(server=datasources[1], key="mannheim2022")
serverKey2=list(server=datasources[2], key="mannheim2022")
ds.setMyServerData(serverKey1, data=XX[[1]], symbol="X")
ds.setMyServerData(serverKey1, data=YY[[1]], symbol="Y")
ds.setMyServerData(serverKey2, data=XX[[2]], symbol="X")
ds.setMyServerData(serverKey2, data=YY[[2]], symbol="Y")
str(ds.getMyServerData(serverKey1, "Y"))
str(ds.getMyServerData(serverKey2, "Y"))

opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;
##########################

##########################
#test solver
##########################
#use case 1: not accounted for the correlated features
m1=ds.LS_MTL_L21(X, Y, lam=.3, C=0, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
#use case 2: accounted for the correlated features
m2=ds.LS_MTL_L21(X, Y, lam=.3, C=10, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m2$Obj, ylab="objective values", xlab="iteration")
#The number of non-zero coefficients for both cohorts
sum(rowSums(m1$W!=0)==2)
sum(rowSums(m2$W!=0)==2)
##########################

##########################
#Tests for algorithm training
##########################
#use case 1:lambda sequence was estimated from data
fit1=ds.MTL_L21_Train(X=X, Y=Y, type="regress", nlambda=5, lam_ratio=0.5, C=0, opts=opts, 
                      datasources=datasources, nDigits=4)
#plot regularization tree
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x^2)))),type = "l", main="solution Path", 
        xlab = "lambda", ylab = "coefficients")
#use case 2: use a given lambda
fit2=ds.MTL_L21_Train(X=X, Y=Y, type="regress", lambda=0.1, nlambda=5, C=0, opts=opts, 
                      datasources=datasources, nDigits=4)
#models
fit2$ws[[1]]
#use case 3: lambda sequence was inputted from users
fit3=ds.MTL_L21_Train(X=X, Y=Y, type="regress", lambda=c(1,0.1,0.01), C=0, opts=opts, 
                      datasources=datasources, nDigits=4)
#plot regularization tree
matplot(t(sapply(fit3$ws, function(x)sqrt(rowSums(x^2)))),type = "l", main="solution Path", 
        xlab = "lambda", ylab = "coefficients")
##########################

##########################
#Tests for cross-validation procedure
##########################
cvResult=ds.MTL_L21_CVInSite(X=X, Y=Y, type="regress", lam_ratio=0.1, nlambda=5, 
                             opts=opts, C=0, datasources=datasources, nDigits=4, nfolds=5)
boxplot(cvResult$mse_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="mean squared error")
fit=ds.MTL_L21_Train(X=X, Y=Y, type="regress", lambda=cvResult$lambda.min, nlambda=5, 
                     opts=opts, C=0, datasources=datasources, nDigits=4 )
#coefficients
fit$ws[[1]]
##########################

##################################################################################################################








##################################################################################################################
#Classification
##################################################################################################################

##########################
#create and upload data
##########################
data=createDataset(ns=c(50, 50), p=60, sp=0.7, type="classify")
XX=data$X; YY=data$Y
X="X"; Y="Y"

serverKey1=list(server=datasources[1], key="mannheim2022")
serverKey2=list(server=datasources[2], key="mannheim2022")
ds.setMyServerData(serverKey1, data=XX[[1]], symbol="X")
ds.setMyServerData(serverKey1, data=YY[[1]], symbol="Y")
ds.setMyServerData(serverKey2, data=XX[[2]], symbol="X")
ds.setMyServerData(serverKey2, data=YY[[2]], symbol="Y")
str(ds.getMyServerData(serverKey1, "Y"))
str(ds.getMyServerData(serverKey2, "Y"))

opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;
##########################

##########################
#test solver
##########################
#use case 1: not accounted for the correlated features
m1=ds.LR_MTL_L21(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
#use case 2: accounted for the correlated features
m2=ds.LR_MTL_L21(X, Y, lam=.01, C=10, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m2$Obj, ylab="objective values", xlab="iteration")
#The number of non-zero coefficients for both cohorts
sum(rowSums(m1$W!=0)==2)
sum(rowSums(m2$W!=0)==2)
##########################

##########################
#Tests for algorithm training
##########################
#use case 1:lambda sequence was estimated from data
fit1=ds.MTL_L21_Train(X=X, Y=Y, type="classify", nlambda=5, lam_ratio=.1, C=0, opts=opts, 
                      datasources=datasources, nDigits=4)
#plot regularization tree
matplot(t(sapply(fit1$ws, function(x)sqrt(rowSums(x^2)))),type = "l", main="solution Path", 
        xlab = "lambda", ylab = "coefficients")
#use case 2: use a given lambda
fit2=ds.MTL_L21_Train(X=X, Y=Y, type="classify", lambda=.01, nlambda=5, C=0, opts=opts, 
                      datasources=datasources, nDigits=4)
#plot models
fit2$ws[[1]]
#use case 3: lambda sequence was inputted from users
fit3=ds.MTL_L21_Train(X=X, Y=Y, type="classify", lambda=c(1,0.1,0.01), C=0, opts=opts, 
                      datasources=datasources, nDigits=4)
matplot(t(sapply(fit3$ws, function(x)sqrt(rowSums(x^2)))),type = "l", main="solution Path", 
        xlab = "lambda", ylab = "coefficients")
##########################

##########################
#Tests for cross-validation procedure
##########################
cvResult=ds.MTL_L21_CVInSite(X=X, Y=Y, type="classify", lam_ratio=0.1, nlambda=5, 
                             opts=opts, C=0, datasources=datasources, nDigits=4, nfolds=5)
#plot the result of CV
boxplot(cvResult$mcr_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="missing classification rate")
fit=ds.MTL_L21_Train(X=X, Y=Y, type="classify", lambda=cvResult$lambda.min, nlambda=5, 
                     opts=opts, C=0, datasources=datasources, nDigits=4 )
#coefficients
fit$ws[[1]]
##########################

##################################################################################################################

DSI::datashield.logout(datasources)


data=createDataset(ns=c(50, 50), p=60, sp=0.7, type="regress")
XX=data$X; YY=data$Y
X=XX[[1]]; Y=matrix(YY[[1]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_L21_R_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_L21_R_Y.rda")

X=XX[[2]]; Y=matrix(YY[[2]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_L21_R_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_L21_R_Y.rda")

data=createDataset(ns=c(50, 50), p=60, sp=0.7, type="classify")
XX=data$X; YY=data$Y
X=XX[[1]]; Y=matrix(YY[[1]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_L21_C_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_L21_C_Y.rda")

X=XX[[2]]; Y=matrix(YY[[2]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_L21_C_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_L21_C_Y.rda")


