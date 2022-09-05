rm(list=ls())
gc()
#load libraries
library(DSI)
library(DSOpal)
library(dsMTLClient)


#random noises, strength of inter-correlation
createDataset=function(ns, p, inteCor, type){
  W=matrix(0,nrow = p, ncol = length(ns))
  W[,1]=stats::rnorm(p)
  for(i in 2:length(ns)){
    W[,i]=W[,i-1]+stats::rnorm(p, sd=inteCor)
  }
  nEdges=length(ns)-1
  rot <- function(x) "[<-"(x, , rev(x))
  g1=matrix(0, ncol=nEdges, nrow=length(ns))
  diag(g1)=1
  g2=matrix(0, ncol=nEdges, nrow=length(ns))
  diag(g2)=-1
  g2=rot(g2)
  G=g1+g2
  
  X=list(); Y=list()
  for(i in 1:length(ns)){
    xx <- matrix(data=stats::rnorm(ns[i]*p),ncol=p, nrow = ns[i])
    xx=apply(xx, 2, function(x)(x-mean(x))/sd(x))
    y=xx %*% W[,i]
    y=(y-mean(y))/sd(y)
    if(type=="regress"){
      yy <- y + stats::rnorm(ns[i], sd = 1, mean = 0)
    } else if (type=="classify"){
      yy <- sign(y + stats::rnorm(ns[i], sd = 1, mean = 0))
    }
    X[[i]]=xx; Y[[i]]=yy
  }
  return(list(X=X, Y=Y, W=W, G=G))
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
data=createDataset(ns=c(50,50), p=60, inteCor=0.7, type="regress")
XX=data$X; YY=data$Y
X="X"; Y="Y"; G=matrix(c(1,-1), 2,1)

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
#Tests for solvers
##########################
m1=ds.LS_MTL_Net(X, Y, lam=0.01, C=10, G=G, opts, datasources=datasources, nDigits=4)
#model correlation
cor(m1$W)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
##########################

##########################
#Tests for training procedure
##########################
#use case 1:lambda sequence was estimated from data
fit1=ds.MTL_Net_Train(X=X, Y=Y, G=G, type="regress", nlambda=5, lam_ratio=0.1, C=1, 
                      opts=opts, datasources=datasources, nDigits=4)
#show correlation of models across cohorts over the lambda sequence
sapply(fit1$ws, function(x)cor(x)[2])
#use case 2: use a given lambda
fit2=ds.MTL_Net_Train(X=X, Y=Y, G=G, type="regress", lambda=0.1, nlambda=5, C=1, 
                      opts=opts, datasources=datasources, nDigits=4)
#correlation between models
cor(fit2$ws[[1]])
#use case 3: lambda sequence was inputted from users
fit3=ds.MTL_Net_Train(X=X, Y=Y, type="regress", lambda=c(1,0.1,0.01), C=1, G=G, 
                      opts=opts, datasources=datasources, nDigits=4)
#correlation of models across cohorts along the lambda sequence
sapply(fit3$ws, function(x)cor(x)[2])
##########################

##########################
#Tests for cross-validation 
##########################
cvResult=ds.MTL_Net_CVInSite(X=X, Y=Y, type="regress", nfolds = 5, lam_ratio=0.1, 
                             nlambda=5, opts=opts, C=1, G=G, datasources=datasources, 
                             nDigits=4)
#plot the cv result
boxplot(cvResult$mse_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="mean squared error")
fit=ds.MTL_Net_Train(X=X, Y=Y, type="regress", lambda=cvResult$lambda.min, nlambda=5, 
                     opts=opts, C=1, G=G, datasources=datasources, nDigits=4)
#show correlation of models across cohorts 
cor(fit$ws[[1]])
##########################

##################################################################################################################







##################################################################################################################
#Classification
##################################################################################################################

##########################
#create and upload data
##########################
data=createDataset(ns=c(50,50), p=60, inteCor=0.7, type="classify")
XX=data$X; YY=data$Y
X="X"; Y="Y"; G=matrix(c(1,-1), 2,1)

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
#Tests for solvers
##########################
m1=ds.LR_MTL_Net(X, Y, lam=0.01, C=1, G=G, opts, datasources=datasources, nDigits=4)
#model correlation
cor(m1$W)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
##########################

##########################
#Tests for training procedure
##########################
#use case 1:lambda sequence was estimated from data
fit1=ds.MTL_Net_Train(X=X, Y=Y, G=G, type="classify", nlambda=5, lam_ratio=0.1, C=1, 
                      opts=opts, datasources=datasources, nDigits=4)
#correlation of models across cohorts along the lambda sequence
sapply(fit1$ws, function(x)cor(x)[2])
#use case 2: use a given lambda
fit2=ds.MTL_Net_Train(X=X, Y=Y, G=G, type="classify", lambda=0.1, nlambda=5, C=1, 
                      opts=opts, datasources=datasources, nDigits=4)
cor(fit2$ws[[1]])
#use case 3: lambda sequence was inputted from users
fit3=ds.MTL_Net_Train(X=X, Y=Y, type="classify", lambda=c(1,0.1,0.01), C=1, G=G, 
                      opts=opts, datasources=datasources, nDigits=4)
#show the correlation of models across cohorts along the lambda sequence
sapply(fit3$ws, function(x)cor(x)[2])
##########################

##########################
#Tests for cross-validation 
##########################
cvResult=ds.MTL_Net_CVInSite(X=X, Y=Y, type="classify", nfolds=5, lam_ratio=0.01, 
                             nlambda=5, opts=opts, C=1, G=G, datasources=datasources, nDigits=4)
#plot the result of CV
boxplot(cvResult$mcr_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="missing classification rate")
fit=ds.MTL_Net_Train(X=X, Y=Y, type="classify", lambda=cvResult$lambda.min, nlambda=5, 
                     opts=opts, C=1, G=G, datasources=datasources, nDigits=4)
#show correlation of models across cohorts 
cor(fit$ws[[1]])
##########################

##################################################################################################################

DSI::datashield.logout(datasources)



data=createDataset(ns=c(50,50), p=60, inteCor=0.7, type="regress")
XX=data$X; YY=data$Y
X=XX[[1]]; Y=matrix(YY[[1]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_Net_R_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_Net_R_Y.rda")

X=XX[[2]]; Y=matrix(YY[[2]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_Net_R_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_Net_R_Y.rda")

data=createDataset(ns=c(50,50), p=60, inteCor=0.7, type="classify")
XX=data$X; YY=data$Y
X=XX[[1]]; Y=matrix(YY[[1]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_Net_C_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_Net_C_Y.rda")

X=XX[[2]]; Y=matrix(YY[[2]], ncol=1)
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_Net_C_X.rda")
save(Y, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_Net_C_Y.rda")



