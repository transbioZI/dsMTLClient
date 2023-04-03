rm(list=ls())
gc()

# Load packages
require(dsBase)
require(dsBaseClient)
require(dsMTLBase)
require(dsMTLClient)
require(resourcer)
require(DSLite)
library(dsMTLClient)



##################################################################################################################
#Regression
##################################################################################################################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsLasso_R_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsLasso_R_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsLasso_R_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsLasso_R_Y.rda"), format = "matrix")

names(resX)=paste0("X",1:2)
names(resY)=paste0("Y",1:2)
res=c(resX, resY)
dslite.server <- newDSLiteServer(resources = res)

builder <- DSI::newDSLoginBuilder()
for (i in 1:2){
  builder$append(server = paste0("server", i), url = "dslite.server", driver = "DSLiteDriver")
}
logindata <- builder$build()
datasources <- datashield.login(logindata, assign=T)
for (j in 1:2){
  DSI::datashield.assign.resource(datasources[j],symbol="X",resource = paste0("X", j))
  DSI::datashield.assign.expr(conns = datasources[j],  symbol = "X", expr = quote(as.resource.object(X)))
  datashield.assign.resource(datasources[j],symbol="Y",resource = paste0("Y", j))
  datashield.assign.expr(conns = datasources[j],  symbol = "Y", expr = quote(as.resource.object(Y)))
}



##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=10; opts$tol=0.01; opts$ter=2;
X="X"
Y="Y"

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
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsLasso_C_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsLasso_C_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsLasso_C_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsLasso_C_Y.rda"), format = "matrix")

names(resX)=paste0("X",1:2)
names(resY)=paste0("Y",1:2)
res=c(resX, resY)
dslite.server <- newDSLiteServer(resources = res)

builder <- DSI::newDSLoginBuilder()
for (i in 1:2){
  builder$append(server = paste0("server", i), url = "dslite.server", driver = "DSLiteDriver")
}
logindata <- builder$build()
datasources <- datashield.login(logindata, assign=T)
for (j in 1:2){
  DSI::datashield.assign.resource(datasources[j],symbol="X",resource = paste0("X", j))
  DSI::datashield.assign.expr(conns = datasources[j],  symbol = "X", expr = quote(as.resource.object(X)))
  datashield.assign.resource(datasources[j],symbol="Y",resource = paste0("Y", j))
  datashield.assign.expr(conns = datasources[j],  symbol = "Y", expr = quote(as.resource.object(Y)))
}
X="X"; Y="Y"

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






