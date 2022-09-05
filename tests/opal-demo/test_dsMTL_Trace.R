rm(list=ls())
gc()
#load libraries
library(DSI)
library(DSOpal)
library(dsMTLClient)



##################################################################################################################
#login data
##################################################################################################################
builder <- DSI::newDSLoginBuilder()
builder$append(server="s1", url = 'https://opal-demo.obiba.org', user = 'dsuser',
               password = 'P@ssw0rd', driver = "OpalDriver", profile="mtl")
builder$append(server="s2", url = 'https://opal-demo.obiba.org', user = 'dsuser',
               password = 'P@ssw0rd', driver = "OpalDriver", profile="mtl")

logindata <- builder$build()
datasources <- DSI::datashield.login(logins = logindata, assign = TRUE)
datashield.symbols(datasources)








##################################################################################################################
#Regression
##################################################################################################################

##########################
#load data
##########################
datashield.assign.resource(datasources[1],symbol="X",resource = "dsMTL_Server1.dsMTL_Trace_R_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsMTL_Server1.dsMTL_Trace_R_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsMTL_Server2.dsMTL_Trace_R_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsMTL_Server2.dsMTL_Trace_R_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;




##########################
#Tests for solvers
##########################
#use case 1: not accounted for the correlated features
m1=ds.LS_MTL_Trace(X, Y, lam=0.5, C=0, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
#use case 2: accounted for the correlated features
m2=ds.LS_MTL_Trace(X, Y, lam=0.5, C=10, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m2$Obj, ylab="objective values", xlab="iteration")
#singular values of the coefficient matrix
corpcor::fast.svd(m1$W)$d
corpcor::fast.svd(m2$W)$d
##########################

##########################
#Tests for algorithm training 
##########################
#use case 1:lambda sequence was estimated from data
fit1=ds.MTL_Trace_Train(X=X, Y=Y, type="regress", nlambda=5, lam_ratio=0.1, C=0, 
                        opts=opts, datasources=datasources, nDigits=4)
#plot the rank of W along the lambda sequence
plot(sapply(fit1$ws, function(x)length(corpcor::fast.svd(x)$d)), xlab="lambda sequence", 
     ylab="ranks")
#use case 2: use a given lambda
fit2=ds.MTL_Trace_Train(X=X, Y=Y, type="regress", lambda=0.1, nlambda=5, C=0, opts=opts, 
                        datasources=datasources, nDigits=4)
#show the correlation between two models
cor(fit2$ws[[1]])
#use case 3: lambda sequence was inputted from users
fit3=ds.MTL_Trace_Train(X=X, Y=Y, type="regress", lambda=c(1,0.1,0.01), C=0, opts=opts, 
                        datasources=datasources, nDigits=4)
#plot the rank of coefficient matrix
plot(sapply(fit3$ws, function(x)length(corpcor::fast.svd(x)$d)), xlab="lambda sequence", 
     ylab="ranks")
##########################

##########################
#Tests for cross-validation procedure
##########################
cvResult=ds.MTL_Trace_CVInSite(X=X, Y=Y, type="regress", lam_ratio=0.1, nlambda=5, 
                               nfolds=5, opts=opts, C=0, datasources=datasources, nDigits=4)
#plot the result of cross-validation
boxplot(cvResult$mse_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="mean squared error")
fit=ds.MTL_Trace_Train(X=X, Y=Y, type="regress", lambda=cvResult$lambda.min, nlambda=5, 
                       opts=opts, C=0, datasources=datasources, nDigits=4)
#show singular values
corpcor::fast.svd(fit$ws[[1]])$d
#show correlation between models
cor(fit$ws[[1]])
##########################

##################################################################################################################









##################################################################################################################
#Classification
##################################################################################################################

##########################
#load data
##########################
datashield.assign.resource(datasources[1],symbol="X",resource = "dsMTL_Server1.dsMTL_Trace_C_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsMTL_Server1.dsMTL_Trace_C_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsMTL_Server2.dsMTL_Trace_C_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsMTL_Server2.dsMTL_Trace_C_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;



##########################
#Tests for solvers
##########################
#use case 1: not accounted for the correlated features
m1=ds.LR_MTL_Trace(X, Y, lam=0.5, C=0, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m1$Obj, ylab="objective values", xlab="iteration")
#use case 2: accounted for the correlated features
m2=ds.LR_MTL_Trace(X, Y, lam=0.5, C=10, opts, datasources=datasources, nDigits=4)
#plot objectives
plot(m2$Obj, ylab="objective values", xlab="iteration")
#singular values of the coefficient matrix
corpcor::fast.svd(m1$W)$d
corpcor::fast.svd(m2$W)$d
##########################

##########################
#Tests for training procedure
##########################
#use case 1:lambda sequence was estimated from data
fit1=ds.MTL_Trace_Train(X=X, Y=Y, type="classify", nlambda=5, lam_ratio=0.1, C=0, 
                        opts=opts, datasources=datasources, nDigits=4)
#plot the rank of coefficient matrix over lambda
plot(sapply(fit1$ws, function(x)length(corpcor::fast.svd(x)$d)), xlab="lambda sequence", 
     ylab="ranks")
#use case 2: use a given lambda
fit2=ds.MTL_Trace_Train(X=X, Y=Y, type="classify", lambda=0.1, nlambda=5, C=0, opts=opts, 
                        datasources=datasources, nDigits=4)
#correlation between models
cor(fit2$ws[[1]])
#use case 3: lambda sequence was inputted from users
fit3=ds.MTL_Trace_Train(X=X, Y=Y, type="classify", lambda=c(1,0.1,0.01), C=0, opts=opts, 
                        datasources=datasources, nDigits=4)
#plot the rank of coefficient matrix
plot(sapply(fit3$ws, function(x)length(corpcor::fast.svd(x)$d)), xlab="lambda sequence", 
     ylab="ranks")
##########################

##########################
#Tests for cross-validation 
##########################
cvResult=ds.MTL_Trace_CVInSite(X=X, Y=Y, type="classify", lam_ratio=0.1, nlambda=5, 
                               nfolds=5, opts=opts, C=0, datasources=datasources, nDigits=4)
#plot the result of CV
boxplot(cvResult$mcr_fold, names=as.character(round(colMeans(cvResult$lam_seq), 3)), 
        xlab="averaged lambda over folds", 
        ylab="missing classification rate")
fit=ds.MTL_Trace_Train(X=X, Y=Y, type="classify", lambda=cvResult$lambda.min, nlambda=5, 
                       opts=opts, C=0, datasources=datasources, nDigits=4)
#show singular values
corpcor::fast.svd(fit$ws[[1]])$d
#show correlation between models
cor(fit$ws[[1]])
##########################

##################################################################################################################

DSI::datashield.logout(datasources)

