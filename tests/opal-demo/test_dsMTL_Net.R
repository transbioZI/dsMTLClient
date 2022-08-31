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




##################################################################################################################
#Regression
##################################################################################################################

##########################
#load data
##########################
datashield.assign.resource(datasources[1],symbol="X",resource = "dsMTL_Server1.dsMTL_Net_R_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsMTL_Server1.dsMTL_Net_R_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsMTL_Server2.dsMTL_Net_R_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsMTL_Server2.dsMTL_Net_R_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"; G=matrix(c(1,-1), 2,1)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;


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
#load data
##########################
datashield.assign.resource(datasources[1],symbol="X",resource = "dsMTL_Server1.dsMTL_Net_C_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsMTL_Server1.dsMTL_Net_C_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsMTL_Server2.dsMTL_Net_C_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsMTL_Server2.dsMTL_Net_C_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"; G=matrix(c(1,-1), 2,1)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;



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

