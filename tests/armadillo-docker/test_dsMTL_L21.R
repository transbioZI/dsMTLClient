rm(list=ls())
gc()
#load libraries
library(DSI)
library(DSMolgenisArmadillo)
library(dsMTLClient)



##################################################################################################################
#login data
##################################################################################################################
builder <- DSI::newDSLoginBuilder(.silent = TRUE)
builder$append(server="s1", url = 'http://localhost:8080', user = 'admin',
               password = 'admin', driver = "ArmadilloDriver", profile="default")
builder$append(server="s2", url = 'http://localhost:8080', user = 'admin',
               password = 'admin', driver = "ArmadilloDriver", profile="default")

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
datashield.assign.resource(datasources[1],symbol="X",resource = "dsmtlserver1/test/dsMTL_L21_R_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsmtlserver1/test/dsMTL_L21_R_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsmtlserver2/test/dsMTL_L21_R_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsmtlserver2/test/dsMTL_L21_R_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;

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
#load data
##########################
datashield.assign.resource(datasources[1],symbol="X",resource = "dsmtlserver1/test/dsMTL_L21_C_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsmtlserver1/test/dsMTL_L21_C_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsmtlserver2/test/dsMTL_L21_C_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsmtlserver2/test/dsMTL_L21_C_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;


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


