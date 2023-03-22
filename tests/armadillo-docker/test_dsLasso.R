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
datashield.assign.resource(datasources[1],symbol="X",resource = "dsmtlserver1/test/dsLasso_R_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsmtlserver1/test/dsLasso_R_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsmtlserver2/test/dsLasso_R_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsmtlserver2/test/dsLasso_R_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
X="X"; Y="Y"

##########################
#set options for solver
##########################
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
#load data
##########################
datashield.assign.resource(datasources[1],symbol="X",resource = "dsmtlserver1/test/dsLasso_C_X")
datashield.assign.expr(conns = datasources[1],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[1],symbol="Y",resource = "dsmtlserver1/test/dsLasso_C_Y")
datashield.assign.expr(conns = datasources[1],  symbol = "Y", expr = quote(as.resource.object(Y)))

datashield.assign.resource(datasources[2],symbol="X",resource = "dsmtlserver2/test/dsLasso_C_X")
datashield.assign.expr(conns = datasources[2],  symbol = "X", expr = quote(as.resource.object(X)))
datashield.assign.resource(datasources[2],symbol="Y",resource = "dsmtlserver2/test/dsLasso_C_Y")
datashield.assign.expr(conns = datasources[2],  symbol = "Y", expr = quote(as.resource.object(Y)))
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






