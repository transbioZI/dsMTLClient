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

##########################
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_Net_R_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsMTL_Net_R_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_Net_R_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsMTL_Net_R_Y.rda"), format = "matrix")

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
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;
X="X"; Y="Y"; G=matrix(c(1,-1), 2,1)


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
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_Net_C_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsMTL_Net_C_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_Net_C_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsMTL_Net_C_Y.rda"), format = "matrix")

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
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;
X="X"; Y="Y"; G=matrix(c(1,-1), 2,1)



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

