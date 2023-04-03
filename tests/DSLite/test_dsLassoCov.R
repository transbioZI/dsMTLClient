# Load packages
require(dsBase)
require(dsBaseClient)
require(dsMTLBase)
require(dsMTLClient)
require(resourcer)
require(DSLite)
library(dsMTLClient)

#Defining function to create dataset
createDataset=function(n, p, sp, type, numCov){
  w = matrix(0,nrow = p, ncol = 1)
  nonSp=as.integer(p*(1-sp))
  signatures=sign(stats::rnorm(nonSp))*stats::rnorm(nonSp, mean=1, sd=0.1)
  w[1:nonSp]=signatures
  
  xx <- matrix(data=stats::rnorm(n*p),ncol=p)
  xx=apply(xx, 2, function(x)(x-mean(x))/sd(x))
  y=xx %*% w
  y=(y-mean(y))/sd(y)
  if(type=="regress"){
    yy <- y + stats::rnorm(n, sd = 0.1, mean = 0)
  } else if (type=="classify"){
    yy <- sign(y + stats::rnorm(n, sd = 0.1, mean = 0))
  }
  covs=xx[,1:numCov]
  xx[,1:numCov]=covs+stats::rnorm(n, sd = 0.1, mean = 0)
  
  return(list(x=xx, y=yy, w=w, covs=covs))
}








######################################################################################################
# Test regression of dsLassoCov
######################################################################################################
n=600; p=200; sp=0.9; type="regress"; numCov=10
data=createDataset(n, p, sp, type, numCov)
data$x = cbind(data$covs, data$x)
XX=list(data$x[1:200,], data$x[201:400, ], data$x[401:600,])
YY=list(data$y[1:200,, drop=F], data$y[201:400,, drop=F], data$y[401:600,, drop=F])
covar=1:10

resX=list()
resY=list()
dataDir= "C:/Users/han.cao/Dropbox/ResearchCollaboration/Augusto/dslassocov_11_10_22/tempData/"
for(i in 1:length(XX)){
  X=XX[[i]]
  save(X, file=paste0(dataDir,"X", i, ".rda"))
  Y=YY[[i]]
  save(Y, file=paste0(dataDir,"Y", i, ".rda"))
  
  resX[[i]] <- resourcer::newResource(paste0("X",i), paste0("file:", dataDir,"X", i, ".rda"), format = "matrix")
  resY[[i]] <- resourcer::newResource(paste0("Y",i), paste0("file:", dataDir,"Y", i, ".rda"), format = "matrix")
}
names(resX)=paste0("X",1:length(XX))
names(resY)=paste0("Y",1:length(XX))

res=c(resX, resY)
dslite.server <- newDSLiteServer(resources = res)
opts=list(init=0, maxIter=50, tol=0.001, ter=2); C=0.1

builder <- DSI::newDSLoginBuilder()
for (i in 1:length(XX)){
  builder$append(server = paste0("server", i), url = "dslite.server", driver = "DSLiteDriver")
}
logindata <- builder$build()
conns <- datashield.login(logindata, assign=T)
for (j in 1:length(XX)){
  datashield.assign.resource(conns[j],symbol="X",resource = paste0("X", j))
  datashield.assign.expr(conns = conns[j],  symbol = "X", expr = quote(as.resource.object(X)))
  datashield.assign.resource(conns[j],symbol="Y",resource = paste0("Y", j))
  datashield.assign.expr(conns = conns[j],  symbol = "Y", expr = quote(as.resource.object(Y)))
}

##################
# training procedure
##################
fitFed=ds.LassoCov_Train(X="X", Y="Y", type="regress", nlambda=50,lam_ratio=0.001, opts=opts, covar=covar, datasources=conns, nDigits=20)
#plot objective
plot(fitFed$Obj)
#plot the regualrization tree
matplot(t(fitFed$ws), type = "l")

##################
# cv procedure
##################
FedcvR = ds.LassoCov_CVInSite(X="X", Y="Y", type="regress",nfolds=10, lam_ratio=0.001, nlambda=10, opts=opts, covar=covar,  datasources=conns, nDigits=20)
#cv plot
plot(colMeans(FedcvR$mse_fold))
fitFed=ds.LassoCov_Train(X="X", Y="Y", type="regress", nlambda=50, lambda=FedcvR$lambda.min, opts=opts, covar=covar, datasources=conns, nDigits=20)
#feature selection accuracy
length(intersect(order(abs(fitFed$ws[11:nrow(fitFed$ws),1]), decreasing = T)[1:10], 11:20))/10
datashield.logout(conns)
######################################################################################################













######################################################################################################
# compare the results of Fed method to Loccal method for classification
######################################################################################################
n=600; p=200; sp=0.9; type="classify"; numCov=10
data=createDataset(n, p, sp, type, numCov)
data$x = cbind(data$covs, data$x)
XX=list(data$x[1:200,], data$x[201:400, ], data$x[401:600,])
YY=list(data$y[1:200,, drop=F], data$y[201:400,, drop=F], data$y[401:600,, drop=F])
covar=1:10

resX=list()
resY=list()
dataDir= "C:/Users/han.cao/Dropbox/ResearchCollaboration/Augusto/dslassocov_11_10_22/tempData/"
for(i in 1:length(XX)){
  X=XX[[i]]
  save(X, file=paste0(dataDir,"X", i, ".rda"))
  Y=YY[[i]]
  save(Y, file=paste0(dataDir,"Y", i, ".rda"))
  
  resX[[i]] <- resourcer::newResource(paste0("X",i), paste0("file:", dataDir,"X", i, ".rda"), format = "matrix")
  resY[[i]] <- resourcer::newResource(paste0("Y",i), paste0("file:", dataDir,"Y", i, ".rda"), format = "matrix")
}
names(resX)=paste0("X",1:length(XX))
names(resY)=paste0("Y",1:length(XX))

res=c(resX, resY)
dslite.server <- newDSLiteServer(resources = res)
opts=list(init=0, maxIter=50, tol=0.001, ter=2); 

builder <- DSI::newDSLoginBuilder()
for (i in 1:length(XX)){
  builder$append(server = paste0("server", i), url = "dslite.server", driver = "DSLiteDriver")
}
logindata <- builder$build()
conns <- datashield.login(logindata, assign=T)
for (j in 1:length(XX)){
  datashield.assign.resource(conns[j],symbol="X",resource = paste0("X", j))
  datashield.assign.expr(conns = conns[j],  symbol = "X", expr = quote(as.resource.object(X)))
  datashield.assign.resource(conns[j],symbol="Y",resource = paste0("Y", j))
  datashield.assign.expr(conns = conns[j],  symbol = "Y", expr = quote(as.resource.object(Y)))
}

fitFed=ds.LassoCov_Train(X="X", Y="Y", type="classify", nlambda=50,lam_ratio=0.01, opts=opts, covar=covar, datasources=conns, nDigits=20)
#plot objective
plot(fitFed$Obj)
#plot the regualrization tree
matplot(t(fitFed$ws), type = "l")

FedcvR = ds.LassoCov_CVInSite(X="X", Y="Y", type="classify",nfolds=10, lam_ratio=0.01, nlambda=10, opts=opts, covar=covar,  datasources=conns, nDigits=20)
#cv plot
plot(colMeans(FedcvR$mcr_fold))
fitFed=ds.LassoCov_Train(X="X", Y="Y", type="classify", nlambda=50, lambda=FedcvR$lambda.min, opts=opts, covar=covar, datasources=conns, nDigits=20)
#feature selection accuracy
length(intersect(order(abs(fitFed$ws[11:nrow(fitFed$ws),1]), decreasing = T)[1:10], 11:20))/10
#0.9
datashield.logout(conns)





