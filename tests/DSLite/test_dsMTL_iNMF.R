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


##########################
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_iNMF_X.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_iNMF_X.rda"), format = "matrix")

names(resX)=paste0("X",1:2)
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
}


##########################
#set options for solver
##########################
solveOpt=list(maxIter=50, tol=0.01, ter=2)


#################################################################################################################################
#Test solvers
#################################################################################################################################
nRow=50; nCols=c(50, 50, 50); 
seedH=100; seedW=c(544, 10); seedHv=c(122, 99)
rank=1
cally <- call('initRanMatsWithSeedsDS', rank, "X", seedH, seedHv[1], seedW[1])
datashield.assign.expr(conns = datasources[1], symbol = "Vars", expr = cally)
cally <- call('initRanMatsWithSeedsDS', rank, "X", seedH, seedHv[2], seedW[2])
datashield.assign.expr(conns = datasources[2], symbol = "Vars", expr = cally)
set.seed(seedH)
newH=  matrix(data = runif(n=nRow*rank, min = 0, max = 2), nrow = nRow, ncol = rank)
mm.dist=ds.solveINMF(datasources, Xs="X", newH=newH, rank=rank, lam=5, Sp=0.01, opts=solveOpt, nDigits=4)
#plot the shared component of both cohorts
plot(mm.dist$H, xlab="shared dimension", ylab="coefficients")
#plot the objectives
plot(mm.dist$objList, ylab="objective values", xlab="iteration")
#################################################################################################################################


#################################################################################################################################
#Tests for algorithm training
#################################################################################################################################
mm = ds.MTL_iNMF_Train(datasources = datasources, Xs = "X", rank = 1, n_initializations = 3, Sp = 0.01, lam = 10, opts=solveOpt)

#Get raw data
library(resourcer)
XX=list()
res <- newResource(url = "https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server1/dsMTL_iNMF_X.rda", format = "matrix")
XX[[1]]=as.resource.object(newResourceClient(res))
res <- newResource(url = "https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server2/dsMTL_iNMF_X.rda", format = "matrix")
XX[[2]]=as.resource.object(newResourceClient(res))

#plot the raw matrices and the identified shared component
par(mfrow=c(1,3))
image(t(apply(XX[[1]], 2, rev)), xlab="raw data matrix 1")
image(t(apply(XX[[2]], 2, rev)), xlab="raw data matrix 2")
image(t(apply(do.call(cbind, mm$H_all), 2, rev)), xlab="3 initializations", ylab="shared component")
#################################################################################################################################

DSI::datashield.logout(datasources)
