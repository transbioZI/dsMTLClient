rm(list=ls())
gc()
#load libraries
library(DSI)
library(DSOpal)
library(dsMTLClient)


simulateData2= function(){
  xx=matrix(0, 50, 50)
  xx[1:30, 1:25]=1
  x1=xx+abs(matrix(rnorm(50*50, mean=0, sd=1)*0.1, 50, 50))

  xx=matrix(0, 50, 50)
  xx[1:20, 1:25]=1
  xx[31:40, 1:25]=1
  x2=xx+abs(matrix(rnorm(50*50, mean=0, sd=1)*0.1, 50, 50))
  
  return(list(x1, x2))
}




#################################################################################################################################
#login data
#################################################################################################################################
builder <- DSI::newDSLoginBuilder()
builder$append(server="study1", url = "https://192.168.56.100:8443/", user = "administrator",
               password = "datashield_test&", driver = "OpalDriver", options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")
builder$append(server="study2", url = "https://192.168.56.100:8443/",  user = "administrator",
               password = "datashield_test&", driver = "OpalDriver", options = "list(ssl_verifyhost=0, ssl_verifypeer=0)")

logindata <- builder$build()
datasources <- DSI::datashield.login(logins = logindata, assign = TRUE)
#################################################################################################################################



#################################################################################################################################
#create and upload data
#################################################################################################################################
XX=simulateData2()
X1=XX[[1]]; X2=XX[[2]]; 

serverKey1=list(server=datasources[1], key="mannheim2022")
serverKey2=list(server=datasources[2], key="mannheim2022")
ds.setMyServerData(serverKey1, data=X1, symbol="X")
ds.setMyServerData(serverKey2, data=X2, symbol="X")
image(t(apply(ds.getMyServerData(serverKey1, "X")[[1]], 2, rev)))
image(t(apply(ds.getMyServerData(serverKey2, "X")[[1]], 2, rev)))

solveOpt=list(maxIter=50, tol=0.01, ter=2)
#################################################################################################################################




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
#plot the raw matrices and the identified shared component
par(mfrow=c(1,3))
image(t(apply(XX[[1]], 2, rev)), xlab="raw data matrix 1")
image(t(apply(XX[[2]], 2, rev)), xlab="raw data matrix 2")
image(t(apply(do.call(cbind, mm$H_all), 2, rev)), xlab="3 initializations", ylab="shared component")
#################################################################################################################################

DSI::datashield.logout(datasources)


XX=simulateData2()
X=XX[[1]]
save(X, file="inst/simuData/opal-demo/dsMTL_Server1/dsMTL_iNMF_X.rda")

X=XX[[2]]
save(X, file="inst/simuData/opal-demo/dsMTL_Server2/dsMTL_iNMF_X.rda")








