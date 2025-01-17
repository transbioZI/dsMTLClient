
#########
rm(list=ls())
gc()

##load required packages
library(dsBase)
library(dsMTLBase)
library(dsBaseClient)
library(dsMTLClient)
library(resourcer)
library(DSLite)



##################################################################################################################
#I. classification
##################################################################################################################

################################################
############A. examples for L21



##########################
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_L21_C_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsMTL_L21_C_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_L21_C_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsMTL_L21_C_Y.rda"), format = "matrix")

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





#########################################
##1. no differential privacy (epsilon=NULL)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;#opts$diffPrivEpsilon=NULL;
X="X"; Y="Y"


##########################
#test solver
##########################
m1=ds.LR_MTL_L21(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)



#############################################################################
##2. differential privacy with epsilon=0.5 and 40 runs for sensitivity analyses

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;opts$diffPrivEpsilon=0.5;opts$nRunsSensitAn=40;
X="X"; Y="Y"


##########################
#test solver
##########################
set.seed(17)
m2=ds.LR_MTL_L21(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)



################################################
############B. examples for Trace



##########################
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_Trace_C_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsMTL_Trace_C_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_Trace_C_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsMTL_Trace_C_Y.rda"), format = "matrix")

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




#########################################
##1. no differential privacy (epsilon=NULL)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;#opts$diffPrivEpsilon=NULL;
X="X"; Y="Y"


##########################
#test solver
##########################
m3=ds.LR_MTL_Trace(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)



#############################################################################
##2. differential privacy with epsilon=0.5 and 40 runs for sensitivity analyses

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;opts$diffPrivEpsilon=0.5;opts$nRunsSensitAn=40;
X="X"; Y="Y"


##########################
#test solver
##########################
set.seed(17)
m4=ds.LR_MTL_Trace(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)


################################################
############C. examples for Net


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





#########################################
##1. no differential privacy (epsilon=NULL)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;#opts$diffPrivEpsilon=NULL;
X="X"; Y="Y"


##########################
#test solver
##########################
ntasks<-2
G0<-diag(ntasks)-(1/ntasks)
m5<-ds.LR_MTL_Net(X, Y, lam=.01, C=0,G=G0, opts, datasources=datasources, nDigits=4)



#############################################################################
##2. differential privacy with epsilon=0.5 and 40 runs for sensitivity analyses

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;opts$diffPrivEpsilon=0.5;opts$nRunsSensitAn=40;
X="X"; Y="Y"


##########################
#test solver
##########################
set.seed(17)
ntasks<-2
G0<-diag(ntasks)-(1/ntasks)
m6<-ds.LR_MTL_Net(X, Y, lam=.01, C=0, G=G0,opts, datasources=datasources, nDigits=4)






##################################################################################################################
#II. regression
##################################################################################################################



################################################
############A. examples for L21


##########################
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_L21_R_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsMTL_L21_R_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_L21_R_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsMTL_L21_R_Y.rda"), format = "matrix")

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



#########################################
##1. no differential privacy (epsilon=NULL)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;#opts$diffPrivEpsilon=NULL;
X="X"; Y="Y"


##########################
#test solver
##########################
m7=ds.LS_MTL_L21(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)



#############################################################################
##2. differential privacy with epsilon=0.5 and 40 runs for sensitivity analyses

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;opts$diffPrivEpsilon=0.5;opts$nRunsSensitAn=40;
X="X"; Y="Y"


##########################
#test solver
##########################
set.seed(17)
m8=ds.LS_MTL_L21(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)



################################################
############B. examples for Trace


##########################
#load data
##########################
resX=list()
resY=list()
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server1/")
resX[[1]] <- resourcer::newResource(paste0("X",1), paste0("file:", dataDir,"dsMTL_Trace_R_X.rda"), format = "matrix")
resY[[1]] <- resourcer::newResource(paste0("Y",1), paste0("file:", dataDir,"dsMTL_Trace_R_Y.rda"), format = "matrix")
dataDir= paste0(getwd(),"/inst/simuData/opal-demo/dsMTL_Server2/")
resX[[2]] <- resourcer::newResource(paste0("X",2), paste0("file:", dataDir,"dsMTL_Trace_R_X.rda"), format = "matrix")
resY[[2]] <- resourcer::newResource(paste0("Y",2), paste0("file:", dataDir,"dsMTL_Trace_R_Y.rda"), format = "matrix")

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




#########################################
##1. no differential privacy (epsilon=NULL)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;#opts$diffPrivEpsilon=NULL;
X="X"; Y="Y"


##########################
#test solver
##########################
m9=ds.LS_MTL_Trace(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)



#############################################################################
##2. differential privacy with epsilon=0.5 and 40 runs for sensitivity analyses

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;opts$diffPrivEpsilon=0.5;opts$nRunsSensitAn=40;
X="X"; Y="Y"


##########################
#test solver
##########################
set.seed(17)
m10=ds.LS_MTL_Trace(X, Y, lam=.01, C=0, opts, datasources=datasources, nDigits=4)





################################################
############C. examples for Net


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




#########################################
##1. no differential privacy (epsilon=NULL)

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;#opts$diffPrivEpsilon=NULL;
X="X"; Y="Y"


##########################
#test solver
##########################
ntasks<-2
G0<-diag(ntasks)-(1/ntasks)
m11<-ds.LS_MTL_Net(X, Y, lam=.01, C=0,G=G0, opts, datasources=datasources, nDigits=4)



#############################################################################
##2. differential privacy with epsilon=0.5 and 40 runs for sensitivity analyses

##########################
#set options for solver
##########################
opts=list();opts$init=0; opts$maxIter=50; opts$tol=0.01; opts$ter=2;opts$diffPrivEpsilon=0.5;opts$nRunsSensitAn=40;
X="X"; Y="Y"


##########################
#test solver
##########################
set.seed(17)
ntasks<-2
G0<-diag(ntasks)-(1/ntasks)
m12<-ds.LS_MTL_Net(X, Y, lam=.01, C=0, G=G0,opts, datasources=datasources, nDigits=4)


##################################################################################################################

DSI::datashield.logout(datasources)


