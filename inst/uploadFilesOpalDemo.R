library("opalr")

#o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')

o <- opal.login('administrator','datashield_test&', url='http://192.168.56.101:8080/')


# create dsMTL_Server1 project

opal.project_create(o, "dsMTL_Server1", database = TRUE)

opal.resources_perm_add(o, "dsMTL_Server1", subject = "dsuser", type = "user", permission = "view")

files=list.files("inst/simuData/opal-demo/dsMTL_Server1/")
names=sapply(strsplit( files, split = "\\."), function(x)x[1])

for (i in 1:length(files)){
opal.resource_create(opal = o, project = 'dsMTL_Server1', name = names[i], format = 'matrix',
                     url = paste0('https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server1/', files[i]))
}


# create dsMTL_Server2 project

opal.project_create(o, "dsMTL_Server2", database = TRUE)

opal.resources_perm_add(o, "dsMTL_Server2", subject = "dsuser", type = "user", permission = "view")

files=list.files("inst/simuData/opal-demo/dsMTL_Server2/")
names=sapply(strsplit( files, split = "\\."), function(x)x[1])

for (i in 1:length(files)){
  opal.resource_create(opal = o, project = 'dsMTL_Server2', name = names[i], format = 'matrix',
                       url = paste0('https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server2/', files[i]))
}



opal.logout(o)



library(DSI)
library(DSOpal)

builder <- newDSLoginBuilder()
builder$append(server = 'study1', url = 'https://opal-demo.obiba.org', 
               user = 'dsuser', password = 'password', 
               resource = 'RSRC.asthma')
logindata <- builder$build()

conns <- datashield.login(logins = logindata, 
                          assign = TRUE, 
                          symbol = 'res')

datashield.assign.expr(conns, symbol = 'asthma', 
                       expr = quote(as.resource.object(res)))

  


o <- opal.login('administrator','password', 
                url='https://opal-demo.obiba.org')

opal.resource_create(opal = o, 
                     project = 'RSRC', 
                     name = 'asthma1', 
                     url = 'https://github.com/isglobal-brge/brgedata/raw/master/data/asthma.rda', 
                     format = 'data.frame')

opal.assign.resource(o, 'client', 'RSRC.asthma1')
opal.execute(o, 'class(client)')



library(DSI)
library(DSOpal)

builder <- newDSLoginBuilder()
builder$append(server = 'study1', url = 'https://opal-demo.obiba.org', 
               user = 'dsuser', password = 'password', 
               resource = 'RSRC.asthma1')
logindata <- builder$build()

conns <- datashield.login(logins = logindata, 
                          assign = TRUE, 
                          symbol = 'res')

datashield.assign.resource(conns,symbol="X",resource = "RSRC.asthma1")
datashield.assign.expr(conns, symbol = 'X', expr = quote(as.resource.object(X)))






library(resourcer)
x <- installed.packages()
class(x)
res <- newResource(url = "https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server1/dsLasso_C_X.rda", format = "matrix")
client <- newResourceClient(res)
class(client)
as.resource.object(client)

