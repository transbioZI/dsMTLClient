library(readr)
library(httr)
library(resourcer)
library(MolgenisArmadillo)

armadillo.set_credentials(server = 'http://127.0.0.1:9000', access_key = "molgenis", secret_key = "molgenis")

# create dsmtlserver1 project
if ('dsmtlserver1' %in% armadillo.list_projects())
  armadillo.delete_project('dsmtlserver1')
armadillo.create_project('dsmtlserver1')

res   <- httr::GET(url = "https://api.github.com/repos/transbioZI/dsMTLClient/contents/inst/simuData/opal-demo/dsMTL_Server1?ref=main")
files <- sapply(content(res), function(x) x$name)
names <- sapply(strsplit(files, split = "\\."), function(x) x[1])

for (i in 1:length(files)){
  resource <- resourcer::newResource(
    name   = names[i],
    url    = paste0('https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server1/', files[i]),
    format = "matrix"
  )
  
  if (! armadillo.upload_resource(project = "dsmtlserver1", folder = "test", resource = resource, name = names[i]))
    print(paste0("Error - dsmtlserver1 armadillo.upload_resource: ", names[i], ", ", files[i]))
}

# create dsmtlserver2 project
if ('dsmtlserver2' %in% armadillo.list_projects())
  armadillo.delete_project('dsmtlserver2')
armadillo.create_project('dsmtlserver2')

res   <- httr::GET(url = "https://api.github.com/repos/transbioZI/dsMTLClient/contents/inst/simuData/opal-demo/dsMTL_Server2?ref=main")
files <- sapply(content(res), function(x) x$name)
names <- sapply(strsplit(files, split = "\\."), function(x) x[1])

for (i in 1:length(files)){
  resource <- resourcer::newResource(
    name   = names[i],
    url    = paste0('https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server2/', files[i]),
    format = "matrix"
  )
  
  if (! armadillo.upload_resource(project = "dsmtlserver2", folder = "test", resource = resource, name = names[i]))
    print(paste0("Error - dsmtlserver2 armadillo.upload_resource: ", names[i], ", ", files[i]))
}

# create serverdatakey
if ('serverdatakey' %in% armadillo.list_projects())
  armadillo.delete_project('serverdatakey')
armadillo.create_project('serverdatakey')

res <- GET(url = "https://raw.githubusercontent.com/transbioZI/dsMTLClient/main/inst/simuData/opal-demo/serverDataKey/myKey.csv")
dtf <- tempfile(fileext = ".csv")
writeBin(content(res, "raw"), dtf)
myKey <- readr::read_csv(dtf)

armadillo.upload_table("serverdatakey", "test", myKey, "myKey")
Footer
