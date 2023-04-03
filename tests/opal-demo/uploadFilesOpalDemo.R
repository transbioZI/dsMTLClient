library(opalr)
library(readr)

o <- opal.login('administrator','password', url='https://opal-demo.obiba.org')

# create dsMTL_Server1 project
opal.project_delete(o, "dsMTL_Server1")
opal.project_create(o, "dsMTL_Server1", tags = list("DataSHIELD","MTL"))
opal.resources_perm_add(o, "dsMTL_Server1", subject = "dsuser", type = "user", permission = "view")

res <- GET(url = "https://api.github.com/repos/transbioZI/dsMTLClient/contents/inst/simuData/opal-demo/dsMTL_Server1?ref=main")
files <- sapply(content(res), function(x) x$name)
names <- sapply(strsplit( files, split = "\\."), function(x)x[1])

for (i in 1:length(files)){
opal.resource_create(opal = o, project = 'dsMTL_Server1', name = names[i], format = 'matrix',
                     url = paste0('https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server1/', files[i]))
}

# create dsMTL_Server2 project
opal.project_delete(o, "dsMTL_Server2")
opal.project_create(o, "dsMTL_Server2", tags = list("DataSHIELD","MTL"))
opal.resources_perm_add(o, "dsMTL_Server2", subject = "dsuser", type = "user", permission = "view")

res <- GET(url = "https://api.github.com/repos/transbioZI/dsMTLClient/contents/inst/simuData/opal-demo/dsMTL_Server2?ref=main")
files <- sapply(content(res), function(x) x$name)
names <- sapply(strsplit( files, split = "\\."), function(x)x[1])

for (i in 1:length(files)) {
  opal.resource_create(opal = o, project = 'dsMTL_Server2', name = names[i], format = 'matrix',
                       url = paste0('https://github.com/transbioZI/dsMTLClient/raw/main/inst/simuData/opal-demo/dsMTL_Server2/', files[i]))
}

# create serverDataKey
opal.project_delete(o, "serverDataKey")
opal.project_create(o, "serverDataKey", database = TRUE, tags = list("DataSHIELD","MTL"))

res <- GET(url = "https://raw.githubusercontent.com/transbioZI/dsMTLClient/main/inst/simuData/opal-demo/serverDataKey/myKey.csv")
dtf <- tempfile(fileext = ".csv")
writeBin(content(res, "raw"), dtf)
myKey <- readr::read_csv(dtf)
opal.table_save(o, myKey, "serverDataKey", "myKey", force = TRUE, id.name = names(myKey)[1])
opal.table_perm_add(o, "serverDataKey", "myKey", subject = "dsuser", type = "user", permission = "view")

opal.logout(o)
