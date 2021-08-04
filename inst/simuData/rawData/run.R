
rm(list=ls())
gc()


prepareData4Opal=function(xx, yy, xName, yName){
  write.csv(xx, file=paste0(xName, ".csv"), quote = T, sep=",")
  write.csv(yy, file=paste0(yName, ".csv"), quote = T, sep=",")
  library(xlsx)
  colnames(xx)=paste0("feature", 1:ncol(xx))
  colnames(yy)="label"
  xx.dic=data.frame(table=basename(xName), name=colnames(xx), valueType="decimal", entityType="Participant")
  write.xlsx(xx.dic, file=paste0(xName, "_dic.xlsx"), sheetName = "Variables", row.names = F)
  yy.dic=data.frame(table=basename(yName), name=colnames(yy), valueType="decimal", entityType="Participant")
  write.xlsx(yy.dic, file=paste0(yName, "_dic.xlsx"), sheetName = "Variables", row.names = F)
}



#######################################################
load(file="dsLasso/Regress.rda")
xName="./dsLasso/Regress/server1/X"; yName="./dsLasso/Regress/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsLasso/Regress/server2/X"; yName="./dsLasso/Regress/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################



#######################################################
load(file="dsLasso/Classify.rda")
xName="./dsLasso/Classify/server1/X"; yName="./dsLasso/Classify/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsLasso/Classify/server2/X"; yName="./dsLasso/Classify/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################






#######################################################
load(file="dsMTL_L21/Regress.rda")
xName="./dsMTL_L21/Regress/server1/X"; yName="./dsMTL_L21/Regress/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_L21/Regress/server2/X"; yName="./dsMTL_L21/Regress/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################



#######################################################
load(file="dsMTL_L21/Classify.rda")
xName="./dsMTL_L21/Classify/server1/X"; yName="./dsMTL_L21/Classify/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_L21/Classify/server2/X"; yName="./dsMTL_L21/Classify/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################




#######################################################
load(file="dsMTL_Trace/Regress.rda")
xName="./dsMTL_Trace/Regress/server1/X"; yName="./dsMTL_Trace/Regress/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_Trace/Regress/server2/X"; yName="./dsMTL_Trace/Regress/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################



#######################################################
load(file="dsMTL_Trace/Classify.rda")
xName="./dsMTL_Trace/Classify/server1/X"; yName="./dsMTL_Trace/Classify/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_Trace/Classify/server2/X"; yName="./dsMTL_Trace/Classify/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################




#######################################################
load(file="dsMTL_Net/Regress.rda")
xName="./dsMTL_Net/Regress/server1/X"; yName="./dsMTL_Net/Regress/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_Net/Regress/server2/X"; yName="./dsMTL_Net/Regress/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################



#######################################################
load(file="dsMTL_Net/Classify.rda")
xName="./dsMTL_Net/Classify/server1/X"; yName="./dsMTL_Net/Classify/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_Net/Classify/server2/X"; yName="./dsMTL_Net/Classify/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################

















prepareData4Opal=function(xx, xName){
  write.csv(xx, file=paste0(xName, ".csv"), quote = T, sep=",")
  library(xlsx)
  colnames(xx)=paste0("feature", 1:ncol(xx))
  xx.dic=data.frame(table=basename(xName), name=colnames(xx), valueType="decimal", entityType="Participant")
  write.xlsx(xx.dic, file=paste0(xName, "_dic.xlsx"), sheetName = "Variables", row.names = F)
}




#######################################################
load(file="dsMTL_iNMF/X.rda")
xName="./dsMTL_iNMF/server1/X"
prepareData4Opal(XX[[1]], xName)
xName="./dsMTL_iNMF/server2/X"; 
prepareData4Opal(XX[[2]], xName)
#######################################################



#######################################################
load(file="dsMTL_iNMF/Classify.rda")
xName="./dsMTL_iNMF/Classify/server1/X"; yName="./dsMTL_iNMF/Classify/server1/Y"
prepareData4Opal(XX[[1]], YY[[1]], xName, yName)
xName="./dsMTL_iNMF/Classify/server2/X"; yName="./dsMTL_iNMF/Classify/server2/Y"
prepareData4Opal(XX[[2]], YY[[2]], xName, yName)
#######################################################