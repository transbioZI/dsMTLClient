ds.calcMCR = function(ws=ws, datasourceTest, X, Y, average=T){
  ws.vec=c(nrow(ws), as.vector(ws))
  ws.text=paste0(as.character(ws.vec), collapse=",")
  cally <- call('calcMCRDS', ws.text, X, Y, average)
  mcr=datashield.aggregate(datasourceTest, cally)  
  return(mcr)
}
