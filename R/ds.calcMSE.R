ds.calcMSE = function(ws, datasourceTest, X, Y, average=T){
  ws.vec=c(nrow(ws), as.vector(ws))
  ws.text=paste0(as.character(ws.vec), collapse=",")
  cally <- call('calcMSEDS', ws.text, X, Y, average)
  mse=datashield.aggregate(datasourceTest, cally)  
  return(mse)
}
