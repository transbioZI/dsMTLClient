ds.setMyServerData= function(myServerKey, data, symbol){
  datashield.assign.table(conns = myServerKey$server, symbol = "localKey", table = "serverDataKey.myKey" )
  
  data.vec=c(nrow(data), as.vector(data))
  data.text=paste0(as.character(data.vec), collapse=",")
  cally = call("setMyServerDataDS", myServerKey$key, data.text)
  datashield.assign.expr(conns = myServerKey$server, symbol = symbol, expr =  cally)
}