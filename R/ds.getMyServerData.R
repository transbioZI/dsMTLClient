ds.getMyServerData= function(myServerKey, data){
  datashield.assign.table(conns = myServerKey$server, symbol = "localKey", table = "serverDataKey.myKey" )
  cally = call("getMyServerDataDS", myServerKey$key, data)
  rData = datashield.aggregate(myServerKey$server, cally)  
  
  return(rData)
}