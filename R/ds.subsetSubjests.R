ds.subsetSubjests= function(datasources, idx, newSymbol, symbol){
  nTasks=length(datasources)
  for (i in 1:nTasks){
    idx.text=paste0(as.character(idx[[i]]), collapse=",")
    cally = call("subsetSubjestsDS", symbol, idx.text)
    datashield.assign.expr(conns = datasources[i], symbol = newSymbol, expr =  cally)
  }
}