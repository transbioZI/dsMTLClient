################################################################################
#
# Package Name: dsMTLClient
# Description: The client-side functions of dsMTL
#
# dsMTL - a computational framework for privacy-preserving, distributed 
#   multi-task machine learning
# Copyright (C) 2021  Han Cao (han.cao@zi-mannheim.de)
# All rights reserved.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# 
################################################################################





################################################################################
#' @title Solver of Federated integrative NMF
#' @description Solver of Federated NMF
#' @param Xs The data matrices of multiple cohorts 
#' @param newH The starting point shared component matrix H
#' @param rank The hyper-parameter indicate the number of  latent variables
#' @param lam   The hyper-parameter associated with L2 term
#' @param Sp   The hyper-parameter controlling the sparsity
#' @param opts Options controlling the optimization procedure     
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' 
#' @return The converged result of optimization
#' @details Solver of Federated NMF
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################
ds.solveINMF=function(datasources, Xs, newH, rank=2, lam=1, Sp=1, opts=list(maxIter=40, tol=0.01, ter=2), nDigits=10){
  
  ds.updateOtherMats=function( datasources){
    newH.vec=round(as.vector(newH), nDigits)
    #newH.vec=as.vector(newH)
    newH.text=paste0(as.character(newH.vec), collapse=",")
    cally=call("updateOtherMatsDS", Xs, newH.text, "Vars$W", "Vars$Hv",  lam, Sp)
    DSI::datashield.assign.expr(conns = datasources, symbol = "Vars", expr = cally)
  }
  
  ds.updateH = function(datasources) {
    cally = call("updateHDS", Xs, "Vars$H", "Vars$W", "Vars$Hv", Sp, lam)
    collectInfo = DSI::datashield.aggregate(datasources, cally)  
    
    num = lapply(collectInfo, function(x) x[[1]])
    den = lapply(collectInfo, function(x) x[[2]])
    
    num  = Reduce('+', num)     
    den  = Reduce('+', den)    
    newH = num / den
    newH[is.nan(newH)] = 0
    
    res=sum(sapply(collectInfo, function(x) x[[3]]))
    pen_c=sum(sapply(collectInfo, function(x) x[[4]]))
    sp_c=sum(sapply(collectInfo, function(x) x[[5]]))
    obj=res+pen_c+sp_c
    return(list(newH=newH, obj=obj, res=res))  
  }
  
  oldExposures = max.col(newH, 'first') 
  const = 0  
  obj=vector()
  res=vector()
  inconsExp_each=vector()
  numNetAccess=0
  oldH=newH
  
  #######################
  #start the optimization
  #######################
  for (iter in 1:opts$maxIter){
    
    #updated H, obj and res
    updatedInfo=ds.updateH(datasources = datasources)
    newH=updatedInfo$newH
    obj=c(obj, updatedInfo$obj)
    res=c(res, updatedInfo$res)
    
    #update other matrices
    ds.updateOtherMats(datasources = datasources)
    
    
    newExposures = max.col(newH, 'first')
    inconsExp_each=c(inconsExp_each, sum(oldExposures!=newExposures))
    numNetAccess=numNetAccess+2
    
    
    ##---------------------------------------------------------------##
    ##                    Check Convergence                          ##
    ##---------------------------------------------------------------##        
    if (iter>1){
      convergence = abs(obj[length(obj)] - obj[length(obj)-1])
      if (opts$ter==1 & convergence <= opts$tol){
        break
      } else if(opts$ter==2 & convergence <= opts$tol*obj[length(obj)-1]){
        break
      } else if(opts$ter==3 & iter==opts$maxIter){
        break
      } else if(opts$ter==4){
        if ( all(oldExposures == newExposures) == FALSE ) {
          oldExposures = newExposures
          const = 0
        } else {
          const = const + 1
          if (const == opts$tol) {
            oldH=newH
            break
          }
        }
      } 
    }
    oldH=newH
  }
  
  return(list(H=oldH, inconsExp_each=inconsExp_each, iter_to_conv=iter, objList=obj, resList=res, 
              numNetAccess=numNetAccess))
}


################################################################################
#' @title Training models of dsMTL_iNMF
#' @description Training models of dsMTL_iNMF
#' @param Xs The data matrices of multiple cohorts 
#' @param myServerKey The key was granted by the server administrator to access the raw data
#' @param n_initializations The number of variables' initializations 
#' @param rank The hyper-parameter indicate the number of  latent variables
#' @param lam   The hyper-parameter associated with L2 term
#' @param Sp   The hyper-parameter controlling the sparsity
#' @param opts Options controlling the optimization procedure     
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' 
#' @return The converged result of optimization
#' @details Training models of dsMTL_iNMF
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################
ds.MTL_iNMF_Train = function(datasources, Xs, rank, myServerKey=NULL, n_initializations, Sp, lam, opts, nDigits=10) {
  #source("./dsMTLClient/ds.getMyServerData.R")
  
  ds.initMatrices=function(datasources, seedH){
    cally <- call('initMatricesDS', rank, Xs, seedH)
    DSI::datashield.assign.expr(conns = datasources, symbol = "Vars", expr = cally)
  }
  
  nSites = length(datasources)
  data.dim=DSI::datashield.aggregate(datasources, call("dimDS",Xs))
  nRow = data.dim[[1]][1]
  nCols = sapply(1:nSites, function(x) data.dim[[x]][2]  )
  
  # Initialization Metrics 
  objList = list()
  resList = list()
  iter_to_conv = vector()
  inconsExp=list()
  H_all=list()
  numNetAccess=vector()
  if(is.null(myServerKey)){
    Hvs_all=NULL
    Ws_all=NULL
  } else {
    Hvs_all=list()
    Ws_all=list()
    DSI::datashield.assign.table(conns = myServerKey$server, symbol = "localKey", table = "serverDataKey.myKey" )
  }
  
  
  ##-----------------------------------------------------------------------##
  ##                              N inits                                  ##
  ##-----------------------------------------------------------------------##
  for (init_n in 1:n_initializations){
    
    ##-------------------------------------------------------------------##
    ##                   data initialization                             ##
    ##-------------------------------------------------------------------##
    seedH=sample(1000,1)
    ds.initMatrices(datasources = datasources, seedH)
    set.seed(seedH)
    newH=matrix(data = stats::runif(n=nRow*rank, min = 0, max = 2), nrow = nRow, ncol = rank)
    
    
    ##-------------------------------------------------------------------##
    ##                   Start matrix factorization                      ##
    ##-------------------------------------------------------------------##
    solveOpts=opts; 
    solution=ds.solveINMF(datasources, Xs, newH, rank=rank, lam=lam, Sp=Sp, opts=solveOpts, nDigits=nDigits)
    
    #need to calculate objective one more time
    solution$objList=solution$objList[-1]; 
    solution$resList=solution$resList[-1]; 

    
    ##-------------------------------------------------------------------##
    ##                          Collect the results                      ##
    ##-------------------------------------------------------------------##
    
    objList[[init_n]]=solution$objList
    resList[[init_n]]=solution$resList
    iter_to_conv[init_n]=solution$iter_to_conv
    inconsExp[[init_n]]=solution$inconsExp_each
    H_all[[init_n]]=solution$H
    numNetAccess[init_n]=solution$numNetAccess+1
    if (!is.null(myServerKey)) {
      Hvs_all[[init_n]]=ds.getMyServerData(myServerKey, "Vars$Hv")[[1]]
      Ws_all[[init_n]]=ds.getMyServerData(myServerKey, "Vars$W")[[1]]
    }
  }  
  
  return(list(H_all = H_all, Vs_all = Hvs_all, Ws_all=Ws_all, iter_to_conv = iter_to_conv, objList = objList, resList=resList,
              inconsExp =inconsExp, numNetAccess=numNetAccess))
  
}
