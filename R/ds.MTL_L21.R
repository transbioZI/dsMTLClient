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
#' @title Solver of FeMTL with least-square loss for joint feature selection
#' @description Solver of FeMTL with least-square loss for joint feature selection
#' @param X The design matrices of multiple cohorts 
#' @param Y Label vectors of multiple cohorts
#' @param lam The hyper-parameter controlling the sparsity   
#' @param C   The hyper-parameter associated with L2 term
#' @param opts A list of options controlling the optimization procedure and specifying the differential privacy component. See Details.      
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' 
#' @return The converged result of optimization
#' @details Solver of FeMTL with least-square loss for joint feature selection.  \cr A list of options controlling the optimization procedure and specifying the differential privacy component based on a privacy parameter epsilon>0 can be provided via the \code{opts} argument:
#' \itemize{
#' \item{\code{init} \cr A value (0 or 1) specifying the starting point of the involved gradient descent algorithm. Specifically, two options are provided: A value of 0 (default) uses the 0 matrix as starting point, while a value of 1 uses a starting point specified by the user. If applicable (i.e., \code{init=1}), the user-defined starting point (matrix) has to be specified via the \code{w0} argument in \code{opts}.}
#' \item{\code{w0} \cr A user-defined starting point (matrix) for the involved gradient descent algorithm, in case \code{init=1} is specified (otherwise, for \code{init=0}, the value is set to NULL, the default).}
#' \item{\code{tol} \cr A value >0 specifying the tolerance of the acceptable precision of solution to terminate the algorithm. Default value is set to 0.01.}
#' \item{\code{maxIter} \cr A value >0 specifying the maximum number of iterations. Default value is set to 50.}
#' \item{\code{ter} \cr A value (1, 2 or 3) specifying one out of three termination rules to determine whether the optimization converges. The first rule (\code{ter=1}) checks whether the current objective value is close enough to 0. The second rule (\code{ter=2}) considers the last two objective values and checks whether the decrement is close enough to 0 (default). The third rule (\code{ter=3}) allows the optimization to be performed for a certain maximum number of iterations (\code{maxIter}).}
#' \item{\code{diffPrivEpsilon} \cr A value >0 serving as a privacy parameter to control the degree of differential privacy. Setting the value to NULL (default) means that no differential privacy is included.}
#' \item{\code{nRunsSensitAn} \cr A value >0 specifying the number of simulation runs for the respective sensitivity analyses in case a differential privacy component is included. Default value is set to 100 (only effective if a corresponding value for \code{diffPrivEpsilon} is provided; otherwise, no differential privacy is included).}
#' }
#' 
#' @import DSI
#' @export  
#' @author Han Cao, Roman Schefzik
################################################################################

ds.LS_MTL_L21 <- function (X, Y, lam, C, opts=list(init=0,w0=NULL,tol=0.01,maxIter=50,ter=2,diffPrivEpsilon=NULL,nRunsSensitAn=100), datasources=NULL, nDigits){
  #min_W sum_k ||Y_k - X_k*W_k||_2^2 +lam||W||_21 + C||W||^2_2 
  #with starting points: 0 or W_0; Data: X and Y; Hyper-parameters: lam, c
  
  proximal_l21 <- function (W, lambda ){
    #prox_f(x)=(1-lam/(max{||x||, lam}))x
    Fnorm <- sqrt(rowSums(W^2))
    zeros <- which(Fnorm==0)              
    prox_f_coef <- 1 - lambda/Fnorm
    prox_f_coef[zeros] <- 0
    prox_f_coef <- prox_f_coef *( prox_f_coef>0 )
    Wp = matrix(rep(prox_f_coef, ncol(W)), nrow=length(prox_f_coef))*W
    return(Wp)
  }
  
  nonsmooth_eval <- function (W){
    return(sum(sqrt(rowSums(W^2)))*lam)
  }
  
  LS_iter_update_tasks <- function(W){
    W=round(W, nDigits)
    callys=lapply(1:nTasks, function(k){ 
      W.text=paste0(as.character(W[,k]), collapse=",")
      cally <- call('LS_iter_updateDS', W.text, X, Y)
      return(cally)
    })
    names(callys)=names(datasources)
    iter_update=DSI::datashield.aggregate(datasources, callys)  
    
    grad_w=sapply(iter_update, function(x)x[[1]]); 
    funcVal=sapply(iter_update, function(x)x[[2]]); 
    
    grad_Ws=grad_w + 2* C * W
    funcVals=sum(funcVal) + C * norm(W, 'f')^2
    
    if(is.null(opts$diffPrivEpsilon)==FALSE){
      
      ##sensitivity analyses (multiple runs)
      grad_Ws_sensit<-list()
      funcVals_sensit<-list()
      nSimulations<-opts$nRunsSensitAn
      
      for(n in 1:nSimulations){
        
        callys.sensit=lapply(1:nTasks, function(k){ 
          W.text=paste0(as.character(W[,k]), collapse=",")
          cally.sensit <- call('LS_simulateDifferencesDS', W.text, X, Y)
          return(cally.sensit)
        })
        names(callys.sensit)=names(datasources)
        iter_update_sensit=DSI::datashield.aggregate(datasources, callys.sensit)  
        
        grad_w_sensit=sapply(iter_update_sensit, function(x)x[[1]]); 
        funcVal_sensit=sapply(iter_update_sensit, function(x)x[[2]]); 
        
        grad_Ws_sensit[[n]]=grad_w_sensit + 2* C * W
        funcVals_sensit[[n]]=sum(funcVal_sensit) + C * norm(W, 'f')^2
      }
      
      ##compute differences original-removed
      gradWsDifferences<-list()
      for(i in 1:nSimulations){
        gradWsDifferences[[i]] <- abs(grad_Ws - grad_Ws_sensit[[i]])
      }
      
      ##find maximum differences over the multiple runs
      maxGradWsDifference <- do.call(pmax, c(gradWsDifferences, na.rm = TRUE))
      
      ##apply differential privacy
      grad_Ws_New <- differential_privacy(
        list(
          og_value = grad_Ws,
          l1_sens = maxGradWsDifference
        ),
        opts$diffPrivEpsilon
      )
      
    }else{grad_Ws_New <-grad_Ws}
    
    return(list(grad_Ws=grad_Ws_New, funcVals=funcVals))
  }
  
  LS_funcVal_eval_tasks <- function ( W){
    W=round(W, nDigits)
    callys <- sapply(1:nTasks, function(k){
      W.text=paste0(as.character(W[,k]), collapse=",")
      cally <- call('LS_funcVal_evalDS', W.text, X, Y)
      return(cally)
    }) 
    names(callys)=names(datasources)
    func=DSI::datashield.aggregate(datasources, callys)  
    
    funcVal<-sum(unlist(func)) + C * norm(W, 'f')^2
    
    if(is.null(opts$diffPrivEpsilon)==FALSE){
      
      ##sensitivity analyses (multiple runs)
      grad_Ws_sensit<-list()
      funcVals_sensit<-list()
      nSimulations<-opts$nRunsSensitAn
      
      for(n in 1:nSimulations){
        
        callys.sensit=lapply(1:nTasks, function(k){ 
          W.text=paste0(as.character(W[,k]), collapse=",")
          cally.sensit <- call('LS_simulateDifferencesDS', W.text, X, Y)
          return(cally.sensit)
        })
        names(callys.sensit)=names(datasources)
        iter_update_sensit=DSI::datashield.aggregate(datasources, callys.sensit)  
        
        grad_w_sensit=sapply(iter_update_sensit, function(x)x[[1]]); 
        funcVal_sensit=sapply(iter_update_sensit, function(x)x[[2]]); 
        
        grad_Ws_sensit[[n]]=grad_w_sensit + 2* C * W
        funcVals_sensit[[n]]=sum(funcVal_sensit) + C * norm(W, 'f')^2
      }
      
      ##compute differences original-removed
      differences<-numeric(nSimulations)
      for(i in 1:nSimulations){
        differences[[i]] <- abs(funcVal - funcVals_sensit[[i]])
      }
      
      ##find maximum differences over the multiple runs
      maxDifference <- max(differences)
      
      ##apply differential privacy
      funcValNew <- differential_privacy(
        list(
          og_value = funcVal,
          l1_sens = maxDifference
        ),
        opts$diffPrivEpsilon
      )
      
    }else{funcValNew<-funcVal}
    
    return(funcValNew)
    
  }
  
  #################################  
  # Main algorithm
  #################################  
  Obj <- vector(); 
  dims=DSI::datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sum(sapply(dims, function(x)x[1]))
  nTasks=length(dims)
  
  
  #initialize a starting point
  if(opts$init==0){
    w0=matrix(0,nrow=nFeats, ncol=nTasks)
  }else if(opts$init==1){
    w0 <- opts$w0
  }    
  
  bFlag <- 0; 
  wz <- w0;
  wz_old <- w0;
  
  t <- 1;
  t_old <- 0;
  iter <- 0;
  gamma <- 1;
  gamma_inc <- 2;
  
  log.niterCall=0; log.nfuncCall=0
  
  while (iter < opts$maxIter){
    alpha <- (t_old - 1) /t;
    ws <- (1 + alpha) * wz - alpha * wz_old;
    
    # compute function value and gradients of the search point
    iter_update=LS_iter_update_tasks(ws)  
    Gws <- iter_update[[1]]
    Fs <- iter_update[[2]]
    log.niterCall=log.niterCall+1
    
    # the Armijo Goldstein line search scheme
    while (TRUE){
      wzp <- proximal_l21(ws - Gws/gamma, lam / gamma);
      Fzp=LS_funcVal_eval_tasks(wzp)
      log.nfuncCall=log.nfuncCall+1
      
      
      delta_wzp <- wzp - ws;
      r_sum <- norm(delta_wzp, 'f')^2
      
      #second order approximation
      Fzp_gamma = Fs + sum(delta_wzp * Gws) + gamma * r_sum/2
      
      if (r_sum <=1e-20){
        bFlag=1; 
        break;
      }
      if (Fzp <= Fzp_gamma) break else {gamma = gamma * gamma_inc}
    }
    
    wz_old = wz; wz = wzp; Obj = c(Obj, Fzp + nonsmooth_eval(wz));
    
    #test stop condition.
    if (bFlag) break;
    if (iter>=2){
      if (opts$ter==1 & abs( Obj[length(Obj)] - Obj[length(Obj)-1] ) <= opts$tol){
        break
      } else if(opts$ter==2 & abs( Obj[length(Obj)] - Obj[length(Obj)-1] ) <= opts$tol*Obj[length(Obj)-1]){
        break
      } else if(opts$ter==3 & iter==opts$maxIter){
        break
      }
    }
    
    iter = iter + 1;
    t_old = t;
    t = 0.5 * (1 + (1+ 4 * t^2)^0.5);
  }
  return(list(W=wzp, Obj=Obj, Logs=c(log.niterCall, log.nfuncCall), gamma=gamma))
}




################################################################################
#' @title Solver of FeMTL with logistic loss for joint feature selection
#' @description Solver of FeMTL with logistic loss for joint feature selection
#' @param X The design matrices of multiple cohorts 
#' @param Y Binary label vectors of multiple cohorts
#' @param lam The hyper-parameter controlling the sparsity   
#' @param C   The hyper-parameter associated with L2 term
#' @param opts A list of options controlling the optimization procedure and specifying the differential privacy component. See Details.     
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' 
#' @return The converged result of optimization
#' @details Solver of FeMTL with logistic loss for joint feature selection. \cr A list of options controlling the optimization procedure and specifying the differential privacy component based on a privacy parameter epsilon>0 can be provided via the \code{opts} argument:
#' \itemize{
#' \item{\code{init} \cr A value (0 or 1) specifying the starting point of the involved gradient descent algorithm. Specifically, two options are provided: A value of 0 (default) uses the 0 matrix as starting point, while a value of 1 uses a starting point specified by the user. If applicable (i.e., \code{init=1}), the user-defined starting point (matrix) has to be specified via the \code{w0} argument in \code{opts}.}
#' \item{\code{w0} \cr A user-defined starting point (matrix) for the involved gradient descent algorithm, in case \code{init=1} is specified (otherwise, for \code{init=0}, the value is set to NULL, the default).}
#' \item{\code{tol} \cr A value >0 specifying the tolerance of the acceptable precision of solution to terminate the algorithm. Default value is set to 0.01.}
#' \item{\code{maxIter} \cr A value >0 specifying the maximum number of iterations. Default value is set to 50.}
#' \item{\code{ter} \cr A value (1, 2 or 3) specifying one out of three termination rules to determine whether the optimization converges. The first rule (\code{ter=1}) checks whether the current objective value is close enough to 0. The second rule (\code{ter=2}) considers the last two objective values and checks whether the decrement is close enough to 0 (default). The third rule (\code{ter=3}) allows the optimization to be performed for a certain maximum number of iterations (\code{maxIter}).}
#' \item{\code{diffPrivEpsilon} \cr A value >0 serving as a privacy parameter to control the degree of differential privacy. Setting the value to NULL (default) means that no differential privacy is included.}
#' \item{\code{nRunsSensitAn} \cr A value >0 specifying the number of simulation runs for the respective sensitivity analyses in case a differential privacy component is included. Default value is set to 100 (only effective if a corresponding value for \code{diffPrivEpsilon} is provided; otherwise, no differential privacy is included).}
#' }
#' 
#' @import DSI
#' @export  
#' @author Han Cao, Roman Schefzik
###############################################################################

ds.LR_MTL_L21 <- function (X, Y, lam, C, opts=list(init=0,w0=NULL,tol=0.01,maxIter=50,ter=2,diffPrivEpsilon=NULL,nRunsSensitAn=100), datasources=NULL, nDigits){
  #min_W sum_k{log(1+exp(-Y_k*X_k*W_k))} +lam||W||_21 + c||W||^2_2 
  #with starting points: 0 or W_0; Data: X and Y; Hyper-parameters: lam, C
  
  proximal_l21 <- function (W, lambda ){
    #prox_f(x)=(1-lam/(max{||x||, lam}))x
    Fnorm <- sqrt(rowSums(W^2))
    zeros <- which(Fnorm==0)              
    prox_f_coef <- 1 - lambda/Fnorm
    prox_f_coef[zeros] <- 0
    prox_f_coef <- prox_f_coef *( prox_f_coef>0 )
    Wp = matrix(rep(prox_f_coef, ncol(W)), nrow=length(prox_f_coef))*W
    return(Wp)
  }
  
  nonsmooth_eval <- function (W){
    return(sum(sqrt(rowSums(W^2)))*lam)
  } 
  
  LR_iter_update_tasks <- function(W){
    W=round(W, nDigits)
    
    ##original analyses
    callys=lapply(1:nTasks, function(k){ 
      W.text=paste0(as.character(W[,k]), collapse=",")
      cally <- call('LR_iter_updateDS', W.text, X, Y)
      return(cally)
    })
    names(callys)=names(datasources)
    iter_update=DSI::datashield.aggregate(datasources, callys)  
    
    grad_w=sapply(iter_update, function(x)x[[1]]); 
    funcVal=sapply(iter_update, function(x)x[[2]]); 
    
    grad_Ws=grad_w + 2* C * W
    funcVals=sum(funcVal) + C * norm(W, 'f')^2
    
    if(is.null(opts$diffPrivEpsilon)==FALSE){
      
      ##sensitivity analyses (multiple runs)
      grad_Ws_sensit<-list()
      funcVals_sensit<-list()
      nSimulations<-opts$nRunsSensitAn
      
      for(n in 1:nSimulations){
        
        callys.sensit=lapply(1:nTasks, function(k){ 
          W.text=paste0(as.character(W[,k]), collapse=",")
          cally.sensit <- call('LR_simulateDifferencesDS', W.text, X, Y)
          return(cally.sensit)
        })
        names(callys.sensit)=names(datasources)
        iter_update_sensit=DSI::datashield.aggregate(datasources, callys.sensit)  
        
        grad_w_sensit=sapply(iter_update_sensit, function(x)x[[1]]); 
        funcVal_sensit=sapply(iter_update_sensit, function(x)x[[2]]); 
        
        grad_Ws_sensit[[n]]=grad_w_sensit + 2* C * W
        funcVals_sensit[[n]]=sum(funcVal_sensit) + C * norm(W, 'f')^2
      }
      
      ##compute differences original-removed
      gradWsDifferences<-list()
      for(i in 1:nSimulations){
        gradWsDifferences[[i]] <- abs(grad_Ws - grad_Ws_sensit[[i]])
      }
      
      ##find maximum differences over the multiple runs
      maxGradWsDifference <- do.call(pmax, c(gradWsDifferences, na.rm = TRUE))
      
      ##apply differential privacy
      grad_Ws_New <- differential_privacy(
        list(
          og_value = grad_Ws,
          l1_sens = maxGradWsDifference
        ),
        opts$diffPrivEpsilon
      )
      
    }else{grad_Ws_New <-grad_Ws}
    
    return(list(grad_Ws=grad_Ws_New, funcVals=funcVals))
  }
  
  
  LR_funcVal_eval_tasks <- function (W){
    W=round(W, nDigits)
    
    ##original analyses
    callys <- sapply(1:nTasks, function(k){
      W.text=paste0(as.character(W[,k]), collapse=",")
      cally <- call('LR_funcVal_evalDS', W.text, X, Y)
      return(cally)
    }) 
    names(callys)=names(datasources)
    func=DSI::datashield.aggregate(datasources, callys)  
    
    funcVal<-sum(unlist(func)) + C * norm(W, 'f')^2
    
    if(is.null(opts$diffPrivEpsilon)==FALSE){
      
      ##sensitivity analyses (multiple runs)
      grad_Ws_sensit<-list()
      funcVals_sensit<-list()
      nSimulations<-opts$nRunsSensitAn
      
      for(n in 1:nSimulations){
        
        callys.sensit=lapply(1:nTasks, function(k){ 
          W.text=paste0(as.character(W[,k]), collapse=",")
          cally.sensit <- call('LR_simulateDifferencesDS', W.text, X, Y)
          return(cally.sensit)
        })
        names(callys.sensit)=names(datasources)
        iter_update_sensit=DSI::datashield.aggregate(datasources, callys.sensit)  
        
        grad_w_sensit=sapply(iter_update_sensit, function(x)x[[1]]); 
        funcVal_sensit=sapply(iter_update_sensit, function(x)x[[2]]); 
        
        grad_Ws_sensit[[n]]=grad_w_sensit + 2* C * W
        funcVals_sensit[[n]]=sum(funcVal_sensit) + C * norm(W, 'f')^2
      }
      
      ##compute differences original-removed
      differences<-numeric(nSimulations)
      for(i in 1:nSimulations){
        differences[[i]] <- abs(funcVal - funcVals_sensit[[i]])
      }
      
      ##find maximum differences over the multiple runs
      maxDifference <- max(differences)
      
      ##apply differential privacy
      funcValNew <- differential_privacy(
        list(
          og_value = funcVal,
          l1_sens = maxDifference
        ),
        opts$diffPrivEpsilon
      )
      
    }else{funcValNew<-funcVal}
    
    return(funcValNew)
    
  }
  
  #################################  
  # Main algorithm
  #################################  
  Obj <- vector(); 
  dims=DSI::datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nTasks=length(dims)
  
  #initialize a starting point
  if(opts$init==0){
    w0=matrix(0,nrow=nFeats, ncol=nTasks)
  }else if(opts$init==1){
    w0 <- opts$w0
  }    
  
  bFlag <- 0; 
  wz <- w0;
  wz_old <- w0;
  
  t <- 1;
  t_old <- 0;
  iter <- 0;
  gamma <- 1;
  gamma_inc <- 2;
  
  log.niterCall=0; log.nfuncCall=0
  
  while (iter < opts$maxIter){
    alpha <- (t_old - 1) /t;
    ws <- (1 + alpha) * wz - alpha * wz_old;
    
    # compute function value and gradients of the search point
    iter_update=LR_iter_update_tasks(ws)  
    Gws <- iter_update$grad_Ws
    Fs <- iter_update$funcVals
    log.niterCall=log.niterCall+1
    
    # the Armijo Goldstein line search scheme
    while (TRUE){
      wzp <- proximal_l21(ws - Gws/gamma, lam / gamma);
      Fzp=LR_funcVal_eval_tasks(wzp)
      
      log.nfuncCall=log.nfuncCall+1
      
      delta_wzp <- wzp - ws;
      r_sum <- norm(delta_wzp, 'f')^2
      
      #second order approximation
      Fzp_gamma = Fs + sum(delta_wzp * Gws) + gamma * r_sum/2
      
      if (r_sum <=1e-20){
        bFlag=1; 
        break;
      }
      
      if (Fzp <= Fzp_gamma) break else {gamma = gamma * gamma_inc}
    }
    
    wz_old = wz; wz = wzp; Obj = c(Obj, Fzp + nonsmooth_eval(wz));
    
    #test stop condition.
    if (bFlag) break;
    if (iter>=2){
      if (opts$ter==1 & abs( Obj[length(Obj)] - Obj[length(Obj)-1] ) <= opts$tol){
        break
      } else if(opts$ter==2 & abs( Obj[length(Obj)] - Obj[length(Obj)-1] ) <= opts$tol*Obj[length(Obj)-1]){
        break
      } else if(opts$ter==3 & iter==opts$maxIter){
        break
      } 
    }
    
    iter = iter + 1;
    t_old = t;
    t = 0.5 * (1 + (1+ 4 * t^2)^0.5);
  }
  return(list(W=wzp, Obj=Obj, Logs=c(log.niterCall, log.nfuncCall), gamma=gamma))
}







################################################################################
#' @title Training a regularization tree for dsMTL_L21
#' @description Training a regularization tree for dsMTL_L21
#' @param X The design matrices of multiple cohorts 
#' @param Y Label vectors of multiple cohorts
#' @param type regression(=regress) or classification(=classify)
#' @param nlambda The length of lambda sequence
#' @param lam_ratio smallest lambda / largest lambda
#' @param lambda The lambda sequence   
#' @param C   The hyper-parameter associated with L2 term
#' @param opts Options controlling the optimization procedure     
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' @param intercept Use intercept(=TRUE) or non-intercept(=FALSE) model 
#' 
#' @return The regularization tree
#' @details Training a regularization tree for dsMTL_L21
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################
ds.MTL_L21_Train = function(X=NULL, Y=NULL, type="regress", nlambda=10, lam_ratio=0.1, lambda=NULL, C=0, 
                         opts=list(init=0, maxIter=50, tol=0.01, ter=2), datasources=NULL, nDigits=10, intercept=F){
  #intercept model
  if (intercept){
    Xnew=paste0(X, ".intercept")
    DSI::datashield.assign.expr(conns = datasources, symbol = Xnew, expr =  call('addInterceptDS', X))
    X=Xnew
  }
  
  #initialize final result
  fit=list();fit$ws=list();fit$Logs=vector();fit$Obj=vector();fit$gamma=vector();fit$type=type
  dims=DSI::datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sapply(dims,function(x)x[1])
  nTasks=length(dims)
  xys=DSI::datashield.aggregate(datasources, call("xtyDS",X, Y ))
  xys=sapply(1:nTasks, function(k)xys[[k]]/nSubs[k])
  xy_norm=sqrt(rowSums(xys^2))
  
  #########################
  #for regression
  #########################
  if (type=="regress"){
    #########################
    #Train the solution path of lasso
    #########################
    #estimate lambda sequence
    if(length(lambda)>1){
      lam_seq=lambda
    } else if(length(lambda)==1){
      lam_max=max(xy_norm)
      lam_min=lambda
      lam_seq=exp(seq(log(lam_max),log(lam_min),length.out = nlambda))
    } else if(is.null(lambda)){
      lam_max=max(xy_norm)
      lam_min=lam_ratio*lam_max
      lam_seq=exp(seq(log(lam_max),log(lam_min),length.out = nlambda))
    }
    
    #warm-start training procedure
    optsTrain=opts
    for(i in 1:length(lam_seq)){
      m=ds.LS_MTL_L21(X=X, Y=Y, lam=lam_seq[i], C=C, opts=optsTrain, datasources=datasources, nDigits=nDigits)
      optsTrain$w0=m$W; optsTrain$init=1
      fit$ws[[i]]=m$W
      fit$Obj=c(fit$Obj, m$Obj)
      fit$Logs=rbind(fit$Logs, m$Logs)
      fit$gamma=c(fit$gamma, m$gamma)
    }
    fit$lam_seq=lam_seq
    names(fit$ws)=paste0("Lam=", round(lam_seq, 2))
  }
  #########################
  #for classification
  #########################
  else if(type=="classify"){
    #########################
    #Train the solution path of lasso 
    #########################
    #estimate lambda sequence
    if(length(lambda)>1){
      lam_seq=lambda
    } else if(length(lambda)==1){
      lam_max=max(xy_norm/2)
      lam_min=lambda
      lam_seq=exp(seq(log(lam_max),log(lam_min),length.out = nlambda))
    } else if(is.null(lambda)){
      lam_max=max(xy_norm/2)
      lam_min=lam_ratio*lam_max
      lam_seq=exp(seq(log(lam_max),log(lam_min),length.out = nlambda))
    }
    
    #warm-start training procedure
    optsTrain=opts
    for(i in 1:length(lam_seq)){
      m=ds.LR_MTL_L21(X=X, Y=Y, lam=lam_seq[i], C=C, opts=optsTrain, datasources=datasources, nDigits=nDigits)
      optsTrain$w0=m$W; optsTrain$init=1
      fit$ws[[i]]=m$W
      fit$Obj=c(fit$Obj, m$Obj)
      fit$Logs=rbind(fit$Logs, m$Logs)
      fit$gamma=c(fit$gamma, m$gamma)
    }
    
    fit$lam_seq=lam_seq
    names(fit$ws)=paste0("Lam=", round(lam_seq, 2))
  }
  
  if(length(lambda)==1){fit$ws=fit$ws[length(fit$ws)]}
  
  return(fit)
}



################################################################################
#' @title Cross-site cross-validation for dsMTL_L21
#' @description Cross-site cross-validation for dsMTL_L21
#' @param X The design matrices of multiple cohorts 
#' @param Y Label vectors of multiple cohorts
#' @param type regression(=regress) or classification(=classify)
#' @param nlambda The length of lambda sequence
#' @param lam_ratio smallest lambda / largest lambda
#' @param lambda The lambda sequence   
#' @param C   The hyper-parameter associated with L2 term
#' @param opts Options controlling the optimization procedure     
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' @param intercept Use intercept(=TRUE) or non-intercept(=FALSE) model 
#' 
#' @return The result of cross-validation
#' @details Cross-site cross-validation for dsMTL_L21
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################
ds.MTL_L21_CVCroSite = function(X=NULL, Y=NULL, type="regress", lam_ratio=0.1, nlambda=10, lambda=NULL,
                             opts=list(init=0, maxIter=50, tol=0.01, ter=2), C=0, datasources=NULL, nDigits=10, intercept=F){
  
  #intercept model
  if (intercept){
    Xnew=paste0(X, ".intercept")
    DSI::datashield.assign.expr(conns = datasources, symbol = Xnew, expr =  call('addInterceptDS', X))
    X=Xnew
  }
  
  dims=DSI::datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sapply(dims,function(x)x[1])
  nTasks=length(dims)
  cvResult=list(); cvResult$type=type; cvResult$C=C; 
  ##source("./dsMTLClient/ds.calcMSE.R")
  ##source("./dsMTLClient/ds.calcMCR.R")
  
  if (type=="regress"){
    mse_fold=vector()
    lam_seq=vector()
    for (k in 1:nTasks){
      fit=ds.MTL_L21_Train(X=X, Y=Y, nlambda=nlambda, lam_ratio=lam_ratio, type="regress", opts=opts, C=C, lambda=lambda, 
                        datasources=datasources[-k], nDigits=nDigits)
      mse_fold=rbind(mse_fold, unlist(sapply(fit$ws, function(x)ds.calcMSE(ws = x, datasourceTest = datasources[k], X=X, Y=Y, average=T))))
      lam_seq=rbind(lam_seq, fit$lam_seq)
    }
    lambda.min=mean(sapply(1:nTasks, function(k)lam_seq[k,order(mse_fold[k,])[1]]))
    colnames(mse_fold)=paste0("Lam",1:ncol(mse_fold))
    colnames(lam_seq)=paste0("Lam",1:ncol(mse_fold))
    cvResult$lam_seq=lam_seq; cvResult$mse_fold=mse_fold; cvResult$lambda.min=lambda.min
  } else if(type=="classify"){
    mcr_fold=vector()
    lam_seq=vector()
    for (k in 1:nTasks){
      fit=ds.MTL_L21_Train(X=X, Y=Y, nlambda=nlambda, lam_ratio=lam_ratio, type="classify", opts=opts, C=C, lambda=lambda, 
                           datasources=datasources[-k], nDigits=nDigits)
      mcr_fold=rbind(mcr_fold, unlist(sapply(fit$ws, function(x)ds.calcMCR(ws = x, datasourceTest = datasources[k], X=X, Y=Y, average=T))))
      lam_seq=rbind(lam_seq, fit$lam_seq)
    }
    lambda.min=mean(sapply(1:nTasks, function(k)lam_seq[k,order(mcr_fold[k,])[1]]))
    colnames(mcr_fold)=paste0("Lam",1:ncol(mcr_fold))
    colnames(lam_seq)=paste0("Lam",1:ncol(mcr_fold))
    cvResult$lam_seq=lam_seq; cvResult$mcr_fold=mcr_fold; cvResult$lambda.min=lambda.min
  }
  return(cvResult)
}





################################################################################
#' @title In-site cross-validation for dsMTL_L21
#' @description In-site cross-validation for dsMTL_L21
#' @param X The design matrices of multiple cohorts 
#' @param Y Label vectors of multiple cohorts
#' @param type regression(=regress) or classification(=classify)
#' @param nfolds The number of folds
#' @param nlambda The length of lambda sequence
#' @param lam_ratio smallest lambda / largest lambda
#' @param lambda The lambda sequence   
#' @param C   The hyper-parameter associated with L2 term
#' @param opts Options controlling the optimization procedure     
#' @param datasources The connections of servers   
#' @param nDigits The number of digits rounded for each number prepared for network transmission 
#' @param intercept Use intercept(=TRUE) or non-intercept(=FALSE) model 
#' 
#' @return The result of cross-validation
#' @details In-site cross-validation for dsMTL_L21
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################
ds.MTL_L21_CVInSite = function(X=NULL, Y=NULL, type="regress", nfolds=10, lam_ratio=0.01, nlambda=10, lambda=NULL,
                            opts=list(init=0, maxIter=50, tol=0.01, ter=2), C=0, datasources=NULL, nDigits=10, intercept=F){

  getCVPartition <- function(nSubs, cv_fold){
    randIdx <- lapply(nSubs, function(x) sample(1:x, x, replace = FALSE)) 
    task_num=length(nSubs)
    
    cvPar = {};
    for (cv_idx in 1: cv_fold){
      # build cross validation data splittings for each task.
      cvTrain = {};
      cvTest = {};
      
      #stratified cross validation
      for (t in 1: task_num){
        te_idx <- seq(cv_idx, nSubs[t], by=cv_fold)
        tr_idx <- seq(1,nSubs[t])[!is.element(1:nSubs[t], te_idx)]
        cvTrain[[t]] = randIdx[[t]][tr_idx]
        cvTest[t] = list(randIdx[[t]][te_idx])
      }
      cvPar[[cv_idx]]=list(cvTrain=cvTrain, cvTest=cvTest);
    }
    return(cvPar)
  }
  
  
  #intercept model
  if (intercept){
    Xnew=paste0(X, ".intercept")
    DSI::datashield.assign.expr(conns = datasources, symbol = Xnew, expr =  call('addInterceptDS', X))
    X=Xnew
  }
  
  #source("./dsMTLClient/ds.subsetSubjests.R")
  #source("./dsMTLClient/ds.calcMSE.R")
  #source("./dsMTLClient/ds.calcMCR.R")

  dims=DSI::datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sapply(dims,function(x)x[1])
  nTasks=length(dims)
  cvResult=list(); cvResult$type=type; cvResult$C=C; 
  cvPar <- getCVPartition(nSubs, nfolds)
  
  if (type=="regress"){
    mse_fold=vector()
    lam_seq=vector()
    for (i in 1:length(cvPar)){
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Xtrain", symbol=X)
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Ytrain", symbol=Y)
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Xtest", symbol=X)
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Ytest", symbol=Y)
      
      fit=ds.MTL_L21_Train(X="Xtrain", Y="Ytrain", nlambda=nlambda, lam_ratio=lam_ratio, type="regress", opts=opts, C=C, lambda=lambda, 
                           datasources=datasources, nDigits=nDigits)
      ws_task=lapply(1:nTasks, function(k) sapply(fit$ws, function(w)w[,k]))
      mse_task=sapply(1:nTasks, function(x) ds.calcMSE(ws = ws_task[[x]], datasourceTest = datasources[x], X="Xtest", Y="Ytest", average=F))
      mse_task=do.call(rbind, mse_task)
      mse_fold=rbind(mse_fold, colMeans(mse_task))
      lam_seq=rbind(lam_seq, fit$lam_seq)
    }
    lambda.min=colMeans(lam_seq)[order(colMeans(mse_fold))[1]]
    colnames(mse_fold)=paste0("Lam",1:ncol(mse_fold))
    colnames(lam_seq)=paste0("Lam",1:ncol(mse_fold))
    cvResult$lam_seq=lam_seq; cvResult$mse_fold=mse_fold; cvResult$lambda.min=lambda.min
    
  } else if(type=="classify"){
    mcr_fold=vector()
    lam_seq=vector()
    for (i in 1:length(cvPar)){
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Xtrain", symbol=X)
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Ytrain", symbol=Y)
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Xtest", symbol=X)
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Ytest", symbol=Y)
      
      fit=ds.MTL_L21_Train(X="Xtrain", Y="Ytrain", nlambda=nlambda, lam_ratio=lam_ratio, type="classify", opts=opts, C=C, lambda=lambda,
                           datasources=datasources, nDigits=nDigits)
      ws_task=lapply(1:nTasks, function(k) sapply(fit$ws, function(w)w[,k]))
      mcr_task=sapply(1:nTasks, function(x) ds.calcMCR(ws = ws_task[[x]], datasourceTest = datasources[x], X="Xtest", Y="Ytest", average=F))
      mcr_task=do.call(rbind, mcr_task)
      mcr_fold=rbind(mcr_fold, colMeans(mcr_task))
      lam_seq=rbind(lam_seq, fit$lam_seq)
    }
    lambda.min=colMeans(lam_seq)[order(colMeans(mcr_fold))[1]]
    colnames(mcr_fold)=paste0("Lam",1:ncol(mcr_fold))
    colnames(lam_seq)=paste0("Lam",1:ncol(mcr_fold))
    cvResult$lam_seq=lam_seq; cvResult$mcr_fold=mcr_fold; cvResult$lambda.min=lambda.min
  }
  return(cvResult)
}

