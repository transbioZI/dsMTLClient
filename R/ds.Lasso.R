ds.LS_Lasso <- function (X, Y, lam, C, opts, datasources, nDigits){
  proximal_l1 <- function (W, lambda ){
    p <- abs(W) - lambda
    p=p*(p>0)
    Wp <- sign(W) * p
    return(Wp)
  }
  
  nonsmooth_eval <- function (W, lam){
    return(lam*sum(abs(W)))
  }  
  
  # Main algorithm
  Obj <- vector(); 
  dims=datashield.aggregate(datasources, call("dimDS",X ))
  nSubs=sapply(dims, function(x)x[1])
  nFeats=dims[[1]][2]
  nTasks= length(dims)
  
  #initialize a starting point
  if(opts$init==0){
    w0=rep(0,nFeats)
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
    ws=round(ws, nDigits)
    w.text=paste0(as.character(ws), collapse=",")
    cally <- call('LS_iter_updateDS', w.text, X, Y)
    iter_update=datashield.aggregate(datasources, cally)  
    Gws <- rowSums(sapply(1:nTasks, function(k)iter_update[[k]][[1]]*nSubs[k]/sum(nSubs))) + 2*C*ws
    Fs <- sum(sapply(1:nTasks, function(k)iter_update[[k]][[2]]*nSubs[k]/sum(nSubs))) + C* sum((ws)^2)
    log.niterCall=log.niterCall+1
    
    # the Armijo Goldstein line search scheme
    while (TRUE){
      wzp <- proximal_l1(ws - Gws/gamma, lam / gamma);
      w.text=paste0(as.character(round(wzp, nDigits)), collapse=",")
      cally <- call('LS_funcVal_evalDS', w.text, X, Y)
      rData=datashield.aggregate(datasources, cally)
      Fzp=sum(sapply(1:nTasks, function(k)rData[[k]]*nSubs[k]/sum(nSubs)))+ C* sum((wzp)^2)
      log.nfuncCall=log.nfuncCall+1
      
      delta_wzp <- wzp - ws;
      r_sum <- base::t(delta_wzp) %*% delta_wzp
      
      #second order approximation
      Fzp_gamma = Fs + base::t(delta_wzp) %*% Gws + gamma * r_sum/2
      
      if (r_sum <=1e-20){
        bFlag=1; 
        break;
      }
      
      if (Fzp <= Fzp_gamma) break else {gamma = gamma * gamma_inc}
    }
    
    wz_old = wz; wz = wzp; Obj = c(Obj, Fzp + nonsmooth_eval(wz, lam));
    
    
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
  
  return(list(w=wzp, Obj=Obj, Logs=c(log.niterCall, log.nfuncCall), gamma=gamma))
}






ds.LR_Lasso <- function (X, Y, lam, C, opts, datasources, nDigits){
  proximal_l1 <- function (W, lambda ){
    p <- abs(W) - lambda
    p=p*(p>0)
    Wp <- sign(W) * p
    return(Wp)
  }
  
  nonsmooth_eval <- function (W, lam){
    return(lam*sum(abs(W)))
  }  
  
  
  # Main algorithm
  Obj <- vector(); 
  dims=datashield.aggregate(datasources, call("dimDS",X ))
  nSubs=sapply(dims, function(x)x[1])
  nFeats=dims[[1]][2]
  nTasks= length(dims)
  
  
  #initialize a starting point
  if(opts$init==0){
    w0=rep(0,nFeats)
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
    ws=round(ws, nDigits)
    w.text=paste0(as.character(ws), collapse=",")
    cally <- call('LR_iter_updateDS', w.text, X, Y)
    iter_update=datashield.aggregate(datasources, cally)  
    Gws <- rowSums(sapply(1:nTasks, function(k)iter_update[[k]][[1]]*nSubs[k]/sum(nSubs))) + 2*C*ws
    Fs <- sum(sapply(1:nTasks, function(k)iter_update[[k]][[2]]*nSubs[k]/sum(nSubs))) + C* sum((ws)^2)
    log.niterCall=log.niterCall+1
    
    # the Armijo Goldstein line search scheme
    while (TRUE){
      wzp <- proximal_l1(ws - Gws/gamma, lam / gamma);
      w.text=paste0(as.character(round(wzp, nDigits)), collapse=",")
      cally <- call('LR_funcVal_evalDS', w.text, X, Y)
      rData=datashield.aggregate(datasources, cally)
      Fzp=sum(sapply(1:nTasks, function(k)rData[[k]]*nSubs[k]/sum(nSubs)))+ C* sum((wzp)^2)
      log.nfuncCall=log.nfuncCall+1
      
      
      delta_wzp <- wzp - ws;
      r_sum <- base::t(delta_wzp) %*% delta_wzp
      
      #second order approximation
      Fzp_gamma = Fs + base::t(delta_wzp) %*% Gws + gamma * r_sum/2
      
      if (r_sum <=1e-20){
        bFlag=1; 
        break;
      }
      
      if (Fzp <= Fzp_gamma) break else {gamma = gamma * gamma_inc}
    }
    
    wz_old = wz; wz = wzp; Obj = c(Obj, Fzp + nonsmooth_eval(wz, lam));
    
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
  
  return(list(w=wzp, Obj=Obj, Logs=c(log.niterCall, log.nfuncCall), gamma=gamma))
}




ds.Lasso_Train = function(X=NULL, Y=NULL, type="regress", nlambda=10, lam_ratio=0.01, lambda=NULL, C=0, 
                       opts=list(init=0, maxIter=20, tol=0.01, ter=2), datasources=NULL, nDigits=10, intercept=F){
  
  #intercept model
  if (intercept){
    Xnew=paste0(X, ".intercept")
    datashield.assign.expr(conns = datasources, symbol = Xnew, expr =  call('addInterceptDS', X))
    X=Xnew
  }
  
  #initialize final result
  fit=list();fit$ws=vector();fit$Logs=vector();fit$Obj=vector();fit$gamma=vector();fit$type=type
  dims=datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sapply(dims,function(x)x[1])
  nTasks=length(dims)
  xys=datashield.aggregate(datasources, call("xtyDS",X, Y ))
  xys=rowSums(do.call(cbind, xys))/sum(nSubs)
  xy_norm=max(abs(xys))
  
  #########################
  #for regression
  #########################
  if (type=="regress"){
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
      m=ds.LS_Lasso(X=X, Y=Y, lam=lam_seq[i], C=C, opts=optsTrain, datasources=datasources, nDigits=nDigits)
      optsTrain$w0=m$w; optsTrain$init=1
      fit$ws=cbind(fit$ws,m$w)
      fit$Obj=c(fit$Obj, m$Obj)
      fit$Logs=rbind(fit$Logs, m$Logs)
      fit$gamma=c(fit$gamma, m$gamma)
    }
    fit$lam_seq=lam_seq
    colnames(fit$ws)=paste0("Lam=", round(lam_seq, 2))
  } else if(type=="classify"){
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
      m=ds.LR_Lasso(X=X, Y=Y, lam=lam_seq[i], C=C, opts=optsTrain, datasources=datasources, nDigits=nDigits)
      optsTrain$w0=m$w; optsTrain$init=1
      fit$ws=cbind(fit$ws,m$w)
      fit$Obj=c(fit$Obj, m$Obj)
      fit$Logs=rbind(fit$Logs, m$Logs)
      fit$gamma=c(fit$gamma, m$gamma)
    }
    fit$lam_seq=lam_seq
    colnames(fit$ws)=paste0("Lam=", round(lam_seq, 2))
  }
  
  rownames(fit$ws)=colnames(X)
  if(length(lambda)==1){fit$ws=fit$ws[,ncol(fit$ws), drop=F]}
  
  return(fit)
}




ds.Lasso_CVCroSite = function(X=NULL, Y=NULL, type="regress", lam_ratio=0.01, nlambda=10, lambda=NULL,
                           opts=list(init=0, maxIter=50, tol=0.001, ter=2), C=0, datasources=NULL, nDigits=10, intercept=F){

    #intercept model
    if (intercept){
      Xnew=paste0(X, ".intercept")
      datashield.assign.expr(conns = datasources, symbol = Xnew, expr =  call('addInterceptDS', X))
      X=Xnew
    }
    
  cvResult=list(); cvResult$type=type; cvResult$C=C; 
  dims=datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sapply(dims,function(x)x[1])
  nTasks=length(dims)
  #source("./dsMTLClient/ds.calcMSE.R")
  #source("./dsMTLClient/ds.calcMCR.R")
  
  if (type=="regress"){
    mse_fold=vector()
    lam_seq=vector()
    for (k in 1:nTasks){
      fit=ds.Lasso_Train(X=X, Y=Y, nlambda=nlambda, lam_ratio=lam_ratio, type="regress", opts=opts, C=C, lambda=lambda, 
                         datasources=datasources[-k], nDigits=nDigits)
      mse_fold=rbind(mse_fold, ds.calcMSE(ws=fit$ws, datasourceTest=datasources[k], X=X, Y=Y, average=F)[[1]])
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
      fit=ds.Lasso_Train(X=X, Y=Y, nlambda=nlambda, lam_ratio=lam_ratio, type="classify", opts=opts, C=C, lambda=lambda, 
                         datasources=datasources[-k], nDigits=nDigits)
      mcr_fold=rbind(mcr_fold, ds.calcMCR(ws=fit$ws, datasourceTest=datasources[k], X=X, Y=Y, average=F)[[1]])
      lam_seq=rbind(lam_seq, fit$lam_seq)
    }
    lambda.min=mean(sapply(1:nTasks, function(k)lam_seq[k,order(mcr_fold[k,])[1]]))
    colnames(mcr_fold)=paste0("Lam",1:ncol(mcr_fold))
    colnames(lam_seq)=paste0("Lam",1:ncol(mcr_fold))
    cvResult$lam_seq=lam_seq; cvResult$mcr_fold=mcr_fold; cvResult$lambda.min=lambda.min
  }
  return(cvResult)
}


getCVPartition <- function(nSubs, cv_fold){
  randIdx <- lapply(nSubs, function(x) sample(1:x, x, replace = FALSE)) 
  task_num=length(nSubs)
  
  cvPar = {};
  for (cv_idx in 1: cv_fold){
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



ds.Lasso_CVInSite = function(X=NULL, Y=NULL, type="regress", nfolds=10, lam_ratio=0.01, nlambda=10, lambda=NULL,
                          opts=list(init=0, maxIter=50, tol=0.01, ter=2), C=0,  datasources=NULL, nDigits=10, intercept=F){
  
  #intercept model
  if (intercept){
    Xnew=paste0(X, ".intercept")
    datashield.assign.expr(conns = datasources, symbol = Xnew, expr =  call('addInterceptDS', X))
    X=Xnew
  }
  
  #source("./dsMTLClient/ds.subsetSubjests.R")
  #source("./dsMTLClient/ds.calcMSE.R")
  #source("./dsMTLClient/ds.calcMCR.R")
  
  dims=datashield.aggregate(datasources, call("dimDS",X ))
  nFeats=dims[[1]][2]
  nSubs=sapply(dims,function(x)x[1])
  nTasks=length(dims)
  cvResult=list(); cvResult$type=type; cvResult$C=C; 
  cvPar <- getCVPartition(nSubs, nfolds)
  
  if (type=="regress"){
    mse_fold=vector()
    lam_seq=vector()
    for (i in 1:length(cvPar)){
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Xtrain", symbol="X")
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Ytrain", symbol="Y")
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Xtest", symbol="X")
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Ytest", symbol="Y")
      
      fit=ds.Lasso_Train(X="Xtrain", Y="Ytrain", nlambda=nlambda, lam_ratio=lam_ratio, type="regress", opts=opts, C=C, lambda=lambda, 
                         datasources=datasources, nDigits=nDigits)
      mse_task=sapply(1:nTasks, function(x) {
        mse=ds.calcMSE(ws = fit$ws, datasourceTest = datasources[x], X="Xtest", Y="Ytest", average=F)
        mse=mse[[1]]*nSubs[x]/sum(nSubs)
        return(mse)})
      mse_fold=rbind(mse_fold, colSums(t(mse_task)))
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
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Xtrain", symbol="X")
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTrain, newSymbol="Ytrain", symbol="Y")
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Xtest", symbol="X")
      ds.subsetSubjests(datasources, idx=cvPar[[i]]$cvTest, newSymbol="Ytest", symbol="Y")

      fit=ds.Lasso_Train(X="Xtrain", Y="Ytrain", nlambda=nlambda, lam_ratio=lam_ratio, type="classify", opts=opts, C=C, lambda=lambda, 
                         datasources=datasources, nDigits=nDigits)
      mcr_task=sapply(1:nTasks, function(x) {
        mcr=ds.calcMCR(ws = fit$ws, datasourceTest = datasources[x], X="Xtest", Y="Ytest", average=F)
        mcr=mcr[[1]]*nSubs[x]/sum(nSubs)
        return(mcr)})
      mcr_fold=rbind(mcr_fold, colSums(t(mcr_task)))
      lam_seq=rbind(lam_seq, fit$lam_seq)
    }
    lambda.min=colMeans(lam_seq)[order(colMeans(mcr_fold))[1]]
    colnames(mcr_fold)=paste0("Lam",1:ncol(mcr_fold))
    colnames(lam_seq)=paste0("Lam",1:ncol(mcr_fold))
    cvResult$lam_seq=lam_seq; cvResult$mcr_fold=mcr_fold; cvResult$lambda.min=lambda.min
  }
  return(cvResult)
}



# Lasso_predict=function(fit, newx){
#   if(fit$type=="regress"){
#     yhat=newx%*%fit$ws
#     
#   }else if(fit$type=="classify"){
#     yhat=1/(1+exp(-newx%*%fit$ws))
#   }
#   return(yhat)
# }


