# ── Data generation ────────────────────────────────────────────────────────────
# Shared with 1_efficiency_access_classification.R
createDataset=function(n, p, sp, type, numCov, noise1, noise2){
  w = matrix(0,nrow = p, ncol = 1)
  nonSp = p-sp
  signatures=sign(stats::rnorm(nonSp))*stats::rnorm(nonSp, mean=1)
  w[1:nonSp]=signatures
  cov=sign(stats::rnorm(n))*stats::rnorm(n, mean=1)
  
  xx <- matrix(data=stats::rnorm(n*p),ncol=p)
  xx[,1:numCov]=xx[,1:numCov] + noise2 * cov
  xx=apply(xx, 2, function(x)(x-mean(x))/sd(x))
  
  ws=w; ws[1:numCov]=0
  y=xx %*% ws 
  y=(y-mean(y))/sd(y) + noise1*cov
  y=(y-mean(y))/sd(y)
  if(type=="regress"){
    yy <- y + stats::rnorm(n, sd = 0.5, mean = 0)
  } else if (type=="classify"){
    yy <- sign(y + stats::rnorm(n, sd = 0.1, mean = 0))
  }
  
  return(list(x=xx, y=yy, w=w, cov=matrix(cov, ncol=1), ws=ws ))
}
