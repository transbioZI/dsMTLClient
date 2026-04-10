rm(list=ls())
gc()

library(dsMTLClient)
library(dsMTLBase)
require(dsBase)
require(dsBaseClient)
require(resourcer)
require(DSLite)
library(svglite)
source("./createDataset.R")



# Experiment parameters
numFeat=c(100, 200, 300, 500, 700, 1000, 1500, 2000, 3000)
nReps=30
opts=list(init=0, maxIter=50, tol=0.001, ter=2)

# Create log directory
dir.create("logs", recursive = TRUE, showWarnings = FALSE)
dir.create("results", recursive = TRUE, showWarnings = FALSE)
mainLogFile = paste0("logs/1_efficiency_classify_main_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
writeLines(paste("=== Classification Efficiency Analysis Started (Sequential) ===", 
                 "\nDate:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 "\nNumber of repetitions:", nReps,
                 "\nFeature dimensions:", paste(numFeat, collapse=", "),
                 "\n"), mainLogFile)

for (rep in 24:nReps){
  
  # Create log file for this repetition
  repLogFile = paste0("logs/1_efficiency_classify_rep", rep, ".log")
  repStartTime = Sys.time()
  
  writeLines(paste0("=== Repetition ", rep, " Started ===\n",
                    "Start time: ", format(repStartTime, "%Y-%m-%d %H:%M:%S")), 
             repLogFile)
  cat("Starting repetition", rep, "of", nReps, "at", format(repStartTime, "%H:%M:%S"), "\n")

  # Generate dataset once per repetition
  dataGenStart = Sys.time()
  dataALL=createDataset(n=300, p=3000, sp=3000-30, type="classify", numCov=10,noise1=1, noise2=1)
  dataGenEnd = Sys.time()
  dataGenTime = as.numeric(difftime(dataGenEnd, dataGenStart, units="secs"))
  
  write(paste0("Data generation time: ", round(dataGenTime, 2), " seconds"), 
        repLogFile, append=TRUE)
  
  sepLeft=c(1, 101, 201)
  sepRight=c(100, 200, 300)
  dataDir= paste0(getwd(), "/tempData/")
  
  # Create tempData directory if it doesn't exist
  dir.create(dataDir, recursive = TRUE, showWarnings = FALSE)

  timeSpend=vector()
  featAcc=vector()
  detailedLog=list()
  
  for(i in 1:length(numFeat)){
    dimStartTime = Sys.time()
    write(paste0("\n--- Processing dimension ", numFeat[i], " (", i, "/", length(numFeat), ") ---"), 
          repLogFile, append=TRUE)
    
    #create 3 local datasets
    data = dataALL
    data$x = data$x[,1:numFeat[i]]
    resX=list()
    resY=list()
    resXdf=list()
    resXres=list()
    Xres=apply(data$x, 2, function(x)lm(x~data$cov)$res)
    X=cbind(data$cov, data$x)
    Y=data$y
    for (d in 1:3){
      xx=X[sepLeft[d]:sepRight[d], ]
      yy=Y[sepLeft[d]:sepRight[d], , drop=F]
      colnames(yy)="Y"
      save(xx, file=paste0(dataDir,"X", d, ".rda"))
      save(yy, file=paste0(dataDir,"Y", d, ".rda"))
      
      Xdf=as.data.frame(xx)
      save(Xdf, file=paste0(dataDir,"Xdf", d, ".rda"))
      
      xx=Xres[sepLeft[d]:sepRight[d], ]
      save(xx, file=paste0(dataDir,"Xres", d, ".rda"))
      
      
      resX[[d]] <- resourcer::newResource(paste0("X",d), paste0("file:", dataDir,"X", d, ".rda"), format = "matrix")
      resY[[d]] <- resourcer::newResource(paste0("Y",d), paste0("file:", dataDir,"Y", d, ".rda"), format = "matrix")
      resXdf[[d]] <- resourcer::newResource(paste0("Xdf",d), paste0("file:", dataDir,"Xdf", d, ".rda"), format = "data.frame")
      resXres[[d]] <- resourcer::newResource(paste0("Xres",d), paste0("file:", dataDir,"Xres", d, ".rda"), format = "matrix")
    }
    names(resX)=paste0("X",1:3)
    names(resY)=paste0("Y",1:3)
    names(resXdf)=paste0("Xdf",1:3)
    names(resXres)=paste0("Xres",1:3)
    
    res=c(resX, resY, resXdf, resXres)
    dslite.server <- newDSLiteServer(resources = res)
    builder <- DSI::newDSLoginBuilder()
    
    #Build three servers
    for (r in 1:3){
      builder$append(server = paste0("server", r), url = "dslite.server", driver = "DSLiteDriver")
    }
    
    #initialize connection
    logindata <- builder$build()
    conns <- datashield.login(logindata, assign=T)
    for (r in 1:length(resX)){
      datashield.assign.resource(conns[r],symbol="X",resource = paste0("X", r))
      datashield.assign.expr(conns = conns[r],  symbol = "X", expr = quote(as.resource.object(X)))
      datashield.assign.resource(conns[r],symbol="Y",resource = paste0("Y", r))
      datashield.assign.expr(conns = conns[r],  symbol = "Y", expr = quote(as.resource.object(Y)))
      
      datashield.assign.resource(conns[r],symbol="Xdf",resource = paste0("Xdf", r))
      datashield.assign.expr(conns = conns[r],  symbol = "Xdf", expr = quote(as.resource.object(Xdf)))
      datashield.assign.resource(conns[r],symbol="Xres",resource = paste0("Xres", r))
      datashield.assign.expr(conns = conns[r],  symbol = "Xres", expr = quote(as.resource.object(Xres)))
    }    
    
    # Method 1: dsLassoCov
    method1StartTime <- Sys.time()
    write(paste0("  Method 1 (dsLassoCov) started at ", format(method1StartTime, "%H:%M:%S")), 
          repLogFile, append=TRUE)
    
    start_time <- Sys.time()
    cvR = ds.LassoCov_CVInSite(X="X", Y="Y", type="classify",nfolds=5, lam_ratio=0.01, nlambda=50, opts=opts, covar=1,  datasources=conns, nDigits=8)
    cat("\014")
    fit=ds.LassoCov_Train(X="X", Y="Y", type="classify", nlambda=50,lambda=cvR$lambda.min, opts=opts, covar=1, datasources=conns, nDigits=8)
    cat("\014")
    end_time <- Sys.time()
    method1Time = as.numeric(difftime(end_time, start_time, units="secs"))
    timeSpend=c(timeSpend, method1Time)
    
    write(paste0("  Method 1 completed in ", round(method1Time, 2), " seconds"), 
          repLogFile, append=TRUE)
    numf = sum(sort(abs(fit$ws[2:length(fit$ws)]), decreasing = T)[1:20]!=0)
    if (numf==0){
      featAcc = c(featAcc, 0)
    } else { 
      featAcc = c(featAcc, length(intersect(order(abs(fit$ws[2:length(fit$ws)]), decreasing = T)[1:numf], 11:30))/20)
    }
    
    write(paste0("  Method 1 accuracy: ", round(featAcc[length(featAcc)], 4)), 
          repLogFile, append=TRUE)

    # Method 2: ds.glm + dsLasso
    method2StartTime <- Sys.time()
    write(paste0("  Method 2 (ds.glm + dsLasso) started at ", format(method2StartTime, "%H:%M:%S")), 
          repLogFile, append=TRUE)
    
    start_time <- Sys.time()
    nFeats=ds.dim("Xdf")[[1]][2]
    betas=vector()
    iters=vector()
    for (feat in 2:nFeats){
      form=paste0("V", feat, "~V1")
      glm.fit=ds.glm(formula = form,data = "Xdf",family = "gaussian", datasources = conns) 
      betas=c(betas, glm.fit$coefficients[2,1])
      iters=c(iters, glm.fit$iter)
      cat("\014")
    }
    cvR2 = ds.Lasso_CVInSite(X="Xres", Y="Y", type="classify",nfolds=5, lam_ratio=0.01, nlambda=50, opts=opts, datasources=conns, nDigits=8, C=0)
    cat("\014")
    fit2=ds.Lasso_Train(X="Xres", Y="Y", type="classify", nlambda=50,lambda=cvR2$lambda.min, opts=opts, datasources=conns, nDigits=8, C=0)
    cat("\014")
    end_time <- Sys.time()
    method2Time = as.numeric(difftime(end_time, start_time, units="secs"))
    timeSpend=c(timeSpend, method2Time)
    
    write(paste0("  Method 2 completed in ", round(method2Time, 2), " seconds"), 
          repLogFile, append=TRUE)
    
    numf = sum(sort(abs(fit2$ws), decreasing = T)[1:20]!=0)
    if (numf==0){
      featAcc = c(featAcc, 0)
    } else { 
      featAcc = c(featAcc, length(intersect(order(abs(fit2$ws), decreasing = T)[1:numf], 11:30))/20)
    }
    
    write(paste0("  Method 2 accuracy: ", round(featAcc[length(featAcc)], 4)), 
          repLogFile, append=TRUE)
    
    dimEndTime = Sys.time()
    dimTotalTime = as.numeric(difftime(dimEndTime, dimStartTime, units="secs"))
    write(paste0("Dimension ", numFeat[i], " total time: ", round(dimTotalTime, 2), " seconds\n"), 
          repLogFile, append=TRUE)
    
    # Store detailed timing for this dimension
    detailedLog[[i]] = list(
      numFeat = numFeat[i],
      method1_time = method1Time,
      method2_time = method2Time,
      total_time = dimTotalTime,
      method1_accuracy = featAcc[length(featAcc)-1],
      method2_accuracy = featAcc[length(featAcc)]
    )
    
    datashield.logout(conns)
  }
  
  # Write final summary for this repetition
  repEndTime = Sys.time()
  repTotalTime = as.numeric(difftime(repEndTime, repStartTime, units="mins"))
  
  write(paste0("\n=== Repetition ", rep, " Completed ==="), repLogFile, append=TRUE)
  write(paste0("End time: ", format(repEndTime, "%Y-%m-%d %H:%M:%S")), repLogFile, append=TRUE)
  write(paste0("Total time: ", round(repTotalTime, 2), " minutes"), repLogFile, append=TRUE)
  write(paste0("\nSummary by dimension:"), repLogFile, append=TRUE)
  
  for (j in 1:length(detailedLog)) {
    logEntry = detailedLog[[j]]
    write(paste0("  Dim ", logEntry$numFeat, ": Method1=", round(logEntry$method1_time, 1), 
                 "s, Method2=", round(logEntry$method2_time, 1), 
                 "s, Total=", round(logEntry$total_time, 1), "s"), 
          repLogFile, append=TRUE)
  }
  
  # Update main log
  write(paste0("Repetition ", rep, " completed in ", round(repTotalTime, 2), " minutes"), 
        mainLogFile, append=TRUE)
  
  cat("Repetition", rep, "completed in", round(repTotalTime, 2), "minutes\n\n")
  
  saveRDS(file=paste0("results/1_Rep", rep, "_result_efficiency_classify.rds"), 
          list(timeSpend=timeSpend, featAcc=featAcc, detailedLog=detailedLog,
               totalTime=repTotalTime, startTime=repStartTime, endTime=repEndTime))
}

# Write summary to main log
write("\n=== All Repetitions Completed ===", mainLogFile, append=TRUE)
write(paste0("Completion time: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), mainLogFile, append=TRUE)

cat("\nAll", nReps, "repetitions completed!\n")
cat("Main log saved to:", mainLogFile, "\n")
cat("Individual logs saved to: logs/1_efficiency_classify_rep*.log\n\n")



# Load required libraries for visualization
library(svglite)

# Aggregate results from all repetitions
timeSpendMat=vector()
featAccMat=vector()
for (i in 1:nReps){
  rData=readRDS(file=paste0("results/1_Rep", i, "_result_efficiency_classify.rds"))
  timeSpendMat=rbind(timeSpendMat, rData$timeSpend)
  featAccMat=rbind(featAccMat, rData$featAcc)
}

# Reshape: 6 methods x 9 dims
# Each column in featAccMat contains results for one (numFeat, method) combination across 30 reps
data_mean <- data.frame(matrix(colMeans(featAccMat), nrow=nMethods))
data_sd <- data.frame(matrix(apply(featAccMat, 2, sd), nrow=nMethods))



# Calculate mean and standard deviation
timeSpend_mean <- matrix(colMeans(timeSpendMat), nrow=2)
timeSpend_sd <- matrix(apply(timeSpendMat, 2, sd), nrow=2)
timeSpend_mean <- timeSpend_mean/60  # Convert to minutes
timeSpend_sd <- timeSpend_sd/60

featAcc_mean <- matrix(colMeans(featAccMat), nrow=2)
featAcc_sd <- matrix(apply(featAccMat, 2, sd), nrow=2)

# Prepare data frames
numFeat=c(100, 200, 300, 500, 700, 1000, 1500, 2000, 3000)
nMethods = 2
methodNames = c("dsLassoCov", "ds.glm + dsLasso")

# Add row and column names
rownames(timeSpend_mean) <- methodNames
colnames(timeSpend_mean) <- as.character(numFeat)
rownames(timeSpend_sd) <- methodNames
colnames(timeSpend_sd) <- as.character(numFeat)

rownames(featAcc_mean) <- methodNames
colnames(featAcc_mean) <- as.character(numFeat)
rownames(featAcc_sd) <- methodNames
colnames(featAcc_sd) <- as.character(numFeat)

# Plot 1: Running Time with error bars (SVG)
svglite("1_result_efficiency_classify_time.svg", width=12, height=7)
x_pos <- barplot(height=timeSpend_mean, 
                 main="Dependence of running time on data dimension (Classification)", 
                 ylab="running time (mins)", beside=TRUE, col=rainbow(2), 
                 xlab="the number of features", ylim=c(0, max(timeSpend_mean + timeSpend_sd) * 1.2))

# Add error bars for running time
for(i in 1:nrow(timeSpend_mean)){
  for(j in 1:ncol(timeSpend_mean)){
    x <- x_pos[i, j]
    y <- timeSpend_mean[i, j]
    se <- timeSpend_sd[i, j]
    segments(x, y-se, x, y+se, lwd=1.5)
    segments(x-0.1, y-se, x+0.1, y-se, lwd=1.5)
    segments(x-0.1, y+se, x+0.1, y+se, lwd=1.5)
  }
}

legend("topleft", methodNames, cex=1.0, bty="n", fill=rainbow(2))
dev.off()
cat("SVG file '1_result_efficiency_classify_time.svg' generated successfully!\n")

# Plot 2: Feature Selection Accuracy with error bars (SVG)
svglite("1_result_efficiency_classify_accuracy.svg", width=12, height=7)
x_pos <- barplot(height=featAcc_mean, 
                 main="Dependence of feature selection accuracy on data dimension (Classification)", 
                 ylab="feature selection accuracy", beside=TRUE, col=rainbow(2), 
                 xlab="the number of features", ylim=c(0, 1))

# Add error bars for accuracy
for(i in 1:nrow(featAcc_mean)){
  for(j in 1:ncol(featAcc_mean)){
    x <- x_pos[i, j]
    y <- featAcc_mean[i, j]
    se <- featAcc_sd[i, j]
    segments(x, y-se, x, y+se, lwd=1.5)
    segments(x-0.1, y-se, x+0.1, y-se, lwd=1.5)
    segments(x-0.1, y+se, x+0.1, y+se, lwd=1.5)
  }
}

legend("topright", methodNames, cex=1.0, bty="n", fill=rainbow(2))
dev.off()
cat("SVG file '1_result_efficiency_classify_accuracy.svg' generated successfully!\n")

# Print summary statistics
cat("\n=== Running Time Summary (Classification) ===\n")
cat("Mean running time (mins) by method and feature dimension:\n")
print(timeSpend_mean)
cat("\nStandard deviation of running time (mins):\n")
print(timeSpend_sd)

cat("\n=== Feature Selection Accuracy Summary (Classification) ===\n")
cat("Mean accuracy by method and feature dimension:\n")
print(featAcc_mean)
cat("\nStandard deviation of accuracy:\n")
print(featAcc_sd)






