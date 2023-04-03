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
#' @title Fit a linear regression model on selected covariates
#' @description Fit a linear regression model with only selected adjusting covariates
#' 
#' @param X Predictors
#' @param Y Outcome
#' @param covar Positions of adjusting covariates in the X dataset
#' @param datasources The connections of servers
#' 
#' @return Estimated beta coefficients for covariates
#' @importFrom dsBaseClient ds.make ds.dataFrameSubset ds.asFactor ds.cbind ds.names ds.glm
#' @details Beta coefficients are employed for the estimation of lambda max
#' @author  Han Cao & Augusto Anguita-Ruiz
################################################################################

ds.lmBetas= function(X,Y,covar, datasources){
  #Create a vector of ones in the server side
  dsBaseClient::ds.make(toAssign = paste0('1', '+', Y, '-', Y),newobj = 'ONES',datasources = datasources)

  #Subset only columns corresponding to covariates from the X dataset
  dsBaseClient::ds.dataFrameSubset(df.name = X,  V1.name = 'ONES',  V2.name = 'ONES',  Boolean.operator = '==', keep.cols = covar, newobj = 'X_lm',  
                     datasources = datasources) 
  
  #Coerce outcome to numeric in the server side 
  dsBaseClient::ds.asNumeric(x.name = Y, newobj = 'Y_lm', datasources = datasources)
  
  #Bind both objects into a new object in the server side
  dsBaseClient::ds.cbind(x = c('Y_lm', 'X_lm'), newobj = 'data_lm',datasources = datasources)
  
  #Define linear model formula
  formula  = paste0('Y_lm~', paste(dsBaseClient::ds.names('data_lm', datasources = datasources)[[1]][-1],collapse='+'),"-1") 
  
  #Run linear model for covariates only
  mod = dsBaseClient::ds.glm(formula = formula,data = 'data_lm',family = 'gaussian', datasources = datasources) 

  betaCov = mod$coefficients[,1] #Extract estimated coefficients
  
  return(betaCov)
  
}
  
