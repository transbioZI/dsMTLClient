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


#Modification by Augusto Anguita-Ruiz.
#Version: 21.10.2022
#Detail: Adding functionality of adjusting for confounders in lasso regression.

################################################################################
#' @title Fit a logistic regression model on selected covariates
#' @description Fit a linear logistic regression model with only selected adjusting covariates
#' 
#' @param X Predictors
#' @param Y Binary-Outcome
#' @param covar Positions of adjusting covariates in the X dataset
#' @return Estimated beta coefficients for covariates
#' @details Beta coefficients are employed for the estimation of lambda max
#' @param datasources The connections of servers
#' 
#' @return Estimated beta coefficients for covariates
#' @details Beta coefficients are employed for the estimation of lambda max
#' @importFrom dsBaseClient ds.make ds.dataFrameSubset ds.cbind ds.names ds.glm
#' @author  Han Cao & Augusto Anguita-Ruiz
################################################################################

ds.LRBetas= function(X,Y,covar, datasources){

  #Create a vector of ones in the server side
  dsBaseClient::ds.make(toAssign = paste0('1', '+', Y, '-', Y),newobj = 'ONES',datasources = datasources)
  
  #Subset only columns corresponding to covariates from the X dataset
  dsBaseClient::ds.dataFrameSubset(df.name = X,  V1.name = 'ONES',  V2.name = 'ONES',  Boolean.operator = '==', keep.cols = covar, newobj = 'X_lr',  
                     datasources = datasources) 
    
  #Coerce outcome to numeric in the server side 
  dsBaseClient::ds.asFactor(input.var.name = Y, newobj = 'Y_lr', datasources = datasources)
  
  #Bind both objects into a new object in the server side
  dsBaseClient::ds.cbind(x = c('Y_lr', 'X_lr'), newobj = 'data_LR',datasources = datasources) 
  
  #Define linear model formula
  formula  = paste0('Y_lr~', paste(dsBaseClient::ds.names('data_LR', datasources = datasources)[[1]][-1],collapse='+'), "-1") 
  
  #Run linear model for covariates only
  mod = dsBaseClient::ds.glm(formula = formula,data = 'data_LR',family = 'binomial', datasources = datasources) 
  
  #Extract estimated coefficients
  betaCov = mod$coefficients[,1] 
  
  return(betaCov)
  
  }
  
