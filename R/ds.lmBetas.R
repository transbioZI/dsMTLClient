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
#Version: 05.07.2022
#Detail: Adding functionality of adjusting for confounders in lasso regression.


################################################################################
#' @title Fit a linear model on selected covariates
#' @description Fit a linear regression model with only selected adjusting covariates
#' @param X Predictors
#' @param Y Outcome
#' @param covar Positions corresponding to adjusting covariates in the X dataset

#' @return Estimated beta coefficients for covariates
#' @details Beta coefficients are employed for the estimation of lambda max

#' @export  
#' @author  Han Cao & Augusto Anguita-Ruiz
################################################################################

ds.lmBetas= function(X,Y,covar){

  ds.make(toAssign = "X-X+1",newobj = "ONES",datasources = conns) #Create a vector of ones in the server side
  ds.dataFrameSubset(df.name = 'X',  V1.name = "ONES",  V2.name = "ONES",  Boolean.operator = "==",keep.cols = covar, newobj = 'X_lm',  datasources = conns) #Subset only columns corresponding to covariates from the X dataset
  ds.asNumeric(x.name = 'Y', newobj = 'Y_lm', datasources = NULL) #Coerce outcome to numeric in the server side
  ds.cbind(x = c("Y_lm", "X_lm"), newobj = "data_lm",datasources = conns) #Bind both objects into a new object in the server side
  formula  = paste(paste(c(ds.names("data_lm")[[1]][1],"~"),collapse=""),paste(ds.names("data_lm")[[1]][-1],collapse="+"),collapse="") #Define linear model formula
  mod = ds.glm(formula = formula,data = "data_lm",family = "gaussian", datasources = conns) #Run linear model for covariates only
  betaCov = mod$coefficients[-1,1] #Extract estimated coefficients
  
  return(betaCov)
  
  }
  
