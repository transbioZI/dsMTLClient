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
#' @title Subset subjects for cross-validation
#' @description Subset subjects for cross-validation
#' @param datasources The connections of servers   
#' @param idx the index of subjects
#' @param newSymbol the name of new variable
#' @param symbol the name of raw data
#' 
#' @details In each server, the subjects of index were selected and copied into a new symbol
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################
ds.subsetSubjests= function(datasources, idx, newSymbol, symbol){
  nTasks=length(datasources)
  for (i in 1:nTasks){
    idx.text=paste0(as.character(idx[[i]]), collapse=",")
    cally = call("subsetSubjestsDS", symbol, idx.text)
    DSI::datashield.assign.expr(conns = datasources[i], symbol = newSymbol, expr =  cally)
  }
}