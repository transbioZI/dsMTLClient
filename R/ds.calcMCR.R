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
#' @title Calculate missing classification rate (MCR)
#' @description Calculate missing classification rate (MCR) on the target server
#' @param ws The list of coefficient vectors for all servers   
#' @param X The name of design matrices   
#' @param Y The name of labels   
#' @param datasourceTest  The connection of the target server
#' @param average The indicator to average or not the prediction probability over all coefficient vectors
#' 
#' @return The MCR of each coefficient vectors (or averaged MCR)
#' @details Calculate missing classification rate (MCR) on the target server
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################


ds.calcMCR = function(ws=ws, datasourceTest, X, Y, average=T){
  ws.vec=c(nrow(ws), as.vector(ws))
  ws.text=paste0(as.character(ws.vec), collapse=",")
  cally <- call('calcMCRDS', ws.text, X, Y, average)
  mcr=DSI::datashield.aggregate(datasourceTest, cally)  
  return(mcr)
}
