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
#' @title Get data from server
#' @description Retrieve the data matrix from the server
#' @param myServerKey The identify of the access. myServerKey$server contained a valid server connection. 
#' myServerKey$key contained a valid key.
#' @param data  The name of the target matrix on the server
#' 
#' @return The requested matrix or error message 
#' @details The "Datakey" mechanism allowed the valid client retrieve the data matrix directly into the memory of the server. The validity 
#' was granted by the server administrator. To achieve this, the administrator generated a key and put the local copy in the dataset 
#' serverDataKey.myKey(project: serverDataKey; table: myKey) where the dataset was created normally using DataSHIELD. The remote copy
#' of the key was hand over to the trustworthy analyst. This was determined by the server administrator 
#' 
#' @import DSI
#' @export  
#' @author Han Cao
################################################################################


ds.getMyServerData= function(myServerKey, data){
  DSI::datashield.assign.table(conns = myServerKey$server, symbol = "localKey", table = "serverDataKey.myKey" )
  cally = call("getMyServerDataDS", myServerKey$key, data)
  rData = DSI::datashield.aggregate(myServerKey$server, cally)  
  
  return(rData)
}
