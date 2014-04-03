

setClass("pattern",
	representation(Vals = "list",
					numValTypes = "numeric",
					numRows = "numeric",
					nextRow = "numeric",
					nextVal = "numeric"
))

#initialize
#helper function
lappend <- function(lst, obj) {
     lst[[length(lst)+1]] <- obj
     return(lst)
}


setMethod(f="initialize", signature="pattern", definition=function(.Object, my.pf.matrix=NULL){
	.Object@Vals = list()
    .Object@numRows = 0
    .Object@nextRow = 1
    .Object@nextVal = 1
    if(length(my.pf.matrix) != 0){
 		pf.rows = dim(my.pf.matrix)[1] 
    	pf.cols = dim(my.pf.matrix)[2]
    	for(i in 1:pf.rows){
      		if(sum(my.pf.matrix[i,])!=0){
      			row.vals = numeric()
      			.Object@numRows = .Object@numRows + 1
      			for(j in 1:pf.cols){
      				if (my.pf.matrix[i, j] == 1){ 
      					row.vals = c(row.vals, "B")	
      				}
      			}
      			.Object@Vals <- lappend(.Object@Vals, row.vals)
        	}	
     	}
      if(.Object@numRows == 0) stop("there is no foreground")
     }	
      .Object@numValTypes = 1 #default is single solid color
    
      return(.Object)
}
)

#friendly constructor for pattern class

pattern <- function(my.pf.matrix){
	return(new(Class="pattern", my.pf.matrix))
}


#Function incrementVal
setGeneric (
	name = "incrementVal",
	def=function(.Object){standardGeneric("incrementVal")}	
)

setMethod("incrementVal", "pattern",
       definition=function(.Object){
       		if(.Object@nextVal == length(.Object@Vals[[.Object@nextRow]])){
       			.Object@nextRow = .Object@nextRow + 1
       			.Object@nextVal = 1
       		} else{
       			.Object@nextVal = .Object@nextVal + 1		
       		}
       		return(.Object)
       }
)

setGeneric (
	name = "getNumValTypes",
	def=function(.Object){standardGeneric("getNumValTypes")}	
)

setMethod("getNumValTypes", "pattern",
       definition=function(.Object){
       		return(.Object@numValTypes)
       }
)


setGeneric (
	name = "getNextVal",
	def=function(.Object){standardGeneric("getNextVal")}	
)

setMethod("getNextVal", "pattern",
       definition=function(.Object){
       		return(.Object@Vals[[.Object@nextRow]][.Object@nextVal])
       }
)



#Function getNumRows
setGeneric (
	name = "getNumRows",
	def=function(.Object){standardGeneric("getNumRows")}	
)

setMethod("getNumRows", "pattern",
       definition=function(.Object){
       		return(.Object@numRows)
       }
)

#changing so myVal is now myVals, an array
setGeneric (
	name = "setRow",
	def=function(.Object, myVals, myRowNum){standardGeneric("setRow")}	
)

setMethod("setRow", "pattern",
       definition=function(.Object, myVals, myRowNum){
       		myNumVals = length(.Object@Vals[[myRowNum]])
       		if(myNumVals != length(myVals)) stop("Array for setRow has the wrong number of elements")
       		.Object@Vals[[myRowNum]] = myVals
       		return(.Object)
       }
)

setGeneric (
	name = "setVal",
	def=function(.Object, myVal, myRowNum, myValNum){standardGeneric("setVal")}	
)

setMethod("setVal", "pattern",
       definition=function(.Object, myVal, myRowNum, myValNum){
       		.Object@Vals[[myRowNum]][myValNum] = myVal
       		return(.Object)
       }
)




setGeneric (
	name = "getNumRows",
	def=function(.Object){standardGeneric("getNumRows")}	
)

setMethod("getNumRows", "pattern",
       definition=function(.Object){
       		return(.Object@numRows)
       }
)


setGeneric(
	name = "getNumVals",
	def=function(.Object, myRowNum){standardGeneric("getNumVals")}
)

setMethod("getNumVals", "pattern",
		definition=function(.Object, myRowNum){
			return(length(.Object@Vals[[myRowNum]]))			
		}
)


