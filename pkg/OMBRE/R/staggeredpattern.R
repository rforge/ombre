setClass(Class="staggered.pattern",
		representation = representation(numElements="numeric"),
		contains="pattern"
)

setMethod(f="initialize", signature="staggered.pattern",
		definition=function(.Object, numElements, my.pf.matrix){
			.Object = callNextMethod(.Object, my.pf.matrix)
			if(numElements == 1) stop("You need more than one color for a staggered pattern.")
			if(numElements > 10) stop("You can only have up to 10 elements.")
			.Object@numValTypes = numElements
			element.names = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
			short.en = element.names[1:numElements]
			for(i in 1:.Object@numRows){
				myNumVals = length(.Object@Vals[[i]])
				short.length=length(short.en)
				if(myNumVals < short.length){
					row.short.en = short.en[1:myNumVals]
				} else if (myNumVals == short.length){
					row.short.en = short.en
				} else {
					repeats = floor(myNumVals/short.length)
					remainder = myNumVals%%short.length
					if(remainder==0){
						remainder.en = numeric()
					} else {
						remainder.en = short.en[1:remainder]
					}
					row.short.en = c(rep(short.en, repeats))
					row.short.en = c(row.short.en, remainder.en)
				}
				.Object = setRow(.Object, row.short.en, i)
				# for(j in 1:myNumVals){
					# .Object = setVal(.Object, row.short.en[j], i, j) 
				# }
				short.en <- Shift(short.en)
			}
			return(.Object)
		}
)

#friendly constructor
staggered.pattern <- function(numElements, my.pf.matrix){
	return(new(Class="staggered.pattern", numElements, my.pf.matrix))
}

Shift <- function(arraytotransform){
	first = arraytotransform[1]
	for(i in 1:length(arraytotransform)){
		if(i != length(arraytotransform)){
			arraytotransform[i] = arraytotransform[i+1]
		} else {
			arraytotransform[i] = first
		}
	}
	return(arraytotransform)
}




