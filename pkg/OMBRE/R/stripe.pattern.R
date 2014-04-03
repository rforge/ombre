#source("pattern.R")
#stripe pattern
setClass(Class="stripe.pattern",
		representation = representation(numStripes="numeric"),
		contains="pattern"
)

#this needs error checking
setMethod(f="initialize",signature="stripe.pattern",
       definition=function(.Object, numStripes, my.pf.matrix){
      	.Object = callNextMethod(.Object, my.pf.matrix)
      	if(numStripes == 1) stop("You need more than one color for stripes!")
      	if(numStripes > 10) stop("You can only have up to 10 stripes.")
      	if(numStripes > .Object@numRows) stop("Your figure is too small for number of stripes.")
      	.Object@numValTypes = numStripes
      	stripe.names = c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K")
      	stripe.width = floor((.Object@numRows)/numStripes)
      	for(i in 1:numStripes){
      		for(j in 1:stripe.width){
      			stripe.row.num = (i-1)*stripe.width + j
      			stripe.length = length(.Object@Vals[[stripe.row.num]])
      			cat("Stripe length is", stripe.length, "\n")
      			stripe.row = c(rep(stripe.names[i], stripe.length))
      			.Object = setRow(.Object, stripe.row, stripe.row.num)
      		}
        }
        remainder = (.Object@numRows)%%numStripes
        if(remainder != 0){
        	for(k in 1:remainder){
        		stripe.row.num = (numStripes*stripe.width)+k
        		stripe.length = length(.Object@Vals[[stripe.row.num]])
        		cat("Stripe length is", stripe.length, "\n")
        		stripe.row = c(rep(stripe.names[numStripes], stripe.length))
        		.Object = setRow(.Object, stripe.row, stripe.row.num)
        	}
        }
     	return(.Object)
	}
)

#friendly constructor for stripe pattern class

stripe.pattern <- function(numStripes, my.pf.matrix){
	return(new(Class="stripe.pattern", numStripes, my.pf.matrix))
}
