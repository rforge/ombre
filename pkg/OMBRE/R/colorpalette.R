#load("ColourLovers.RData")

# cl.palettes = data.frame()
# cl.api = getURL("http://www.colourlovers.com/api/palettes?numResults=100&orderCol=numVotes&keywords=%22ombre%22&sortBy=DESC&format=json")
# cl.json = fromJSON(cl.api)

setClass("color.palette",
	representation(colors = "character",
					numcolors = "numeric"
))

#initialize
setMethod(f="initialize",signature="color.palette",
       definition=function(.Object, my.colors){
      .Object@colors <- my.colors
      .Object@numcolors <- length(my.colors)
		return(.Object)
	}
)

#friendly default constructor
color.palette <- function(my.colors){
	return(new(Class = "color.palette", my.colors))
}

#accesses the ColourLovers API (gulp)
color.palette.cl <- function(){
	data("ColourLovers")
	data("cl.json")

    #get the data
	num.palettes = length(cl.json)
	#choose palette randomly
	r.palette.index = sample(c(1:num.palettes), 1)
	r.palette = cl.json[[r.palette.index]]$colors
	for(i in 1:length(r.palette)){
		r.palette[i] = paste("#", r.palette[i], sep="")
	}
	r.palette = c("white", r.palette)
	cat(cl.json[[r.palette.index]]$title, "\n")
	cat(cl.json[[r.palette.index]]$userName, "\n")
	return(new(Class="color.palette", r.palette))
}

setGeneric (
	name = "getColors",
	def=function(.Object){standardGeneric("getColors")}	
)

setMethod("getColors", "color.palette",
       definition=function(.Object){
       		return(.Object@colors)
       }
)

setGeneric (
	name = "truncateColors",
	def=function(.Object, numLeft){standardGeneric("truncateColors")}	
)

setMethod("truncateColors", "color.palette",
       definition=function(.Object, numLeft){
       		if(.Object@numcolors < numLeft){
       			stop("Your existing color palette is too small")
       		}
       		.Object@numcolors = numLeft + 1
       		.Object@colors = .Object@colors[1:(numLeft+1)]
       		return(.Object)
       }
)


setGeneric (
	name = "flipColors",
	def=function(.Object){standardGeneric("flipColors")}	
)

setMethod("flipColors", "color.palette",
definition=function(.Object){
	foregroundColors = .Object@colors[2:length(.Object@colors)]
	bgColor=.Object@colors[1]
    .Object@colors = c(bgColor, rev(foregroundColors))
	return(.Object)
}
)


setGeneric (
	name = "getNumColors",
	def=function(.Object){standardGeneric("getNumColors")}	
)

setMethod("getNumColors", "color.palette",
       definition=function(.Object){
       		return(.Object@numcolors)
       }
)


