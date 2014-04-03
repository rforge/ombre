#note: at the moment if you draw directly you will end up with something upside down if you inspect 
#the matrix on the R consule

#that's why the function that imports excel files will flip the matrix first.  Flipping is easy-thank
#goodness for R!


#note, first color in palette is background
#figure rows 
setClass("pixel.fig",
         representation(height="numeric",
                        width="numeric",
                    	my.color.palette="color.palette",
                    	my.pattern = "pattern",
                        figure="matrix"
                        ))
                        

setMethod(f="initialize",signature="pixel.fig",
       definition=function(.Object, this.color.palette){
       .Object@my.color.palette <- this.color.palette
		return(.Object)
	}
)

#friendly constructor
pixel.fig <- function(this.color.palette){
	return(new(Class="pixel.fig", this.color.palette))
}

#csv constructor
pixel.fig.csv <- function(file, this.color.palette, pattern.type="plain", overlap = 0.25){
	new.figure <- read.table(file, sep=",", header=FALSE)
    new.figure <- as.matrix(new.figure)
    fig.rows = dim(new.figure)[1]
    fig.cols = dim(new.figure)[2]
    new.figure <- apply(new.figure, c(1, 2), replaceStar)    
    if(sum(sum(new.figure)) == 0) stop("Input file is improperly formatted.")
    colnames(new.figure) <- NULL
    new.figure <- apply(new.figure, 2, rev)
    figcsv <- pixel.fig(this.color.palette)
    ncolors <- getNumColors(this.color.palette) - 1
    if(pattern.type=="stripe"){
    	figcsv@my.pattern <- stripe.pattern(ncolors, new.figure)
    } else if (pattern.type=="staggered"){
    	figcsv@my.pattern <- staggered.pattern(ncolors, new.figure)
    } else if (pattern.type == "ombre"){
    	figcsv@my.pattern <- ombre.pattern(ncolors, overlap, new.figure)
    } else {
    	figcsv@my.pattern <- pattern(new.figure)
    }
    figcsv@my.color.palette <- this.color.palette
    figcsv@figure <-new.figure
    figcsv@width <- dim(new.figure)[2]
    figcsv@height <-  dim(new.figure)[1]
	return(figcsv)
}

setGeneric (
	name = "setFigureFile",
	def=function(.Object, file){standardGeneric("setFigureFile")}	
)

replaceStar <- function(input){
	if(input == "*"){
		return(1)
	} else {
		return(0)
	}
}

setMethod("setFigureFile", "pixel.fig",
       definition=function(.Object, file){
       		new.figure <- read.table(file, sep=",", header=FALSE)
       		new.figure <- as.matrix(new.figure)
       		fig.rows = dim(new.figure)[1]
       		fig.cols = dim(new.figure)[2]
       		new.figure <- outer(1:figrows, 1:figcols, FUN=replaceStar)
       		new.figure <- apply(new.figure, )
       		colnames(new.figure) <- NULL
       		new.figure <- apply(new.figure, 2, rev)
       		.Object@figure <- new.figure
       		.Object@width <- dim(new.figure)[2]
       		.Object@height <- dim(new.figure)[1]
       		return(.Object)
       }
)


setGeneric (
	name = "getFigure",
	def=function(.Object, file){standardGeneric("getFigure")}	
)

setMethod("getFigure", "pixel.fig",
       definition=function(.Object){
       		return(.Object@figure)
       }
)

setGeneric (
	name = "setColorPalette",
	def=function(.Object, palette){standardGeneric("setColorPalette")}	
)

setMethod("setColorPalette", "pixel.fig",
		definition=function(.Object, palette){
			.Object@my.color.palette <- palette
			return(.Object)			
		}
)



setGeneric (
	name = "getHeight",
	def=function(.Object){standardGeneric("getHeight")}	
)

setMethod("getHeight", "pixel.fig",
       definition=function(.Object){
       		return(.Object@height)
       }
)

setGeneric (
	name = "setHeight",
	def=function(.Object, height){standardGeneric("setHeight")}	
	
)

setMethod("setHeight", "pixel.fig",
       definition=function(.Object, height){
       		.Object@height = height
       		if(.Object@width !=0){
		  .Object@figure = matrix(0, nrow=height, ncol = width)
			}
       }
)


 
setGeneric (
	name = "getWidth",
	def=function(.Object){standardGeneric("getWidth")}	
)

setMethod("getWidth", "pixel.fig",
       definition=function(.Object){
       		return(.Object@width)
       }
)

setGeneric (
	name = "setWidth",
	def=function(.Object, width){standardGeneric("setWidth")}	
)

setMethod("setWidth", "pixel.fig",
       definition=function(.Object, width){
       		.Object@width = width
       		if(.Object@height != 0){
       			.Object@figure = matrix(0, nrow=height, ncol = width)
       		}
       }
)
 


setGeneric (
	name = "setPoint",
	def=function(.Object, x, y){standardGeneric("setPoint")}	
)

setMethod("setPoint", "pixel.fig",
       definition=function(.Object, x, y){
       		if(x > .Object@height) stop("x value is out of bounds")
       		if(y > .Object@width) stop("y value is out of bounds")
       		.Object@figure[y, x] <- 1
       		return(.Object)
       }
)
 


setGeneric (
	name = "clearPoint",
	def=function(.Object, x, y){standardGeneric("clearPoint")}	
)

setMethod("clearPoint", "pixel.fig",
       definition=function(.Object, x, y){
       		if(x > .Object@height) stop("x value is out of bounds")
       		if(y > .Object@width) stop("y value is out of bounds")
       		.Object@figure[y, x] <- 0
       		return(.Object)
       }
)



#better version would use setPoint
setGeneric (
	name = "setStraightLine",
	def=function(.Object, p1, p2){standardGeneric("setStraightLine")}	
)

setMethod("setStraightLine", "pixel.fig",
       definition=function(.Object, p1, p2){
       		if(length(p1) != 2) stop("p1 is not a point")
       		if(p1[1] > .Object@height) stop ("x value of p1 is out of bounds")
       		if(p1[2] > .Object@width) stop ("y value of p1 is out of bounds")
       	    if(length(p2) != 2) stop("p2 is not a point")
       		if(p2[1] > .Object@height) stop ("x value of p2 is out of bounds")
       		if(p1[2] > .Object@width) stop ("y value of p2 is out of bounds")
       	    #horizontal line
       	    if(p1[2] == p2[2]){
       	    	shared.y <- p1[2]
       	    	start.x = min(p1[1], p2[1])
       	    	end.x = max(p1[1], p2[1])
       	    	num.squares = end.x - start.x + 1
       	    	for(i in 1:num.squares){
       	    		.Object@figure[shared.y, start.x + (i - 1)] <- 1
       	    	}
       	    	return(.Object)
       	    } else if (p1[1] == p2[1]){     #vertical line
       	    	shared.x <- p1[1]
       	    	start.y = min(p1[2], p2[2])
       	    	end.y = max(p1[2], p2[2])
       	    	num.squares = end.y - start.y + 1
       	    	for(i in 1:num.squares){
       	    		.Object@figure[start.y + (i -1), shared.x] <- 1
       	    	}
       	    	return(.Object)
       	    } else {
       	    	stop("Points do not define a straight line")
       	    }
      }
)

setGeneric (
	name = "selectAllPoints",
	def=function(.Object){standardGeneric("selectAllPoints")}	
)

setMethod("selectAllPoints", "pixel.fig",
       definition=function(.Object){
       		 .Object@figure = matrix(1, nrow=.Object@height, ncol = .Object@width)
       		return(.Object)
       }
)



setGeneric (
	name = "clearAllPoints",
	def=function(.Object){standardGeneric("clearAllPoints")}	
)

setMethod("clearAllPoints", "pixel.fig",
       definition=function(.Object){
       		 .Object@figure = matrix(0, nrow=.Object@height, ncol = .Object@width)
       		return(.Object)
       }
)





setMethod("print", "pixel.fig",
       definition=function(x, ...){
       		toprint <- x@figure	
       		ncolors <- getNumColors(x@my.color.palette)
       		if((ncolors-1) != getNumValTypes(x@my.pattern)){
       			stop("Wrong number of colors for the pattern you have chosen.")
       		} 	
       		toprint <- Process(toprint, x@my.pattern)
       		toprint.m <- melt(toprint,varnames = c("X1", "X2"))
       		#theme_set(theme_nothing())
       		##geom_tile(aes(fill = value), colour="black")
       		p1 <- ggplot(toprint.m, aes(X2, X1)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=getColors(x@my.color.palette))+
labs(x = NULL,y = NULL)+scale_x_continuous(expand=c(0,0))+scale_y_continuous(expand=c(0,0))+
             theme(legend.position = "none",axis.ticks = element_blank(),
                          axis.text.x=element_blank(),
                          axis.text.y=element_blank(),
                          panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            panel.border = element_blank(),
       		                 plot.margin=unit(c(0, 0, 0, 0), "lines"),
       		               axis.ticks.margin=unit(c(0),"lines"),
                          line = element_blank(),
                                text = element_blank(),
                                title = element_blank(),
                              axis.ticks.length = unit(0.001, "mm"))


return(p1)
})

#probably should make pattern objects

Process <- function(my.figure, my.pattern){
	for(i in 1:dim(my.figure)[1]){
		for(j in 1:dim(my.figure)[2]){
			if(my.figure[i, j] == 1){
				my.figure[i, j] = getNextVal(my.pattern)
				my.pattern = incrementVal(my.pattern)
			} else {
				my.figure[i, j] = "A"
			}
		}
	}
	return(my.figure)
}
	
	

#MAIN

