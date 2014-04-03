
#today-start working on just two colors
#note: heart figure, 5 colors, crashes 


setClass(Class="ombre.pattern", 
			representation = representation(numColors="numeric", overlap="numeric"),
			contains="pattern"
)


setMethod(f="initialize", signature="ombre.pattern", definition=function(.Object, numColors, overlap, my.pf.matrix){
		.Object = callNextMethod(.Object, my.pf.matrix)
		if(numColors == 1) stop("You need more than one color for an ombre pattern.")
		if(numColors > 5) stop("No more than 5 colors presently supported")
		if(overlap > 0.5) stop ("Overlap parameter must be between 0 and 1/2.")
		if(overlap < 0) stop ("Overlap parameter must be between 0 and 1/2.")
		.Object@numValTypes=numColors
		color.names = c("B", "C", "D", "E", "F")
		ombre.rows = .Object@numRows
		num.blocks = (numColors * 2) - 1
		solid.block = ombre.rows/numColors
		block.codes = character()
		numrow.codes = numeric()
		color.codes = matrix(nrow=num.blocks, ncol=2)
		for(i in 1:num.blocks){
			if((i%%2) == 1){
				new.code = "solid"
			} else {
				new.code = "trans"
			}
			block.codes = c(block.codes, new.code)	
		}
		for(i in 1:num.blocks){
			if((i%%2) == 1){
				newrow.code = solid.block * (1 - overlap*2)
			} else {
				if(i == 2){
					if(i == (num.blocks - 1)){
						newrow.code = 4 * overlap * solid.block
					} else {
						newrow.code = 3 * overlap * solid.block
					}	
				} else if(i == (num.blocks - 1)){
					if(i == 2){
						newrow.code = 4 * overlap * solid.block
					} else {
						newrow.code = 3 * overlap * solid.block
					}
				} else {
					newrow.code = 2 * overlap * solid.block
				}
			}
			numrow.codes = c(numrow.codes, newrow.code)
		}
		cat(numrow.codes, "\n")
		cat(sum(numrow.codes), "\n")
		if(sum(numrow.codes) != ombre.rows) stop("Something is wrong")
		numrow.codes = make.discrete(numrow.codes, ombre.rows)
		if(sum(numrow.codes) != ombre.rows) stop("Something is wrong")
		cat(numrow.codes, "\n")
		cat(sum(numrow.codes), "\n")
		for(i in 1:num.blocks){
			if((i%%2) == 1){
				color.codes[i, 1] = color.names[(i + 1)/2]
			} else {
				color.codes[i, 1] = color.names[i/2]
				color.codes[i, 2] = color.names[(i + 2)/2]
			}
		}	
		start.row = 1
		for(i in 1:num.blocks){
			end.row = start.row + numrow.codes[i] - 1 	
			if(block.codes[i] == "solid"){
				.Object <- make.solid(.Object, start.row, end.row, color.codes[i, 1])
			} else {
				.Object <- make.ombre(.Object, start.row, end.row, color.codes[i, 1], color.codes[i, 2])
			}
			start.row = end.row + 1
		}
		return(.Object)
	}
)

#friendly constructor

ombre.pattern <- function(numColors, overlap, my.pf.matrix){
	return(new(Class="ombre.pattern", numColors, overlap, my.pf.matrix))
}

make.discrete <- function(input, input.sum){
	if(sum(input) != input.sum) stop("Vector is in wrong format")
	for(i in 1:length(input)){
		orig = input[i]
		if(i%%2 == 1){
			input[i] = floor(orig)
		} else {
			input[i] = ceiling(orig)
		}
	}
	new.sum = sum(input)
	remainder = input.sum - new.sum
	old.final = input[length(input)]
	input[length(input)] = old.final + remainder
	return(input)
}

make.solid <- function(.Object, my.start.row, my.end.row, my.color){
	my.rows = my.end.row - my.start.row + 1
	for(i in 1:my.rows){
		row.length = length(.Object@Vals[[my.start.row + i - 1]])
		solid.row = c(rep(my.color, row.length))
		.Object = setRow(.Object, solid.row, my.start.row + i - 1)
	}
	return(.Object)	
}

make.ombre <- function(.Object, my.start.row, my.end.row, start.color, end.color){
	my.rows = my.end.row - my.start.row + 1
	proportion.denom = 1/(my.rows + 1)
	for(i in 1:my.rows){
		row.length = length(.Object@Vals[[my.start.row + i - 1]])
		solid.row = c(rep(start.color, row.length))
		num.end.colors = proportion.denom * i * row.length
		num.end.colors = round(num.end.colors)
		end.colors = sample(c(1:row.length), num.end.colors, replace=FALSE)
		solid.row[end.colors] = end.color
		.Object = setRow(.Object, solid.row, my.start.row + i - 1) 
	}
	return(.Object)
}


