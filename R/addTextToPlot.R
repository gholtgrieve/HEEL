#'@title Add Text to a Plot Using Relaive Positoning
#'@description
#'Easy little way to add a text to a plot.  The plot area is rescaled to range from 0 to 1 in both directions.
#'Text placement is on this scale with larger values of y and x moving the placement up and to the right respectively.\cr
#'
#'@usage
#'addTextToPlot(x, y, text, ...)
#'
#'@param x Numeric value of horizontal positon from 0 (left) to 1 (right)
#'@param y Numeric value of vertical positon from 0 (bottom) to 1 (top)
#'@param text Character string to be written on the plot.
#'@param ... Additional arguments passed to text().
#'@return None
#'@author Gordon W. Holtgrieve
#'@examples
#'addTextToPlot(0.5, 0.5, text="X", )  #X marks the spot, in this case the center)
#'addTextToPlot(0.95, 0.05, text="y = mx + b", adj=1) #right justified
#'@export

addTextToPlot <- function(x, y, text, ...){
  text2 <- as.character(text)

  # Error handling
  if(is.null(x) | is.null(y) | is.null(text)) stop("Error: Missing arguments.")
  if(!is.character(text2)) stop("Error: Argument 'text' must be of type character. Unable to coerce.")
  if(x > 1 | x < 0) stop("Error: Argument 'x' should be from 0 and 1.")
  if(y > 1 | y < 0) stop("Error: Argument 'y' should be from 0 and 1.")

  op <- par(usr=c(0,1,0,1))
  text(x=x, y=y, text, ...)
  par(op)
}
