#' Converts a given value from the scale of 0 and 1 to between -Inf and Inf
#'
#' @param p numeric value to be converted
#' @return numeric value of the converted input
#' @seealso \code{\link{logistic}} returns the inverse value (ie. the logistic function)
#' @export
#' @examples
#' logit(0.5)
logit <- function(p){
  return(log(p/(1-p)))
}

#' Converts a given value from the scale of -Inf and +Inf to 0 and 1
#'
#' @param p numeric value to be converted
#' @return numeric value of the converted input
#' @seealso \code{\link{logistic}} returns the inverse value (ie. the logit function)
#' @export
#' @examples
#' logistic(56)
#' logistic(logit(0.5))
logistic <- function(p){
  return(1/(1+exp(-p)))
}

#' Transforms a given value from between -Inf and Inf to xmin and xmax
#'
#' @param x numeric value to be scaled
#' @param xmin numeric value representing the lower bound of the new scale
#' @param xmax numeric value representing the upper bound of the new scale
#' @return numeric value with the converted input
#' @seealso \code{\link{transform_logit}} inverse function
#' @export
#' @examples
#' transform_logistic(5,3,10)
transform_logistic <- function(x, xmin, xmax){
    y <- (xmax-xmin)/(1 + exp(-x)) + xmin
    return(y)
}

#' Transforms a given value from between to xmin and xmax to -Inf and Inf
#' @param x numeric value to be scaled
#' @param xmin numeric value representing the lower bound of the old scale
#' @param xmax numeric value representing the upper bound of the old scale
#' @return numeric value with the converted input
#' @seealso \code{\link{transform_logistic}} inverse function
#' @export
#' @examples
#' transform_logit(0,3,10)
transform_logit <- function(x, xmin, xmax){
    y <- -log(((xmax-xmin)/(x-xmin))-1)
    return(y)
}


# Converts x from scale min to max to 0 to 1
toUnitScale <- function(y, min=1,max=100,logflag=FALSE,logbase=10){
    if(logflag){
        rtn <- (log(y,logbase)-log(min,logbase))/(log(max,logbase)-log(min,logbase))
    }
    else{
        rtn <- (y-min)/(max-min)
    }
    rtn
}

# Converts x from scale 0 to 1 back to scale min to max
fromUnitScale <- function(x,min=1,max=100,logflag=FALSE,logbase=10){
    if(logflag){
        rtn <- min*logbase^(x*(log(max,logbase)-log(min,logbase)))
    } 
    else{
        rtn <- min + (max-min)*x
    }
    rtn
}
