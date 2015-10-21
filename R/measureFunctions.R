
#' Root mean squared error 
#' 
#' @description Calculate the root mean squared difference between two paired numerics. Via the square root of, the sum of the squared difference divided by the number of pairs.
#'
#' @param model numeric not associated with mean-standard deviations, likely model results
#' @param data numeric mean associated with a standard devation, likely data
#' @param ... ignore other parameters passed to this function
#'
#' @return a numeric with the root mean squared error between model and mean. \eqn{\sqrt{\frac{\sum{(m-d)^2}}{n}}}
#' @export
#' @import assertthat
#'
RMSE.measure <- function(model, data, ...){
  assert_that(identical(class(as.vector(model)), class(as.vector(data))))
  assert_that(identical(class(as.vector(model)), 'numeric'))
  
    if(length(as.vector(model)) != length(as.vector(data))){
        stop('Error in rtedem::RMSE.measure: model and data must have same number of points [', dim(as.array(model)), ']!=[', dim(as.array(data)), ']')
      return(NA)
    }
    
  return(sqrt(sum((model-data)^2, na.rm=TRUE)/sum(is.finite(model-data), na.rm=TRUE)))
}


#' Negative log-likelihood
#'
#' @description Calculate the negative log likelihood for model results compaired to paired data mean and normal standard deviation.
#' @param model numerics not associated with (mean, standard deviation) pairs, likely model results
#' @param data numerics mean associated with (mean, standard deviation) pairs, likely data results
#' @param sd numerics standard deviation (normal) associated with (mean, standard deviation) pairs, likely data results
#'
#' @return a numeric scalar 2 times the sum of, the difference between the model and data, squared, divided by the standard deviation. \eqn{2 \sum{(\frac{m-d}/{s})^2}}
#' @export
#'
ll.measure <- function(model, data, sd){

    if(length(model) == 0 && length(data) == 0){
        return(-Inf)
    }
  
    if(length(as.vector(model)) != length(as.vector(data))){
        stop('Error in helperFunc::ll.measure: model and data must have same number of points [', dim(as.array(model)), ']!=[', dim(as.array(data)), ']')
    }
    if(length(as.vector(sd)) != length(as.vector(data))){
        stop('Error in helperFunc::ll.measure: sd and data must have same number of points [', dim(as.array(sd)), ']!=[', dim(as.array(data)), ']')
    }

    if(FALSE){
        filter <- is.finite(model+data) & sd > 0
        temp <- dnorm(model[filter], mean=data[filter], sd=sd[filter], log=TRUE)
        return(sum(temp, na.rm=TRUE))
    }else{
        temp <- ((data-model)/sd)^2
        temp <- temp[is.finite(temp)]
        return(2*sum(temp, na.rm=TRUE))
    }

}
