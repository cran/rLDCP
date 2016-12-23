#' @title Define the data structure
#' @description Data structure provides the GLMP input. It constructor receives the input values and the method that defines
#' the data structure, i.e., the set of preprocesing techniques.
#' @param input is the input data. May be a vector, list or matrix with numbers.
#' @param method is the function with the data preprocesing techniques needed to prepare the GLMP input. The method must have
#' one argument, the \code{input} data:
#'
#' \code{my_method <- function(input)}
#' @return The generated \code{data_structure = list(input, method)}
#' @examples
#'
#' values <- matrix(c(34,11,9,32), ncol=2)
#'
#' my_method <- function (input){
#'  output <- c(mean(input[,1]), mean(input[,2]))
#'  output
#' }
#'  my_data_structure <- data_structure(values,my_method)
#' @export
data_structure <- function(input,method){

  if(!is.function(method)) stop("Illegal parameter: method must be a function.")
  params <- formals(method)
  if(is.null(params) | length(params) != 1) stop("Illegal parameter: method must be one parameter.")

  obj <- list(input = input,
              method = method)
  class(obj) <- "data_structure"
  obj
}
