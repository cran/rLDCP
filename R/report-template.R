#' @title Define the report template
#' @description
#' The text generation algorithm contains the programming code capable of generating
#' the appropriate report to each specific user. Algorithms must select and order
#' the linguistic expressions to generate the text included in the report.
#' #'
#' The \code{report_template} constructor receive as arguments the list of properties
#' and the method (programming code) capable of generating the appropriate report.
#' @param properties may be a vector, list or matrix with the user's needs,
#' preferences and goals. By default \code{properties = NULL}.
#' @param method is the function that generates the appropriate report. The method must have
#' two arguments: the list of properties and the list of pms:
#' \code{my_report_method}  \code{<-} \code{function(properties, pm){...}}.
#' @param description is the result of call the report template. By default is NULL
#' @return The generated \code{report_template= list(properties, method,description)}
#' @examples
#' properties = NULL
#' report_method <- function(properties,pm){
#'   pm_report(pm$pm_frame)
#' }
#' my_report <- report_template(properties,
#'                     report_method)
#' @export
report_template <- function(properties=NULL,method, description=NULL){

  if(!is.function(method)) stop("Illegal parameter: method must be a function.")

  params <- formals(method)
  if(is.null(params) | length(params) != 2) stop("Illegal parameter: method must be two parameters.")

  obj <- list(properties = properties,
              method = method,
              description = description)

  class(obj) <- "report_template"
  obj
}

