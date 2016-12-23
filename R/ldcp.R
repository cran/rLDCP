#' @title Define the LDCP system
#' @description
#' Linguistic Descriptions of Complex Phenomena (LDCP) is a technology focused on
#' modeling complex phenomena, interpreting input data and generating automatic
#' text reports customized to the user needs. #'
#' The \code{ldcp} constructor receive as arguments: the \code{data_structure},
#' the \code{\link{glmp}} and the \code{\link{report_template}}.
#' @param data is the \code{\link{data_structure}} object.
#' @param glmp is the \code{\link{glmp}} object.
#' @param report is the \code{\link{report_template}} object.
#' @return The generated system \code{ldcp = list(data, glmp, report)}
#' @seealso \code{\link{cp}} and \code{\link{pm}}
#' @examples
#' \dontrun{my_ldcp <- ldcp(my_data,my_glmp,my_report)}
#' @export
ldcp <-function(data,glmp,report){

  if(class(data) != "data_structure") stop("Illegal parameter: data must be an instance of data_structure class")
  if(class(glmp) != "glmp") stop("Illegal parameter: glmp must be an instance of glmp class")
  if(class(report) != "report_template") stop("Illegal parameter: report must be an instance of report_template class")

  obj <- list(data=data,
              glmp=glmp,
              report = report)

  class(obj) <- "ldcp"
  obj
}

#' @title Execute the LDCP system
#' @description
#' Execute the \code{\link{ldcp}} system in order to obtain the linguistic report.
#' This method follows these three sequential steps 1) Data acquisition, 2) Interpretation and 3)
#' Report generation.
#' Data acquisition process gets the input data and prepares the data structure. Then, the data
#' are interpreted using the GLMP. The result is a set of computational perceptions (CP) that are valid to describe
#' these data. Finally, the report generation process generates a linguistic report using the report template
#' and the previous set of CPs.
#' @param ldcp is the \code{\link{ldcp}} system.
#' @param input is the system input data. May be a vector, list, or matrix with numbers. It is a new input to the \code{\link{data_structure}} object. By default, is NULL.
#' @return The \code{\link{ldcp}} object that contains the execution results.
#' @examples
#' \dontrun{my_ldcp <- ldcp_run(my_ldcp)}
#' @export
ldcp_run <- function(ldcp, input=NULL){

  if(class(ldcp) != "ldcp") stop("Illegal parameter: ldcp must be an instance of ldcp class")

  if(!is.null(input)) ldcp$data$input <- input

  input <- ldcp$data$method(ldcp$data$input)

  ldcp$glmp$pm <- ldcp$glmp$method(ldcp$glmp$pm,input)
  ldcp$report$description <- ldcp$report$method(ldcp$report$properties,ldcp$glmp$pm)
  ldcp
}

