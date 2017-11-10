#' @title Define the GLMP
#' @description
#' Granular Linguistic Model of Phenomena (GLMP) is a network of \code{\link{cp}} and \code{\link{pm}} objects.
#' that allows the designer to model computationally her/his own perceptions. The input data are introduced
#' into the model through 1PMs which interpret the input data and create CPs. Then, 2PMs take several CPs
#' as input and generate 2CPs. Of course, following the same scheme, is possible to add additional upper levels.
#'
#' The \code{glmp} constructor receive as arguments the list of pms and the method with the computational model.
#' @param pms is the list of \code{\link{pm}} objects included in the \code{glmp}.
#' @param method is the function with the glmp computational model. The method must have two arguments: the list of
#' \code{\link{pm}} objects defined in the \code{glmp} and the input \code{data}:
#'
#' \code{my_glmp_method <- function(pm,input)}
#' @return The generated \code{glmp = list(pm, method)}
#' @examples
#' \dontrun{glmp_method <- function(pm,input){
#'
#'   pm$pm_depth   <- pm_infer(pm$pm_depth, input[1])
#'   pm$pm_height  <- pm_infer(pm$pm_height,input[2])
#'   pm$pm_width   <- pm_infer(pm$pm_width, input[3])
#'
#'   pm$pm_frame  <- pm_infer(pm$pm_frame, list( pm$pm_depth$y,
#'                                            pm$pm_height$y,
#'                                            pm$pm_width$y)
#'   )
#'   pm
#' }
#'
#' my_glmp <- glmp(list(pm_depth  = pm_depth,
#'                      pm_height = pm_height,
#'                      pm_width  = pm_width,
#'                      pm_frame  = pm_frame),
#'                 glmp_method)
#'                 }
#' @export
glmp <- function(pms,method){

  if(!is.function(method)) stop("Illegal parameter: method must be a function.")
  params <- formals(method)
  if(is.null(params) | length(params) != 2) stop("Illegal parameter: method must be two parameters.")

  if(class(pms) != "list") stop("Illegal parameter: pms must be an instance of list class")

  if(class(pms) != "list" & length(pms)==0) stop("Illegal number of pms")

  for(i in 1:length(pms))
    if (class(pms) == "list" & class(pms[[i]]) !="pm" & class(pms[[i]]) != "pm_multidimensional")
      stop('Illegal parameter class: ', class(pms[[i]]), ". The pms parameter must contain instances of pm or pm_multidimensional class.")

  obj <- list(pms = pms,
              method = method)

  class(obj) <- "glmp"
  obj
}

