#' @title Define the PM
#' @description
#' Perception Mapping (PM) is used to create and aggregate \code{\link{cp}} objects. Each PM receives
#' a set of inputs (\code{\link{cp}} objects or numerical values) which are aggregated into a single CP.
#' @param u is a vector of n input \code{\link{cp}}s \code{u = (u1, u2,..., un)}. In the special case
#' of first level perception mappings (1PM) the inputs are numerical values provided either by sensors or
#' obtained from a database.
#' @param y is the output \code{\link{cp}}.
#' @param g is an aggregation function employed to calculate w from the input \code{\link{cp}}s. For example,
#' \code{g} might be implemented using a set of fuzzy rules. In the case of 1PMs, \code{g} is built using a set of
#' membership functions.
#' @param t is a text generation algorithm which allows generating the sentences in \code{A}. In simple cases,
#' \code{t} is a linguistic template, e.g., cat("Alabama has", value, "the number of women in the last census".
#'
#' @return The generated \code{pm = list(u,y,g,t)}

#' @seealso \code{\link{cp}}
#'
#' @examples
#' \dontrun{cp_depth <- cp("cp_depth",c("far",
#'                             "bit far",
#'                             "good",
#'                             "close",
#'                             "very close"))
#'
#' g_depth <- function(u,y){
#'   y$w <- degree_mf(fuzzy_partitions(triangle_mf(450,450,550),
#'                                  triangle_mf( 450,550,600),
#'                                  trapezoid_mf(550,600,800, 1000),
#'                                  triangle_mf( 800,1000,1300),
#'                                  trapezoid_mf( 1000,1300,1500,1500)),u)
#'   y
#' }
#'
#' pm_depth  <- pm(y=cp_depth, g=g_depth)
#'
#' ########################### HEIGHT DEFINITION ###########################################
#' cp_height <- cp("cp_height", c("high",
#'                              "average high",
#'                              "centered",
#'                              "average low",
#'                              "low"))
#'
#' g_height <- function(u,y){
#'   y$w <- degree_mf(fuzzy_partitions(trapezoid_mf(-1000,-1000,-600,-400),
#'                                                triangle_mf(-600,-400,0),
#'                                                trapezoid_mf(-400,0,200,400),
#'                                                triangle_mf(200,400,600),
#'                                                trapezoid_mf(400,600,1000,1000)),u)
#'   y
#' }
#'
#' pm_height <- pm(y=cp_height, g=g_height)
#'
#' ########################### WIDTH DEFINITION ###########################################
#' cp_width <- cp("cp_width",  c("left",
#'                                "average left",
#'                                 "centered",
#'                                 "average right",
#'                                  "right"))
#'
#' g_width <- function(u,y){
#'   y$w <- degree_mf(fuzzy_partitions(triangle_mf(-1000,-600,-400),
#'                                                triangle_mf(-600,-400,0),
#'                                               triangle_mf(-400,0,400),
#'                                                triangle_mf(0,400,600),
#'                                                triangle_mf(400,600,1000,1000)),
#'                               u)
#'   y
#' }
#'
#' pm_width  <- pm(y=cp_width,  g=g_width)
#'
#' ########################### FRAME DEFINITION ###########################################
#' cp_frame <- cp("cp_frame", c("bad",
#'                              "middle",
#'                              "good"))
#'
#' g_frame <- function(u,y){
#'
#'   operator <- operator(min, max)
#'
#'   y$w<- infer_rules(fuzzy_rules( fuzzy_rule(0,0,1,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,1),
#'                            fuzzy_rule(1,1,1,1,1, 1,1,1,1,1, 1,1,0,1,1, 1,0,0),
#'                            fuzzy_rule(1,1,1,1,1, 1,0,0,0,1, 0,0,1,0,0, 1,0,0),
#'                            fuzzy_rule(1,0,0,0,1, 1,1,1,1,1, 0,0,1,0,0, 1,0,0),
#'                            fuzzy_rule(0,1,0,1,0, 0,1,0,1,0, 0,0,1,0,0, 0,1,0)),
#'                      operator,
#'                      list(u[[1]]$w,u[[2]]$w,u[[3]]$w))
#'
#'   y
#' }
#'
#' t_frame <- function(y){
#'
#'   templates <- c("It has been taken a bad framed photo",
#'                  "It has been taken a middle framed photo",
#'                  "It has been taken a good framed photo")
#'
#'   return( templates[which.max(y$w)])
#' }
#'
#' pm_frame <-  pm(y=cp_frame, g=g_frame, t=t_frame)
#' }
#' @export
#'
#'
pm <- function(u=NULL, y, g, t=NULL){

  if(!is.function(g)) stop("Illegal parameter: g must be a function.")
  if(!is.null(t) && !is.function(t)) stop("Illegal parameter: t must be a function.")
  if(class(y) != "cp") stop("Illegal parameter: y must be an instance of cp class")

  xpm <- list(u = u,
              y = y,
              g = g,
              t = t
  )
  class(xpm) <- "pm"
  xpm
}
#' @title Call the g function
#' @description
#' It call the \code{g} function in order to make the inference,
#'  i.e.,  map inputs \code{u} to output \code{y}.
#' @param pm is the \code{pm} object.
#' @param u is the new \code{pm} input. By default is NULL.
#' @return the \code{pm} obtained after calling \code{g}.
#' @seealso \code{\link{cp}}
#'
#' @examples
#'
#' cp_depth <- cp("cp_depth", c("far",
#'                            "bit far",
#'                            "good",
#'                            "close",
#'                            "very close"))
#'
#' g_depth <- function(u,y){
#'    y$w <- degree_mf(fuzzy_partitions(triangle_mf(450,450,550),
#'                                               triangle_mf( 450,550,600),
#'                                               trapezoid_mf(550,600,800, 1000),
#'                                               triangle_mf( 800,1000,1300),
#'                                               trapezoid_mf( 1000,1300,1500,1500)),u)
#'  y
#' }
#'
#' pm_depth  <- pm(y=cp_depth, g=g_depth)
#' pm_depth   <- pm_infer(pm_depth, 650)
#'
#' @export
pm_infer <- function(pm,u=NULL){

  if(class(pm) != "pm") stop("Illegal parameter: pm must be an instance of pm class")

  if(!is.null(u)) pm$u = u
  pm$y <- pm$g(pm$u, pm$y)
  pm
}

#' @title Generate the report of y
#' @description
#' It call the \code{t} function in order to generate the linguistic
#' descriptions that better describe the output \code{y}.
#' @param pm is the \code{pm} object.
#' @return the description obtained after calling \code{t}.
#'
#' @examples
#' cp_frame <- cp("cp_frame", c("bad",
#'                            "middle",
#'                            "good"))
#'
#'                            g_frame <- function(u,y){
#'
#' operator <- operator(min, max)
#'
#'   y$w<- infer_rules(fuzzy_rules( fuzzy_rule(0,0,1,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,1),
#'                            fuzzy_rule(1,1,1,1,1, 1,1,1,1,1, 1,1,0,1,1, 1,0,0),
#'                            fuzzy_rule(1,1,1,1,1, 1,0,0,0,1, 0,0,1,0,0, 1,0,0),
#'                            fuzzy_rule(1,0,0,0,1, 1,1,1,1,1, 0,0,1,0,0, 1,0,0),
#'                            fuzzy_rule(0,1,0,1,0, 0,1,0,1,0, 0,0,1,0,0, 0,1,0)),
#'                      operator,
#'                      list(u[[1]]$w,u[[2]]$w,u[[3]]$w))
#'
#'   y
#' }
#'
#' t_frame <- function(y){
#'
#'  templates <- c("It has been taken a bad framed photo",
#'                  "It has been taken a middle framed photo",
#'                  "It has been taken a good framed photo")
#'
#'   return( templates[which.max(y$w)])
#' }
#'
#' pm_frame <-  pm(y=cp_frame, g=g_frame, t=t_frame)
#' pm_report(pm_frame)
#' @export
pm_report <- function(pm){

  if(class(pm) != "pm") stop("Illegal parameter: pm must be an instance of pm class")

   description <- pm$t(pm$y)
   description
 }



