#' @title Define the CP
#' @description
#' In general, CP corresponds with specific parts of the analyzed phenomenon at a certain degree of
#' granularity. To create a computational model of the analyzed phenomenon,
#' the designer analyzes the everyday use of natural language about the monitored phenomenon with
#' the aim of identifying different parts (units of information or granules) based on his/her
#' subjective perceptions. According with Zadeh (1996), a granule is a clump of
#' elements which are drawn together by indistinguishability, similarity, proximity or functionality.
#' The GLMP handles granules by using CPs.
#' @param name is the identifier of the CP.
#' @param a is a vector \code{A = (a1, a2,... , an)} of linguistic expressions that
#'        represents the whole linguistic domain of CP, e.g. we have the linguistic domain "statistical data"
#'        that is represented with three linguistic variables (bad, good, very good).
#' @param b is a vector \code{B = (b1, b2,... , bn)} of linguistic expressions (words or
#' sentences in natural language) that represents the reliability of the CP, e.g., the reliability of
#' the "statistical data" are (low, moderate, high). By default (\code{b = NULL}), the CP does not manage information about reliability.
#' @param r is a vector \code{R = (r1, r2,... , rn)} of relevance degrees 0 <= ri <=1 assigned to each ai in the
#' specific context, e.g., the relevance of the linguistic expressions (bad, good, very good) is (0.5, 0.5, 1)
#' means the perception of "very good" is more relevant than the other two choices.
#' By default (\code{r = NULL}), the function create a r vector with the maximum degree of relevance for all linguistic expression, e.g., (1,1,1).
#' @return The generated \code{CP = list(a, w, r, b, wb)} where \code{w} and \code{wb} are vectors with the validity degrees (wi and wbi in [0,1]) of the linguistic expressions in \code{a} and \code{b} respectively.
#' These vectors are initialized with 0.
#' @examples
#' myCP <-cp("myCP", c("bad", "good", "very good"))
#' myCP <- cp("myCP", c("bad", "good", "very good"), c("low", "moderate", "high"))
#' myCP <- cp("myCP", c("bad", "good", "very good"), r=c(1,0.8,0.9))
#' myCP <- cp("myCP", c("bad", "good", "very good"), c("low", "moderate", "high"), c(1,0.8,0.9))
#'
#' @export
#' @importFrom methods is
cp <- function(name, a, b= NULL, r = NULL){

  stopifnot(is(name, "character"))
  stopifnot(is(a, "character"))

 if (!is.null(b)) stopifnot(is(b, "character"))
 if (!is.null(r)) stopifnot(is(r, "numeric"))

  xcp <- list(
    a = a,
    w = rep(0,length(a))
  )


  if (!is.null(b)){
    xcp$b = b
    xcp$wb = rep(0,length(b))
  }


  if (!is.null(r))
    xcp$r = r
  else
    xcp$r = rep(1,length(a))

  attr(xcp, "class") <- "cp"
  xcp
}


print.cp <- function(obj) {
  cat("A=(", obj$a, ")\n")
  cat("W=(",obj$w, ")\n")
  cat("R=(", obj$r, ")\n")
  cat("B=(", obj$b, ")\n")
  cat("BA=(",obj$wb, ")\n")
}
