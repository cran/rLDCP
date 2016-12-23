###############################################
#
#  GENERIC MEMBERSHIP FUNCTIONS
#
###############################################
#' @title Define generic calculation of fuzzy membership degrees
#' @description
#' It is a generic function in charge of computing fuzzy membership degrees. Namely, it identifies the specific
#' membership function to consider and run the related method for computing the membership degree for a given
#' input value. It takes as input an object (\code{trapezoid_mf}, \code{triangle_mf} and \code{fuzzy_partitions}) and the related input values
#' @param shape is the object (\code{trapezoid_mf}, \code{triangle_mf} and \code{fuzzy_partitions}) to dispatch to.
#' @param input is the value to be assess.
#' @return the membership degree for a given input values.
#' @examples
#'
#' w <- degree_mf(triangle_mf(450,450,550),450)
#'
#' w <- degree_mf(fuzzy_partitions(triangle_mf(450,450,550),
#'                                 triangle_mf(450,550,600),
#'                                 trapezoid_mf(550,600,800,1000),
#'                                 triangle_mf(800,1000,1300),
#'                                 trapezoid_mf(1000,1300,1500,1500)),450)
#' @export
degree_mf <- function(shape, input){
  UseMethod("degree_mf")
}

#' @export
degree_mf.fuzzy_partitions <- function(shape, input){

    w <- rep(0,length(shape))

  for(j in 1:length(shape))
    w[j] <- degree_mf(shape[[j]],input)
  w
}


#' @export
degree_mf.trapezoid_mf <- function(shape, input){

  result <- 0

  if (input >= shape$a & input < shape$b & (shape$b-shape$a) != 0)
    result <- (input - shape$a) / (shape$b - shape$a)
  else if (input >= shape$b & input < shape$c)
    result <- 1
  else if (input >= shape$c & input <= shape$d & (shape$d - shape$c) !=0)
    result <- (shape$d - input) / (shape$d - shape$c)
  else if (input == shape$c & (shape$d - shape$c) ==0)
    result <- 1

  result
}
#' @export
degree_mf.triangle_mf <-function(shape, input){
  result <- 0

  if(input >= shape$a & input <= shape$b & shape$b-shape$a != 0)
    result  <- (input-shape$a) / (shape$b-shape$a)
  else if(input == shape$b)
    result <- 1
  else if(input >= shape$b & input <= shape$c & shape$c-shape$b != 0)
    result <- (shape$c-input) / (shape$c-shape$b)

  result
}

###############################################
#
#  CONSTRUCTORS
#
###############################################

#' @title Define the trapezoid membership function
#' @description
#' It is a constructor of trapezoidal shapes. They take as input the numerical values which
#' define the anchor points in membership functions.
#' @param a the trapezoid point a.
#' @param b the trapezoid point b.
#' @param c the trapezoid point c.
#' @param d the trapezoid point d.
#'
#' @return the \code{(trapezoid_mf <- list(a,b,c,d))}
#' @export
#'
#' @examples
#' trapezoid_mf(0, 1, 2, 3)
trapezoid_mf <- function(a, b, c, d){

  if (a > b)  stop('Illegal parameter condition: a > b')
  if (c > d)  stop('Illegal parameter condition: c > d')

  trap <- list( a=a,
                 b=b,
                 c=c,
                 d=d
  )
  class(trap)  <- "trapezoid_mf"
  trap
}

#' @title Define the triangle membership function
#' @description
#' It is a constructor of triangular shapes. They take as input the numerical values which
#' define the anchor points in membership functions.
#' @param a the trapezoid point a.
#' @param b the trapezoid point b.
#' @param c the trapezoid point c.
#'
#' @return the \code{(triangle_mf <- list(a,b,c))}
#' @export
#'
#' @examples
#' triangle_mf(0, 1, 2)
triangle_mf <- function(a, b, c){

  if (a > b)  stop('Illegal parameter condition: a > b')
  if (c < b)  stop('Illegal parameter condition: c < b')
  trip <- list( a=a,
                b=b,
                c=c
  )
  class(trip) <- "triangle_mf"
  trip
}

#' @title Define the fuzzy parititions
#' @description
#' It is a constructor of fuzzy partitions, it defines a set of membership functions.  It takes as input a set of
#' \code{trapezoid_mf} or \code{triangle_mf} or objects in the shape_mf class.
#' @param ... are the diferent partitions, e.g., \code{trapezoid_mf} or \code{triangle_mf}.
#' @return the \code{(fuzzy_partitions <- list(...)}
#' @export
#'
#' @examples
#' fuzzy_partitions(triangle_mf(450,450,550),
#'                  triangle_mf(450,550,600),
#'                  trapezoid_mf(550,600,800, 1000),
#'                  triangle_mf(800,1000,1300),
#'                  trapezoid_mf(1000,1300,1500,1500))
fuzzy_partitions <- function(...){

  fp <- list(...)

  if(length(fp) ==0)  stop('Illegal number of partitions: length(...) > 0')

  for(i in 1:length(fp))
    if (all(is.element(class(fp[[i]]), c("trapezoid_mf","triangle_mf","shape_mf"))==FALSE))
        stop('Illegal parameter class: ', class(fp[[i]]), ". This function can receive as input some instances of the triangle_mf, trapezoid_mf o shape_mf class.")

  class(fp) <- "fuzzy_partitions"
  fp
}
