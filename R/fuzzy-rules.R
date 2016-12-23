###############################################
#
#  GENERIC RULE FUNCTIONS
#
###############################################


#' @title Define the fuzzy rule
#' @description
#' We define a fuzzy rule using the numbers 1 and 0.
#' rule(0,0,1,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,1)
#'
#' This is an example of fuzzy_rule(0,0,1,0,0,1). In the fuzzy rule the number 1 means that the linguistic
#' expression is included and the number 0 means that the linguistic expression is not included.
#'
#' @param ... the 0 and 1 that compose the fuzzy rule.
#' @return the \code{fuzzy_rule <- c(...)}
#' @examples
#' # For example, the rule "IF CPtemp IS warm THEN CPcomfort IS very comfortable"
#' #is coded as:
#'
#'  fuzzy_rule(0,1,0,0,0,1)
#'
#' # Where, the first three values (0,1,0) correspond with the linguistic
#' # expressions Atemp=(cold, warm, hot) that define the room temperature (CPtemp).
#' # The last three values (0,0,1) are related to the linguistic expressions
#' # Acomfort=(uncomfortable, comfortable and very comfortable) that define
#' # the room comfort (CPcomfort).
#' #
#' @export
fuzzy_rule <- function(...){
  r <- c(...)

  if(!is.numeric(r)) stop("Illegal parameters. The parameters must be numeric values.")
  if(any(seq(2,9,1) %in% r)) stop("Illegal parameters. The parametes must be 0 and 1 values.")

  class(r) <- "fuzzy_rule"
  r
}

#' @title Define the fuzzy rules
#' @description
#' It is a constructor of fuzzy rules, the arguments are the diferent \code{\link{fuzzy_rule}} object.
#' @param ... one or more \code{\link{fuzzy_rule}} objects.
#' @return \code{fuzzy_rules <- list(...)}
#' @export
#'
#' @examples
#'
#' fuzzy_rules(fuzzy_rule(0,0,1, 0,0,1, 0,0,1),
#'      fuzzy_rule(1,0,0, 1,0,0, 1,0,0),
#'      fuzzy_rule(0,1,0, 0,1,0, 0,1,0))
fuzzy_rules <- function(...){

  rls <- list(...)

  if(length(rls)==0) stop("Illegal number of rules.")

  for(i in 1:length(rls))
    if (class(rls[[i]]) !="fuzzy_rule")
      stop('Illegal parameter class: ', class(rls[[i]]), ". This function can receive as input some instances of fuzzy_rule class.")

  #calculates the length of the rules
  lenR = unique(sapply(rls, length))
  #Check that all the rules have the same lengths
  if(length(lenR) !=1) stop("Illegal lenght of rule: All rules must have the same length.")

  class(rls) <- "fuzzy_rules"
  rls
}

#' @title Make the inference
#' @description Make an inference with the fuzzy rules.
#'
#' @param rules the set of fuzzy rules.
#' @param operator the \code{\link{operator}} object.
#' @param input is the list of validity degrees related to the input \code{\link{cp}} objects.
#'
#' @return A vector that containd the result of the inference.
#' @examples
#' ## In the example the input of the fuzzy rule correspond with two CPs and each CP has 3
#' ## linguistic variables, e.g, {"bad", "good", "very good"}. The output also
#' ## correspond with a CP with 3 linguistic variables.
#'
#'  infer_rules(fuzzy_rules(fuzzy_rule(0,0,1, 0,0,1, 0,0,1),
#'                    fuzzy_rule(1,0,0, 1,0,0, 1,0,0),
#'                    fuzzy_rule(0,1,0, 0,1,0, 0,1,0)),
#'              operator(min, max),
#'              list(c(0,0.5,0.5),c(0.5,0.5,0)))
#' ## [1] 0.0 0.5 0.0
#' @export
infer_rules <- function(rules, operator, input){

  if (class(rules) !="fuzzy_rules") stop('Illegal parameter class: the object rules have the class ', class(rules))
  if (class(operator) !="operator") stop('Illegal parameter class: the object operator have the class ', class(operator))
  if (class(input) !="list") stop('Illegal parameter class: the object input have the class ', class(list))

  #calculates the length of the rules
  lenR = unique(sapply(rules, length))

  if(length(input) == 0) stop('Illegal lenght of parameter input: length(input)=', length(input))

  #calculates the length of the input
  lenI = sum(sapply(input, length))

  if(lenI >= lenR[1]) stop('Illegal lenght of parameter input: length(input)>=length(rules[[1]]) ')

  output <- rep(0,lenR[1]-lenI)

  for(rule in rules){
    pos=1
    value =-1
    for(i in 1:length(input)){
      sum=0
      for(j in 1:length(input[[i]])){
        sum = sum + rule[pos] * input[[i]][j]
        pos= pos+1
      }
      if(value == -1)
        value = sum
      else
        value = operator$conjunction(sum, value)
    }
    for(j in 1:length(output)){
      output[j] <- operator$disjunction(rule[pos] * value, output[j])
      pos = pos+1
    }
    }

  output
}

#' @title Define the operator
#' @description
#' The operator defines the conjunction and disjunction functions used in the fuzzy rules.
#' It takes as input parameters the function used to implement the conjunction,
#' and the function used to implement the disjunction, e.g., "operator(min, max)", where
#' min and max are functions defined by the R language that calculate the maximum and
#' minimum, respectively, from a set of values received as input. Note that, we implicitly assign
#' to the fuzzy implication operator (THEN) the function given for conjunction
#' @param conjunction is the method used to make the conjunction.
#' @param disjunction is the method used to make the disjunction.
#'
#' @return the \code{opertator} object \code{my_op <- list(conjunction, disjunction)}.
#' @export
#'
#' @examples
#' operator <- operator(min, max)
operator <- function(conjunction, disjunction){

  if(!is.function(conjunction)) stop("Illegal parameter: conjunction is not a function")
  if(!is.function(disjunction)) stop("Illegal parameter: disjunction is not a function")

  op <- list(
    conjunction = conjunction,
    disjunction = disjunction)

  class(op) <- "operator"
  op
}



