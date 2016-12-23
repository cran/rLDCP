library(rLDCP) 


####################################################################
###################### gaussian mf definition #######################
####################################################################
gaussian_mf <- function(m,sig){
  trip <- list(m=m, sig=sig)
  class(trip) <- c("gaussian_mf","shape_mf")
  trip
}


degree_mf.gaussian_mf <-function(shape, input){
  result <- exp((-(input - shape$m)^2)/(2 * shape$sig^2))
  result
}

####################################################################
###################### input file definition #######################
####################################################################
input1 <- read.csv(file="data.csv", 
sep=";", dec=".", 
header=TRUE) 

####################################################################
###################### data definition #############################
####################################################################

temperature <- c(t(input1[1])) 

 if( min(temperature) < 10) 
stop("Invalid min value in data structure: NA.") 
 if( max(temperature) > 30)  
stop("Invalid max value in data structure: temperature.") 

light <- c(t(input1[2])) 

 if( min(light) < 250) 
stop("Invalid min value in data structure: NA.") 
 if( max(light) > 900)  
stop("Invalid max value in data structure: light.") 
rm(input1)

####################################################################
###################### Data Structure definition ##################
####################################################################

input <- c()

my_method <- function (input){
  input
}

my_data <- data_structure(input,my_method)

####################################################################
###################### CP definition ###############################
####################################################################
cp_temp <- cp("cp_temp", c("cold", "warm", "hot"))
cp_light <- cp("cp_light", c("low", "medium", "high"))
cp_comfort <- cp("cp_comfort", c("uncomfortable", "comfortable", "very_comfortable"))

####################################################################
###################### PMs definition ##############################
####################################################################

g_pm_temp <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(gaussian_mf(5,6.5), 
                                       gaussian_mf(20,6.5), 
                                       gaussian_mf(35,6.5)), 
                      u)
  y 
}

t_pm_temp<- function(y){
templates <- c(" the temperature is cold ", " the temperature is warm ", " the temperature is hot ")
return(templates[which.max(y$w)])
}

pm_temp <- pm(y=cp_temp, g=g_pm_temp, t=t_pm_temp)

g_pm_light <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(gaussian_mf(250,105), 
                                       gaussian_mf(500,105), 
                                       gaussian_mf(750,105)),
                      u)
  y 
}

t_pm_light<- function(y){
templates <- c(" the light intensity is low", " the light intensity is medium", " the light intensity is high")
return(templates[which.max(y$w)])
}

pm_light <- pm(y=cp_light, g=g_pm_light, t=t_pm_light)

g_pm_comfort<- function(u,y){
operator <- operator(  min , max )
 y$w <- infer_rules(fuzzy_rules(fuzzy_rule(1, 0, 0, 1, 0, 0, 1, 0, 0), 
fuzzy_rule(0, 1, 0, 1, 0, 0, 1, 0, 0), 
fuzzy_rule(0, 0, 1, 1, 0, 0, 1, 0, 0), 
fuzzy_rule(1, 0, 0, 0, 1, 0, 1, 0, 0), 
fuzzy_rule(0, 1, 0, 0, 1, 0, 0, 1, 0), 
fuzzy_rule(0, 0, 1, 0, 1, 0, 1, 0, 0), 
fuzzy_rule(1, 0, 0, 0, 0, 1, 1, 0, 0), 
fuzzy_rule(0, 1, 0, 0, 0, 1, 0, 0, 1), 
fuzzy_rule(0, 0, 1, 0, 0, 1, 1, 0, 0)), operator, 
list( u[[1]]$w, u[[2]]$w ))
y }

t_pm_comfort<- function(y){
templates <- c("The room is uncomfortable", "The room is comfortable", "The room is very comfortable")
return(templates[which.max(y$w)])
}

pm_comfort <- pm(y=cp_comfort, g=g_pm_comfort, t=t_pm_comfort)

####################################################################
###################### GLMP definition ##############################
####################################################################

glmp_method <- function(pm,input){
pm$pm_temp  <- pm_infer(pm$pm_temp, input[1])
pm$pm_light  <- pm_infer(pm$pm_light, input[2])
pm$pm_comfort  <- pm_infer(pm$pm_comfort, list(pm$pm_temp$y,pm$pm_light$y))
pm
}
my_glmp <- glmp(list(pm_temp = pm_temp, pm_light = pm_light, pm_comfort = pm_comfort), glmp_method)
####################################################################
###################### Report Template definition ##################
####################################################################

report_method <- function(properties,pm){ 
paste (pm_report(pm$pm_comfort), 
", because",
pm_report(pm$pm_temp), 
"and",
pm_report(pm$pm_light), 
".",
 sep="")}
properties = NULL
my_report <- report_template(properties,report_method)
####################################################################
###################### LDCP definition #############################
####################################################################

my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)
for(i in 1:length(temperature)){
current_input <- c(temperature[i],light[i])
my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)
cat("Input: c(temperature,light), c(", paste(current_input, collapse=",", sep=""), ")", sep="", fill = TRUE)
cat("Output: ",my_ldcp$report$description,fill = TRUE)
cat(fill = TRUE)
}
