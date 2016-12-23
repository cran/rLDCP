# This example is inspired in the LDCP system called InProfilePhoto presented by:
#
# Patricia Conde-Clemente, Jose M. Alonso, and Gracian Trivino. InProfilePhoto: A mobile app to assist people with
# visual disabilities in taking profile photos. Mathware & Soft Computing, 21(1):23-25, June 2014
#
# Downloable in http://www.atlantis-press.com/php/download_paper.php?id=8432
#
########################### DEPTH DEFINITION ###########################################

cp_depth <- cp("cp_depth",c("far",
                          "bit far",
                           "good",
                           "close",
                          "very close"))

g_depth <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(triangle_mf(450,450,550),
                                            triangle_mf( 450,550,600),
                                            trapezoid_mf(550,600,800, 1000),
                                            triangle_mf( 800,1000,1300),
                                            trapezoid_mf( 1000,1300,1500,1500)),
                   u)
  y
}

pm_depth  <- pm(y=cp_depth, g=g_depth)

########################### HEIGHT DEFINITION ###########################################

cp_height <- cp( "cp_height", c("high",
                          "average high",
                          "centered",
                          "average low",
                          "low"))

g_height <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(trapezoid_mf(-1000,-1000,-600,-400),
                                               triangle_mf(-600,-400,0),
                                               trapezoid_mf(-400,0,200,400),
                                               triangle_mf(200,400,600),
                                               trapezoid_mf(400,600,1000,1000)),u)
  y
}

pm_height <- pm(y=cp_height, g=g_height)

########################### WIDTH DEFINITION ###########################################

cp_width <- cp( "cp_width", c("left",
                   "average left",
                   "centered",
                   "average right",
                   "right"))

g_width <- function(u,y){
  y$w <- degree_mf(fuzzy_partitions(triangle_mf(-1000,-600,-400),
                                               triangle_mf(-600,-400,0),
                                               triangle_mf(-400,0,400),
                                               triangle_mf(0,400,600),
                                               trapezoid_mf(400,600,1000,1000)),
                              u)
  y
}

pm_width  <- pm(y=cp_width,  g=g_width)

########################### FRAME DEFINITION ###########################################

cp_frame <- cp( "cp_frame", c("bad",
                  "middle",
                  "good"))

g_frame <- function(u,y){

 operator <- operator(min, max)

 y$w<- infer_rules(fuzzy_rules( fuzzy_rule(0,0,1,0,0, 0,0,1,0,0, 0,0,1,0,0, 0,0,1),
                          fuzzy_rule(1,1,1,1,1, 1,1,1,1,1, 1,1,0,1,1, 1,0,0),
                          fuzzy_rule(1,1,1,1,1, 1,0,0,0,1, 0,0,1,0,0, 1,0,0),
                          fuzzy_rule(1,0,0,0,1, 1,1,1,1,1, 0,0,1,0,0, 1,0,0),
                          fuzzy_rule(0,1,0,1,0, 0,1,0,1,0, 0,0,1,0,0, 0,1,0)),
                     operator,
                     list(u[[1]]$w,u[[2]]$w,u[[3]]$w))
  y
}

t_frame <- function(y){

  templates <- c("It has been taken a bad framed photo",
   "It has been taken a middle framed photo",
   "It has been taken a good framed photo")

 return( templates[which.max(y$w)])
}

pm_frame <-  pm(y=cp_frame, g=g_frame, t=t_frame)


########################### GLMP DEFINITION ###########################################

glmp_method <- function(pm,input){

  pm$pm_depth   <- pm_infer(pm$pm_depth, input[1])
  pm$pm_height  <- pm_infer(pm$pm_height,input[2])
  pm$pm_width   <- pm_infer(pm$pm_width, input[3])

  pm$pm_frame  <- pm_infer(pm$pm_frame, list( pm$pm_depth$y,
                                          pm$pm_height$y,
                                          pm$pm_width$y)
  )
  pm
}

my_glmp <- glmp(list(pm_depth  = pm_depth,
                     pm_height = pm_height,
                     pm_width  = pm_width,
                     pm_frame  = pm_frame),
                glmp_method)

########################### REPORT DEFINITION ##########################################

properties = NULL
report_method <- function(properties,pm){
  pm_report(pm$pm_frame)
}

my_report <- report_template(properties,
                    report_method)

########################### INPUT DATA DEFINITION #####################################

values <- c(650,10,0)

my_method <- function (input){
  input
}

data <- data_structure(values,my_method)

########################### LDCP DEFINITION ###########################################

my_ldcp <- ldcp(data,my_glmp,my_report)

########################### RUN LDCP SYSTEM ###########################################

my_ldcp <- ldcp_run(my_ldcp)

print(my_ldcp$report$description)



