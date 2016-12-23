

realizer_create_output_path <- function(outputPath){
    if (!dir.exists(outputPath))
      dir.create(outputPath)
}

realizer_open_file <- function(output){
  sink(output, append = FALSE)
}

realizer_close_file <- function(){
  sink()
}

realizer_add_libraries <-function(){
  cat("library(rLDCP) ", fill = TRUE)
  cat(fill=TRUE)
  }

realizer_input <- function(input){
  cat("####################################################################", fill = TRUE)
  cat("###################### input file definition #######################", fill = TRUE)
  cat("####################################################################", fill = TRUE)

  for (i in 1:length(input[1,])){
    if(!file.exists(input["path", i]))
      stop("Invalid path to file:", input["path", i] )
     cat( input["id", i], sep="")
     cat(" <- read.csv(file=", paste("\"", input["path", i], "\", ", sep=""), fill = TRUE, sep="")
     cat("sep=\"",input["separator", i],"\", dec=\"", input["dec", i], "\", ", fill = TRUE, sep="")
     cat("header=", input["header", i], ") ", fill = TRUE, sep="")
  }
  cat(fill=TRUE)
}

realizer_data_structure <-function(dataStructure,input){
  cat("####################################################################", fill = TRUE)
  cat("###################### data definition #############################", fill = TRUE)
  cat("####################################################################", fill = TRUE)
  for (i in 1:length(dataStructure[1,])){
    cat(fill=TRUE)
    cat( dataStructure["id",i], " <- c(t(",
         dataStructure["ref",i], "[",
         dataStructure["position",i], "])) ", sep="", fill = TRUE)

    cat(fill=TRUE)
    cat(" if( min(",dataStructure["id",i], ") < ", dataStructure["min_value",i], ") ", sep="", fill = TRUE)
    cat("stop(\"Invalid min value in data structure: ", dataStructure[[i]]["id"] ,".\") " , sep="", fill = TRUE)
    cat(" if( max(",dataStructure["id",i], ") > ", dataStructure["max_value",i], ")  ", sep="", fill = TRUE)
    cat("stop(\"Invalid max value in data structure: ", dataStructure["id",i] ,".\") " , sep="", fill = TRUE)
  }

  for (i in 1:length(input[1,])){
    cat( "rm(",input["id",i], ")",sep="", fill = TRUE)
  }

  cat(fill=TRUE)

  cat("####################################################################", fill = TRUE)
  cat("###################### Data Structure definition ##################", fill = TRUE)
  cat("####################################################################", fill = TRUE)

  cat(fill=TRUE)
  cat("input <- c()", fill = TRUE)
  cat(fill=TRUE)
  cat("my_method <- function (input){", fill = TRUE)
  cat("input", fill = TRUE)
  cat("}", fill = TRUE)
  cat(fill=TRUE)
  cat("my_data <- data_structure(input,my_method)", fill = TRUE)

  cat(fill=TRUE)
}

realizer_cp_list <- function(cpList){
  cat("####################################################################", fill = TRUE)
  cat("###################### CP definition ###############################", fill = TRUE)
  cat("####################################################################", fill = TRUE)

  for(i in 1:length(cpList)){
    cat( cpList[[i]]$name, " <- cp(\"",cpList[[i]]$name, "\", c(", sep="")
    cat( paste("\"", cpList[[i]]$a, "\"", collapse=", ", sep=""))
    cat( "))", sep="", fill = TRUE)
  }
  cat(fill=TRUE)
}

realizer_pm_list <- function(pmList){
  cat("####################################################################", fill = TRUE)
  cat("###################### PMs definition ##############################", fill = TRUE)
  cat("####################################################################", fill = TRUE)

  for(i in 1:length(pmList)){
    cat(fill=TRUE)

    cat("g_",pmList[[i]]$name, "<- function(u,y){" , sep="", fill = TRUE)

    if(length(pmList[[i]]$g$mf) != 0){

      cat(" y$w <- degree_mf(fuzzy_partitions(")
        for(mf_i in 1:length(pmList[[i]]$g$mf)){
          if(length(pmList[[i]]$g$mf[[mf_i ]]$vertex) == 4)
          cat("trapezoid_mf(" )
        else if(length(pmList[[i]]$g$mf[[mf_i ]]$vertex) == 3)
          cat("triangle_mf(" )
        else
          stop("Membership function no defined in PM:", pmList[[i]]$name )
        cat( paste( pmList[[i]]$g$mf[[mf_i]]$vertex, collapse=", ", sep=""))
        cat(") ")
        if(mf_i  < length(pmList[[i]]$g$mf))
          cat(", ", fill = TRUE)
        }

      cat("), u)", fill = TRUE)
    }

    if(length(pmList[[i]]$g$rule$rules) != 0){
      cat("operator <- operator( ", tolower(pmList[[i]]$g$rule$conjunction), "," , tolower(pmList[[i]]$g$rule$disjunction), ")", fill = TRUE)
      cat(" y$w <- infer_rules(fuzzy_rules(")
       for(r_i in 1:length(pmList[[i]]$g$rule$rules)){
        cat("fuzzy_rule(", paste( pmList[[i]]$g$rule$rules[[r_i]], collapse=", "), ")", sep="")
        if(r_i  < length(pmList[[i]]$g$rule$rules))  cat(", ", fill = TRUE)
       }
      cat("), operator, ", fill = TRUE)
      cat("list(", paste("u[[", c(1:length(pmList[[i]]$u$value)), "]]$w", collapse=", ", sep=""),"))", fill = TRUE)
      }

    cat("y }", fill = TRUE)
    cat(fill=TRUE)
    cat("t_" ,pmList[[i]]$name, "<- function(y){", sep="", fill = TRUE)
    cat("templates <- c(", paste( "\"", pmList[[i]]$t$value, "\"", collapse=", ", sep=""), ")", sep="", fill = TRUE)
    cat("return(templates[which.max(y$w)])", fill = TRUE)
    cat("}", fill = TRUE)
    cat(fill=TRUE)
    cat( pmList[[i]]$name, " <- pm(y=", pmList[[i]]$y$value, ", g=g_", pmList[[i]]$name,
         ", t=t_", pmList[[i]]$name, ")", sep="", fill = TRUE)
  }
  cat(fill=TRUE)
}

realizer_glmp <- function(pmList,dataStructure){

  cat("####################################################################", fill = TRUE)
  cat("###################### GLMP definition ##############################", fill = TRUE)
  cat("####################################################################", fill = TRUE)

  cat(fill=TRUE)
  cat("glmp_method <- function(pm,input){", fill = TRUE)

  for(i in 1:length(pmList)){
    cat("pm$",pmList[[i]]$name, "  <- pm_infer(pm$", pmList[[i]]$name, ", ", sep="")

    if(pmList[[i]]$u$type[1] == "data.ref"){ #solo puedo haber un elemento
      if(length(pmList[[i]]$u$value) > 1)
        stop("Realizer. Invalid number of elements in u. PM name:", pmList[[i]]$name)
      cat("input[",  which(dataStructure["id",]== pmList[[i]]$u$value[1]) ,"])", sep="", fill = TRUE)

    }
    else if(pmList[[i]]$u$type[1] == "cp.ref"){ #puede contener muchos elementos
      cat("list(", paste("pm$", pmList[[i]]$u$ref, "$y", collapse = ",", sep=""), "))", sep="", fill = TRUE)
    }
    }
  cat("pm", fill = TRUE)
  cat("}", fill = TRUE)

  cat("my_glmp <- glmp(list(")
  for(i in 1:length(pmList)){
    cat(pmList[[i]]$name, "=",pmList[[i]]$name)
    if(i < length(pmList))
      cat(", ")
  }
  cat("), glmp_method)", fill = TRUE)
}

realizer_report_template <-function(reportTemplate){

  cat("####################################################################", fill = TRUE)
  cat("###################### Report Template definition ##################", fill = TRUE)
  cat("####################################################################", fill = TRUE)

  cat(fill=TRUE)
  cat( "report_method <- function(properties,pm){ ", fill = TRUE)
  cat( "paste (")
  for(i in 1:length(reportTemplate)){
    if(names(reportTemplate[i])== "output.ref"){
      cat( "pm_report(pm$", reportTemplate[i], "), ", sep="", fill = TRUE)
    }else  if(names(reportTemplate[i])== "text"){
      cat( "\"",reportTemplate[i],"\",", sep="", fill = TRUE)
    }
  }
  cat( " sep=\"\")}", fill = TRUE)
  cat("properties = NULL", fill = TRUE)
  cat("my_report <- report_template(properties,report_method)", fill = TRUE)
}


realizer_ldcp<-function(dataStructure){
  cat("####################################################################", fill = TRUE)
  cat("###################### LDCP definition #############################", fill = TRUE)
  cat("####################################################################", fill = TRUE)


  cat(fill=TRUE)
  cat("my_ldcp <- ldcp(data=my_data,glmp=my_glmp,report=my_report)", fill = TRUE)

  if(length(dataStructure["id"])>=1){
    cat("for(i in 1:length(",dataStructure["id",1],")){", sep="", fill = TRUE)
    cat(" current_input <- c(")
    for (i in 1:length(dataStructure[1,])){
      cat(dataStructure["id",i], "[i]", sep="")
      if(i < length(dataStructure[1,]))
        cat(",")
    }
    cat(")", fill = TRUE)

    cat("my_ldcp <- ldcp_run(ldcp=my_ldcp,input=current_input)", fill = TRUE)

     cat("cat(\"Input: c(")
    for (i in 1:length(dataStructure[1,])){
      cat(dataStructure["id",i], sep="")
      if(i < length(dataStructure[1,]))
        cat(",")
    }
    cat("), c(\", paste(current_input, collapse=\",\", sep=\"\"), \")\", sep=\"\", fill = TRUE)", fill = TRUE)

    cat("cat(\"Output: \",my_ldcp$report$description,fill = TRUE)", fill = TRUE)
    cat("cat(fill = TRUE)", fill = TRUE)
    cat("}", fill = TRUE)
  }


}

