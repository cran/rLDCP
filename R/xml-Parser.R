
parser_parse <-function(xmlfile){
  doc = XML::xmlTreeParse(xmlfile)
  xmlList <- XML::xmlSApply(doc, function(x) XML::xmlSApply(x, XML::xmlToList))

  #############################################################
  # CPs Parse  ################################################
  #############################################################

  xmlCPs <- xmlList$glmp[,1]
  xmlNumberCPs <- as.numeric(xmlList$glmp[,1]$.attrs)

  cpAux <- list(name = "id", a = character(0))
  cpList <- list()
  for(i in 1:xmlNumberCPs){
    cpList[[i]] <- cpAux
  }

  cp_i = 1
  i = 1

  while (i <= (length(xmlCPs)-1)){

    if(!is.null(names(xmlCPs[[i]])) && (names(xmlCPs[[i]])=="count")){

       xmlNumberLe <- as.numeric(xmlCPs[[i]])

       if( length(cpList[[cp_i]]$a) != xmlNumberLe)
       stop("Invalid value of attribute \"count\". The number of le in a is:", length(cpList[[cp_i]]$a))

      i = i+1
      cpList[[cp_i]]$name = xmlCPs[[i]]
      cp_i = cp_i+1 #end of the cp definition
    }
    else{
      cpList[[cp_i]]$a = c(cpList[[cp_i]]$a, xmlCPs[[i]])
    }
    i = i+1
  }

  if( cp_i-1 != xmlNumberCPs)
    stop("Invalid value of attribute \"count\". The number of cp is:", cp_i-1)

  cpNamePosition = (unlist(cpList))[which(names(unlist(cpList))=="name.id")]

  rm(xmlCPs)
  rm(xmlNumberCPs)

#############################################################
# PM Definicion  ###############################################
#############################################################

  xmlPMs <- xmlList$glmp[,2]
  xmlNumberPMs <- as.numeric(xmlList$glmp[,2]$.attrs)

  pmAux <- list(name = "id",
                u = list(type = character(0),value = character(0), ref= character(0)),
                y = list(type = character(0),value = character(0)),
                g = list(mf = list(), rule = list(conjunction = character(0), disjunction = character(0), rules= list() )),
                t= list(le = character(0),value = character(0)))
  pmList <- list()
  for(i in 1:xmlNumberPMs){
    pmList[[i]] <- pmAux
  }

  network <- matrix(nrow=xmlNumberPMs,ncol = 2)
  colnames(network) <- c("pm", "y")
  to = 0
  i=1
  for(i in 1:xmlNumberPMs){ #siempre son 5 elementos (id,u,y,g,t) por PM
    from = to +1
    to = to + 5
    pmCurrent <- xmlPMs[from:to]
    network[i,"pm"] <- pmCurrent[[5]]
    network[i,"y"] <- unlist(pmCurrent[[2]])
  }

  to = 0
  i=1
  for(i in 1:xmlNumberPMs){ #siempre son 5 elementos (id,u,y,g,t) por PM
    from = to +1
    to = to + 5
    pmCurrent <- xmlPMs[from:to]
    #name
    pmList[[i]]$name <- pmCurrent[[5]]
    #u
    pmList[[i]]$u$type <- names(pmCurrent[[1]])
    pmList[[i]]$u$value <- unlist(pmCurrent[[1]])
    pmList[[i]]$u$ref <- sapply(FUN = function(x) network[which(network[,"y"]==x), "pm"],pmList[[i]]$u$value)
    #y
    pmList[[i]]$y$type <- names(pmCurrent[[2]])
    pmList[[i]]$y$value <- unlist(pmCurrent[[2]])
    #g
    for(j in 1: length(pmCurrent[[3]])){

      if(colnames(pmCurrent[[3]])== "fuzzy_partitions"){
        aux <- list(le = character(0), vertex = character(0))
        xmlNumberMF =  pmCurrent[[3]][".attrs",][[1]]
        if( (length(pmCurrent[[3]][,1])-1) != xmlNumberMF)
          stop("Invalid value of attribute \"count\". The number of mf is:", (length(pmCurrent[[3]][,1])-1), " in PM name: ", pmList[[i]]$name)

          for(p in 1: xmlNumberMF){
              pmList[[i]]$g$mf[[p]] <- aux
              pmList[[i]]$g$mf[[p]]$le= rbind(pmCurrent[[3]][[p]][[1]])
              pmList[[i]]$g$mf[[p]]$vertex= rbind(as.numeric(pmCurrent[[3]][[p]][2:length(pmCurrent[[3]][[p]])]))
          }
      }

      if(colnames(pmCurrent[[3]])== "fuzzy_rules"){

         xmlNumberRules =  as.numeric(pmCurrent[[3]][".attrs",][[1]]["count"])
         pmList[[i]]$g$rule$conjunction =  pmCurrent[[3]][".attrs",][[1]]["conjunction"]
         pmList[[i]]$g$rule$disjunction =  pmCurrent[[3]][".attrs",][[1]]["disjunction"]

        indexesAnt = as.numeric(sapply( FUN=function (x) {which(x ==cpNamePosition)[1]} , pmList[[i]]$u$value))
        indexCon = as.numeric(sapply( FUN=function (x) {which(x ==cpNamePosition)[1]} , pmList[[i]]$y$value))
        ruleLenght = sum(sapply(FUN=function(x) length(cpList[[x]]$a), c(indexesAnt,indexCon)))

        rules_i = 1
        count_i=1;
        while(rules_i <= xmlNumberRules){
          cuRule <- as.numeric()
           for(a in indexesAnt){
             if ( pmCurrent[[3]][,1][[count_i]]$.attrs == cpList[[a]]$name){
               le.refs <- as.character(pmCurrent[[3]][,1][[count_i]][names(pmCurrent[[3]][,1][[count_i]])=="le.ref"])
               antValues = sapply( FUN=function (x) {as.numeric(x ==cpList[[a]]$a)} , le.refs)
                if(length(antValues[1,]) > 1){ #creo la representaciOn binaria del antecedente.
                  for(u in 2:length(antValues[1,])){
                  antValues[,1] <- antValues[,1] | antValues[,u]
                  }
                }
               cuRule <- c(cuRule, antValues[,1])
                   count_i = count_i + 1
             }else{ # doy por echo que el usuario no incluye el indice es DC (Dont Care) todo unos.
              cuRule <- c(cuRule, rep(1,length(cpList[[a]]$a)))
            }
          }
          cuRule <- c(cuRule, as.numeric(pmCurrent[[3]][,1][[count_i]]$le.ref==cpList[[indexCon]]$a))
          count_i = count_i + 1

          if(ruleLenght != length(cuRule)) # si al terminar la longitud no es la adecuada, es que se ha definido mal en el xml
            stop("Incorrect rule: " ,  rules_i, ". Please check the antecedents order and the linguistic expressions.", " in PM name: ", pmList[[i]]$name)

          pmList[[i]]$g$rule$rules[[rules_i]] <- cuRule
          rules_i = rules_i + 1
        }

         if(length(pmCurrent[[3]][,1]) != count_i)
           stop("Invalid number of rules in PM name: ", pmList[[i]]$name)
      }
    }

    #t
    j=1
    while (j <=length(pmCurrent[[4]])){
      pmList[[i]]$t$value <- c(pmList[[i]]$t$value, pmCurrent[[4]][[j]])
      pmList[[i]]$t$le <- c(pmList[[i]]$t$le, pmCurrent[[4]][[j+1]])
      j = j+ 2
    }
  }

  reportTemplate = xmlList$report_template

  for(i in 1:length(reportTemplate)){
     if(names(reportTemplate[i])== "output.ref"){
      pm.ref = network[which(network[,"y"]==reportTemplate[i]), "pm"]
       if(length(pm.ref) == 0)
         stop("Invalid name of CP in the reportTemplate: " , reportTemplate[i])
      reportTemplate[i] = pm.ref
    }
  }




  #############################################################
  # output data   ###############################################
  #############################################################

  parse_data <- list(input = xmlList$input,
                    dataStructure = xmlList$data_structure,
                    cpList = cpList,
                    pmList = pmList,
                    reportTemplate = reportTemplate)

  #############################################################
  # PM Definicion  ###############################################
  #############################################################

  return(parse_data)

}


