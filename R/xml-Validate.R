
validate <-function(xmlfile, fileSchema){

  schema <- XML::xmlSchemaParse(file =fileSchema )
  tryCatch( XMLValidate <- XML::xmlTreeParse(file =xmlfile, useInternalNodes = TRUE , error = NULL),
    XMLError = function(e) {
      cat("There was an error in the XML at line",  e$line, "column", e$col, "\n",  e$message, "\n")
    })
  result <- XML::xmlSchemaValidate(schema, XMLValidate)
  if(result$status != 0){
    cat("The xml is invalid.", fill =TRUE)
    for(i in 1:length(result$errors)){
      e <- result$errors[[i]]
      cat("(line", e$line, "column", e$col, ")",e$msg, fill =TRUE)
    }
  }else{
    cat("The xml is valid", fill =TRUE)
  }
  XML::free(XMLValidate)
}
