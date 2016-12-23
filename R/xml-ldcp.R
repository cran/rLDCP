#' @title XML to rLDCP
#' @description
#' The function takes as input the path to a XML file that contains a LDCP system.
#' Then it validates the LDCP system and generates its corresponding in R code.
#' This R code is stored in an output file. The output file path is another
#' function parameter.
#' @param input is the XML source path file. E.g. "/folder/ldcp_system.xml".
#' @param output is the R destination path file. E.g. "/folder/ldcp_system.R".
#' @return If the process ends without error, the user will receive two messages:
#' one indicates that the XML is valid and the other indicates that the code has
#' been generated successfully. Otherwise, the user will receive the detailed list of errors.
#' @examples
#' \dontrun{xml2rldcp('extdata/comfortableroom.xml','comfortableroom.R')}
#'
#' ## The xml is valid
#' ## The code has been generated successfully
#' @export
xml2rldcp <- function(input,output){
  validate_xml(input)
 generate_code(input,output)
}

#' @title Validate the XML file
#' @description
#' The function takes as input the path to a XML file that contains a LDCP system.
#' Then it validates the LDCP system.
#' @param xmlfile is the XML source path file. E.g. "/folder/ldcp_system.xml".
#' @param schema is the ldcp schema path file. By default is "ldcpSchema.xsd".
#' @return If the process ends without error, the user will receive the message that indicates
#' that the XML is valid. Otherwise, the user will receive the detailed list of errors.
#' @examples
#' \dontrun{validate_xml('extdata/comfortableroom.xml')}
#'
#' ## The xml is valid
#'
#' @export
validate_xml <- function(xmlfile,schema=NULL){

   if(is.null(schema))
   schema = system.file("ldcpSchema.xsd", package="rLDCP")

   validate(xmlfile,schema)
}

#' @title Generate the R code
#' @description
#' The function takes as input the path to a XML file that contains a LDCP system.
#' Then it generates its corresponding in R code. This R code is stored in an output
#' file. The output file path is another function parameter.
#' @param input is the XML source path file. E.g. "/folder/ldcp_system.xml".
#' @param output is the R destination path file. E.g. "/folder/ldcp_system.R".
#' @return If the process ends without error, the user will receive a message that indicates
#'  that the code has been generated successfully. Otherwise, the user will receive the
#'  detailed list of errors.
#' @examples
#' \dontrun{generate_code('extdata/comfortableroom','comfortableroom')}
#'
#' ## The code has been generated successfully
#' @export
generate_code <- function(input, output){
   parse_data <- parser_parse(input)
   realizer_create_output_path(normalizePath(dirname(output)))
   realizer_open_file(output)
   realizer_add_libraries()
   realizer_input(parse_data$input)
   realizer_data_structure(parse_data$dataStructure,parse_data$input)
   realizer_cp_list(parse_data$cpList)
   realizer_pm_list(parse_data$pmList)
   realizer_glmp(parse_data$pmList,parse_data$dataStructure)
   realizer_report_template(parse_data$reportTemplate)
   realizer_ldcp(parse_data$dataStructure)
   realizer_close_file()
   cat("The code has been generated successfully", fill =TRUE)
}
