#' Access HEEL protocols in a desired format.
#'
#' RMarkdown document for the desired protocol is rendered in the specified format and save in
#' the working directory.  The file is then launched via the system.  If no arguemts are provided
#' the function will print a list of the avaiable protocols.
#'
#' @usage
#' get.protocol(protocol, format)
#' @param protocol Name of desired protocol form the following list.  Character vector of length 1.
#' \describe{
#'   \item{"Data Management"}
#'   \item{"Dish Cleaning"}
#'   \item{"d15N-AA"}
#'   }
#' @param format File format to be generated. Options are "pdf", "html", "word", "markdown", "rmarkdown". Character vector of length 1.
#' @examples
#' get.protocol("Data Management", "html")
#' @author Gordon W. Holtgrieve
#' @export
#' @import rmarkdown
#' @import knitr
#' @importFrom stringr str_replace

get.protocol <- function(protocol = NULL, format=NULL){
  listo <- cbind(1:2, c("Data Management", "Dish Cleaning", "d15N-AA"), c("DataManagement.Rmd", "DishCleaning.Rmd", "d15NAA.CSIA.AC-PV.Rmd"))

# If no arguments print list of supported protocols
  if (is.null(protocol) | is.null(format)) {
    print(listo)
    return()
  }

# Check if protocol is one of the available options
  row <- which(listo == protocol, arr.ind=TRUE)[1]
  file.name <- listo[row,3]

# Check if format is one of the available options and set parameters
    if (format == "pdf") {
        file.type <- "pdf_document"
        output.file <- stringr::str_replace(file.name, ".Rmd", ".pdf")
    } else if (format == "html"){
        file.type <- "html_document"
        output.file <- stringr::str_replace(file.name, ".Rmd", ".html")
    } else if (format == "word"){
      file.type <- "word_document"
      output.file <- stringr::str_replace(file.name, ".Rmd", ".docx")
    } else if (format == "markdown"){
      file.type <- "md_document"
      output.file <- stringr::str_replace(file.name, ".Rmd", ".md")
    } else if (format == "rmarkdown") {
      rmd.file.path <-  system.file("Rmd", file.name, package = "HEEL", mustWork = T)  # Get the full path to the .Rmd file
      file.copy(rmd.file.path, file.name) # Copy the .Rmd file to current working directory
      system(paste("open", file.name)) # Launch the file using the system
      return()
    } else {
      stop("Error: format is not one of the supported options.")
    }

# Get the full path to the desire .Rmd file
  rmd.file.path <-  system.file("Rmd", file.name, package = "HEEL", mustWork = T)

# Render requested file using knitr
  rmarkdown::render(input = rmd.file.path, output_format = file.type,
                    output_file = output.file, output_dir = getwd())

# Launch the file using the system
  system(paste("open", output.file))
}


