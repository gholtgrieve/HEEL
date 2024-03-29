#' @title Access HEEL protocols in a desired format.
#'
#' @description RMarkdown document for the desired protocol is rendered in the specified format and save in
#' the working directory.  The file is then launched via the system.  If no arguemts are provided
#' the function will print a list of the avaiable protocols.
#'
#' @param protocol Name of desired protocol form the following list.  Character vector of length 1.
#'  \describe{
#'    \item{Data Management}{}
#'    \item{"ish Cleaning}{}
#'    \item{Muffle Furnace}{}
#'    \item{d15N-AA}{}
#'    \item{Exetainer Analysis}{}
#'  }
#' @param format File format to be generated. Options are "pdf", "html", "word", "markdown", "rmarkdown". Character vector of length 1.
#'
#' @import rmarkdown
#' @import knitr
#' @importFrom stringr str_replace
#'
#' @author Gordon W. Holtgrieve
#'
#' @export


get.protocol <- function(protocol = NULL, format=NULL){
  listo <- matrix(ncol = 2, byrow = T, data = c("Data Management", "DataManagement.Rmd",
                                                "Dish Cleaning", "DishCleaning.Rmd",
                                                "Muffle Furnace", "MuffleFurnace.Rmd",
                                                "d15N-AA", "d15NAA.CSIA.AC-PV.Rmd",
                                                "Exetainer Analysis", "ExetainerAnalysis.Rmd"),
                  dimnames = list(NULL, c("Argument", "File Name")))

# If no arguments print list of supported protocols
  if (is.null(protocol) | is.null(format)) {
    print(listo)
    return()
  }

# Check if protocol is one of the available options
  row <- which(listo == protocol, arr.ind=TRUE)[1]
  file.name <- listo[row, c("File Name")]

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


