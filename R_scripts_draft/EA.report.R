#' @title
#'
#' @description This function ...
#'
#' @usage EA.check(results)
#'
#' @param data.file     Character vector of full file names for raw data files from the instrument. Must be a .csv file!
#'
#' @import readr
#' @importFrom tools file_path_sans_ext
#'
#' @return
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export

EA.report <- function(results){

  # Write .csv files

  # Get the full path to the desire .Rmd file
  rmd.file.path <-  system.file("Rmd", "EAoutput.Rmd", package = "HEEL", mustWork = T)
  file.type <- "pdf_document"
  output.file <- paste0("EA_report_",sequenceID,".pdf")


  # Render requested file using knitr
  rmarkdown::render(input = rmd.file.path, output_format = file.type,
                    output_file = output.file, output_dir = getwd())

  # Launch the file using the system
  system(paste("open", output.file))

}
