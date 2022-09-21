#' @title Write final data file and generate report
#'
#' @description This internal function ...
#'
#' @usage EA.creport(results)
#'
#' @param data.file     Character vector of full file names for raw data files from the instrument. Must be a .csv file!
#'
#' @importFrom rmarkdown render
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

  output.file <- "EA_report.pdf"


  # Render requested file using knitr
  rmarkdown::render(input = rmd.file.path,
                    output_file = output.file,
                    output_dir = results$processed.data.dir
                    )

  # Launch the file using the system
  system(paste("open", output.file))

}
