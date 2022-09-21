#' @title Write final data file and generate report
#'
#' @description This internal function ...
#'
#' @usage EA.report(results)
#'
#' @param data.file     Character vector of full file names for raw data files from the instrument. Must be a .csv file!
#'
#' @importFrom rmarkdown render
#'
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export

EA.report <- function(results){

  # Write .csv files
  results$standard.CN <- rename(as_tibble(results$sample.CN), d.13C.12C.raw = d.13C.12C, d.15N.14N.raw = d.15N.14N)
  results$standard.CN <-rename(as_tibble(results$sample.CN), d.13C.12C.raw = d.13C.12C, d.15N.14N.raw = d.15N.14N)

  write_csv(results$standard.CN, paste0(results$processed.data.dir,"/standard_CN.csv"))
  write_csv(results$sample.CN, paste0(results$processed.data.dir,"/sample_CN.csv"))

  # Get the full path to the desire .Rmd file
  rmd.file.path <-  system.file("Rmd", "EAoutput.Rmd", package = "HEEL", mustWork = T)
  file.type <- "pdf_document"

  output.file <- "EA_report.pdf"


  # Render requested file using knitr
  rmarkdown::render(input = rmd.file.path,
                    output_file = output.file,
                    output_dir = results$processed.data.dir,
                    params = results
                    )

}
