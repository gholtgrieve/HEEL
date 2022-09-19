#' @title Finalizes raw EA data from NACHO
#'
#' @description This function is used to decompose and finalize EA data from solid samples analyzed on the Delta V irMS systems (NACHO).
#' The function writes a .csv with the thinned raw data and a .pdf with all relevant information about the analysis.  The finalized
#' sample data are written as a .csv file and a dataframe of the data is also returned.
#'
#' The input data file has strict formatting and data requirements.  (This all needs updating) Each sample or standard should have four rows of data: two for  N and
#' 2 C.  Deviations for the is will result in the function failing.  The follow columns must be present in the input data file for the function to work.
#'   \describe{
#'     \item{Row}{Row number for the IsoDat sequence file.}
#'     \item{Analysis}{Unique number assigned by IsoDat for each analysis.}
#'     \item{Identifier 1}{Unique identifier for each sample or standard in the file. If two runs are combined, be sure this field remains unique. Character.}
#'     \item{Comment}{Identifies whether the particular analysis is a 'STD', 'QTY', 'SAMPLE', 'BLANK', 'ZERO', or 'DUMMY'. These are the only vaild options for this field. Character.}
#'     \item{Amount}{Mass of SAMPLE, STANDARD, QTY, or DUMMY in miligrams (mg). BLANK and ZERO can be 0.0 or blank. Numeric.}
#'     \item{Is Ref _}{Flag that identifies wheter this line line is for a reference gas peak (==1) or not (==0). Numeric.}
#'     \item{Area 28}{Peak area for mass 28 in Vs. Numeric.}
#'     \item{d 15N/14N}{Ratio of 15N to 14N in delta units relative to the refernce gas (i.e., N2 tank). Numeric.}
#'     \item{Area 44}{Peak area for mass 44 in Vs. Numeric.}
#'     \item{d 13C/12C}{Ratio of 13C to 12C in delta units relative to the refernce gas (i.e., CO2 tank). Numeric.}
#'     }
#'
#' The remaining columns should be unchanged from what is created by IsoDat.  There should be a total of 49 columns of data in the raw data file.
#' Column names will be modified when imported to R.
#'
#' @usage EA.NACHO(data.file.dir = getwd(), combine.runs = F, area.cutoff = F)
#'
#' @param data.file      Character vector of full file names for raw data files from the instrument. Must be a .csv file!
#' @param data.file.dir  Character vector of length 1 containing file path to raw .csv files.
#' @param combine.runs   Boolean flag identifying if the standard across all runs in the folder should be combined into a single standard curve or if each run should be analyzed independently.
#'                       FALSE means each run is corrected independently. Default is to combine standards (==T).
#' @param area.cutoff    Defines the peak area (Vs) cutoff below which an individual analysis (injection) is dropped.  FALSE means no cutoff applied.  Numeric defines the cutoff value.
#'
#' @import tidyverse
#' @importFrom tools file_path_sans_ext
#'
#' @return Dataframe of data for each unknown sample that includes following finalized values (i.e., what you want...):
#'   \describe{
#'     \item{d13C.vs.VPDB}{13C:12C of bulk carbon in the sample in delta notation with units of per mil relatve to Vienna Pee Dee Belemite.}
#'     \item{d15N.vs.air}{15N:14N of bulk nitrogen in the sample in delta notation with units of per mil relatve to atmospheric air.}
#'     \item{pctC}{Percent carbon in the sample on a mass basis.}
#'     \item{pctN}{Percent nitrogen in the sample on a mass basis.}
#'    }
#'
#' @author Gordon W. Holtgrieve
#'
#' @export

EA.NACHO <- function(data.file.dir = getwd(), combine.runs = F, area.cutoff = F){

results <- list(analysis.dates=NA,
                data.files=NA,
                raw.sequence.data=NA,
                zero.flag=NA,
                blank.flag=NA,
                blank.correct.flag=NA,
                drift.correct.flag=NA,
                standard.plots=NA,
                standard.coefficients=NA,
                known.standard.values=NA,
                calibration.coefficients=NA,
                error.analysis.results=NA,
                standard.CN=NA,
                sample.CN=NA,
                zero.CN=NA,
                blank.CN=NA,
                run.comments=NA)

  results$analysis.dates <- readline("Enter date(s) samples were run on the irMS. Use format YYYY-MM-DD:  ")

  results$data.files <- list.files(data.file.dir)

  # Now walk through the data decomposition steps
  results[c("standard.CN","sample.CN","blank.CN", "zero.flag", "blank.flag")] <- EA.organize(results)
  results[c("standard.CN","sample.CN","blank.correct.flag")] <- EA.blank.correct(results)
  #EA.check.peak.areas(area.cutoff = area.cutoff)
  results[c("standard.CN","sample.CN","drift.correct.flag")] <- EA.drift.correct(results)
  results[c("standard.plots","standard.coefficients")] <- EA.plot.standards(results)
  results[c("standard.CN","sample.CN","calibration.coefficients")] <- EA.adjust(results)
  results[c("known.standard.values", "error.analysis.results")] <- EA.check(results)
  #EA.report(results)

  return(results$sample.CN)
}
