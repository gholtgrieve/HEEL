#' @title Finalizes raw EA data from NACHO
#'
#' @description This function is used to decompose and finalize EA data from solid samples analyzed on the Delta V irMS systems (NACHO).
#' The function writes a .csv with the thinned raw data and a .pdf with all relevant information about the analysis.  The finalized
#' sample data are also returned as a dataframe.
#'
#' The input data file has strict formatting and data requirements. Each sample or standard should have four rows of data: two for  N and
#' two for C.  Deviations from this is will result in the function failing. Blanks and zeros are an excpetion and can have only two rows.
#' The follow columns must be present in the input data file for the function to work.
#'   \describe{
#'     \item{Row}{Row number for the IsoDat sequence file.}
#'     \item{Analysis}{Unique number assigned by IsoDat for each analysis.}
#'     \item{Identifier 1}{Unique identifier for each sample or standard in the file. If two runs are combined, be sure this field remains unique. Character.}
#'     \item{Comment}{Identifies whether the particular analysis is a 'STD', 'QTY', 'SAMPLE', 'BLANK', 'ZERO', or 'DUMMY'. These are the only vaild options for this field. Character.}
#'     \item{Amount}{Mass of SAMPLE, STANDARD, QTY, or DUMMY in miligrams (mg). BLANK and ZERO can be 0.0 or blank. Numeric.}
#'     \item{Is Ref _}{Flag that identifies wheter this line line is for a reference gas peak (==1) or not (==0). Numeric.}
#'     \item{Area 28}{Peak area for mass 28 in Vs. Numeric.}
#'     \item{d 15N/14N}{Ratio of 15N to 14N in delta units relative to the reference gas (i.e., N2 tank). Numeric.}
#'     \item{Area 44}{Peak area for mass 44 in Vs. Numeric.}
#'     \item{d 13C/12C}{Ratio of 13C to 12C in delta units relative to the reference gas (i.e., CO2 tank). Numeric.}
#'     }
#'
#' The remaining columns should be unchanged from what is created by IsoDat. There should be a total of 49 columns of data in the raw data file.
#' Column names will be modified when imported by the function.
#'
#' @usage EA.NACHO(data.files, return.mass.percent.CN = T)
#'
#' @param data.files     Character vector that contains raw data file names with file path. If length is >1, then all the files will be combined and analyzed
#'                       together using a single, combined calibration curve.
#' @param return.mass.percent.CN Boolean to indicate whether sample mass data should be return as mass C or N as a percent of total sample mass.
#'                               If FALSE, returned values are total mass of C or N in th sample.
#'
#' @import tidyverse rmarkdown
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

EA.NACHO <- function(data.files, return.mass.percent.CN = T){

results <- list(data.files=data.files,
                processed.data.dir=NA,
                raw.sequence.data=NA,
                return.mass.percent.CN = return.mass.percent.CN,
                zero.flag=NA,
                blank.flag=NA,
                blank.correct.flag=NA,
                drift.correct.flag=NA,
                standard.plots=NA,
                measured.standard.means=NA,
                standard.coefficients=NA,
                known.standard.values=NA,
                calibration.coefficients=NA,
                error.analysis.results=NA,
                standard.CN=NA,
                sample.CN=NA,
                zero.CN=NA,
                blank.CN=NA,
                peak.area.flags=NA,
                run.comments=NA)

    results[c("processed.data.dir", "raw.sequence.data")] <- EA.load.data(results)
    results[c("standard.CN","sample.CN","blank.CN", "zero.flag", "blank.flag")] <- EA.organize(results)
    results[c("standard.CN","sample.CN","blank.correct.flag")] <- EA.blank.correct(results)
    results[c("standard.CN","sample.CN","drift.correct.flag")] <- EA.drift.correct(results)
    results[c("standard.plots","standard.coefficients")] <- EA.plot.standards(results)
    results[c("peak.area.flags")] <- EA.check.peak.areas(results)
    results[c("standard.CN","sample.CN","calibration.coefficients","measured.standard.means")] <- EA.adjust(results)
    results[c("known.standard.values", "error.analysis.results")] <- EA.check(results)
    EA.report(results)

    return(results$sample.CN)
}
