#' @title Organizes raw EA data from NACHO
#'
#' @description This function is used to parse the raw EA data from solid samples analyzed on the Delta V irMS systems (NACHO) for further analysis.
#' The function is part of the workflow for decomposing and standardizing EA data and not intended to be used directly. Data are split into the following categories: blanks, zeros, standards, samples

# The raw sequence data (passed to function via the results list) has strict formatting and data requirements. Each sample (unknown material for analysis), standard (known material), or 'dummy'
# (conditioner than can be a sample or standard this is immediately discarded) should have four (4) rows of data: two for N (masses 28, 29 and 30) and two for C (masses 44, 45, 46).
# Blanks and zeros may have fewer rows. Deviations from this format is will result in the function failing. The first ten columns include critical identification information.
# Key column variables are described below. The remaining columns should be unchanged from what is created by IsoDat.  There should be a total of 49 columns of data in the raw data file.
# Analysis	Identifier 1	Identifier 2	Comment	Amount	Gasconfiguration	Peak Nr	Is Ref _	Rt
#   \describe{
#     \item{Analysis}{Analysis number assigned by IsoDat. Unique and continusous form the insrument. Numeric.}
#     \item{Identifier.1}{First of two decriptions for the analsyis assinged by the user. MUST BE UNIQUE!! This column will identfy Character.}
#     \item{Identifier.2}{Second of two decriptions for the analsyis assinged by the user. By convention we use this column to record the well number the sample/std/duumy/blank came from. Character.}
#     \item{Comment}{Extra place to put useful information. By convention we use this column to record if that drop is a CONDITIONER, DUMMY, SAMPLE, STD, QTY, BLANK or ZERO. Character.}
#     \item{Amount}{Mass of material packed into the tin in milligrams. Entered by user. Blanks should have '0'.  Zeros should have nothing enetred. Numeric.}
#     \item{Gasconfiguration}{Identifies if the cups were set to N2 (massess 28, 29, 30) or CO2 (46, 47, 48) for that peak.  Not used. Character.}
#     \item{Peak.Nr}{Peak number for the run. Should be 1-4, in order, only. The exceptions are blanks and zeros, which should have 2 peaks. Anything other than this is trouble.
#     This is not used but can be useful for identifying missing peaks. Numeric.}
#     \item{Is.Ref._}{Indentifies if a given peak is the reference peak or not. Reference peaks are idenfied as '1'. All others are '0'. Numeric.}
#     \item{Rt}{Retention time of the peak. This is not used but can be useful for identifying missing peaks. Numeric.}
#   }

#' @usage EA.organize(results)
#'
#' @param results List containing results from previous functions.
#'
#' @importFrom stringr str_c str_detect
#' @importFrom tools file_path_sans_ext
#' @importFrom lubridate mdy
#'
#' @return List with four tables: standard C & N data ($standard.CN), sample C & N data ($sample.CN), blank C & N data ($blank.CN).  Blank table may have zero rows.
#' Two boolean flags to indicate if there are detectable zeros and blanks.  Also returns the analysis date at a character.
#'   \describe{
#'     \item{standard.CN}{Dataframe of raw data IsoDat data for the standards and QTY standards.}
#'     \item{sample.CN}{Dataframe of raw data IsoDat data for the unknown samples form the sequence.}
#'     \item{blank.CN}{Dataframe of raw data IsoDat data for any measurable blanks in the sequence.}
#'     \item{zero.CN}{Dataframe of raw data IsoDat data for any measurable zeros in the sequence.}
#'    }
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export


EA.organize <- function(results){

  raw.sequence.data <- results$raw.sequence.data # Load data

  zero.flag = F #Initialize flag
  blank.flag = F #Initialize flag

  analysis.date <- mdy(raw.sequence.data$Date[1]) #Save date of first run in sequence as the "analysis date."

  #Make a column of unique IDs for each analysis in the sequence or sequences (if multiple sequences have been combined)
  raw.sequence.data$unique.ID <- stringr::str_c(raw.sequence.data$Identifier.1, raw.sequence.data$Analysis, sep="_")


  keep.all <- c("Row","Analysis","Identifier.1","Comment", "Amount", "unique.ID")
  keep.N <- c("Area.28", "d.15N.14N")
  keep.C <- c("Area.44", "d.13C.12C")
  refN2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "N2" & raw.sequence.data$Is.Ref._ == 1), c(keep.all, keep.N, keep.C)] #This subsets the reference N2 peaks
  refCO2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "CO2" & raw.sequence.data$Is.Ref._ == 1), c(keep.all, keep.N, keep.C)] #This subsets the reference C02 peaks
  N2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "N2" & raw.sequence.data$Is.Ref._ == 0), c("Analysis", keep.N)] #This is the sample N info
  CO2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "CO2" & raw.sequence.data$Is.Ref._ == 0), c(keep.all, keep.C)] #This is the sample CO2 info
  tempCN <- merge(x = CO2,N2, by = "Analysis", all = T) #N and CO2 sample & standards results are combined into one object

  #Separate zeros from everything else, save as a .csv, and change zero flag to TRUE. If zeros are not detectable the dataframe will be empty and nrow() will return zero (0).
  zero.CN <- tempCN[stringr::str_detect(tempCN$Comment, "ZERO|zero|Zero"),]
  if(nrow(zero.CN)!=0)  {
    zero.flag = T
    zero.CN$Comment[stringr::str_detect(zero.CN$Comment, "ZERO|zero|Zero")] <- "ZERO"
  }

  #Separate blanks (not zeros) from everything else, save as a .csv, and change blank flag to TRUE. If blanks are not detectable the dataframe will be empty and nrow() will return zero (0)
  blank.CN <- tempCN[stringr::str_detect(tempCN$Comment, "BLANK|blank|Blank"),]
  if(nrow(blank.CN)!=0) {
    blank.flag = T
    blank.CN$Comment[stringr::str_detect(blank.CN$Comment, "BLANK|blank|Blank")] <- "BLANK"
  }

  #Separate unknown samples from everything else. Standardize Comments column to SAMPLE
  sample.CN <- tempCN[stringr::str_detect(tempCN$Comment, "SAMPLE|sample|Sample|SAMPLES|samples|Samples"),]
  sample.CN$Comment[stringr::str_detect(sample.CN$Comment, "SAMPLE|sample|Sample|SAMPLES|samples|Samples")] <- "SAMPLE"

  #Separate standards (both regular and QTY) from everything else
  standard.CN <- tempCN[stringr::str_detect(tempCN$Comment, "STANDARD|standard|Standard|STD|std|Std|STANDARDS|Standards|standards|QTY|qty|Qty"),]
  standard.CN$Comment[stringr::str_detect(standard.CN$Comment, "STANDARD|standard|Standard|STD|std|Std|STANDARDS|Standards|standards")] <- "STANDARD"
  standard.CN$Comment[stringr::str_detect(standard.CN$Comment, "QTY|qty|Qty")] <- "QTY"

  #Add empty columns to standard.CN for mass percent C & N and the expected d13C and d15N.  Populated in EA.standards() function.
  standard.CN$group <- NA
  standard.CN$mass.C.mg <- NA
  standard.CN$mass.N.mg <- NA
  standard.CN$known.d13C <- NA
  standard.CN$known.d15N <- NA

  #Record the column names that have the current working mass C, mass N, d13C, and d15. The working column will change as dat are corrected
  #for blanks, drift, etc.  This list helps to tack what has been done.
  current.data.column <- vector(mode="character", length = 4,
                                x=c())
  names (current.data.column) <- c("peak.area.44", "peak.area.28","12C.13C", "14N.15N")

 #Make a list containing standards, samples, and blanks to be returned for easy access by subsequent functions.
  return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.CN=blank.CN, zero.CN=zero.CN,
              zero.flag=zero.flag, blank.flag=blank.flag, analysis.date=analysis.date))
}
