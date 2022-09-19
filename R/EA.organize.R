#' @title Organizes raw EA data from NACHO
#'
#' @description This function is used to parse the raw EA data from solid samples analyzed on the Delta V irMS systems (NACHO) for further analysis.
#' The function is part of the workflow for decomposing and standardizing EA data and not intended to be used directly.

# The raw sequence data (passed to function via the results list) has strict formatting and data requirements. Each sample (unknown material for analysis), standard (known material), or 'dummy'
# (conditioner than can be a sample or standard this is immediately discarded) should have four (4) rows of data: two for N (masses 28, 29 and 30) and two for C (masses 44, 45, 46).
# Blanks and zeros may have fewer rows. Deviations from this format is will result in the function failing. The first ten columns include critical identification information.
# Key column variables are described below. The remaining columns should be unchanged from what is created by IsoDat.  There should be a total of 49 columns of data in the raw data file.
# Analysis	Identifier 1	Identifier 2	Comment	Amount	Gasconfiguration	Peak Nr	Is Ref _	Rt
#   \describe{
#     \item{Analysis}{Analysis number assigned by IsoDat. Unique and continusous form the insrument. Numeric.}
#     \item{Identifier.1}{First of two decriptions for the analsyis assinged by the user. MUST BE UNIQUE!! This column will identfy Character.}
#     \item{Identifier.2}{Second of two decriptions for the analsyis assinged by the user. By convention we use this column to record the well number the sample/std/duumy/blank came from. Character.}
#     \item{Comment}{Extra place to put useful information. by convention we use this column to record if that drop is a DUMMY, SAMPLE, STD, or BLANK. Character.}
#     \item{Amount}{Mass of material packed into the tin in miligrams. Entered by user. Blanks should have '0'.  Zeros should have nothing enetred. Numeric.}
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
#' @import stringr
#' @importFrom tools file_path_sans_ext
#'
#' @return List with three tables: standard C & N data ($standard.CN), sample C & N data ($sample.CN), blank C & N data ($blank.CN).  Blank table may have zero rows.
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#' @export


EA.organize <- function(results){

  raw.sequence.data <- results$raw.sequence.data
  zero.flag = F
  blank.flag = F

  raw.sequence.data$unique.ID <- str_c(raw.sequence.data$Identifier.1, raw.sequence.data$Analysis, sep="_")
  keep.all <- c("Row","Analysis","Identifier.1","Comment", "Amount", "unique.ID")
  keep.N <- c("Area.28", "d.15N.14N")
  keep.C <- c("Area.44", "d.13C.12C")
  refN2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "N2" & raw.sequence.data$Is.Ref._ == 1), c(keep.all, keep.N, keep.C)] #This subsets the reference N2 peaks
  refCO2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "CO2" & raw.sequence.data$Is.Ref._ == 1), c(keep.all, keep.N, keep.C)] #This subsets the reference C02 peaks
  N2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "N2" & raw.sequence.data$Is.Ref._ == 0), c("Analysis", keep.N)] #This is the sample N info
  CO2 <- raw.sequence.data[which(raw.sequence.data$Gasconfiguration == "CO2" & raw.sequence.data$Is.Ref._ == 0), c(keep.all, keep.C)] #This is the sample CO2 info
  tempCN <- merge(x = CO2,N2, by = "Analysis") #N and CO2 sample & standards results are combined into one object
  rm(N2, CO2)

  #Separate zeros from everything else and save as a .csv. If zeros are not detectable the dataframe will be empty and nrow() will return zero.
  zero.CN <- tempCN[str_detect(tempCN$Comment, "ZERO|zero|Zero"),]
  if(nrow(blank.CN)!=0)  zero.flag = T

  #Separate blanks (not zeros) from everything else and save as a .csv. If blanks are not detectable the dataframe will be empty and nrow() will return zero
  blank.CN <- tempCN[str_detect(tempCN$Comment, "BLANK|blank|Blank"),]
  if(nrow(blank.CN)!=0)  blank.flag = T

  #Separate unknown samples from everything else and save as a .csv.
  sample.CN <- tempCN[str_detect(tempCN$Comment, "SAMPLE|SAMPLES|sample|Sample"),]
  #write.csv(x = sample.CN, file = paste0("sample.CN_",sequenceID,".csv"), row.names = FALSE)

  #Separate standards (both regular and QTY) from everything else
  standard.CN <- tempCN[str_detect(tempCN$Comment, "STANDARD|standard|Standard|STD|std|Std|QTY|qty|Qty"),]

  #Add expected mass C and N in standards based on amount (mass) and known composition of the standards.
  GA.Amt.N <- 0.0952 #proportion N by mass
  GA.Amt.C <- 0.408168 #proportion C by mass
  SALMON.Amt.N <- 0.1183 #proportion N by mass
  SALMON.Amt.C <- 0.457 #proportion C by mass

  #Start with salmon standard
  index <- str_detect(standard.CN$Identifier.1, "sal|SAL|Sal|Salmon|salmon|SALMON")
  standard.CN$mass.N.mg[index] <- standard.CN$Amount[index] * SALMON.Amt.N
  standard.CN$mass.C.mg[index] <- standard.CN$Amount[index] * SALMON.Amt.C
  standard.CN$group[index] <- "SALMON"

  #GA1 standard (same mass for both GA1 and GA2)
  index <- str_detect(standard.CN$Identifier.1, "GA1|ga1|Ga1")
  standard.CN$mass.N.mg[index] <- standard.CN$Amount[index] * GA.Amt.N
  standard.CN$mass.C.mg[index] <- standard.CN$Amount[index] * GA.Amt.C
  standard.CN$group[index] <- "GA1"

  #GA2 standard (same mass for both GA1 and GA2)
  index <- str_detect(standard.CN$Identifier.1, "GA2|ga2|Ga2")
  standard.CN$mass.N.mg[index] <- standard.CN$Amount[index] * GA.Amt.N
  standard.CN$mass.C.mg[index] <- standard.CN$Amount[index] * GA.Amt.C
  standard.CN$group[index] <- "GA2"

 #Make a list containing standards, samples, and blanks to be returned for easy access by subsequent functions.
  return(list(standard.CN=standard.CN, sample.CN=sample.CN, blank.CN=blank.CN, zero.flag=zero.flag, blank.flag=blank.flag))
}