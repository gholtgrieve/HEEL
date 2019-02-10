#'Dataset containing isotope standards and their accepted values
#'
#'Dataframe containing information on isotope reference materials. Includes accepted
#'primary standards (e.g., VSNOW, VPDB), accepted international standards (e.g.,
#'USGS 40, NBS 19), and working standards from the University of Washington HEEL
#'and IsoLab (e.g., GA1, KD).
#'
#' @docType data
#' @usage data(isotope.standards)
#' @format A dataframe with 53 rows and 21 variables:
#' \describe{
#'   \item{Name}{Primary identifier for the isotope standard. Character string.}
#'   \item{InternationalSTD}{Identifies as accepted international standard material. Boolean.}
#'   \item{WorkingSTD}{Identifies as working standard for the HEEL. Boolean.}
#'   \item{Method}{Identifies analytical method the standard is used for.  Options are
#'                 AA for amino acid, EA for elemental analysis, FA for fatty acid, NO3 for
#'                 nitrate, H2O for water.  Numeric.}
#'   \item{d15N_vsAir}{15N:14N relative to atmospheric air in standard delta notation.  Numeric.}
#'   \item{R_15N14N}{Ratio of 15N to 14N. Numeric.}
#'   \item{massPctN}{Mass percent nitrogen. Numeric}
#'   \item{d13C_vsVPDB}{13C:12C relative to VPDB in standard delta notation.  Numeric.}
#'   \item{R_13C12C}{Ratio of 13C to 12C. Numeric.}
#'   \item{massPctC}{Mass percent carbon. Numeric}
#'   \item{d34S_vsVCDT}{34S:32S relative to VCDT in standard delta notation.  Numeric.}
#'   \item{R_34S32S}{Ratio of 34S to 32S. Numeric.}
#'   \item{massPctS}{Mass percent sulfur. Numeric}
#'   \item{d18O_vsVSMOW}{18O:16O relative to VSMOW in standard delta notation.  Numeric.}
#'   \item{R_18O16O}{Ratio of 18O to 16O. Numeric.}
#'   \item{d17O_vsVSMOW}{17O:16O relative to VSMOW in standard delta notation.  Numeric.}
#'   \item{R_17O16O}{Ratio of 17O to 16O. Numeric.}
#'   \item{massPctO}{Mass percent oxygen. Numeric}
#'   \item{dD_vsVSMOW}{D:H (2H:1H) relative to VSMOW in standard delta notation.  Numeric.}
#'   \item{R_DH}{Ratio of D to H (2H to 1H). Numeric.}
#'   \item{massPctH}{Mass percent hydrogenn. Numeric.}
#' }
"isotope.standards"
