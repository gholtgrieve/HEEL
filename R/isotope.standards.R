#'Dataset containing isotope standards and their accepted values
#'
#'Dataframe containing information on isotope reference materials organized as a
#'ragged array. Includes accepted primary standards (e.g., VSNOW, VPDB), accepted
#'international standards (e.g., USGS 40, NBS 19), and working standards from the
#'University of Washington HEEL and IsoLab (e.g., GA1, KD).
#'
#' @docType data
#' @usage data(isotope.standards)
#' @format A dataframe with 179 rows and 3 variables:
#' \describe{
#'   \item{Name}{Primary identifier for the isotope standard. Character string.}
#'   \item{dataType}{Identifies the data type in the Value column. Character string.}
#'   \item{Value}{Identifies as working standard for the HEEL. Numeric.}
#' }
"isotope.standards"

