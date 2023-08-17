#' @title Determines relevant standards for the run.
#'
#' @description This internal function determines the standards used in a run and reports that back to the user + saves for later use.
#' It also calculates the expected mass C and N of the standards based on the based on measured weight (amount) and known percent C or N.
#'
#' @usage EA.standards(results)
#'
#' @param results List containing XXX
#'
#' @return List with X objects...
#'
#' @author Gordon W. Holtgrieve
#'
#' @keywords internal
#'
#' @export

EA.standards <- function(results){

  ## Access data ##
  standard.CN <- results$standard.CN

  known.standard.values <- data.frame(group = c("GA1", "GA2", "SALMON", "PL"),
                                      d13C_VPDB = c(-28.3, -13.7, -21.3, -25.99),
                                      d15N_air = c(-4.6, -5.7, 11.3, 1.98),
                                      mass.percent.C = c(40.8168, 40.8168, 45.7, 45.5),
                                      mass.percent.N = c(9.52, 9.52, 11.83, 2.965))

  #Identify which standards are in the run.
  index <- stringr::str_detect(standard.CN$Identifier.1, "sal|SAL|Sal|Salmon|salmon|SALMON")
  standard.CN$group[index] <- "SALMON"

  index <- stringr::str_detect(standard.CN$Identifier.1, "GA1|ga1|Ga1")
  standard.CN$group[index] <- "GA1"

  index <- stringr::str_detect(standard.CN$Identifier.1, "GA2|ga2|Ga2")
  standard.CN$group[index] <- "GA2"

  index <- stringr::str_detect(standard.CN$Identifier.1, "PL|pl|PeachLeaves")
  standard.CN$group[index] <- "PL"

  standards.in.run <- unique(standard.CN$group)
  print("The following reference standards have been identified:")
  print(standards.in.run)

  #Add expected mass C and N of standards based on measured weight (amount) and known percent C or N
    for (i in 1:length(standards.in.run)){
      STD.values <- known.standard.values[standards.in.run[i],]
      index <- standard.CN$group == standards.in.run[i]

      standard.CN$mass.C.mg[index] <- standard.CN$Amount[index] * STD.values[1,3]/100
      standard.CN$mass.N.mg[index] <- standard.CN$Amount[index] * STD.values[1,4]/100
      standard.CN$known.d13C[index] <- STD.values[1,1]
      standard.CN$known.d15N[index] <- STD.values[1,2]
    }

  return(list(standard.CN=standard.CN, known.standard.values=known.standard.values, standards.in.run=standards.in.run))

  }
