#' @title Finalizes extetainer gas ratio data from NACHO
#'
#' @description This function is used to decompose and finalize gas ratio data from exetainers analyzed on the Delta V irMS systems (NACHO).  The first step
#' is thin and organige the raw data.  Plots are generated (as .pdfs) to look for mass effects on the results ansd the user is asked whether to correct for
#' sample mass differences among injections and samples.  After corrections, measured values are calibrated against the working standards included
#' in the run.  The function writes a .csv with the thinned raw data and a .txt with all relevant information about the analysis.  The finalized
#' sample data are writen as a .csv file and a dataframe of the data is also returned.
#'
#' The input data file has strick formatting and data requirments.  Each sample or standard should have seven rows of data: 3 for massess 32, 34
#' and 40; 2 for masses 28 and 29, and 2 for masses 44, 45, 46.  Deviations for the is will result in the function failing.  In additon the first four columns
#' must be the following four identification columns in this order:
#'   \describe{
#'     \item{analysisDate}{Data of the run(s) on NACHO. Character.}
#'     \item{Identifier.1}{Unique identifier for each exetainer in the file. If two runs are combined, be sure this field remains unique. Character.}
#'     \item{Identifier.2}{Mililiters of air added to air standards OR volume of headspace in the sample or water standard vial. Headspace volume is the difference
#'           of the inital, full vial weigth and weight after generating the headspace. Numeric.}
#'     \item{Comment}{Identifies whether the extetainer is a 'airSTD', 'waterSTD', 'Sample' or 'Conditioner'. These are the only vaild options for this field. Character.}
#'   }
#' The remaining columns should be unchaged from what is created by IsoDat.  There should be a total of 45 columsn of data in the raw data file.
#'
#' @usage exetainers.NACHO(data.file, analysis.date, area.cutoff = F, injections.standards = c(F, T, T, T), injections.samples = c(F, T, T, T), lab.air.T = 20, system.pressure.atm = 1.49701, salinity = 0)
#'
#' @param data.file     Full file name for raw data from the instrument. Must be a .csv file! Character.
#' @param area.cutoff   Defines the peak area (Vs) cutoff below which an individual analysis (injection) is droped.  FALSE means no cutoff applied.  Numeric defines the cutoff value.
#' @param injections.standards  Boolean vector length 4 idenitfying which injections of the standards to drop (F) or keep (T) for use in the calculations.
#' @param injections.samples    Boolean vector length 4 idenitfying which injections of the samples to drop (F) or keep (T) for use in the calculations.
#' @param lab.air.T     Temperature of the mass spec lab at time of analysis in degrees Celcius.  Defaults to 20.  Numeric.
#' @param system.pressure.atm   Pressure of the inlet system in atmospheres.  Defaults to 1.49701 (22 psi).  Numeric.
#' @param salinity      Salninty of the sample in ppt.  Defaults to 0.  Numeric.
#'
#' @return Dataframe of finalized data for each exeatiner including:
#'   \describe{
#'     \item{d180_02.vs.air}{18O:16O of diatomic oxygen (O2) in the headspace in standard delta notation relative to atmospheric air.}
#'     \item{d180_02.vs.VSMOW}{18O:16O of diatomic oxygen (O2) in the headspace in standard delta notation relative to VSMOW.}
#'     \item{d170_02.vs.air}{17O:16O of diatomic oxygen (O2) in the headspace in standard delta notation relative to atmospheric air.
#'           Use with caution as this method has not been validated.}
#'     \item{d15N_N2.vs.air}{15N:14N of diatomic nitrogen (N2) in the headspace in standard delta notation relative to atmospheric air.
#'           Use with caution as this method has not been validated.}
#'     \item{O2.Ar}{Oxygen to argon ratio (mass 32:40) of headspace gas.}
#'     \item{d13C.CO2}{13C:12C of carbon dioxide (CO2) in the headspace (after acidificatioon) in standard delta notation relative to VPDB.}
#'     \item{d13C.TotalDIC}{13C:12C of dissolved inroganic carbon in standard delta notation relative to VPDB calculated from the headspace
#'           CO2 and using Henry's constant to estimate the 13C:12C of CO2 that raims dissoved in water.  Note that it may be necessary to
#'           further correct these data based on co-analyzed DIC standards.}
#'    }
#' @author Gordon W. Holtgrieve
#' @export
#' @import tidyverse
#' @importFrom tools file_path_sans_ext

exetainers.NACHO <- function (data.file, area.cutoff = F, injections.standards = c(F, T, T, T),
                              injections.samples = c(F, T, T, T), lab.air.T = 20,
                              system.pressure.atm = 1.49701, salinity = 0){

  require(tidyverse)

  if(is.numeric(area.cutoff)){
    areaCutoff <- area.cutoff
    flagUseAreaCutoff <- T
  } else if (area.cutoff == F){
    flagUseAreaCutoff <- F
    areaCutoff <- 10
  } else stop("Parameter 'area.cutoff' must be FALSE or numeric.")

  # Standard values
  bottle.d13C <- -37.25     # Known bottle standard d13C-DIC value, value last updated 08 Dec 2018
  air.d18O.vsSMOW <- get.isotope.standard(std = "air", isotope.system = "O18")$delta  # returns 23.8
  air.d17O.vsSMOW <- NA     # Reference here
  air.d15N.vsAir <- get.isotope.standard(std = "air", isotope.system = "N")$delta  #returns 0
  air.32O2.40Ar <- 22.426   # Reference here

  #Establish a vector of column names
  colNames <- c("analysisDate", "Identifier.1", "Identifier.2", "Comment", "Method", "Amount", "Analysis", "Line", "Gasconfiguration", "Peak.Nr",
                "IsRef_", "Rt", "Width", "BGD.32", "BGD.33", "BGD.34", "BGD.40", "BGD.28", "BGD.29", "BGD.44", "BGD.45", "Ampl.32", "Ampl.33", "Ampl.34",
                "Ampl.40", "Ampl.28", "Ampl.29", "Ampl.44", "Ampl.45", "Area.32", "Area.40", "Area.28", "Area.44", "R.33O2.32O2", "R.34O2.32O2",
                "R.32O2.40Ar", "R.29N2.28N2", "R.45CO2.44CO2", "d.33O2.32O2", "d.34O2.32O2", "d.32O2.40Ar", "d.29N2.28N2", "d.15N.14N", "d.45CO2.44CO2", "d.13C.12C")

  #############################################################################
           ####Functions####
  #############################################################################
  #Function Convert_STDs
  Convert_STDs <- function(sample, std){
    temp <- (((1000+sample)/(1000+std))-1)*1000
    return(temp)
  }

  PowerFunction <- function(x,y, par){
    a <- par[1]
    b <- par[2]
    c <- par[3]
    sigma <- par[4]

    y_hat <- a - b * I(x ^ c)

    return(-sum(dnorm(y,mean=y_hat,sd=sigma,log=T)))
  }

  PlotByX <- function(x, y, colorVector, xlab, ylab, inset, mean, legendBool, xlim){
    group <- sort(unique(as.numeric(colorVector)))
    n <- length(group)
    cols <- rainbow(n)
    plot(x, y, xlim = xlim, cex=1.5, xlab=xlab, ylab=ylab, type="n")
    for (i in 1:n){
      temp <- data.frame(x[colorVector==group[i]], y[colorVector==group[i]])
      points(temp, col=cols[i], pch=19, cex=1.5)
    }
    abline(h=mean, lwd=2, lty=2)
    if(legendBool) legend("bottomright", paste(group, "mL of deck air"), pch=19, col=cols, bty="n", inset=inset)
  }

  findPvalue <- function (model){
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    return(as.numeric(p))
  }


  calc.d13C.DIC <- function (d13C.CO2, Vg, lab.air.T, system.pressure.atm){
    # Calculates d13C of total DIC from headspace d13C-CO2 following modified method of
    # Assayag et al. Rapid Commun. Mass Spectrom. 2006; 20: 2243–2251. Modified to
    # to calculate Henery's constant as a function of temperature, pressure,, and salinity.
    # Uses following equation: d13-DIC = d13C-CO2 - alpha / (1 + Vg/(Vl*R*T*KH))
    # Vg: Volume gas (headspace)
    # Vl: Volume liquid
    # alpha: Equilibrum fractionation factor in per mil
    # R: Ideal gas constant with units of L atm mol-1 K-1
    # T: Temperture in Kelvin
    # KH: Constant based on Henry's law, units of mol L-1 atm-1

    alpha = 1.07  # Gas-liquid equilibrium fractionaton factor for CO2 (in per mil)
    R = 0.0820568    # Ideal gas constant with units of L atm mol-1 K-1
    exetainerVol_mL <- 12.095 #Based on direct measurment (by weight) of 12 replicate Exetainers.  12.095 mL +/- 0.102
    Vl <- exetainerVol_mL - Vg
    T_K <- 273.15 + lab.air.T

    KH <- get.K0.CO2(temperature = lab.air.T, salinity = salinity, pressure.atm = system.pressure.atm)

    d13.DIC <- d13C.CO2 - alpha / (1 + (Vg / (Vl* R * T_K * KH)))

    return(d13.DIC)
  }

  #############################################################################
           ####Main script#####
  #############################################################################
  #############################################################################
           ####Load and index data####
  #############################################################################
  #Load raw data file
  rawData <- read_csv(data.file)
  names(rawData) <- colNames

  #Determine number of samples and standards in this run
  exetainers <- unique(rawData[,c("Identifier.1","Identifier.2","Comment", "analysisDate")])
  nTotal <- length(exetainers$Identifier.1)
  nSamples <- sum(exetainers$Comment=="Sample")
  nStandards <- sum(exetainers$Comment=="airSTD")
  nConditioners <- sum(exetainers$Comment=="Conditioner")
  nWaterStandards <- sum(exetainers$Comment=="waterSTD")
  print(nSamples + nStandards + nConditioners)   #Note: nTotal = nSamples + nStandards + nConditioners
  nRuns <- length(unique(rawData$analysisDate))

  #############################################################################
           ####Thin data to what is needed for calculations####
  #############################################################################
  #separate airSTD volumes (Conditioners|Standards) VS headspace volumes (WaterSTDs|Samples)
  exetainers$deckAir_mL <- exetainers$Vg <- NA

  index <- exetainers$Comment=="Conditioner" | exetainers$Comment=="airSTD"
  exetainers$deckAir_mL[index] <- exetainers$Identifier.2[index]

  index <- exetainers$Comment=="waterSTD" | exetainers$Comment=="Sample"
  exetainers$Vg[index] <- exetainers$Identifier.2[index]

  #Loop through each sample checking the data and compiling the important results into a new data frame
  #Set up blank data frame to store the key data for analysis
  thinnedData <- data.frame(Identifier.1 = rep(exetainers$Identifier.1, each=4),
                            Identifier.2 = rep(exetainers$Identifier.2, each=4),
                            Comment = rep(exetainers$Comment, each=4),
                            Injection = 1:4,
                            Vg = rep(exetainers$Vg, each=4),
                            deckAir_mL = rep(exetainers$deckAir_mL, each=4),
                            analysisDate = rep(exetainers$analysisDate, each=4),
                            R.28.40=NA,
                            Area.32=NA,
                            d.34O2.32O2=NA,
                            d.33O2.32O2=NA,
                            Area.40=NA,
                            d.32O2.40Ar=NA,
                            Area.28=NA,
                            d.15N.14N=NA,
                            Area.44=NA,
                            d.13C.12C=NA,
                            Small.Sample.Mass=NA
                            )

  j=1

  for (i in 1:nTotal){
    print(paste("The current sample is", exetainers$Identifier.1[i], "which is a", exetainers$Comment[i]))
    #Pull out the data for one sample
    oneSample <- rawData[which(rawData$Identifier.1==exetainers$Identifier.1[i]),]

    #Find indexes for line that contains the various pieves of data from each of the 4 injections
    OAr.index <- which(oneSample$Peak.Nr==3) #for masses 32,34,40
    N2.index <- which(oneSample$Peak.Nr==4) #for masses 28, 29, 30
    CO2.index <- which(oneSample$Peak.Nr==6) #for masses 44, 45, 46

    #Now compile the following values into the summary data frame
    #   R 28/40 (ampl 28/ ampl 40)
    #   rd 34O2/32O2
    #   rd 32O2/40Ar
    #   d 13C/12C
    thinnedData$analysisDate[j:(j+3)] <- as.character(oneSample$analysisDate[1])
    thinnedData$R.28.40[j:(j+3)] <- oneSample$Area.40[OAr.index]/oneSample$Area.28[N2.index]
    thinnedData$Area.32[j:(j+3)] <- oneSample$Area.32[OAr.index]
    thinnedData$d.34O2.32O2[j:(j+3)] <- oneSample$d.34O2.32O2[OAr.index]
    thinnedData$d.33O2.32O2[j:(j+3)] <- oneSample$d.33O2.32O2[OAr.index]
    thinnedData$Area.40[j:(j+3)] <- oneSample$Area.40[OAr.index]
    thinnedData$d.32O2.40Ar[j:(j+3)] <- oneSample$d.32O2.40Ar[OAr.index]
    thinnedData$Area.28[j:(j+3)] <- oneSample$Area.28[N2.index]
    thinnedData$d.15N.14N[j:(j+3)] <- oneSample$d.15N.14N[N2.index]
    thinnedData$Area.44[j:(j+3)] <- oneSample$Area.44[CO2.index]
    thinnedData$d.13C.12C[j:(j+3)] <- oneSample$d.13C.12C[CO2.index]
    thinnedData$Small.Sample.Mass[j:(j+3)] <- oneSample$Area.32[OAr.index] <= areaCutoff

    j <- j+4
  }

  write.csv(thinnedData, paste(tools::file_path_sans_ext(data.file),"_thinned.csv", sep=""))


  #############################################################################
  ####Drop conditioners, delete first injection and split into samples and standards.#####
  #############################################################################

  if (flagUseAreaCutoff){
    temp <- thinnedData[thinnedData$Identifier.1=="airSTD",]
    index <- list(temp$Identifier.2[temp$keep], temp$analysisDate[temp$keep])
    airSTDs <- aggregate(temp[temp$keep,-c(1:2,4:5)],by=index,mean, na.rm=T)

  } else {
    #Compile the air standards, thin to only the selected injections (defined above), and average
    std.parameters <- c("deckAir_mL", "R.28.40", "Area.32", "d.34O2.32O2", "d.33O2.32O2", "Area.40",
                    "d.32O2.40Ar", "Area.28", "d.15N.14N", "Area.44", "d.13C.12C")
    temp <- thinnedData[thinnedData$Comment=="airSTD",]
    index <- list(temp$Identifier.1[injections.standards])
    airSTDs <- aggregate(temp[injections.standards, std.parameters],by=index,mean, na.rm=T)
    rm(temp)

    sample.parameters <- c("Vg", "R.28.40", "Area.32", "d.34O2.32O2", "d.33O2.32O2", "Area.40",
                    "d.32O2.40Ar", "Area.28", "d.15N.14N", "Area.44", "d.13C.12C")
    temp <- thinnedData[thinnedData$Comment=="Sample",]
    index <- list(temp$Identifier.1[injections.samples])
    samples <- aggregate(temp[injections.standards,sample.parameters],by=index,mean, na.rm=T)
    rm(temp)
  }

  #############################################################################
        ####Make corrections for sample mass and save output to use later if desired####
  #############################################################################
  # d18O-O2 data
    airSTDs_mean_d.34O2.32O2 <- mean(airSTDs$d.34O2.32O2, na.rm=T)
    residuals_d.34O2.32O2 <- airSTDs$d.34O2.32O2 - airSTDs_mean_d.34O2.32O2
    lmOut_d.34O2.32O2 <- lm(residuals_d.34O2.32O2 ~ airSTDs$Area.32)

  # d17O-O2 data
    airSTDs_mean_d.33O2.32O2 <- mean(airSTDs$d.33O2.32O2, na.rm=T)
    residuals_d.33O2.32O2 <- airSTDs$d.33O2.32O2 - airSTDs_mean_d.33O2.32O2
    lmOut_d.33O2.32O2 <- lm(residuals_d.33O2.32O2 ~ airSTDs$Area.32)

  # O2:Ar data
    airSTDs_mean_d.32O2.40Ar <- mean(airSTDs$d.32O2.40Ar, na.rm=T)
    residuals_d.32O2.40Ar <- airSTDs$d.32O2.40Ar - airSTDs_mean_d.32O2.40Ar
    tempData <- data.frame(x= airSTDs$Area.32, y=residuals_d.32O2.40Ar)
    flag1 <- FALSE
    flag1 <- inherits(try(modelOut_d.32O2.40Ar <- nls(y ~ c + a*tanh(b*x/a), data=tempData, start=list(a=100, b=10, c=-20)), silent = T), "try-error")
    if(flag1){
      modelOut_d.32O2.40Ar <- lm(y ~ x, data = tempData)
    }

  # d15N-N2 data
    airSTDs_mean_d.15N.14N <- mean(airSTDs$d.15N.14N, na.rm=T)
    residuals_d.15N.14N <- airSTDs$d.15N.14N - airSTDs_mean_d.15N.14N
    lmOut_d.15N.14N <- lm(residuals_d.15N.14N ~ airSTDs$Area.28)

  # d13C-CO2 data
    airSTDs_mean_d.13C.12C <- mean(airSTDs$d.13C.12C, na.rm=T)
    residuals_d.13C.12C <- airSTDs$d.13C.12C - airSTDs_mean_d.13C.12C
    lmOut_d.13C.12C <- lm(residuals_d.13C.12C ~ airSTDs$Area.44)


  #############################################################################
           ####Plot air standards in a .pdf for evaluation#####
  #############################################################################

  pdf(file=paste(tools::file_path_sans_ext(data.file),"_plots.pdf"), width=10, height=8)
  oldPar <- par(mfrow=c(2,2), mar=c(5,5,2,2))

  ####Plot area 32 vs. d18O-O2 and area 32 vs. d18O-O2 residuals
  PlotByX(x=airSTDs$Area.32, y=airSTDs$d.34O2.32O2, colorVector=airSTDs$deckAir_mL, xlab="Area mass 32 (Vs)", ylab="d18O-O2 (per mil vs. working standard)",
          inset=c(0.07,0), mean=airSTDs_mean_d.34O2.32O2, legendBool=T,xlim=c(min(c(samples$Area.32,airSTDs$Area.32)),max(c(samples$Area.32,airSTDs$Area.32))))
  rug(samples$Area.32,lwd=2,col=c("red"))
  PlotByX(x=airSTDs$Area.32, y=residuals_d.34O2.32O2, colorVector=airSTDs$deckAir_mL, xlab="Area mass 32 (Vs)", ylab="residuals of d18O-O2",
          inset=c(0.07,0), mean=airSTDs_mean_d.34O2.32O2, legendBool=F,xlim=c(min(c(samples$Area.32,airSTDs$Area.32)),max(c(samples$Area.32,airSTDs$Area.32))))
  rug(samples$Area.32,lwd=2,col=c("red"))
  lines(airSTDs$Area.32, fitted(lmOut_d.34O2.32O2), lwd=2)
  pValue = findPvalue (lmOut_d.34O2.32O2)
  legend('topright',legend = paste("p = ",pValue), bty="n",pch=NA)

  ####Plot area 32 vs. d17O-O2
  PlotByX(x=airSTDs$Area.32, y=airSTDs$d.33O2.32O2, colorVector=airSTDs$deckAir_mL, xlab="Area mass 32 (Vs)", ylab="d17O-O2 (per mil vs. working standard)",
          inset=c(0.07,0), mean=airSTDs_mean_d.33O2.32O2, legendBool=F,xlim=c(min(c(samples$Area.32,airSTDs$Area.32)),max(c(samples$Area.32,airSTDs$Area.32))))
  rug(samples$Area.32,lwd=2,col=c("red"))
  PlotByX(x=airSTDs$Area.32, y=residuals_d.33O2.32O2, colorVector=airSTDs$deckAir_mL, xlab="Area mass 32 (Vs)", ylab="residuals of d17O-O2",
          inset=c(0.07,0), mean=airSTDs_mean_d.33O2.32O2, legendBool=F,xlim=c(min(c(samples$Area.32,airSTDs$Area.32)),max(c(samples$Area.32,airSTDs$Area.32))))
  rug(samples$Area.32,lwd=2,col=c("red"))
  lines(airSTDs$Area.32, fitted(lmOut_d.33O2.32O2), lwd=2)
  pValue = findPvalue (lmOut_d.33O2.32O2)
  legend('topright',legend = paste("p = ",pValue), bty="n",pch=NA)

  ####Plot area 32 vs. 32:40
  PlotByX(x=airSTDs$Area.32, y=airSTDs$d.32O2.40Ar, colorVector=airSTDs$deckAir_mL, xlab="Area mass 32 (Vs)", ylab="R 32:40 (per mil vs. working standard)",
          inset=c(0.07,0), mean=airSTDs_mean_d.32O2.40Ar, legendBool=T,xlim=c(min(c(samples$Area.32,airSTDs$Area.32)),max(c(samples$Area.32,airSTDs$Area.32))))
  rug(samples$Area.32,lwd=2,col=c("red"))
  PlotByX(x=airSTDs$Area.32, y=residuals_d.32O2.40Ar, colorVector=airSTDs$deckAir_mL, xlab="Area mass 32 (Vs)", ylab="residuals of R 32:40",
          inset=c(0.07,0), mean=airSTDs_mean_d.32O2.40Ar, legendBool=F,xlim=c(min(c(samples$Area.32,airSTDs$Area.32)),max(c(samples$Area.32,airSTDs$Area.32))))
  tempData <- data.frame(x=airSTDs$Area.32, y=fitted(modelOut_d.32O2.40Ar))
  lines(tempData[order(tempData$x),], lwd=2)
  rug(samples$Area.32,lwd=2,col=c("red"))


  ####Plot area 28 vs. d15N-N2
  PlotByX(x=airSTDs$Area.28, y=airSTDs$d.15N.14N, colorVector=airSTDs$deckAir_mL, xlab="Area mass 28 (Vs)", ylab="d15N-N2 (per mil vs. working standard)",
          inset=c(0.07,0), mean=airSTDs_mean_d.15N.14N, legendBool=F,xlim=c(min(c(samples$Area.28,airSTDs$Area.28)),max(c(samples$Area.28,airSTDs$Area.28))))
  rug(samples$Area.28,lwd=2,col=c("red"))
  PlotByX(x=airSTDs$Area.28, y=residuals_d.15N.14N, colorVector=airSTDs$deckAir_mL, xlab="Area mass 28 (Vs)", ylab="residuals of d15N-N2",
          inset=c(0.07,0), mean=airSTDs_mean_d.15N.14N, legendBool=F,xlim=c(min(c(samples$Area.28,airSTDs$Area.28)),max(c(samples$Area.28,airSTDs$Area.28))))
  rug(samples$Area.28,lwd=2,col=c("red"))
  lines(airSTDs$Area.28, fitted(lmOut_d.15N.14N), lwd=2)
  pValue = findPvalue (lmOut_d.15N.14N)
  legend('topright',legend = paste("p = ",pValue), bty="n",pch=NA)


  ####Plot area 44 vs. d13C-CO2
  PlotByX(x=airSTDs$Area.44, y=airSTDs$d.13C.12C, colorVector=airSTDs$deckAir_mL, xlab="Area mass 44 (Vs)", ylab="d13C-CO2 (per mil vs. working standard)",
          inset=c(0.07,0), mean=airSTDs_mean_d.13C.12C, legendBool=T,xlim=c(min(c(samples$Area.44,airSTDs$Area.44),na.rm=T),max(c(samples$Area.44,airSTDs$Area.44),na.rm=T)))
  rug(samples$Area.44,lwd=2,col=c("red"))
  PlotByX(x=airSTDs$Area.44, y=residuals_d.13C.12C, colorVector=airSTDs$deckAir_mL, xlab="Area mass 44 (Vs)", ylab="residuals of d13C-CO2",
          inset=c(0.07,0), mean=airSTDs_mean_d.13C.12C, legendBool=F,xlim=c(min(c(samples$Area.44,airSTDs$Area.44),na.rm=T),max(c(samples$Area.44,airSTDs$Area.44),na.rm=T)))
  rug(samples$Area.44,lwd=2,col=c("red"))
  lines(airSTDs$Area.44, fitted(lmOut_d.13C.12C), lwd=2)
  pValue = findPvalue (lmOut_d.13C.12C)
  legend('topright',legend = paste("p = ",pValue), bty="n",pch=NA)

  dev.off()



  #############################################################################
       ####Make corrections for sample mass, asking user####
  #############################################################################
  # d18O-O2 data
  flag_d.34O2.32O2 <- as.logical(readline("Do you want to correct 34O2:32O2 for sample mass (area 32)?  Enter T/F."))
  if(flag_d.34O2.32O2){
    residuals_d.34O2.32O2 <- airSTDs$d.34O2.32O2 - airSTDs_mean_d.34O2.32O2
    lmOut_d.34O2.32O2 <- lm(residuals_d.34O2.32O2 ~ airSTDs$Area.32)

    offset <- samples$Area.32 * coefficients(lmOut_d.34O2.32O2)[2] + coefficients(lmOut_d.34O2.32O2)[1]
    samples$d.34O2.32O2_corr <- samples$d.34O2.32O2 - offset
  }

  # d17O-O2 data
  flag_d.33O2.32O2 <- as.logical(readline("Do you want to correct 33O2:32O2 for sample mass (area 32)?  Enter T/F."))
  if(flag_d.33O2.32O2){
    offset <- samples$Area.32 * coefficients(lmOut_d.33O2.32O2)[2] + coefficients(lmOut_d.33O2.32O2)[1]
    samples$d.33O2.32O2_corr <- samples$d.33O2.32O2 - offset
  }

  # O2:Ar data
  flag_d.32O2.40Ar <- as.logical(readline("Do you want to correct 32O2:40Ar for sample mass (area 32)?  Enter T/F."))
  if(flag_d.32O2.40Ar){
    if(flag1){
      offset <- samples$Area.32 * coefficients(modelOut_d.32O2.40Ar)[2] + coefficients(modelOut_d.32O2.40Ar)[1]
    } else{
      offset <- coefficients(modelOut_d.32O2.40Ar)[3] + coefficients(modelOut_d.32O2.40Ar)[1] * tanh(coefficients(modelOut_d.32O2.40Ar)[2] * samples$Area.32 / coefficients(modelOut_d.32O2.40Ar)[1])
    }
      samples$d.32O2.40Ar_corr <- samples$d.32O2.40Ar - offset
  }

  # d15N-N2 data
  flag_d.15N.14N <- as.logical(readline("Do you want to correct 15N2:14N2 for sample mass (area 28)?  Enter T/F."))
  if(flag_d.15N.14N){
    offset <- samples$Area.28 * coefficients(lmOut_d.15N.14N)[2] + coefficients(lmOut_d.15N.14N)[1]
    samples$d.15N.14N_corr <- samples$d.15N.14N - offset
  }

  # d13C-CO2 data
  flag_d.13C.12C <- as.logical(readline("Do you want to correct 13C:12C for sample mass (area 44)?  Enter T/F."))
  if(flag_d.13C.12C){
    offset <- samples$Area.44 * coefficients(lmOut_d.13C.12C)[2] + coefficients(lmOut_d.13C.12C)[1]
    samples$d.13C.12C_corr <- samples$d.13C.12C - offset
  }


  # #############################################################################
  ####Convert corrected values to relative to accepted international standards####
  # #############################################################################

  #d18O-O2
  if(flag_d.34O2.32O2){
    samples$d180_02.vs.air <- Convert_STDs(samples$d.34O2.32O2_corr, airSTDs_mean_d.34O2.32O2)
    workingSTD.vsVSMOW <- Convert_STDs(airSTDs_mean_d.34O2.32O2, air.d18O.vsSMOW)   #Convert d18O to VSMOW scale
    samples$d180_02.vs.VSMOW <- Convert_STDs(samples$d.34O2.32O2_corr, workingSTD.vsVSMOW)
    } else {
    samples$d180_02.vs.air <- Convert_STDs(samples$d.34O2.32O2, airSTDs_mean_d.34O2.32O2)
    workingSTD.vsVSMOW <- Convert_STDs(airSTDs_mean_d.34O2.32O2, air.d18O.vsSMOW)   #Convert d18O to VSMOW scale
    samples$d180_02.vs.VSMOW <- Convert_STDs(samples$d.34O2.32O2, workingSTD.vsVSMOW)
  }

  #d17O-O2
  if(flag_d.33O2.32O2){samples$d170_02.vs.air <- Convert_STDs(samples$d.33O2.32O2_corr, airSTDs_mean_d.33O2.32O2)
    } else {samples$d170_02.vs.air <- Convert_STDs(samples$d.33O2.32O2, airSTDs_mean_d.33O2.32O2)
    }

  #d15N-N2
  if(flag_d.15N.14N){samples$d15N_N2.vs.air <- Convert_STDs(samples$d.15N.14N_corr, airSTDs_mean_d.15N.14N)
  } else {
    samples$d15N_N2.vs.air <- Convert_STDs(samples$d.15N.14N, airSTDs_mean_d.15N.14N)
  }

  #Ar:O2
  workingSTD.vsAir <- air.32O2.40Ar*((1000/(1000+airSTDs_mean_d.32O2.40Ar)))
  if(flag_d.32O2.40Ar){samples$O2.Ar <- workingSTD.vsAir*(1000+samples$d.32O2.40Ar_corr)/1000
    } else {samples$O2.Ar <- workingSTD.vsAir*(1000+samples$d.32O2.40Ar)/1000
  }

  #d13C-CO2
  airSTDs_mean_d.13C.12C <- mean(airSTDs$d.13C.12C, na.rm=T)
  ##Convert d13C to VPDB scale
  workingSTD.vsVPDB <- Convert_STDs(airSTDs_mean_d.13C.12C, bottle.d13C)
  if(flag_d.13C.12C){samples$d13C.CO2 <- Convert_STDs(samples$d.13C.12C_corr, workingSTD.vsVPDB)
    } else {samples$d13C.CO2 <- Convert_STDs(samples$d.13C.12C,workingSTD.vsVPDB)
  }
  #Calculate d13C of total DIC from the headspace d13C-CO2
  samples$d13C.TotalDIC <- calc.d13C.DIC(samples$d13C.CO2, Vg=samples$Vg, lab.air.T=lab.air.T, system.pressure.atm = system.pressure.atm)

  #Flag samples with low sample mass for area 32
  samples$flag.smallArea32 <- FALSE
  samples$flag.smallArea32[samples$Area.32 <= areaCutoff] <- TRUE

  #Write the results
  sampleOutputFileName <-  paste(tools::file_path_sans_ext(data.file),"_results.csv",sep="")
  standardsOutputFileName <- paste(tools::file_path_sans_ext(data.file),"_airSTDs.csv", sep="")
  write.csv(samples, sampleOutputFileName)
  write.csv(airSTDs, standardsOutputFileName)


  # #################################################################################
      ####Write run report as a text file that includes precision/accuracy information and error analysis.####
  # #################################################################################
  ###########################################################################################################
      ####Output results to text file####
      # Modeled after IsoLab output files
  ###########################################################################################################
  sink(paste(tools::file_path_sans_ext(data.file),"_reduced_final.txt",sep=""), append=FALSE, split=FALSE)

  #Run and instrument information
  cat("RUN AND INSTRUMENT INFORMATION", "\n", sep = "\t")
  cat("Instrument ID:", "Thermo Delta V (NACHO)", "\n", sep = "\t")
  cat("Analysis Date(s):", unique(exetainers$analysisDate), "\n", sep = "\t")
  cat("\n")
  cat("\n")
  cat("Raw data file location:", getwd(), "\n", sep = "\t")
  cat("Raw data file name:", data.file, "\n", sep = "\t")
  cat("Reduced sample data file name:", sampleOutputFileName, "\n", sep = "\t")
  cat("Reduced standards data file name:", standardsOutputFileName, "\n", sep = "\t")
  cat("\n")
  cat("Data reduction performed", format(Sys.time(), "%a %b %d %X %Y"))
  cat("\n")
  cat("\n")

  #Reference standards information.
  cat("STANDARD VALUES FOR THIS RUN", "\n", sep = "\t")
  cat("Each air standard vial was sub-sampled 4 times.  The sub-samples marked T below were averaged and used in subsequent calculations.", "\n")
  write.table(data.frame(Sub.sample=1:4, Used=injections.standards), file="", sep="\t", row.names=F)
  cat("\n")
  cat("\n")

  cat("AIR STANDARD RELATIVE TO THE WORKING STANDARD (TANK)", "\n", sep = "\t")
  write.table(airSTDs, file="", sep="\t", row.names=F)
  cat("\n")
  cat("\n")

  cat("SUMMARY MEAN AND STANDARD DEVIATION FOR AIR STANDARDS.", "\n")
  cat("delta 34O2:32O2: ", round(airSTDs_mean_d.34O2.32O2,3), "+/- ", round(sd(airSTDs$d.34O2.32O2, na.rm=T),3),"\n", sep = "\t")
  cat("delta 33O2:32O2: ", round(airSTDs_mean_d.33O2.32O2,3), "+/- ", round(sd(airSTDs$d.33O2.32O2, na.rm=T),3),"\n", sep = "\t")
  cat("delta 32O2:40Ar: ", round(airSTDs_mean_d.32O2.40Ar,3), "+/- ", round(sd(airSTDs$d.32O2.40Ar, na.rm=T),3),"\n", sep = "\t")
  cat("delta 15N2:14N2: ", round(airSTDs_mean_d.15N.14N,3), "+/- ", round(sd(airSTDs$d.15N.14N, na.rm=T),3),"\n", sep = "\t")
  cat("delta 13CO2:12CO2: ", round(airSTDs_mean_d.13C.12C,3), "+/- ", round(sd(airSTDs$d.13C.12C, na.rm=T),3),"\n", sep = "\t")
  cat("Accepted delta 13CO2 vs 12CO2 of the CO2 standard (vs VPDB): ", bottle.d13C, "\n", sep = "\t")
  cat("\n")
  cat("\n")


  #Report regression results for standardizing to VSMOW
  cat("SLOPE CORRECTION FOR SAMPLE MASS EFFECTS.","\n")
  if (flag_d.34O2.32O2){
    cat("delta 34O2:32O2 were corrected using a linear model of the residuals vs. vs. Area 32 with intercept and slope of ",round(coefficients(lmOut_d.34O2.32O2),3), "\n")
  } else {
    cat("delta 34O2:32O2 were not corrected", "\n")
  }

  if (flag_d.33O2.32O2){
    cat("delta 33O2:32O2 were corrected using a linear model of the residuals vs. vs. Area 32 with intercept and slope of ",round(coefficients(lmOut_d.33O2.32O2),3), "\n")
  } else {
    cat("delta 33O2:32O2 were not corrected", "\n")
  }

  if (all(flag_d.32O2.40Ar, flag1)){
    cat("delta 33O2:32O2 were corrected using a linear model of the residuals vs. vs. Area 32 with intercept and slope of ",round(coefficients(modelOut_d.32O2.40Ar),3), "\n")
  } else if (flag_d.32O2.40Ar){
    cat("delta 32O2:40Ar were corrected using a saturating model of the residuals vs. vs. Area 32 of the form y = c + a*tanh(b*Area 32/a).", "\n")
    cat("\t", "The coefficients for this model were ",round(coefficients(nlsOut_d.32O2.40Ar),3), "\n")
  }  else{
    cat("delta 32O2:40Ar were not corrected", "\n")
  }

  if (flag_d.15N.14N){
    cat("delta 15N2:12N2 were corrected using a linear model of the residuals vs. vs. Area 28 with intercept and slope of ",round(coefficients(lmOut_d.15N.14N),3), "\n")
  } else {
    cat("delta 15N2:12N2 were not corrected", "\n")
  }

  if (flag_d.13C.12C){
    cat("delta 13CO2:12CO2 were corrected using a linear model of the residuals vs. vs. Area 44 with intercept and slope of ",round(coefficients(lmOut_d.13C.12C),3), "\n")
  } else {
    cat("delta 13CO2:12CO2 were not corrected", "\n")
  }

  cat("\n"); cat("\n")

  cat("Headspace d13C-CO2 was converted to d13C of total DIC (d13C_DIC) following a modified method of Assayag et al. Rapid Commun. Mass Spectrom. 2006; 20: 2243–2251.","\n")

  cat("\n"); cat("\n")

  #Report actual results
  cat("Each sample vial was sub-sampled 4 times.  The sub-samples marked T below were averaged and used in subsequent calculations.", "\n")
  write.table(data.frame(Sub.sample=1:4, Used=injections.samples), file="", sep="\t", row.names=F)
  cat("\n")
  cat("\n")

  cat("SAMPLE RESULTS.","\n")
  write.table(samples, file="", sep="\t", row.names=F)

  sink()

  return(samples)  #Return dataframe of finalized sample data.

}
