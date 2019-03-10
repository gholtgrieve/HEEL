#' @title Finalizes extetainer gas ratio data from NACHO
#'
#' @description This function is used to decompose and finalize gas ratio data from exetainers analyzed on the Delta V irMS systems (NACHO).  The first step
#' is thin and organige the raw data.  Plots are generated (as .pdfs) to look for mass effects on the results ansd the user is asked whether to correct for
#' sample mass differences among injections and samples.  After corrections, measured values are calibrated against the working standards included
#' in the run.  The function writes a .csv with the thinned raw data and a .txt with all relevant information about the analysis.  The finalized
#' sample data are writen as a .csv file and adataframe the data is also returned.
#'
#' @usage exetainers.NACHO(data.file, analysis.date, area.cutoff = F, injections.standards = c(F, T, T, T), injections.samples = c(F, T, T, T), lab.air.T = 20, system.pressure.atm = 1.49701, salinity = 0)
#'
#' @param data.file     Full file name for raw data from the instrument. Must be a .csv file! Character.
#' @param analysis.date Date of analysis on NACHO as a character.
#' @param area.cutoff   Defines the peak area (Vs) cutoff below which an individual analysis (injection) is droped.  FALSE means no cutoff applied.  Numeric defines the cutoff value.
#' @param injections.standards  Boolean vector length 4 idenitfying which injections of the standards to drop (F) or keep (T) for use in the calculations.
#' @param injections.samples    Boolean vector length 4 idenitfying which injections of the samples to drop (F) or keep (T) for use in the calculations.
#' @param lab.air.T     Temperature of the lab at time of analysis in degrees Celcius.  Defaults to 20.  Numeric.
#' @param system.pressure.atm   Pressure of the inlet system in atmospheres.  Defaults to 1.49701 (22 psi).  Numeric.
#' @param salinity      Salninty of the sample in ppt.  Defaults to 0.  Numeric.
#'
#' @return Dataframe of :
#'   \describe{
#'     \item{}{}
#
#'    }
#' @author Gordon W. Holtgrieve
#' @export
#'

exetainers.NACHO <- function (data.file, analysis.date, area.cutoff = F,
                              injections.standards = c(F, T, T, T), injections.samples = c(F, T, T, T),
                              lab.air.T = 20, system.pressure.atm = 1.49701, salinity = 0){

  require(dplyr)

  if(is.nummeric(area.cutoff)){
    areaCutoff <- area.cutoff
    flagUseAreaCutoff <- T
  } else if (area.cutoff == F){
    flagUseAreaCutoff <- F
  } else stop("Parameter 'area.cutoff' must be FALSE or numeric.")

  # Standard values
  bottle.d13C <- -37.25     # Known bottle standard d13C-DIC value, value last updated 08 Dec 2018
  air.d18O.vsSMOW <- get.isotope.standard(std = "air", isotope.system = "O18")$delta  # returns 23.8
  air.d17O.vsSMOW <- NA     # Reference here
  air.d15N.vsAir <- get.isotope.standard(std = "air", isotope.system = "N")$delta  #returns 0
  air.32O2.40Ar <- 22.426   # Reference here


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
  rawData <- read.csv(data.file, header=T)
  rawData$analysisDate <- as.factor(analysisDate)

  #Determine number of samples and standards in this run
  exetainers <- unique(rawData[,c("Identifier.1","Identifier.2","Comment")])
  nTotal <- length(exetainers[,1])
  nSamples <- sum(exetainers$Comment=="Sample")
  nStandards <- sum(exetainers$Comment=="Standard")
  nConditioners <- sum(exetainers$Comment=="Conditioner")
  nWaterStandards <- sum(exetainers$Comment=="WaterSTD")
  print(nSamples + nStandards + nConditioners)   #Note: nTotal = nSamples + nStandards + nConditioners
  nRuns <- length(levels(rawData$analysisDate))

  #############################################################################
           ####Thin data to what is needed for calculations####
  #############################################################################
  #separate airSTD volumes (Conditioners|Standards) VS headspace volumes (WaterSTDs|Samples)
  x=rep(NA,length(exetainers$Identifier.1))
  y=rep(NA,length(exetainers$Identifier.1))
  for (i in 1:nTotal){
    x[i]=ifelse(exetainers[i,3]=="Conditioner"|exetainers[i,3]=="Standard",exetainers$Identifier.2[i],NA)
    y[i]=ifelse(exetainers[i,3]=="WaterSTD"|exetainers[i,3]=="Sample",exetainers$Identifier.2[i],NA)
  }

  exetainers <- exetainers %>%
    mutate(deckAir_mL = x) %>%
    mutate(Vg = y) %>%
    select(Identifier.1, deckAir_mL, Vg, Comment)


  #Loop through each sample checking the data and compiling the important results into a new data frame
  #Set up blank data frame to store the key data for analysis
  thinnedData <- data.frame(Identifier.1=rep(exetainers[,1],each=4),
                            deckAir_mL=rep(exetainers[,2],each=4),
                            Vg=rep(exetainers[,3],each=4),
                            Comment=rep(exetainers[,4], each=4),
                            analysisDate=NA,
                            Injection=1:4,
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
                            Small.Sample.Mass=NA)

  j=1

  for (i in 1:nTotal){
    print(paste("The current sample is", exetainers[i,1], "which is a", exetainers[i,4]))
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
    temp <- thinnedData[thinnedData$Identifier.1=="Standard",]
    index <- list(temp$Identifier.2[temp$keep], temp$analysisDate[temp$keep])
    airSTDs <- aggregate(temp[temp$keep,-c(1:2,4:5)],by=index,mean, na.rm=T)

  } else {
    #Compile the air standards, thin to only the selected injections (defined above), and average
    std.parameters <- c("deckAir_mL", "R.28.40", "Area.32", "d.34O2.32O2", "d.33O2.32O2", "Area.40",
                    "d.32O2.40Ar", "Area.28", "d.15N.14N", "Area.44", "d.13C.12C")
    temp <- thinnedData[thinnedData$Comment=="Standard",]
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
    nlsOut_d.32O2.40Ar <- nls(y ~ c + a*tanh(b*x/a), data=tempData, start=list(a=175, b=11, c=-120))

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
  tempData <- data.frame(x=airSTDs$Area.32, y=fitted(nlsOut_d.32O2.40Ar))
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
  flag_d.34O2.32O2 <- readline("Do you want to correct 34O2:32O2 for sample mass (area 32)?  Enter T/F.")
  if(flag_d.34O2.32O2){
    residuals_d.34O2.32O2 <- airSTDs$d.34O2.32O2 - airSTDs_mean_d.34O2.32O2
    lmOut_d.34O2.32O2 <- lm(residuals_d.34O2.32O2 ~ airSTDs$Area.32)

    offset <- samples$Area.32 * coefficients(lmOut_d.34O2.32O2)[2] + coefficients(lmOut_d.34O2.32O2)[1]
    samples$d.34O2.32O2_corr <- samples$d.34O2.32O2 - offset
  }

  # d17O-O2 data
  flag_d.33O2.32O2 <- readline("Do you want to correct 33O2:32O2 for sample mass (area 32)?  Enter T/F.")
  if(flag_d.33O2.32O2){
    offset <- samples$Area.32 * coefficients(lmOut_d.33O2.32O2)[2] + coefficients(lmOut_d.33O2.32O2)[1]
    samples$d.33O2.32O2_corr <- samples$d.33O2.32O2 - offset
  }

  # O2:Ar data
  flag_d.32O2.40Ar <- readline("Do you want to correct 32O2:40Ar for sample mass (area 32)?  Enter T/F.")
  if(flag_d.32O2.40Ar){
    offset <- coefficients(nlsOut_d.32O2.40Ar)[3] + coefficients(nlsOut_d.32O2.40Ar)[1] * tanh(coefficients(nlsOut_d.32O2.40Ar)[2] * samples$Area.32 / coefficients(nlsOut_d.32O2.40Ar)[1])
    samples$d.32O2.40Ar_corr <- samples$d.32O2.40Ar - offset
  }

  # d15N-N2 data
  flag_d.15N.14N <- readline("Do you want to correct 15N2:14N2 for sample mass (area 28)?  Enter T/F.")
  if(flag_d.15N.14N){
    offset <- samples$Area.28 * coefficients(lmOut_d.15N.14N)[2] + coefficients(lmOut_d.15N.14N)[1]
    samples$d.15N.14N_corr <- samples$d.15N.14N - offset
  }

  # d13C-CO2 data
  flag_d.13C.12C <- readline("Do you want to correct 13C:12C for sample mass (area 44)?  Enter T/F.")
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
  write.csv(samples, paste(tools::file_path_sans_ext(data.file),"_results.csv",sep=""))
  write.csv(airSTDs, paste(tools::file_path_sans_ext(data.file),"_airSTDs.csv", sep=""))


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
  cat("Analysis Date:", analysisDate, "\n", sep = "\t")
  cat("\n")
  cat("\n")
  cat("Raw data file location:", getwd(), "\n", sep = "\t")
  cat("Raw data file name:", data.file, "\n", sep = "\t")
  cat("Reduced data file name:", outputFileName, "\n", sep = "\t")
  cat("\n")
  cat("Data reduction performed", format(Sys.time(), "%a %b %d %X %Y"), "\n", sep = "\t")
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

  if (flag_d.32O2.40Ar){
    cat("delta 32O2:40Ar were corrected using a saturating model of the residuals vs. vs. Area 32 of the form y = c + a*tanh(b*Area 32/a).", "\n")
    cat("\t", "The coefficients for this model were ",round(coefficients(nlsOut_d.32O2.40Ar),3), "\n")
  } else {
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