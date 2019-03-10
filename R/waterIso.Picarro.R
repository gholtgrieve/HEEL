#' Finalizes water isotope data from a Picarro CRDS
#'
#' This function is used to decompose and finalize raw water isotope data from a Picarro CRDS instrument.  The first step
#' is to correct for linear drift of the instrument.  The second step calibrate the measured values against the working standards included
#' in the run and calculate QA/QC metrics.  The function writes a .txt with all relevant information included.  A dataframe of
#' the finalized sample data is also returned.
#'
#' @usage waterIso.Picarro(data.file, sample.file, working.STDs = c("BW","KD","SW"))
#'
#' @param data.file   Full file name for raw data from the instrument. Must be a .csv file! File name must start with the instrument ID number follow by "_". Character.
#' @param sample.file Full file name for the sample information that coresponds to the raw data.  Also, must be a .csv file! The file must include the folowing headings (and only the following headings).  Best to use the template!
#'   \describe{
#'     \item{vialNum}{The vail number of the sample.}
#'     \item{trayNum}{The tray positon on the autosampler.}
#'     \item{sampleType}{Aceptable identifiers include: Conditioner, Standard, and Sample.}
#'     \item{SampleDesc}{Unique identifier of the sample.}
#'     }
#' @param working.STDs Character vector of length 3 listing names of working stadards included in the run. The first two will be used for correction and calibration, the third as the QA/QC check.  Defaults to IsoLab standards "BW", "KD", "SW".
#' @return Dataframe of finalized water isotope data including:
#'   \describe{
#'     \item{trayNum}{The tray positon on the autosampler.}
#'     \item{vialNum}{The vail number of the sample.}
#'     \item{SampleDesc}{Unique identifier of the sample.}
#'     \item{H2O.ppmv.mean}{Average water concentration across saved injections in ppm by volume.}
#'     \item{H2O.ppmv.SD}{Standard deviation of water concentration across saved injections in ppm by volume.}
#'     \item{dD.mean.VSMOW}{Average 2H:1H of water across saved injections in standard delta notation relative to VSMOW.}
#'     \item{dD.SD}{Standard deviation of 2H:1H across saved injections in standard delta notation relative to VSMOW.}
#'     \item{d18O.mean.VSMOW}{Average 18O:16O of water across saved injections in standard delta notation relative to VSMOW.}
#'     \item{d18O.SD}{Standard deviation of 18O:16O across saved injections in standard delta notation relative to VSMOW.}
#'    }
#' @author Gordon W. Holtgrieve
#' @export
#'

waterIso.Picarro <- function(data.file, sample.file, working.STDs = c("BW","KD","SW")){

#Output file name
output.file <- paste(tools::file_path_sans_ext(data.file),"_reduced_final.txt", sep="")

#create a data frame with the d18O and dD of possible standards
refSTDs <- data.frame(Name = get.isotope.standard(working.STDs, "H")$std,
                      dD_vsVSMOW = get.isotope.standard(working.STDs, "H")$delta,
                        d18O_vsVSMOW = get.isotope.standard(working.STDs, "O18")$delta)

#Identify which two standards are to be used in the correction and which is the check.
refWater1  <-  working.STDs[1]
refWater2  <-  working.STDs[2]
checkWater <-  working.STDs[3]  #Used for validation

###########################################################################################################
#   Functions
#   Some useful functions for later on.
###########################################################################################################
Calculate.Memory <- function(current, previous, numInj, is.dD = T){
  x <- mean(previous[(numInj-1):numInj])
  y <- mean(current[(numInj-1):numInj])
  z <- current[1]

  d <- abs(y-x)
  e <- abs(y-z)

  memory <- e/d

  if(is.dD == T && d >= 10){
    return(memory*100)
  } else if (is.dD == F && d >= 2){
    return(memory*100)
  }

  return(NA) #default is to return NA
}


Calculate.nDiscard <- function(memory, cutoff){
  i=1
  f <- memory
  while (f > cutoff){
    f <- memory^i
    i <- i + 1
  }
  return(i)
}

###########################################################################################################
###########################################################################################################

###########################################################################################################
#   Load data, combine, and error check
###########################################################################################################
  rawData <- read.csv(data.file, header=T)
  sampleData <- read.csv(sample.file, header=T)

  data <- merge(x=rawData, y=sampleData, by.x="Sample", by.y="trayNum", all.x = T)

#find total number of injections per sample
  numInj <- max(data$Inj.Nr)
  cat("It appears the number of injections per sample is ", numInj,".","\n", sep="")
  flag <- readline("Is that correct?. Type T or F.")
  if(flag==F) stop("You entered false when the data says true.  Check to see who is wrong.")

#identify total number of vials analysed (standards + samples)
  vials <- unique(data$Sample)
  numVials <- length(vials)

#Find indices of reference waters (i.e. standards), conditioners, and unknown samples
  standards.index <- which(data$sampleDesc == refWater1 | data$sampleDesc == refWater2)
  samples.index <- which(data$sampleType=="Sample")
  conditioners.index <- which(data$sampleType=="Conditioner")

#Identify instruemnt and check with user.
  instrumentID <- strsplit(data.file, "_")[[1]][1]
  instruments <- matrix(c("HIDS2064", "HBDS39", "HBDS2213", "OSB Picarro", "Gorky", "DrDeSoto"), ncol=2, byrow=F)
  instrumentName <- instruments[which(instruments[,1] == instrumentID),2]
  cat("It appears the instrument was ", instrumentName,".","\n", sep="")
  flag <- as.logical(readline("Is that correct (T/F)?"))
  if(flag==F) instrumentName <- readline("Enter the instrument name.")

###########################################################################################################

###########################################################################################################
#  Calculate the dD & d18O "memory" for this run.  Set number of injections to discard.
###########################################################################################################
#   The memory calculation for dD and 18O is:
#   range = abs of the last two injections from the current vial minus the last two injectiosn from the previous vial
#   remaining = abs of last two injections of current vial minus first injection of the current vial
#   memory (%) = remaining / range * 100

#Establish vectors for memory of each vial (dD and d18O)
  dD.memory <- rep(NA, numVials-1)
  d18O.memory <- rep(NA, numVials-1)

#Loop through whole dataset (standards + samples) and calculate memory from each vial starting at the second one.
# Thin to conecutive vials with > 10 per mil (dD) and 2 per mil (d18O) difference
# Average and report to screen.
  for (i in 2:numVials){
    prev.Vial <- data[data$Sample==vials[i-1],c("d.D_H.Mean", "d.18_16.Mean")]
    current.Vial <- data[data$Sample==vials[i],c("d.D_H.Mean", "d.18_16.Mean")]
    if (length(na.omit(current.Vial[,1])) == numInj && length(na.omit(prev.Vial[,1])) == numInj){
      dD.memory[i-1] <- Calculate.Memory(current.Vial$d.D_H.Mean, prev.Vial$d.D_H.Mean, numInj, is.dD=T)
      d18O.memory[i-1] <- Calculate.Memory(current.Vial$d.18_16.Mean, prev.Vial$d.18_16.Mean, numInj, is.dD=F)
    }
  }

# Print results of memory calc to screen and ask user how many injections to discard.
  cat("\n")
  cat("Average memory for this run (dD) is ", round(mean(dD.memory, na.rm=T),2), ".","\n", sep="")
  cat("Average memory for this run (d18O) is ", round(mean(d18O.memory, na.rm=T),2), ".", "\n","\n", sep="")

  cat("Number of injections to achieve < 1% carryover is ", Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.01), ".", "\n", sep="")
  cat("Number of injections to achieve < 0.1% carryover is ", Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.001), ".", "\n", sep="")
  cat("Number of injections to achieve < 0.01% carryover is ", Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.0001), ".", "\n", sep="")

# Set default number of injections to discard at the number to achieve < 0.1% carryover
# or numInj - 3, whichever is smaller.
  nDiscard <- Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.001)
  if (nDiscard > (numInj - 3)) nDiscard <- (numInj - 3)

# Display default number of discards and user if that what they want to use.
  cat("The default number of injections to discard is ", nDiscard, ".", "\n", sep="")
  newDiscard <- as.logical(readline("Would you like to change the number of sample to discard (T/F)?"))
  #If the anser to the above is T, ask user how many injections to discard and replace default with answer?
  if (newDiscard) nDiscard <- as.numeric(readline("How many injections would you like to discard?"))

###########################################################################################################


###########################################################################################################
#  #Calculate means and standard deviations for each vial after discarding the specified number of injections.
###########################################################################################################
  vialMeans <- data.frame(sampleNum=vials, trayNum=NA, vialNum=NA, sampleType=NA, sampleDesc=NA, H2O.ppmv.mean=NA, H2O.ppmv.SD=NA, dD.mean=NA, dD.SD=NA, dD.res=NA, d18O.mean=NA, d18O.SD=NA, d18O.res=NA)

  for (i in vials){
    temp <- data[data$Sample==i,]
    vialMeans$trayNum[vialMeans$sampleNum==i] <- temp$Port[1]
    vialMeans$vialNum[vialMeans$sampleNum==i] <- temp$vialNum[1]
    vialMeans$sampleType[vialMeans$sampleNum==i] <- as.character(temp$sampleType[1])
    vialMeans$sampleDesc[vialMeans$sampleNum==i] <- as.character(temp$sampleDesc[1])

    vialMeans$H2O.ppmv.mean[vialMeans$sampleNum==i] <- mean(temp$H2O_Mean[temp$Inj.Nr>nDiscard], na.rm=T)
    vialMeans$H2O.ppmv.SD[vialMeans$sampleNum==i] <- sd(temp$H2O_Mean[temp$Inj.Nr>nDiscard], na.rm=T)

    vialMeans$dD.mean[vialMeans$sampleNum==i] <- mean(temp$d.D_H.Mean[temp$Inj.Nr>nDiscard], na.rm=T)
    vialMeans$dD.SD[vialMeans$sampleNum==i] <- sd(temp$d.D_H.Mean[temp$Inj.Nr>nDiscard], na.rm=T)

    vialMeans$d18O.mean[vialMeans$sampleNum==i] <- mean(temp$d.18_16.Mean[temp$Inj.Nr>nDiscard], na.rm=T)
    vialMeans$d18O.SD[vialMeans$sampleNum==i] <- sd(temp$d.18_16.Mean[temp$Inj.Nr>nDiscard], na.rm=T)
  }

#Seperate into standards and samples (remove conditioners)
  vialMeans_standards <- vialMeans[vialMeans$sampleType == "Standard",]
  vialMeans_samples <- vialMeans[vialMeans$sampleType == "Sample",]

###########################################################################################################

###########################################################################################################
#  Calculate instrumental drift and present to user.  If requested, drift correct data.
###########################################################################################################
# Calculate means for standards to use in residual calcs
  dD.STDs.meas.mean <- tapply(vialMeans_standards$dD.mean,INDEX=vialMeans_standards$sampleDesc,FUN=mean)
  d18O.STDs.meas.mean <- tapply(vialMeans_standards$d18O.mean,INDEX=vialMeans_standards$sampleDesc,FUN=mean)

#Calculate the residuals from the mean for each standard not identified as a Conditioner
  for (i in refSTDs$Name){
    index <- which(vialMeans_standards$sampleDesc==i)
    vialMeans_standards$dD.res[index] <-  vialMeans_standards$dD.mean[index]- dD.STDs.meas.mean[i]
    vialMeans_standards$d18O.res[index] <-  vialMeans_standards$d18O.mean[index]- d18O.STDs.meas.mean[i]
  }

#Plot data and run linear model of residuals against sample number.
  dD.drift.lm <- lm(vialMeans_standards$dD.res~vialMeans_standards$sampleNum)
  dD.drift.lm.sum <- summary(dD.drift.lm)
  plot(x=vialMeans_standards$sampleNum, y=vialMeans_standards$dD.res, pch=19, cex=1.5,
       xlab="Sample Number", ylab="Residual from mean value", main="dD-H2O")
  lines(predict(dD.drift.lm)~vialMeans_standards$sampleNum, lwd=2)
  print(dD.drift.lm.sum)
  f <- dD.drift.lm.sum$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)

# Set default for drift correction to TRUE if the linear model is significant to 0.05.
  if(p <= 0.05) dD.drift.flag <- TRUE; rm(p,f)

#Ask user how many injections to discard and replace default with answer?
  dD.drift.flag <- as.logical(readline("Do you want to drift correct the dD data (T/F)?"))

#Repeat for d18O
  d18O.drift.lm <- lm(vialMeans_standards$d18O.res~vialMeans_standards$sampleNum)
  d18O.drift.lm.sum <- summary(d18O.drift.lm)
  plot(x=vialMeans_standards$sampleNum, y=vialMeans_standards$d18O.res, pch=19, cex=1.5,
       xlab="Sample Number", ylab="Residual from mean value", main="d18O-H2O")
  lines(predict(d18O.drift.lm)~vialMeans_standards$sampleNum, lwd=2)
  print(d18O.drift.lm.sum)
  f <- d18O.drift.lm.sum$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)

# Set default for drift correction to TRUE if the linear model is significant to 0.05.
  if(p <= 0.05) d18O.drift.flag <- TRUE

#Ask user how many injections to discard and replace default with answer?
  d18O.drift.flag <- as.logical(readline("Do you want to drift correct the d18O data (T/F)?"))

#Combine regression coeffiences into to matrix for easier output later
  driftCoeff <- matrix(NA, nrow=2, ncol=2, dimnames=list(c("dD", "d18O"), c("Intercept", "Slope")))
  driftCoeff[1,] <- coefficients(dD.drift.lm)
  driftCoeff[2,] <- coefficients(d18O.drift.lm)

#Drift correct sample data using the residuals if flagged T.
  if(dD.drift.flag){
    vialMeans_samples$dD.mean.driftCorr <- vialMeans_samples$dD.mean - (driftCoeff[1,1] + driftCoeff[1,2]*vialMeans_samples$sampleNum)
  } else {vialMeans_samples$dD.mean.driftCorr <- vialMeans_samples$dD.mean}

  if(d18O.drift.flag){
    vialMeans_samples$d18O.mean.driftCorr <- vialMeans_samples$d18O.mean - (driftCoeff[2,1] + driftCoeff[2,2]*vialMeans_samples$sampleNum)
  } else {vialMeans_samples$d18O.mean.driftCorr <- vialMeans_samples$d18O.mean}

  #Drift correct standards data using the residuals if flagged T.
  if(dD.drift.flag){
    vialMeans_standards$dD.mean.driftCorr <- vialMeans_standards$dD.mean - (driftCoeff[1,1] + driftCoeff[1,2]*vialMeans_standards$sampleNum)
  } else {vialMeans_standards$dD.mean.driftCorr <- vialMeans_standards$dD.mean}

  if(d18O.drift.flag){
    vialMeans_standards$d18O.mean.driftCorr <- vialMeans_standards$d18O.mean - (driftCoeff[2,1] + driftCoeff[2,2]*vialMeans_standards$sampleNum)
  } else {vialMeans_standards$d18O.mean.driftCorr <- vialMeans_standards$d18O.mean}

###########################################################################################################

###########################################################################################################
# Re-calculate means for each standard for H2O, dD and d18O after drift correction
###########################################################################################################

  H2O.STDs.meas.mean <- tapply(vialMeans_standards$H2O.ppmv.mean,INDEX=vialMeans_standards$sampleDesc,FUN=mean)
  dD.STDs.meas.mean <- tapply(vialMeans_standards$dD.mean.driftCorr,INDEX=vialMeans_standards$sampleDesc,FUN=mean)
  d18O.STDs.meas.mean <- tapply(vialMeans_standards$d18O.mean.driftCorr,INDEX=vialMeans_standards$sampleDesc,FUN=mean)

  H2O.STDs.meas.sd <- tapply(vialMeans_standards$H2O.ppmv.mean,INDEX=vialMeans_standards$sampleDesc,FUN=sd)
  dD.STDs.meas.sd <- tapply(vialMeans_standards$dD.mean.driftCorr,INDEX=vialMeans_standards$sampleDesc,FUN=sd)
  d18O.STDs.meas.sd <- tapply(vialMeans_standards$d18O.mean.driftCorr,INDEX=vialMeans_standards$sampleDesc,FUN=sd)

  standardsMeans.meas <- data.frame(Name=names(dD.STDs.meas.mean), dD.STDs.meas.mean, dD.STDs.meas.sd, d18O.STDs.meas.mean, d18O.STDs.meas.sd, H2O.STDs.meas.mean, H2O.STDs.meas.sd)
  standardsMeans.meas[,-1] <- round(standardsMeans.meas[,-1],2)  #round to two decimals
  STDs.temp <- merge(refSTDs, standardsMeans.meas, by="Name")

  STDs.temp$status <- NA
  STDs.temp$status[which(STDs.temp$Name==refWater1)] <- "refWater1"
  STDs.temp$status[which(STDs.temp$Name==refWater2)] <- "refWater2"
  STDs.temp$status[which(STDs.temp$Name==checkWater)] <- "checkWater"

  names(STDs.temp) <- c("Name", "dD.accepted", "d18O.accepted", "dD.measured.mean", "dD.measured.sd", "d18O.measured.mean", "d18O.measured.sd", "H2O.ppmv.mean", "H2O.ppmv.SD", "status")
###########################################################################################################

###########################################################################################################
#  Standardize to VSMOW using the accepted values of the two reference standards
###########################################################################################################
#Calculate linear relationship betwen the measured and known value of the standards
  dD.std.lm <- lm(STDs.temp[STDs.temp$Name==refWater1 | STDs.temp$Name==refWater2,c("dD.accepted","dD.measured.mean")])
  d18O.std.lm <- lm(STDs.temp[STDs.temp$Name==refWater1 | STDs.temp$Name==refWater2,c("d18O.accepted","d18O.measured.mean")])

#Combine regression coeffiences into to matrix for easier output later
  stdCoeff <- matrix(NA, nrow=2, ncol=2, dimnames=list(c("dD", "d18O"), c("Intercept", "Slope")))
  stdCoeff[1,] <- coefficients(dD.std.lm)
  stdCoeff[2,] <- coefficients(d18O.std.lm)

  vialMeans_samples$dD.mean.VSMOW <- stdCoeff[1,1] + stdCoeff[1,2]*vialMeans_samples$dD.mean.driftCorr
  vialMeans_samples$d18O.mean.VSMOW <- stdCoeff[2,1] + stdCoeff[2,2]*vialMeans_samples$d18O.mean.driftCorr

#round data to three decimal places
  colIndex <- c("dD.SD", "d18O.SD", "dD.mean.VSMOW", "d18O.mean.VSMOW")
  vialMeans_samples[,colIndex] <- round(vialMeans_samples[,colIndex],3)
###########################################################################################################

###########################################################################################################
#  Assess accuracy and precision
###########################################################################################################

  QC <- matrix(data=NA,nrow=2,ncol=2,dimnames=list(c("dD", "d18O"),c("Accuracy", "Precision")))

  QC[1,1] <- abs((stdCoeff[1,1] + stdCoeff[1,2] * STDs.temp$dD.measured.mean[STDs.temp$Name==checkWater]) - STDs.temp$dD.accepted[STDs.temp$Name==checkWater])
  QC[1,2] <- sd(vialMeans_standards$dD.mean[vialMeans_standards$sampleDesc==checkWater], na.rm=T)
  QC[2,1] <- abs((stdCoeff[2,1] + stdCoeff[2,2] * STDs.temp$d18O.measured.mean[STDs.temp$Name==checkWater]) - STDs.temp$d18O.accepted[STDs.temp$Name==checkWater])
  QC[2,2] <- sd(vialMeans_standards$d18O.mean[vialMeans_standards$sampleDesc==checkWater], na.rm=T)
###########################################################################################################



###########################################################################################################
# Output results to text file
# Modeled after IsoLab output files
###########################################################################################################
  sink(output.file, append=FALSE, split=FALSE)

#Run and instrument information
  cat("RUN AND INSTRUMENT INFORMATION", "\n", sep = "\t")
  cat("Instrument ID:", instrumentID, "\n", sep = "\t")
  cat("Instrument name:", instrumentName, "\n", sep = "\t")
  cat("\n")
  cat("Raw data file name:", data.file, "\n", sep = "\t")
  cat("Sample identifier data file name:", sample.file, "\n", sep = "\t")
  cat("Reduced data file name:", output.file, "\n", sep = "\t")
  cat("\n")
  cat("Analysis start:", as.character(rawData$Time.Code[1]), "\n", sep = "\t")
  cat("Analysis end:", as.character(rawData$Time.Code[length(rawData$Time.Code)]), "\n", sep = "\t")
  cat("Data reduction performed", format(Sys.time(), "%a %b %d %X %Y"), "\n", sep = "\t")
  cat(paste("Analyzed using R", getRversion()))
  cat("----Installed Packages----")
  for (package_name in sort(loadedNamespaces())) {
    cat(paste(package_name, packageVersion(package_name)))
  }
  cat("\n")
  cat("\n")

#Reference standards information.
  cat("REFERENCE MATERIALS USED IN THIS RUN.", "\n", sep = "\t")
  write.table(STDs.temp, file="", sep = "\t", row.names=F)
  cat("\n"); cat("\n")

#Report results from memory analysis.
  cat("RESULTS OF ANALYSIS OF INSTRUMENT MEMORY EFFECTS.", "\n", sep = "\t")
  cat("The memory calculation for dD and 18O is:","\n", sep = "\t")
  cat("   range = abs of the last two injections from the current vial minus the last two injections from the previous vial.","\n", sep = "\t")
  cat("   remaining = abs of last two injections of current vial minus first injection of the current vial.","\n", sep = "\t")
  cat("   memory (%) = remaining / range * 100","\n", sep = "\t")
  cat("\n")
  cat("Average memory for this run (dD) is ", round(mean(dD.memory, na.rm=T),2), ".","\n", sep = "\t")
  cat("Average memory for this run (d18O) is ", round(mean(d18O.memory, na.rm=T),2), ".", "\n","\n", sep = "\t")

  cat("Number of injections to achieve < 1% carryover is ", Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.01), "\n", sep="\t")
  cat("Number of injections to achieve < 0.1% carryover is ", Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.001), "\n", sep="\t")
  cat("Number of injections to achieve < 0.01% carryover is ", Calculate.nDiscard(mean(dD.memory, na.rm=T)/100, 0.0001), "\n", sep="\t")
  cat("\n")
  cat("Number of discarded initial injections for each sample:", nDiscard,"\n", sep = "\t")
  cat("\n")
  cat("\n")

#Report results from drift corrections
  cat("DRIFT CORRECTION INFORMATION.","\n")
  cat("Samples corrected for instrumental drift (dD): ", dD.drift.flag, "\n", sep = "\t")
  cat("Samples corrected for instrumental drift (d18O): ", d18O.drift.flag, "\n", sep = "\t")
  cat("Drift correction regression coefficients of linear model between sample number and residual deviation from the mean.  All references waters included.","\n")
  cat("Drift corrected for by subtracting (linearly) predicted residual deviation as a function of sample number to all samples and standards.", "\n")
  write.table(round(driftCoeff,4),file="",sep="\t", row.names=F)
  cat("\n"); cat("\n")

#Report regression results for standardizing to VSMOW
  cat("CORRECTION TO INTERNATIONAL STANDARD (VSMOW) USING REFERENCE WATERS.","\n")
  cat("Standardization regression coefficients of linear model between measured and accepted values of refWater1 and refWater2.","\n")
  write.table(round(stdCoeff,4), file="", sep="\t", row.names=F)
  cat("\n"); cat("\n")

#Report QA/QC results
  cat("ACCURACY AND PRECISION INFORMATION FROM CHECK WATER AFTER CORRECTIONS AND STANDARDIZATION.","\n")
  cat("Check water:", checkWater,"\n", sep = "\t")
  write.table(round(QC,3), file="", sep="\t", row.names=F)
  cat("\n"); cat("\n")

#Report actual results
  cat("SAMPLE RESULTS.","\n")
  colIndex <- c("trayNum", "vialNum", "sampleDesc", "H2O.ppmv.mean", "H2O.ppmv.SD", "dD.mean.VSMOW", "dD.SD", "d18O.mean.VSMOW", "d18O.SD")
  write.table(vialMeans_samples[,colIndex], file="", sep="\t", row.names=F)

  sink()

  return(vialMeans_samples[,colIndex])
}
