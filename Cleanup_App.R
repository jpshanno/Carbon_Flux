#####################
#Authors:  Joe Shannon & Matt Van Grinsven
#Purpose:  Allow for interactive data selection and cleaning of IRGA samples and  generate efflux estimates. 
#Requirements: Your data file must contain the columns 'uniqueID', 'site', 'CO2 Ref', 'mb Ref', 'mbR Temp', 'RH', 'DC', 'DT', 'ATMP'.
#				You must have permission to create folders in the output directory.
#				Ensure that the packages data.table, XLConnect, ggplot2, and png are installed. install.packages(c("data.table","XLConnect","ggplot2","png"))
#Note:  XLSX files load very slowly into R
#####################

library(data.table)
library(xlsx)
library(ggplot2)
library(dplyr)

#Load the data and create a unique ID for each sample
gflux <- data.table(read.xlsx(choose.files(,caption="Select a *.xlsx (data must be on the first worksheet)file"), sheetIndex=1,stringsAsFactors=F))
setkey(gflux,uniqueID)   #This sorts the data by the unique key, and makes the records callable by that unique ID
setwd(choose.dir(caption="Select your output folder"))

for(i in 1:length(unique(gflux$uniqueID))){
     if(i==1) {
          if(length(grep("Processed_Plots",list.files(".")))==0) {dir.create("./Processed_Plots")}
          if(length(grep("Processed_Summary",list.files(".")))==0) {dir.create("./Processed_Summary")}
          if(length(grep("Processed_Tables",list.files(".")))==0) {dir.create("./Processed_Tables")}}
     #Plot the raw data of a sample and identify the start and end of good data by clicking. Write to csv
          collarID <-  unique(gflux$uniqueID)[i]
          plotname <- paste("Plot",collarID, sep="-")
          plotted <- plot(CO2.Ref~DT, data=gflux[collarID], 
                          ylim=c(min(gflux[collarID][,CO2.Ref]-5),
                                 min(gflux[collarID][,CO2.Ref]+75)),
                          main="Select the beginning and end of the data you would like to keep:")
          legend("topleft", legend=collarID, bty="n", cex=0.75)
          endpoints <- identify(gflux[collarID][,DT], gflux[collarID][,CO2.Ref], n=2)
     	goodoutput <- gflux[collarID][endpoints[1]:endpoints[2]];points(CO2.Ref~DT, data=goodoutput, col="red")
          write.csv(goodoutput,paste("./Processed_Tables/",collarID,".csv",sep=""),row.names=F)
          
     #Plot the good data and click to remove any outliers
          plot(CO2.Ref~DT, data=goodoutput, main=paste("Selected Data from",collarID), type="p")
          legend("topleft", legend=c("Click on points you wish to remove.", "Right click and choose 'Stop' when finished, or","If in RStudio click 'Finish'"), pch=16, bty="n", cex=0.75)
          badpoints <- goodoutput[identify(goodoutput[collarID][,DT], goodoutput[collarID][,CO2.Ref], labels="*", atpen=T)][,RecNo]
          if(length(badpoints)!=0) {
               goodoutput_rm <- setkey(goodoutput,RecNo)[!J(badpoints)]
               plot(CO2.Ref~DT, data=goodoutput_rm)
               write.csv(goodoutput_rm, paste("./Processed_Tables/",collarID,"_Outliers_Removed.csv",sep=""),row.names=F)
               png(filename=paste("./Processed_Plots/", plotname, "_Outliers_Removed.png", sep=""), height=6, width=6, units="in", res=72)
                    plot(CO2.Ref~DT, data=goodoutput_rm)
                    dev.off()
               rm(badpoints)}
          
     	
  
     #Create a plot of the trimmed data and save
          png(filename= paste("./Processed_Plots/", plotname, ".png", sep=""), height=6, width=6, units="in", res=72)
     	plot(CO2.Ref~DT, data=goodoutput)
          dev.off()
  
     #Fit linear regression to the trimmed data and save the coefficients to CSV
          SSlm <- lm(CO2.Ref~DT, data=goodoutput)
          SSlmsum <- summary(SSlm)
     	write.csv(SSlm$coef, paste("./Processed_Summary/",collarID,".csv",sep=""))

     #Create a data frame with model fit data and write to csv
          if(i==1) {outputsummary <- data.frame(id = collarID, 
                                                intercept = as.numeric(SSlm$coef[1]), 
                                                slope = as.numeric(SSlm$coef[2]), 
                                                r2 = as.numeric(SSlmsum$r.squared), 
                                                Adjr2 = as.numeric(SSlmsum$adj.r.squared), 
                                                RSE = as.numeric(SSlmsum$sigma), 
                                                dFree = as.numeric(SSlmsum$df[2]), 
                                                dFree_R = as.numeric(SSlmsum$fstatistic[2]), 
                                                fstat = as.numeric(SSlmsum$fstatistic[1]), 
                                                pvalue = as.numeric(pf(SSlmsum$fstatistic[1], SSlmsum$fstatistic[2], SSlmsum$df[2], lower.tail=F)))}
	     if(i>1) {outputsummary <- rbind(outputsummary,data.frame(id = collarID, 
                                                                   intercept = as.numeric(SSlm$coef[1]), 
                                                                   slope = as.numeric(SSlm$coef[2]), 
                                                                   r2 = as.numeric(SSlmsum$r.squared), 
                                                                   Adjr2 = as.numeric(SSlmsum$adj.r.squared), 
                                                                   RSE = as.numeric(SSlmsum$sigma), 
                                                                   dFree = as.numeric(SSlmsum$df[2]), 
                                                                   dFree_R = as.numeric(SSlmsum$fstatistic[2]), 
                                                                   fstat = as.numeric(SSlmsum$fstatistic[1]),
                                                                   pvalue = as.numeric(pf(SSlmsum$fstatistic[1], SSlmsum$fstatistic[2], SSlmsum$df[2], lower.tail=F))))}
	     write.csv(outputsummary,"./gflux_output_summary.csv", append=T, row.names=F)
	     write.csv(t(as.matrix(SSlmsum)), file=paste("./Processed_Summary/", collarID,".csv", sep = ""))
  
	rm(list=c("i","plotname"))
}


###Code to continue data processing (w/o redoing what you've done) if R crashes before you finish working through all of your data
#Call location of unique ID's so that you can re-run if error occurs 
##unique(gflux$uniqueID)
##rename"gflux" "gflux.full" then use next line to start a new loop
##gflux <- gflux.full[J(unique(gflux.full[,uniqueID])[190:205])]
##Make sure and run lines 11,12, and 13 before beginning loop 
##Make sure to save outputsummary before re-running, and re-name internal and .csv as eg. "outputsummary"


#Flux rate conversion info to go from ppm/s to delta g of CO2/m2/hr.  Column names need to be update
gflux_notes<-data.table(read.xlsx("./Data/IRGA-Ottawa_2013_Notes.xlsx", sheetIndex="Sheet1", stringsAsFactors=F))
gflux_notes[,Volume_Chamber_cm3:=as.numeric(Volume_Chamber_cm3)]

#Before importing any of the following data remove any undesireable files from the directory or change the directory to one of your choosing
workingtables_dir <- "./Output/Processed_Tables/"
trimmedfiles <- list.files(workingtables_dir)
for(i in 1:length(trimmedfiles)){
  if(i==1){processedoutput <- fread(paste(workingtables_dir,trimmedfiles[i],sep=""))}
  if(i>1){processedoutput <- rbind(processedoutput, fread(paste(workingtables_dir,trimmedfiles[i],sep="")))}
}
processedoutput <- processedoutput[!processedoutput[,max(ATMP)-min(ATMP),by=uniqueID][V1>0]]
p.out2 <- processedoutput[,list(mean(mbR.Temp)+273.15, mean(RH), mean(ATMP*100)),by=uniqueID]
setnames(p.out2,names(p.out2),c("uniqueID","temp_K","RH","pressure_Pa"))

setkey(gflux_notes,uniqueID)
setkey(p.out2,uniqueID)
fluxcalc <- gflux_notes[,list(uniqueID,air_temp,soil_temp_5cm,soil_temp_10cm,Volume_Chamber_cm3)][p.out2]

gflux_reg <- fread("./Output/Processed_Summary/gflux_output_summary.csv")
setkey(gflux_reg,id)
efflux <- gflux_reg[,list(id,slope)][fluxcalc]
efflux[,molar_volume_L_mol:=(8314.4621*temp_K)/pressure_Pa]
efflux[,delta_CO2_g_m2_hr:=(3600*slope*44.01*Volume_Chamber_cm3/1000)/(0.0499*1000*molar_volume_L_mol)]
efflux[,delta_C_g_m2_hr:=(3600*slope*12.01*Volume_Chamber_cm3/1000)/(0.0499*1000*molar_volume_L_mol)]


#setkey(gflux,uniqueID)
#gflux[,delta_CO2_ppm_s:=as.numeric(coef(lm(co2~elapsed))[2]), by=uniqueID]
#gflux[,r2:=summary(lm(co2~elapsed))$adj.r, by=uniqueID]
#gflux[,pressure_Pa:=mean(100*baro, na.rm=T),by=uniqueID]
#gflux[,temperature_K:=mean(tempC, na.rm=T) + 273.15, by=uniqueID]
#fluxrates <- unique(gflux[,list(uniqueID,delta_CO2_ppm_s,r2,pressure_Pa,temperature_K)])



#write.xlsx(fluxrates, file="C:/Users/BlackAshEAB/Documents/Data_Manually_Entered/IRGA-Ottawa_2012-CleanedJS.xlsx", sheetName="R_Out", row.names=F, append=T)


#write.table(fluxrates, "clipboard", sep="\t", row.names=F)


#gflux[,fitco2:=fitted(lm(co2~elapsed+I(elapsed^2))), by=uniqueID]



