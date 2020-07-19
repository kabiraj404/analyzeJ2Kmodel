#' Daily water balance of the basin
#'
#'Plots the daily storage in the basin. We know that the Input in a basin is equal to output _+ storage. Therefore the plot aims to show the storage of the basin. Theoritically a perfect model should have the storage(waterbalance) near to zero so that the water cycle is complete and there is no water water storage in the basin. If you see some spikes or higher values in the plot, it might be that the water has been stuck in some HRUs. Check them out.
#'@param TimeLoop The plot is based on the timeloop.dat file present in output folder of J2K model
#'
#'@return Plot with the daily water balance
#'
#'@examples
#'J2K_WatBalplot <- plot(x=years, y= daily_water_storage_in_basin)
#'
#'@export


J2K_WatBalplot <- function(){
  #read header names
  variable_head <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data

  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")


  #Calculation for the Daily WaterBalance
  WaterBalance <- timeloop %>%
    arrange(Date) %>%
    mutate(	Year = year,
            Date = Date,
            Input = precip + iceRunoff,
            Output = actET + catchmentRD1_w + catchmentRD2_w + catchmentRG1_w + catchmentRG2_w,
            diffIntStor = intercStorage - lag(intercStorage, default = first(intercStorage)),
            diffSWE = snowTotSWE - lag(snowTotSWE, default = first(snowTotSWE)),
            diffSnowS_G = snowStorage_G - lag(snowStorage_G, default = first(snowStorage_G)),
            diffactMPS = actMPS - lag(actMPS, default = first(actMPS)),
            diffactLPS = actLPS - lag(actLPS, default = first(actLPS)),
            diffactRG1 = actRG1 - lag(actRG1, default = first(actRG1)),
            diffactRG2 = actRG2 - lag(actRG2, default = first(actRG2)),
            diffchannelStorage_w = channelStorage_w - lag(channelStorage_w, default = first(channelStorage_w)),
            diffactDPS = actDPS - lag(actDPS, default = first(actDPS)),
            glacestor = glacStorage,
            Storage = diffIntStor + diffSWE + diffSnowS_G + diffactMPS+ diffactLPS + diffactRG1 + diffactRG2 + diffchannelStorage_w + diffactDPS + glacestor,
            WatBal = Input - Output - Storage
    )
  WaterBalance <- WaterBalance[-1,]

  #Visualization of the timeLoop
  return(ggplot(data=WaterBalance, mapping=aes(y =  WatBal, x = Date))+
           geom_line(color="blue") +
           theme_bw() + ylab("Water Balance"))

}
##################################end of the J2K_WatBalplot ###############################################




#'Annual plot of input vs output
#'
#'Visulization of annual Input and Output
#'
#'@param TimeLoop Considers the timeloop file from the Output folder from J2K model
#'
#'@return Annual input and output. Note_1:It should have atleast 360 days to be considered a year. Note_2: The sum of the variables is considered for the visulization.
#'
#'@examples
#'J2K_inVSoutInfo <- plot(x=input_output, y=Yearly_sum_value)
#'
#'@export

J2K_inVSoutInfo <- function(){

  #read header names
  variable_head <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")

  #For the analysis of the Input and output variables
  #Removing the years with less than 360 years of data
  timeloop <- timeloop %>%
    add_count(year) %>%
    filter(n > 360)

  #Visulization of the Input and Output variables
  WaterBalance2 <- timeloop %>%
    group_by(year)%>%
    summarise(
      Precip = sum(precip),
      Ice_runoff = sum(iceRunoff),
      actET = sum(actET),
      Annual_RD1 = sum(catchmentRD1_w),
      Annual_RD2 = sum(catchmentRD2_w),
      Annual_RG1 = sum(catchmentRG1_w),
      Annual_RG2 = sum(catchmentRG2_w),
      Snowmelt_Out_G = sum(snowMelt),
      Glacier_runoff = sum(glacierRunoff), #glacierRunoff=snowmeltg+icerunoff+rainrunoff
      SnowMelt_G = sum(snowMelt_G), #plus iceRunoff
      Rain_runoff = sum(rainRunoff)
    )

  a <- which(colnames(WaterBalance2)== "Precip")
  b <- which(colnames(WaterBalance2)== "Ice_runoff")
  c <- which(colnames(WaterBalance2)== "actET")
  d <- which(colnames(WaterBalance2)== "Annual_RD1")
  e <- which(colnames(WaterBalance2)== "Annual_RD2")
  f <- which(colnames(WaterBalance2)== "Annual_RG1")
  g <- which(colnames(WaterBalance2)== "Annual_RG2")
  h <- which(colnames(WaterBalance2)== "Snowmelt_Out_G")

  WaterBalance3 <- WaterBalance2 %>%
    mutate(
      Input = rowSums(.[a:b]),
      Total_Discharge = rowSums(.[d:g]),
      Output = rowSums(.[c:g]),
      PERofET_inPrecip = (actET/Precip*100),
      PERofRD1_inQ = (Annual_RD1/Total_Discharge*100),
      PERofRD2_inQ = (Annual_RD2/Total_Discharge*100),
      PERofRG1_inQ = (Annual_RG1/Total_Discharge*100),
      PERofRG2_inQ = (Annual_RG2/Total_Discharge*100),
      PERofIce_inGlaRunoff = (Ice_runoff/Glacier_runoff*100),
      PERofSnowMelt_G_inGlaRunoff = (SnowMelt_G/Glacier_runoff*100),
      PERofRainRunof_inGlaRunoff = (Rain_runoff/Glacier_runoff*100)
    )

  #Visualization of the Input Vs Output
  w4 <-WaterBalance3 %>%
    select(year,Input,Output)

  w4 <- reshape2::melt(w4, id.vars="year")

  return(ggplot(w4, aes(variable,value)) +
           geom_col(aes(fill=variable),position="identity")+
           scale_fill_manual(values=c("#56B4E9","#E69F00"), "Legend") +
           theme_classic()+ ylab("mm") + xlab("") +
           facet_grid(.~year))

}
##################################end of the J2K_inVSoutInfo ###############################################





#'Major components of the water balance
#'
#'Visulization of major components groups in Input, Output and Snow/Glacier
#'
#'@param TimeLoop Output from J2K model
#'
#'@return Info on the water balance components. Note_1: the sum of the variables is considered for now.
#'
#'@examples
#'WatBal_majorcomponents() <- plot(x=variables, y=value)
#'
#'@export


J2K_WatBalMajorComps <- function(){

  #read header names
  variable_head <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")

  WaterBalance2 <- timeloop %>%
    group_by(year)%>%
    summarise(
      Precip = sum(precip),
      Ice_runoff = sum(iceRunoff),
      actET = sum(actET),
      Annual_RD1 = sum(catchmentRD1_w),
      Annual_RD2 = sum(catchmentRD2_w),
      Annual_RG1 = sum(catchmentRG1_w),
      Annual_RG2 = sum(catchmentRG2_w),
      Snowmelt_Out_G = sum(snowMelt),
      Glacier_runoff = sum(glacierRunoff), #glacierRunoff=snowmeltg+icerunoff+rainrunoff
      SnowMelt_G = sum(snowMelt_G), #plus iceRunoff
      Rain_runoff = sum(rainRunoff)
    )

  a <- which(colnames(WaterBalance2)== "Precip")
  b <- which(colnames(WaterBalance2)== "Ice_runoff")
  c <- which(colnames(WaterBalance2)== "actET")
  d <- which(colnames(WaterBalance2)== "Annual_RD1")
  e <- which(colnames(WaterBalance2)== "Annual_RD2")
  f <- which(colnames(WaterBalance2)== "Annual_RG1")
  g <- which(colnames(WaterBalance2)== "Annual_RG2")
  h <- which(colnames(WaterBalance2)== "Snowmelt_Out_G")

  WaterBalance3 <- WaterBalance2 %>%
    mutate(
      Input = rowSums(.[a:b]),
      Total_Discharge = rowSums(.[d:g]),
      Output = rowSums(.[c:g]),
      PERofET_inPrecip = (actET/Precip*100),
      PERofRD1_inQ = (Annual_RD1/Total_Discharge*100),
      PERofRD2_inQ = (Annual_RD2/Total_Discharge*100),
      PERofRG1_inQ = (Annual_RG1/Total_Discharge*100),
      PERofRG2_inQ = (Annual_RG2/Total_Discharge*100),
      PERofIce_inGlaRunoff = (Ice_runoff/Glacier_runoff*100),
      PERofSnowMelt_G_inGlaRunoff = (SnowMelt_G/Glacier_runoff*100),
      PERofRainRunof_inGlaRunoff = (Rain_runoff/Glacier_runoff*100)
    )

  #Visulization of the Input and Output Variables
  yr <- which(colnames(WaterBalance3)== "year")
  WaterBal3 = select (WaterBalance3, -yr)

  WaterBal3 <- WaterBal3 %>%
    summarise_all(funs(mean))

  WatBalance <- WaterBal3 %>%
    gather(Input,Output,Glacier_runoff, key="WatBal", value="Value")

  WatBalance <- reshape2::melt(WatBalance, id.vars="WatBal")

  a1 <- filter(WatBalance, WatBal == "Input" & variable == "Precip")
  a2 <- filter(WatBalance, WatBal == "Input" & variable == "Ice_runoff")
  a3 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RD1")
  a4 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RD2")
  a5 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RG1")
  a6 <- filter(WatBalance, WatBal == "Output" & variable == "Annual_RG2")
  a7 <- filter(WatBalance, WatBal == "Output" & variable == "actET")
  a8 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "SnowMelt_G")
  a9 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "Ice_runoff")
  a10 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "Rain_runoff")
  a11 <- filter(WatBalance, WatBal == "Glacier_runoff" & variable == "Snowmelt_Out_G")

  WatBalance2 <- rbind(a1, a2,a3,a4, a5, a6, a7,a8,a9,a10,a11)

  cols <- c(Precip="deepskyblue", Ice_runoff="blue4",
            Annual_RD1="brown", Annual_RD2="darkgoldenrod4", Annual_RG1= "goldenrod3", Annual_RG2= "dimgray", actET= "chocolate3",
            SnowMelt_G="darkolivegreen3", Ice_runoff="blue4", Rain_runoff="gold", Snowmelt_Out_G= "green")

  title <- as_labeller(c(Input="Input",Output="Output", Glacier_runoff="Snow and Glaicer"))

  return(ggplot(WatBalance2, aes(variable,value)) +
           geom_col(position="identity", fill= cols) +
           scale_color_manual(values = cols)+
           theme_bw()+ ylab("mm") + xlab("") +
           facet_wrap(.~WatBal, scales= "free", labeller= title))
}
######################################end of J2K_WatBalMajorComps #############################################





#'Export the data
#'
#'Export the summary data in the working folder.The quick overview is displayed in the console. The supplimnet informaiton you would get is the percentage of the different variables. For example: yearly percentage of glacier in runoff. percentage of snowrunoff, evapotranspiration etc.
#'
#'@param TimeLoop Based on the timeloop file the yearly value of the different variables and their percentage are saved as .csv file in the Output folder of the model
#'
#'@return Saves the summary of the waterbalance in the folder
#'
#'@examples
#'J2K_WatBalsummarySave<- write.csv("Info_On_Water_Balance.csv" in workingFolder)
#'
#'@export

J2K_WatBalsummarySave <- function(){
  #read header names
  variable_head <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #read data
  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  timeloop <- timeloop[,1:length(variable_head)]
  #assign column names
  colnames(timeloop) <- c("Date",variable_head[-1])
  timeloop$Date <- as.Date(timeloop$Date, format= "%Y-%m-%d")
  timeloop$year <- format(as.Date(timeloop$Date), format= "%Y")

  #For the analysis of the Input and output variables
  #Removing the years with less than 360 years of data
  timeloop <- timeloop %>%
    add_count(year) %>%
    filter(n > 360)

  #Visulization of the Input and Output variables
  WaterBalance2 <- timeloop %>%
    group_by(year)%>%
    summarise(
      Precip = sum(precip),
      Ice_runoff = sum(iceRunoff),
      actET = sum(actET),
      Annual_RD1 = sum(catchmentRD1_w),
      Annual_RD2 = sum(catchmentRD2_w),
      Annual_RG1 = sum(catchmentRG1_w),
      Annual_RG2 = sum(catchmentRG2_w),
      Snowmelt_Out_G = sum(snowMelt),
      Glacier_runoff = sum(glacierRunoff), #glacierRunoff=snowmeltg+icerunoff+rainrunoff
      SnowMelt_G = sum(snowMelt_G), #plus iceRunoff
      Rain_runoff = sum(rainRunoff)
    )

  a <- which(colnames(WaterBalance2)== "Precip")
  b <- which(colnames(WaterBalance2)== "Ice_runoff")
  c <- which(colnames(WaterBalance2)== "actET")
  d <- which(colnames(WaterBalance2)== "Annual_RD1")
  e <- which(colnames(WaterBalance2)== "Annual_RD2")
  f <- which(colnames(WaterBalance2)== "Annual_RG1")
  g <- which(colnames(WaterBalance2)== "Annual_RG2")
  h <- which(colnames(WaterBalance2)== "Snowmelt_Out_G")

  WaterBalance3 <- WaterBalance2 %>%
    mutate(
      Input = rowSums(.[a:b]),
      Total_Discharge = rowSums(.[d:g]),
      Output = rowSums(.[c:g]),
      PERofET_inPrecip = (actET/Precip*100),
      PERofRD1_inQ = (Annual_RD1/Total_Discharge*100),
      PERofRD2_inQ = (Annual_RD2/Total_Discharge*100),
      PERofRG1_inQ = (Annual_RG1/Total_Discharge*100),
      PERofRG2_inQ = (Annual_RG2/Total_Discharge*100),
      PERofIce_inGlaRunoff = (Ice_runoff/Glacier_runoff*100),
      PERofSnowMelt_G_inGlaRunoff = (SnowMelt_G/Glacier_runoff*100),
      PERofRainRunof_inGlaRunoff = (Rain_runoff/Glacier_runoff*100),
      PERofSnowOutG_inQ = (Snowmelt_Out_G/Total_Discharge*100)
    )
  #Saving the information as a csv file in the same folder
  print(write.csv(WaterBalance3, paste(workingFolder,"Info_On_Water_Balance.csv"), row.names=TRUE))

  print("The FILE is SAVED is saved in the workingFolder")
  #Selecting some parameters to display in the Console at the end
  WaterBalance4 <- WaterBalance3 %>%
    select(year,PERofET_inPrecip, PERofIce_inGlaRunoff, PERofSnowOutG_inQ)%>%
    mutate_if(is.numeric, round, 2)

  #Displaying the percentage in the Console itself
  print(kable(WaterBalance4, format = "pandoc", caption = "Brief summary of Water-balance in percentage"))
}
################################end of the J2K_WatBalsummarySave #################################################




#' Snow Cover Visulization
#'
#'Helps in the visulization of the daily snow cover area
#'
#'@param TimeLoop Based on the timeLoop of J2K model
#'
#'@return visual of MODIS and model snow cover area daily
#'
#'@examples
#'J2K_snowcoverTS <- plot(x=days, y=snow cover area )
#'
#'@export

J2K_snowcoverTS <- function(){
  #read header
  #workingFolder<- "C:\\Users\\kkhatiwada\\Dropbox\\J2000_Panjshir\\Panjshir_Hybrid_4snow\\"
  header <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #get the timeloop
  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  #keep the length of the timeloop same as header lenthg
  timeloop <- timeloop[,1:length(header)]
  #keep the header names
  colnames(timeloop) <- c(header)

  #extract the month and years
  timeloop$Dates <- as.Date(timeloop$ID, "%Y-%m-%d")
  timeloop$Year <- format(timeloop$Dates,"%Y")
  timeloop$Month <- format(timeloop$Dates,"%m")

  timeloop$modisSnow[timeloop$modisSnow=="Inf"]<-NA

  #timeseries plot
  analysis1 <-	timeloop %>%
    mutate(MODISMovingMean = (rollmean(modisSnow, 8, fill = NA, partial=TRUE)/1000000),
           J2KMovingMean = (rollmean(snowCoverAreaT, 8, fill = NA,  partial=TRUE)/1000000))

  return(ggplot(analysis1, aes(Dates)) +
           geom_line(aes(y=MODISMovingMean, colour="red"))+
           geom_line(aes(y=J2KMovingMean, colour="black" )) + theme_bw() +
           ylab("snow cover area (km2)") + xlab("") +
           scale_colour_discrete(name = "Legend", labels = c("MODIS", "J2Kmodel")))
}
##############################################end of the J2K_snowcoverTS ########################################





#'Monthly snow cover area (km2)
#'
#'Helps in the visulization of the monthly average snow cover area. This will be particularly useful if someone is trying to observe the change in each month.
#'
#'@param TimeLoop It considers the input as MODIS and the output from J2K model and makes a comparative plot
#'
#'@return Monthly snow cover area information
#'
#'@examples
#'J2K_MonthMeanSC <- plot(x=years, y=monthly area)
#'
#'@export

J2K_MonthMeanSC <- function(){
  #read header
  header <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #get the timeloop
  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  #keep the length of the timeloop same as header lenthg
  timeloop <- timeloop[,1:length(header)]
  #keep the header names
  colnames(timeloop) <- c(header)

  #extract the month and years
  timeloop$Dates <- as.Date(timeloop$ID, "%Y-%m-%d")
  timeloop$Year <- format(timeloop$Dates,"%Y")
  timeloop$Month <- format(timeloop$Dates,"%m")

  timeloop$modisSnow[timeloop$modisSnow=="Inf"]<-NA

  #timeseries plot
  analysis1 <-	timeloop %>%
    mutate(MODISMovingMean = (rollmean(modisSnow, 8, fill = NA, partial=TRUE)/1000000),
           J2KMovingMean = (rollmean(snowCoverAreaT, 8, fill = NA,  partial=TRUE)/1000000))

  #for monthly visulization in line graph
  analysis2 <- analysis1 %>%
    group_by(Month) %>%
    summarize (MODIS =mean(MODISMovingMean, na.rm=TRUE),
               J2Kmodel = mean(J2KMovingMean, na.rm=TRUE))
  #
  AnnualPlot <- reshape2::melt(analysis2, id.vars='Month')
  return(ggplot(data = AnnualPlot, aes(x = Month, y = value, 	colour = variable, group = variable)) + geom_line()+  geom_point()+
           theme_bw()+ ylab("Snow cover area (km2)") + xlab("Months") + scale_y_continuous(labels = comma))

}
###############################################end of the J2K_MonthMeanSC ########################################




#'Annual sum of total snow cover area (km2)
#'
#'Helps in the visulization of annual snow cover area. This will be particularly useful if someone is trying to observe the total change in volume in a particular area.
#'
#'@param TimeLoop It considers the input as MODIS and the output from J2K model and makes a comparative plot
#'
#'@return Annual snow cover area information
#'
#'@examples
#'J2K_AnnualSumSC <- plot(x=years, y=totalarea)
#'
#'@export

J2K_AnnualSumSC <- function(){
  #read header
  header <- unlist(strsplit(scan(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),"",nlines = 1,skip = 5,sep = "\n"),split = "\t"))
  #get the timeloop
  timeloop <- fread(paste(workingFolder, "output\\current\\TimeLoop.dat",sep=""),skip = 10)
  #keep the length of the timeloop same as header lenthg
  timeloop <- timeloop[,1:length(header)]
  #keep the header names
  colnames(timeloop) <- c(header)

  #extract the month and years
  timeloop$Dates <- as.Date(timeloop$ID, "%Y-%m-%d")
  timeloop$Year <- format(timeloop$Dates,"%Y")
  timeloop$Month <- format(timeloop$Dates,"%m")

  timeloop$modisSnow[timeloop$modisSnow=="Inf"]<-NA

  #timeseries plot
  analysis1 <-	timeloop %>%
    mutate(MODISMovingMean = (rollmean(modisSnow, 8, fill = NA, partial=TRUE)/1000000),
           J2KMovingMean = (rollmean(snowCoverAreaT, 8, fill = NA,  partial=TRUE)/1000000))

  #for monthly visulization in line graph
  analysis2 <- analysis1 %>%
    group_by(Month) %>%
    summarize (MODIS =mean(MODISMovingMean, na.rm=TRUE),
               J2Kmodel = mean(J2KMovingMean, na.rm=TRUE))

  #for annual visulization in bar plot
  analysis3 <-	timeloop %>%
    group_by(Year) %>%
    summarize (MODIS =sum(modisSnow, na.rm=TRUE),
               J2Kmodel = sum(snowCoverAreaT, na.rm=TRUE))

  AnnualPlot2 <- reshape2::melt(analysis3, id.vars='Year')
  return(ggplot(data = AnnualPlot2, aes(x = Year, y = value, fill = variable, group = variable)) + geom_col(stat='identity', position='dodge')+
           theme_bw()+ ylab("Sum of the total area (km2)") + xlab("Years") + scale_y_continuous(labels = comma))

}
###################################end of the J2K_AnnualSumSC ####################################################






#'Monthly precipitation and discharge ( mm)
#'
#'This function helps in the visulization of the precipitation and discharge in the same unit(mm). Daily average value of the provided station/gauge point is computed to form the montly value of precipitaiton vs discharge
#'
#'@param input Considers the input value for the J2K model
#'
#'@return Helps in the monthly visulization of the P vs Q  in same unit
#'
#'@examples
#'J2K_RainVsRunoffmmMonthly <- plot(x=years, y=value (mm))
#'
#'@export

J2K_RainVsRunoffmmMonthly <- function(){
  # reading Observed runoff
   Date_format <- readline("Some of the date format are %d.%m.%Y; %Y-%m-%d. What is date format of your data?")
  runoff2 <- fread(paste(workingFolder,"input\\local\\orun.dat",sep=""), fill= TRUE, skip = 16)
  #converting to date format
  runoff2$Date <- as.Date(runoff2$V1, format= Date_format)

  #removing the time if kept in new column
  runoff2$Orun <- as.numeric(runoff2$V2)

  runoff <-  runoff2 %>% dplyr::select("Date", "Orun")

  #creating table of month and year for the analysis latter
  runoff$year <- format(runoff$Date, format= "%Y")
  runoff$month <- format(runoff$Date, format= "%m")
  #treat -9999 as NA
  runoff <- na_if(runoff, -9999)


  # reading Observed precipitation
  precip2 <- fread(paste(workingFolder,"input\\local\\rain.dat",sep=""),skip = 16)

  #fixing the issue of having time as a column
  precip2$V2 <- as.numeric(precip2$V2)

  ##calulating the average of precipitation of the area.
  precip2 <- na_if(precip2, -9999)
  precip2$Avgp = rowMeans(precip2[,-1], na.rm=TRUE)

 #creating a new column with the provided date
  precip2$Date <- as.Date(precip2$V1, format= Date_format)

   precip <-  precip2 %>% dplyr::select("Date", "Avgp")


  #joining the file with the runoff file
	InPQ <- runoff %>%
   dplyr::left_join(precip, by = "Date")


  #for converting the discharge to mm
  header4 <- unlist(strsplit(scan(paste(workingFolder,"parameter\\hrus.par",sep=""),"",skip = 1, nlines = 1, sep = "\n"),split = "\t"))
  hrus4area <- fread(paste(workingFolder,"parameter\\hrus.par",sep=""),skip = 5)
  colnames(hrus4area) <- c(header4)


  #getting the total  area
  BasinArea <- sum(hrus4area$area, na.rm = TRUE)
  #converting Q to mm and creating a new column runoffinmm
  PnQq <- InPQ %>%
    dplyr::mutate(runoffinmm = Orun*1000*24*3600/BasinArea)

  PnQ <- PnQq %>%
    add_count(year) %>%
    filter(n > 360)

  ##plots
  #annual average plot
  AnnualMPlot <- PnQ %>%
    group_by(month) %>%
    summarise(Monthly_average_Precip =mean(Avgp, na.rm=TRUE)*30,
              Monthly_average_Runoff = mean(runoffinmm,na.rm=TRUE)*30)

  AnnualPlot2 <- reshape2::melt(AnnualMPlot, id.vars='month')

  return(ggplot(AnnualPlot2, aes(x=month, y=value, fill=variable)) +
           geom_bar(stat='identity', position='dodge') +
           scale_fill_manual(values=c("#56B4E9","#E69F00"), "Legend") +
           theme_classic()+ ylab("mm") + xlab("Months"))
}
#######################################end of the J2K_RainVsRunoffmmMonthly #######################################







#'Annual sum of precipitation and discharge in mm
#'
#'This function helps in the visulization of annual of both the precipitation and discharge in the same unit(mm). Daily average precipitation from all the available station is computed to form the annual sum of the precipitaiton vs discharge of provided discharge station.
#'
#'@param input Considers the input value for the J2K model
#'
#'@return Helps in the annual visulization of the P vs Q  in same unit
#'
#'@examples
#'J2K_RainVsRunoffmmYearly <- plot(x=years, y=value (mm))
#'
#'@export

J2K_RainVsRunoffmmYearly <- function(){
  # reading Observed runoff
  Date_format <- readline("Some of the date format are %d.%m.%Y; %Y-%m-%d. What is date format of your data?")
  runoff2 <- fread(paste(workingFolder,"input\\local\\orun.dat",sep=""), fill= TRUE, skip = 16)

  #converting to date format and selecting the discharge
  runoff2$Date <- as.Date(runoff2$V1, format= Date_format)

  runoff2$Orun <- as.numeric(runoff2$V2)

  runoff <-  runoff2 %>% dplyr::select("Date", "Orun")

  #creating table of month and year for the analysis latter
  runoff$year <- format(runoff$Date, format= "%Y")
  runoff$month <- format(runoff$Date, format= "%m")
  #treat -9999 as NA
  runoff <- na_if(runoff, -9999)


  # reading Observed precipitation
  precip2 <- fread(paste(workingFolder,"input\\local\\rain.dat",sep=""),skip = 16)

  #fixing the issue of having time as a column
  precip2$V2 <- as.numeric(precip2$V2)

  ##calulating the average of precipitation of the area.
  precip2 <- na_if(precip2, -9999)
  precip2$Avgp = rowMeans(precip2[,-1], na.rm=TRUE)

 #creating a new column with the provided date
  precip2$Date <- as.Date(precip2$V1, format= Date_format)

   precip <-  precip2 %>% dplyr::select("Date", "Avgp")


  #joining the file with the runoff file
InPQ <- runoff %>%
   dplyr::left_join(precip, by = "Date")


  #for converting the discharge to mm
  header4 <- unlist(strsplit(scan(paste(workingFolder,"parameter\\hrus.par",sep=""),"",skip = 1, nlines = 1, sep = "\n"),split = "\t"))
  hrus4area <- fread(paste(workingFolder,"parameter\\hrus.par",sep=""),skip = 5)
  colnames(hrus4area) <- c(header4)


  #getting the total  area
  BasinArea <- sum(hrus4area$area, na.rm = TRUE)
  #converting Q to mm and creating a new column runoffinmm
  PnQq <- InPQ %>%
    dplyr::mutate(runoffinmm = Orun*1000*24*3600/BasinArea)

  PnQ <- PnQq %>%
    add_count(year) %>%
    filter(n > 360)

  ##plots
  #annual average plot
  AnnualPlot <- PnQ %>%
    group_by(year) %>%
    summarise(Annual_Precip =mean(Avgp, na.rm=TRUE)*360,
              Annual_Runoff = mean(runoffinmm, na.rm=TRUE)*360 )

  AnnualPlot <- reshape2::melt(AnnualPlot, id.vars='year')

  return(ggplot(AnnualPlot, aes(x=year, y=value, fill=variable)) +
           geom_bar(stat='identity', position='dodge') +
           scale_fill_manual(values=c("#56B4E9","#E69F00"), "Legend") +
           theme_classic()+ ylab("mm") + xlab("Years"))
}
##########################################end of the J2K_RainVsRunoffmmYearly #####################################








#'Fixing the Non-Glacial to Glacial Routing issue
#'
#'This will help to idenify the HRU ID which were providing the water from the non-glacier area to the glacier area and also solve the issue and save the file.
#'
#'@param hrus Considers the hrus for the J2K model
#'
#'@return Helps in solving the non-glacier to glacier routing issue and saving the improved file
#'
#'@examples
#'J2K_FixmyNG2G <- write.csv(improved file, in the working folder)
#'
#'@export

J2K_FixmyNG2G <- function(){

header22 <- unlist(strsplit(scan(paste(workingFolder,"parameter\\hrus.par",sep=""),"",skip = 1, nlines = 1, sep = "\n"),split = "\t"))
hrus4NG2Gg <- fread(paste(workingFolder,"parameter\\hrus.par",sep=""),skip = 5)
hrus4NG2G <- head(hrus4NG2Gg,-1)
colnames(hrus4NG2G) <- c(header22)


#identifying the glacier IDs
Gid <-   hrus4NG2G[which(hrus4NG2G$landuseID == 222),]
#idenifying the nOnglacier IDs
NGid <-   hrus4NG2G[which(hrus4NG2G$landuseID < 222),]

# identifying the to_ploy which have been proving water to glacial ID
okeyID <- hrus4NG2G[ which(NGid$to_poly %in% Gid$ID ),]

messedID <-   NGid[ which(hrus4NG2G$ID %in% okeyID$ID),]

#making the to_poly which provided water to glcier iD as zero
messedID$to_poly <- 0
messedID$to_reach <- messedID$subbasin

#idenifying the IDs that were messing around by proving water to the glacier ID
M <- messedID$ID

#removing the IDs which were supplying water to glacier. THis is just to add the new one that do not do so
hrus4NG2G22 <- hrus4NG2G[!(hrus4NG2G$ID %in% messedID$ID),]

hrus4NG2G3 = bind_rows(hrus4NG2G22, messedID)

#hrus4NG2G3o <- hrus4NG2G3[order(hrus4NG2G3[,1]), ]

#Saving the information as a csv file in the same WorkingFolder
 print(write.csv(hrus4NG2G3, paste(workingFolder,"UpdatedHRUsV1.csv"), row.names=TRUE))

 print("Please find the updated HRUS file in the workingFolder. For your information, following HRU_ID were messing with you: ")

  #Displaying the percentage in the Console itself
  print(M)
 }

