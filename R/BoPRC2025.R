
TaurangaTides<-read.csv("Book1.csv")
usethis::use_data(TaurangaTides, internal = TRUE, overwrite = TRUE)


##################################################################################
#                               GENERAL FUNCTIONS                                #
##################################################################################

Write.Excel <- function(x,row.names=FALSE,col.names=TRUE) {
  if (row.names==FALSE)
    write.table(x,"clipboard-16384",sep="\t",row.names=FALSE,col.names=TRUE)
  if (row.names==TRUE)
    write.table(x,"clipboard-16384",sep="\t",row.names=TRUE,col.names=NA)
  if(col.names==FALSE)
    write.table(x,"clipboard-16384",sep="\t",row.names=row.names,col.names=FALSE)
}

##################################################################################

Convert.Columns <- function(x, y=NA) {
  for(i in 1:length(y)) {
    if (y[i] == "numeric") {
      x[i] <- as.numeric(x[[i]])
    }
    else if(y[i] == "character"){
      x[i] <- as.character(x[[i]])
    }
    else if(y[i] == "factor"){
      x[i] <- as.factor(x[[i]])
    }
    else if(y[i] == "integer"){
      x[i] <- as.integer(x[[i]])
    }

    else{
      stop("Incorrect class type. For more information on this error, run: ?Convert.Columns")
    }
  }
  return(x)
}

##################################################################################

Hazen.Percentile  <- function(input,percentile){
  if(class(input)!="integer"& class(input)!="numeric"){
    stop("Data class must be either numeric or integer. For more information on this error,  run: ?Hazen.Percentile")
  }

  if(class(percentile)!="numeric" & class(percentile)!="integer"){
    stop("Percentile class must be either numeric or integer. For more information on this error, run: ?Hazen.Percentile")
  }

  input <- input[!is.na(input)]
  rawdatasort=sort(input, decreasing = FALSE)
  smallp = percentile / 100
  n=length(input)

  if(smallp < 0.5){
    nmin = 1 / (2 * smallp) + 0.00001

  } else{
    nmin = 1 / (2 * (1 - smallp)) + 0.00001
  }

  if(nmin > n){
    stop("Not Enough Data. For more information on this error, run: ?Hazen.Percentile")

  }

  rHazen = 0.5 + smallp * n
  ri = floor(rHazen)
  rf = rHazen - ri
  HazenPercentile = as.numeric((1 - rf) * rawdatasort[ri] + rf * rawdatasort[ri+1])
  return(HazenPercentile)

}

##################################################################################

Percentile_Plot <- function(Percentile){
  require(ggplot2)
  df <- data.frame(quartile=c("Q1","Q2","Q3","Q4"),x=c(1,1,1,1),y=c(25,25,25,25))

  if(!is.na(Percentile)){
    ggplot()+
      geom_bar(aes(x=x,y=y,fill=quartile), data=df,stat = "identity")+
      coord_flip()+
      ylab("")+
      geom_point(aes(x=1,y=Percentile),size=5,shape=21,fill="#3d3d29")+
      scale_fill_manual(values=c("#00A2E1", "#62BD19", "#FFC726","#FF671F"))+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      scale_y_continuous(breaks = seq(0,100, by = 5))+
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(),legend.position = "none")+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(), panel.grid.minor = element_blank())
  }else{
    ggplot()+
      geom_bar(aes(x=x,y=y,fill=quartile), data=df,stat = "identity")+
      coord_flip()+
      ylab("")+
      #geom_point(aes(x=1,y=Percentile),size=5,shape=21,fill="#3d3d29")+
      scale_fill_manual(values=c("#00A2E1", "#62BD19", "#FFC726","#FF671F"))+
      theme(axis.text.x=element_text(angle=45,hjust=1))+
      scale_y_continuous(breaks = seq(0,100, by = 5))+
      theme(axis.title.y = element_blank(),axis.text.y = element_blank(), axis.ticks.y = element_blank(),legend.position = "none")+
      theme(panel.background = element_blank(),
            panel.grid = element_blank(), panel.grid.minor = element_blank())

  }
}


# do
Percentile_Calc <- function(df, colnum, value){
  p <- ecdf(as.numeric(as.character(df[,colnum])))
  return(p(value)*100)
}

##################################################################################



##################################################################################
#                                NOF FUNCTIONS                                   #
##################################################################################

NOFLakesPhytoplankton <- function(data, time=Sys.time(), start="", end=""){
  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFLakesPhytoplankton")
  }

  # if(!("Chloro a (mg/m^3)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFLakesPhytoplankton")
  # }

  as.Date(time)
  #extract the current year
  nowy<-year(time)
  #extract the current month
  nowm<-month(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data<- data[with(data, Time> start & Time < end), ]
  }

  #if it is currently past july
  else if (nowm>7){
    #take the current year and minus 1
    lasty<-nowy-1
    #start date is 1st of July, 1 years ago
    starte<-ISOdate(lasty,7,1, tz=Sys.timezone())
    #end date is 30th of June this year
    ende<-ISOdate(nowy,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }

  #if it is currently before july
  else if(nowm<7){
    #take the current year and minus 1 (last year)
    lastyear<-nowy-1
    #take last year and minus 1
    startyear<-lastyear-1
    #start date is 1st of July, 1 years before last year (2 years ago)
    starty<-ISOdate(startyear,7,1, tz=Sys.timezone())
    #end date is 30th of June, last year
    endy<-ISOdate(lastyear,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starty & Time <endy),]
  }




  names(data) <- c("ID","Name","Time","Value")



  ChlASummary<- data %>% group_by(Name) %>% summarise("Minimum" = min(Value,na.rm = TRUE),
                                                      "Maximum" = max(Value,na.rm = TRUE), "Median" = median(Value,na.rm = TRUE),
                                                      n = length(Value[!is.na(Value)]))




  for (i in 1:nrow(ChlASummary)){
    MedianBand <- ifelse(ChlASummary[i,4] <=2,1,ifelse(ChlASummary[i,4]<=5,2,ifelse(ChlASummary[i,4]<=12,3,4)))
    MaximumBand<- ifelse(ChlASummary[i,3] <=10,1,ifelse(ChlASummary[i,3]<=25,2,ifelse(ChlASummary[i,3]<=60,3,4)))
    CombBandNo <- ifelse(MedianBand >= MaximumBand,MedianBand,MaximumBand)
    FinalBand  <- ifelse(CombBandNo == 1, "A", ifelse(CombBandNo == 2 , "B", ifelse(CombBandNo == 3, "C", "D")))
    ChlASummary$AttributeBand[i]  <- FinalBand

  }

  return(ChlASummary)

}

##################################################################################

NOFLakesRiversCyanobacteria<-function(data,time=Sys.Date(), start="", end="") {

  if(ncol(data)!=5){
    stop("Incorrect data frame. For more information on this error, run: ?NOFLakesPhytoplankton")
  }

  # if(!(c("Total Cyanobacteria (mm^3/l)","Potentially Toxic Cyanobacteria (mm^3/l)") %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFLakesPhytoplankton")
  # }

  require(lubridate)
  require(dplyr)
  as.Date(time)
  #extract the current year
  nowy<-year(time)
  #extract the current month
  nowm<-month(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }

  #if it is currently past july
  else if (nowm>7){
    #take the current year and minus 3
    lasty<-nowy-3
    #start date is 1st of July, 3 years ago
    starte<-ISOdate(lasty,7,1, tz=Sys.timezone())
    #end date is 30th of June this year
    ende<-ISOdate(nowy,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }

  #if it is currently before july
  else if(nowm<7){
    #take the current year and minus 1 (last year)
    lastyear<-nowy-1
    #take last year and minus 3
    startyear<-lastyear-3
    #start date is 1st of July, 3 years before last year (4 years ago)
    starty<-ISOdate(startyear,7,1, tz=Sys.timezone())
    #end date is 30th of June, last year
    endy<-ISOdate(lastyear,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starty & Time <endy),]
  }




  names(data)<-c("ID","Name","Time","Cyano","PT_Cyano")

  cyanosummary<- data %>% group_by(Name) %>% summarise(PToxic=quantile(PT_Cyano, 0.8,na.rm=TRUE),
                                                       NToxic=quantile(Cyano, 0.8, na.rm=TRUE),  ptoxicband=ifelse(PToxic>1.8, "D", ifelse(PToxic>1, "C", ifelse(PToxic>0.5,"B","A"))),
                                                       ntoxicband=ifelse(NToxic>10,"D", ifelse(NToxic>1.8, "C", ifelse(NToxic>0.5, "B", "A"))))
  for (i in nrow(cyanosummary)){

    finalband<- ifelse(cyanosummary$NToxic<=1, cyanosummary$ntoxicband, ifelse(cyanosummary$PToxic>1,cyanosummary$ptoxicband, cyanosummary$ntoxicband))
    cyanosummary$AttributeBand<-finalband[i]
    numericband<- ifelse(cyanosummary$NToxic<=1.0, cyanosummary$NToxic, ifelse(cyanosummary$PToxic>1.0, cyanosummary$PToxic, cyanosummary$NToxic))
    cyanosummary$NumericBand<-numericband[i]
    stat<-ifelse(cyanosummary$NToxic<=1.0, "Total Biovolume", ifelse(cyanosummary$PToxic>1.0, "Potentially Toxic Biovolume", "Total Biovolume"))
    cyanosummary$Stat<-stat[i]
  }
  return(cyanosummary)
}

##################################################################################

NOFLakesTN <- function(data, laketype="Stratified", time=Sys.Date(), start="", end=""){
  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFLakesTN")
  }

  # if(!("N (Tot) (g/m^3)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFLakesTN")
  # }

  if(!(laketype == "Polymictic"|laketype == "polymictic"|laketype == "Stratified"|laketype == "stratified" )){
    stop("Incorrect laketype used. For more information on this error, run: ?NOFLakesTN")
  }

  require(lubridate)
  require(dplyr)
  as.Date(time)
  #extract the current year
  nowy<-year(time)
  #extract the current month
  nowm<-month(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }

  #if it is currently past july
  else if (nowm>7){
    #take the current year and minus 1
    lasty<-nowy-1
    #start date is 1st of July, 3 years ago
    starte<-ISOdate(lasty,7,1, tz=Sys.timezone())
    #end date is 30th of June this year
    ende<-ISOdate(nowy,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }

  #if it is currently before july
  else if(nowm<7){
    #take the current year and minus 1 (last year)
    lastyear<-nowy-1
    #take last year and minus 1
    startyear<-lastyear-1
    #start date is 1st of July, 1 years before last year (1 years ago)
    starty<-ISOdate(startyear,7,1, tz=Sys.timezone())
    #end date is 30th of June, last year
    endy<-ISOdate(lastyear,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starty & Time <endy),]
  }

  names(data) <- c("ID","Name","Time","Value")
  data$Value <- data$Value*1000

  TNSummary<- data %>% group_by(Name) %>% summarise("Minimum" = min(Value,na.rm = TRUE),
                                                    "Maximum" = max(Value,na.rm = TRUE), "Median" = median(Value,na.rm = TRUE),
                                                    n = length(Value[!is.na(Value)]))

  for (i in 1:nrow(TNSummary)){
    if (laketype == "Polymictic"|laketype == "polymictic"){
      FinalBand<- ifelse(TNSummary[i,4] <=300,"A",ifelse(TNSummary[i,4]<=500,"B",ifelse(TNSummary[i,4]<=800,"C","D")))

    }else{

      FinalBand<- ifelse(TNSummary[i,4] <=160,"A",ifelse(TNSummary[i,4]<=350,"B",ifelse(TNSummary[i,4]<=750,"C","D")))

    }
    TNSummary$AttributeBand[i]  <- FinalBand

  }
  return(TNSummary)
}

##################################################################################

NOFLakesTP <- function(data, time=Sys.Date(), start="", end="") {

  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFLakesTP")
  }

  # if(!("P (Tot) (g/m^3)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFLakesTP")
  # }

  require(lubridate)
  require(dplyr)
  as.Date(time)
  #extract the current year
  nowy<-year(time)
  #extract the current month
  nowm<-month(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }

  #if it is currently past july
  else if (nowm>7){
    #take the current year and minus 1
    lasty<-nowy-1
    #start date is 1st of July, 3 years ago
    starte<-ISOdate(lasty,7,1, tz=Sys.timezone())
    #end date is 30th of June this year
    ende<-ISOdate(nowy,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }

  #if it is currently before july
  else if(nowm<7){
    #take the current year and minus 1 (last year)
    lastyear<-nowy-1
    #take last year and minus 1
    startyear<-lastyear-1
    #start date is 1st of July, 1 years before last year (2 years ago)
    starty<-ISOdate(startyear,7,1, tz=Sys.timezone())
    #end date is 30th of June, last year
    endy<-ISOdate(lastyear,6,30, tz=Sys.timezone())

    data<-data[with(data, Time> starty & Time <endy),]
  }

  names(data) <- c("ID","Name","Time","Value")
  data$Value <- data$Value*1000

  TPSummary<- data %>% group_by(Name) %>% summarise("Minimum" = min(Value,na.rm = TRUE),
                                                    "Maximum" = max(Value,na.rm = TRUE), "Annual Median" = median(Value,na.rm = TRUE),
                                                    n = length(Value[!is.na(Value)]))

  for (i in 1:nrow(TPSummary)){
    AttributeBand = ifelse(TPSummary[i,4] <=10,"A",ifelse(TPSummary[i,4]<=20,"B",ifelse(TPSummary[i,4]<=50,"C","D")))
    TPSummary$AttributeBand[i]<-AttributeBand
  }


  return(data.frame(TPSummary))

}

##################################################################################


NOFLakesRiversNH3 <- function (data, start = "", end = ""){


  # if(!("Ammoniacal N (g/m^3)" %in% names(data))){
  #   print("Ammoniacal N parameter must be used. For more information on this error, run: ?NOFLakesRiversNH3")
  # }

  #if(class(adjust)!="logical"){
  # stop("Adjust arguement must be logical. For more information on this error, run: ?NOFLakesRiversNH3")
  # }


  require(dplyr)
  if (ncol(data)==5) {

    # if(!("pH (pH Units)" %in% names(data))){
    #   print("pH parameter must be used. For more information on this error, run: ?NOFLakesRiversNH3")
    # }

    names(data) <- c("ID", "Name", "Time", "Value", "pH")
    data$pH <- round(data$pH, 1)
    less <- data[data$pH < 6, ]
    less$Adjusted = less$Value * 2.86
    less <- less[, c(1, 2, 3, 6)]
    greater <- data[data$pH > 9, ]
    greater$Adjusted <- greater$Value * 0.2
    greater <- greater[, c(1, 2, 3, 6)]
    pH <- c(6, 6.1, 6.2, 6.3, 6.4, 6.5, 6.6, 6.7, 6.8, 6.9,
            7, 7.1, 7.2, 7.3, 7.4, 7.5, 7.6, 7.7, 7.8, 7.9, 8,
            8.1, 8.2, 8.3, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 9)
    Ratio <- c(2.86, 2.84, 2.82, 2.8, 2.77, 2.73, 2.7, 2.64,
               2.59, 2.51, 2.42, 2.32, 2.21, 2.09, 1.94, 1.79, 1.63,
               1.47, 1.31, 1.14, 1, 0.87, 0.73, 0.62, 0.53, 0.44,
               0.38, 0.32, 0.27, 0.23, 0.2)
    reftable <- data.frame(pH = as.numeric(pH), Ratio = as.numeric(Ratio))
    mergedata <- merge(data[, c("ID", "Name", "Time", "Value",
                                "pH")], reftable, by = "pH")
    mergedata$Adjusted <- mergedata$Value/mergedata$Ratio
    data <- mergedata
    data <- data[, c(2, 3, 4, 7)]
    data <- rbind(data, greater, less)
    print("Data adjusted to pH 8.0 and temperature of 20deg")
  }
  else {
    print("Data not adjusted")
  }

  require(lubridate)
  require(dplyr)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }


  data <- data[,c(1:4)]
  names(data) <- c("ID", "Name", "Time", "Value")
  if (nchar(start) > 1) {
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),
                        tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),
                      tz = "Etc/GMT+12")
    data <- data[with(data, Time > start & Time < end),
    ]
  }

  data$Value[data$Value<0]<-0

  NH3Summary <- data %>% group_by(Name) %>% summarise(Minimum = min(Value,
                                                                    na.rm = TRUE), Maximum = max(Value, na.rm = TRUE), Median = median(Value,
                                                                                                                                       na.rm = TRUE), n = length(Value[!is.na(Value)]))
  for (i in 1:nrow(NH3Summary)) {
    MedianBand <- ifelse(NH3Summary[i, 4] <= 0.03, 1, ifelse(NH3Summary[i,
                                                                        4] <= 0.24, 2, ifelse(NH3Summary[i, 4] <= 1.3, 3,
                                                                                              4)))
    MaximumBand <- ifelse(NH3Summary[i, 3] <= 0.05, 1, ifelse(NH3Summary[i,
                                                                         3] <= 0.4, 2, ifelse(NH3Summary[i, 3] <= 2.2, 3,
                                                                                              4)))
    CombBandNo <- ifelse(MedianBand >= MaximumBand, MedianBand,
                         MaximumBand)
    FinalBand <- ifelse(CombBandNo == 1, "A", ifelse(CombBandNo ==
                                                       2, "B", ifelse(CombBandNo == 3, "C", "D")))
    NH3Summary$AttributeBand[i] <- FinalBand
  }
  return(data.frame(NH3Summary))
}


##################################################################################

NOFRiversNO32 <- function (data, time=Sys.Date(), start="", end=""){
  require(dplyr)

  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFRiversNO3")
  }

  # if(!("Nitrite Nitrate (as N) (g/m^3)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFRiversNO3")
  # }

  as.Date(time)
  #extract the current year
  nowy<-year(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }


  else {
    #take last year
    lastyear<-nowy-1
    #take last year and minus 1
    fouryears<-lastyear-1
    #start date is 1st of Jan, 1 years ago
    starte<-ISOdate(fouryears,1,1, tz=Sys.timezone())
    #end date is 31st of Dec this year
    ende<-ISOdate(lastyear,12,31, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }

  names(data) <- c("ID", "Name", "Time", "Value")

  NO3Summary <- data %>% group_by(Name) %>% summarise(Minimum = min(Value,
                                                                    na.rm = TRUE), Maximum = max(Value, na.rm = TRUE), Median = median(Value,
                                                                                                                                       na.rm = TRUE), Percentile95 = quantile(Value, 0.95, na.rm = TRUE),
                                                      n = length(Value[!is.na(Value)]))
  for (i in 1:nrow(NO3Summary)) {
    MedianBand <- ifelse(NO3Summary[i, 4] <= 1, 1, ifelse(NO3Summary[i,
                                                                     4] <= 2.4, 2, ifelse(NO3Summary[i, 4] <= 6.9, 3,
                                                                                          4)))
    PercentileBand <- ifelse(NO3Summary[i, 5] <= 1.5, 1,
                             ifelse(NO3Summary[i, 5] <= 3.5, 2, ifelse(NO3Summary[i,
                                                                                  5] <= 9.8, 3, 4)))
    CombBandNo <- ifelse(MedianBand >= PercentileBand, MedianBand,
                         PercentileBand)
    FinalBand <- ifelse(CombBandNo == 1, "A", ifelse(CombBandNo ==
                                                       2, "B", ifelse(CombBandNo == 3, "C", "D")))
    NO3Summary$AttributeBand[i] <- FinalBand
  }
  return(data.frame(NO3Summary))
}

##################################################################################

NOFLakesRiversECOLI <- function (data,time=Sys.Date(), start = "", end = ""){
  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFLakesRiversECOLI")
  }

  # if(!("E coli (cfu/100ml)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFLakesRiversECOLI")
  # }

  require(dplyr)
  as.Date(time)
  #extract the current year
  nowy<-year(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }

  else {
    #take last year
    lastyear<-nowy-1
    #take last year and minus 1
    fouryears<-lastyear-1
    #start date is 1st of Jan, 2 years ago
    starte<-ISOdate(fouryears,1,1, tz=Sys.timezone())
    #end date is 31st Dec last year
    ende<-ISOdate(lastyear,12,31, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }
  names(data) <- c("ID", "Name", "Time", "Value")
  if (nchar(start) > 1) {
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),
                        tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"), tz = "Etc/GMT+12")
    data <- data[with(data, Time > start & Time < end),
    ]
  }
  ECOLISummary <- data %>% group_by(Name, ID) %>% summarise(
    n = length(Value[!is.na(Value)]),
    Mean = mean(Value,na.rm = TRUE),
    Median = median(Value,na.rm = TRUE),
    Percentile95 = quantile(Value, 0.95, na.rm = TRUE),
    PercentExceed540 = sum(Value >540, na.rm = TRUE)/length(Value[!is.na(Value)]) *100,
    PercentExceed260 = sum(Value >260, na.rm = TRUE)/length(Value[!is.na(Value)]) *100,
    PercentLess540 = sum(Value <=540, na.rm = TRUE)/length(Value[!is.na(Value)]) *100,
    ProposedBand = ifelse(PercentExceed540 <5 & Median <= 130 & Percentile95 <= 540 & PercentExceed260 <20, "A",
                          ifelse(PercentExceed540 <= 10 & Median <= 130 & Percentile95 <= 1000 & PercentExceed260 < 30,"B",
                                 ifelse(PercentExceed540 <= 20 & Median <=130 & Percentile95 <= 1200 & PercentExceed260 < 34, "C",
                                        ifelse(PercentExceed540 <= 30 & Median > 130 & Percentile95 > 1200 & PercentExceed260 > 34, "D",
                                               ifelse(PercentExceed540 > 30 & Median > 260 & Percentile95 > 1200 & PercentExceed260 >50, "E",
                                                      ECOLI_Banding(PercentExceed540,PercentExceed260,Median,Percentile95,err_mtd)))))),
    `ProposedSwimmable?` = ifelse(ProposedBand =="A" || ProposedBand == "B" || ProposedBand =="C", "Swimmable","Not Swimmable"),
    PercentLess280_Marine = sum(Value <=280, na.rm = TRUE)/length(Value[!is.na(Value)]) *100
  )
  return(ECOLISummary)
}



##################################################################################

ECOLI_Banding <- function(PercentExceed540,PercentExceed260,Median,Percentile95,method="max"){
  Band540 <- ifelse(PercentExceed540<5, 1,
                    ifelse(PercentExceed540<=10, 2,
                           ifelse(PercentExceed540<=20, 3,
                                  ifelse(PercentExceed540<=30,4,5))))
  Band260 <- ifelse(PercentExceed260<20,1,
                    ifelse(PercentExceed260<=30,2,
                           ifelse(PercentExceed260<=34,3,
                                  ifelse(PercentExceed260<50,4,5))))
  BandMedian <- ifelse(Median<=130,2,
                       ifelse(Median<260,4,5))

  BandPercentile95 <- ifelse(Percentile95<540,1,
                             ifelse(Percentile95<1000,2,
                                    ifelse(Percentile95<1200,3,4.5)))


  if (method == "max" | method == "MAX"| method == "Max"){
    Band=ifelse(round(max(Band540,Band260,BandMedian,BandPercentile95),0)==1,"A",
                ifelse(round(max(Band540,Band260,BandMedian,BandPercentile95),0)==2,"B",
                       ifelse(round(max(Band540,Band260,BandMedian,BandPercentile95),0)==3,"C",
                              ifelse(round(max(Band540,Band260,BandMedian,BandPercentile95),0)==4,"D","E"))))
  }else if(method == "mean" | method == "MEAN"| method == "Mean"){
    Band=ifelse(round(mean(Band540,Band260,BandMedian,BandPercentile95),0)==1,"A",
                ifelse(round(mean(Band540,Band260,BandMedian,BandPercentile95),0)==2,"B",
                       ifelse(round(mean(Band540,Band260,BandMedian,BandPercentile95),0)==3,"C",
                              ifelse(round(mean(Band540,Band260,BandMedian,BandPercentile95),0)==4,"D","E"))))


  }else if(method == "95th" | method == "95TH"){

    Band = ifelse(Percentile95<540,"A",
                  ifelse(Percentile95<1000,"B",
                         ifelse(Percentile95<1200,"C","D")))

  }else{

    stop("Incorrect method. For for information on this error, run: ?Ecoli_Banding")
  }

  return(Band)

}

##################################################################################

NOFRiversPeriphyton<-function(data, time=Sys.Date(), start="", end="",class="Default"){
  require(dplyr)

  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFRiversPeriphyton")
  }

  # if(!("Chloro Periphyton (mg/m^2)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFRiversPeriphyton")
  # }

  as.Date(time)
  #extract the current year
  nowy<-year(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }


  else {
    #take last year
    lastyear<-nowy-1
    #take last year and minus 3
    fouryears<-lastyear-3
    #start date is 1st of January, 3 years ago
    starte<-ISOdate(fouryears,1,1, tz=Sys.timezone())
    #end date is 31th of December last year
    ende<-ISOdate(lastyear,12,31, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }

  names(data) <- c("ID","Name","Time","Value")


  PeriphytonSummary<- data %>% group_by(Name) %>% summarise(Minimum = min(Value,na.rm = TRUE),
                                                            Maximum = max(Value,na.rm = TRUE), Median = median(Value,na.rm = TRUE),
                                                            Percentile92 = quantile(Value,0.92,na.rm = TRUE), Percentile83=quantile(Value, 0.83, na.rm=TRUE),
                                                            n = length(Value[!is.na(Value)]))

  if(class=="Default"|class=="default"){
    for (i in 1:nrow(PeriphytonSummary)){
      FinalBand<-ifelse(PeriphytonSummary[i,4] <=50,"A",ifelse(PeriphytonSummary[i,4]<=120,"B",ifelse(PeriphytonSummary[i,4]<=200,"C","D")))
      PeriphytonSummary$AttributeBand[i]<-FinalBand
    }

  }
  else if(class=="Productive"|class=="productive"){
    for (i in 1:nrow(PeriphytonSummary)){
      FinalBand<-ifelse(PeriphytonSummary[i,5] <=50,"A",ifelse(PeriphytonSummary[i,5]<=120,"B",ifelse(PeriphytonSummary[i,5]<=200,"C","D")))
      PeriphytonSummary$AttributeBand[i]<-FinalBand
    }

  }

  else{
    stop("Incorrect class. For more information on this error, run. ?NOFRiversPeriphyton")
  }

  return(data.frame(PeriphytonSummary))
}


##################################################################################

NOFRiversDRP <- function(data, time=Sys.Date(),start="", end=""){
  require(dplyr)

  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFRiversDRP")
  }

  # if(!("DRP (g/m^3)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFRiversDRP")
  # }

  as.Date(time)
  #extract the current year
  nowy<-year(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }


  else {
    #take last year
    lastyear<-nowy-1
    #take last year and minus 5
    sixyears<-lastyear-5
    #start date is 1st of January, 5 years ago
    starte<-ISOdate(sixyears,1,1, tz=Sys.timezone())
    #end date is 31th of Decemeber, last year
    ende<-ISOdate(lastyear,12,31, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }


  names(data) <- c("ID","Name","Time","Value")
  DRPSummary<- data %>% group_by(Name) %>% summarise("Minimum" = min(Value,na.rm = TRUE),
                                                     "Maximum" = max(Value,na.rm = TRUE), "Median" = median(Value,na.rm = TRUE),
                                                     Percentile95 = quantile(Value,0.95, na.rm=TRUE),
                                                     n = length(Value[!is.na(Value)]))

  for(i in 1:nrow(DRPSummary)){
    attribute95<-ifelse(DRPSummary[i,5]<=0.021,1, ifelse(DRPSummary[i,5]<=0.03, 2, ifelse(DRPSummary[i,5]<=0.054, 3, 4)))
    attributeMed<-ifelse(DRPSummary[i,4]<=0.006,1, ifelse(DRPSummary[i,4]<=0.01,2, ifelse(DRPSummary[i,4]<=0.018,3,4)))
    finalattribute<-ifelse(max(c(attributeMed,attribute95))==1,"A", ifelse(max(c(attributeMed,attribute95))==2,"B",ifelse(max(c(attributeMed,attribute95))==3,"C","D")))
    DRPSummary$AttributeBand[i]<-finalattribute
  }

  return(data.frame(DRPSummary))
}

##################################################################################

NOFRiversSFS <- function(data, time=Sys.Date(), start="", end="", class){
  require(dplyr)

  if(ncol(data)!=4){
    stop("Incorrect data frame. For more information on this error, run: ?NOFRiversSFS")
  }

  # if(!("Water Clarity (m)" %in% names(data))){
  #   print("Incorrect parameter used. For more information on this error, run: ?NOFRiversSFS")
  # }

  as.Date(time)
  #extract the current year
  nowy<-year(time)

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }


  else {
    #take last year
    lastyear<-nowy-1
    #take last year and minus 5
    sixyears<-lastyear-5
    #start date is 1st of January, 5 years ago
    starte<-ISOdate(sixyears,1,1, tz=Sys.timezone())
    #end date is 31th of Decemeber, last year
    ende<-ISOdate(lastyear,12,31, tz=Sys.timezone())

    data<-data[with(data, Time> starte & Time <ende),]
  }
  names(data) <- c("ID","Name","Time","Value")

  SFSSummary<- data %>% group_by(Name) %>% summarise("SSCClass"=class,"Minimum" = min(Value,na.rm = TRUE),
                                                     "Maximum" = max(Value,na.rm = TRUE), "Median" = median(Value,na.rm = TRUE),
                                                     n = length(Value[!is.na(Value)]))

  for(i in 1:nrow(SFSSummary)){

    if(class == 1){
      attributeband<-ifelse(SFSSummary[i,5]>=1.78, "A", ifelse(SFSSummary[i,5]>=1.55, "B", ifelse(SFSSummary[i,5]>1.34, "C","D")))
      SFSSummary$AttributeBand[i]<-attributeband

    }else if(class == 2){
      attributeband<-ifelse(SFSSummary[i,5]>=0.93,"A", ifelse(SFSSummary[i,5]>=0.76, "B", ifelse(SFSSummary[i,5]>0.61,"C","D")))
      SFSSummary$AttributeBand[i]<-attributeband

    }else if(class == 3){

      attributeband<-ifelse(SFSSummary[i,5]>=2.95,"A",ifelse(SFSSummary[i,5]>=2.57,"B", ifelse(SFSSummary[i,5]>2.22,"C","D")))
      SFSSummary$AttributeBand[i]<-attributeband

    }else if(class == 4){
      attributeband<-ifelse(SFSSummary[i,5]>=1.38, "A", ifelse(SFSSummary[i,5]>=1.17, "B", ifelse(SFSSummary[i,5]>0.98, "C","D")))
      SFSSummary$AttributeBand[i]<-attributeband
    }
    else{
      stop("Incorrect class. FOr more information on this error, run: ?NOFRiversSFS")
    }

  }
  return(data.frame(SFSSummary))
}

##################################################################################

NOFAll<- function(site, start="", end="", time=Sys.Date(),periclass="default", TNlaketype="Stratified", SFSClass=1){

  if(class(site)!="character"){
    stop("Incorrect site input. For more information on this error, run: ?NOFAll")
  }

  if(site %in% NERMN_River()){
    peri<-if("Chloro Periphyton" %in% LocationWQParameters(site)){
      perid<-AQMultiExtract(site,"Chloro Periphyton", start, end,time)
      NOFRiversPeriphyton(perid, class=periclass)$AttributeBand
    }

    ammo<-if("Ammoniacal N" %in% LocationWQParameters(site)){
      ammod<-AQMultiExtract(site, c("Ammoniacal N","pH"),start, end)
      NOFLakesRiversNH3(ammod)$AttributeBand

    }

    nitra<-if("Nitrite Nitrate (as N)" %in% LocationWQParameters(site)){
      nitrad<-AQMultiExtract(site, "Nitrite Nitrate (as N)",start, end, time)
      NOFRiversNO3(nitrad)$AttributeBand
    }

    sedi<-if("Water Clarity" %in% LocationWQParameters(site)){
      sedid<-AQMultiExtract(site,"Water Clarity", start, end, time)
      NOFRiversSFS(sedid, class=SFSClass)$AttributeBand
    }

    ecoli<-if ("E coli" %in% LocationWQParameters(site)) {
      ecolid<-AQMultiExtract(site,"E coli",start, end, time)
      NOFLakesRiversECOLI(ecolid)$ProposedBand
    }

    dphos<-if("DRP" %in% LocationWQParameters(site)){
      dphosd<-AQMultiExtract(site, "DRP",start,end, time)
      NOFRiversDRP(dphosd)$AttributeBand
    }

    out<-list(peri,sedi,ammo,nitra,ecoli,dphos)
    names(out)<-c("Periphyton NOF Band","Suspended Fine Sediment NOF Band", "Ammonia NOF Band", "Nitrate NOF Band","E Coli NOF Band","Dissolved  Reactive Phosphorus NOF ")
    out<-out[!sapply(out,is.null)]


  }

  else if(site %in% lakelist()){
    chloro<-if("Chloro a" %in% LocationWQParameters(site)){
      chlorod<-AQMultiExtract(site, "Chloro a",start,end, time)
      NOFLakesPhytoplankton(chlorod)$AttributeBand
    }

    nitro<-if ("N (Tot)" %in% LocationWQParameters(site)) {
      nitrod<-AQMultiExtract(site,"N (Tot)", start, end, time)
      NOFLakesTN(nitrod,  laketype=TNlaketype)$AttributeBand
    }

    phos<-if("P (Tot)" %in% LocationWQParameters(site)) {
      phosd<-AQMultiExtract(site, "P (Tot)",start,end, time)
      NOFLakesTP(phosd)$AttributeBand
    }

    ammo<-if("Ammoniacal N" %in% LocationWQParameters(site)){
      ammod<-AQMultiExtract(site, c("Ammoniacal N","pH"),start, end)
      NOFLakesRiversNH3(ammod)$AttributeBand

    }

    ecoli<-if ("E coli" %in% LocationWQParameters(site)) {
      ecolid<-AQMultiExtract(site,"E coli",start, end, time)
      NOFLakesRiversECOLI(ecolid)$ProposedBand
    }

    cyano<-if(c("Total Cyanobacteria","Potentially Toxic Cyanobacteria")  %in% LocationWQParameters(site)){
      cyanod<-AQMultiExtract(site,c("Total Cyanobacteria", "Potentially Toxic Cyanobacteria"), start, end )
      NOFLakesRiversCyanobacteria(cyanod)$ProposedBand
    }

    out<-list(chloro,nitro, phos,ammo,ecoli, cyano)
    names(out)<-c("PhytoPlankton NOF Band","Total Nitrogen NOF Band","Total Phosphuros NOF Band", "Ammonia NOF Band","E Coli NOF Band",  "Cyanobacteria NOF Band")
    out<-out[!sapply(out,is.null)]
  }

  else{
    stop("Site not found. For more information on this error, run: ?NOFAll")
  }


  return(as.data.frame(out, row.names=c(site,"x"))[1,])

}


##################################################################################



##################################################################################
#                               BPU FUNCTIONS                                    #
##################################################################################

BPU_List <- function(BPU){

  if(BPU == "Non_VA/Gentle") {
    return(c("EP623312"))
  }else if (BPU == "Non_VA/Steep"){
    return(c("RN123610",
             "RO629568",
             "QM756918",
             "OK300616",
             "SO991920",
             "NK608503",
             "ML715056",
             "UP337950",
             "QJ471191"))
  }else if (BPU == "VA/Gentle"){
    return(c("EL204387",
             "GI540308",
             "EK778375",
             "EL613536",
             "FO497605",
             "FN834668",
             "FL356693",
             "FO620177",
             "GO089653",
             "DP784306",
             "DO406909",
             "FI639597",
             "FI629587",
             "HL003442",
             "GI416337",
             "CO543022",
             "EL174017",
             "MK788545",
             "FL230406",
             "FI655592",
             "FI663580",
             "DO047598",
             "FD445529",
             "GN849464",
             "GM781934",
             "GN922883",
             "HM024983",
             "EK598179",
             "JI148319",
             "IG265664",
             "FC231176",
             "JL350292",
             "JM977587",
             "GJ662805",
             "BQ711622",
             "BQ723939",
             "EK405487",
             "DO686858",
             "DO712717",
             "FK014535",
             "CQ105067",
             "EK335717",
             "CP466747",
             "DP281304",
             "CO938527",
             "EL204137",
             "DL358586",
             "KL919939",
             "HL046466",
             "HL150416",
             "MK307635",
             "FO761142"))
  }else if(BPU == "VA/Steep"){
    return(c("BQ966369",
             "FJ527888",
             "IN515245",
             "IN224385",
             "JK491452",
             "JM102399",
             "IK564876",
             "IL663193",
             "IL818464",
             "IL711163",
             "LI953392",
             "LK149881",
             "BQ708712",
             "BR809582",
             "BS961133",
             "HN674689",
             "EO451883",
             "KL998150",
             "LK082095",
             "LL020847",
             "FK338089",
             "FJ325943",
             "LM111224",
             "KM938159",
             "IK604969",
             "BQ739463",
             "IG691428"))
  }else {
    stop("Biophysical unit not recognised. For more information on this error, run: ?BPU_List")
  }
}

##################################################################################

BPU_Check <- function(SiteID){
  if(SiteID %in% BPU_List("Non_VA/Gentle")==TRUE){
    return("Non_VA/Gentle")
  }else if(SiteID %in% BPU_List("Non_VA/Steep")==TRUE){
    return("Non_VA/Gentle")
  }else if(SiteID %in% BPU_List("VA/Gentle")==TRUE){
    return("VA/Gentle")
  }else if(SiteID %in% BPU_List("VA/Steep")==TRUE){
    return("VA/Steep")
  }else{
    stop("Unknown biophysical unit. For more information on this error, run: ?BPU_Check")
  }
}



##################################################################################
#                         SWIMMABILITY FUNCTIONS                                 #
##################################################################################

MAC <- function(data, start="",end="",Type = "Freshwater"){

  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }


  require(dplyr)

  names(data) <- c("ID", "Name", "Time", "Value")

  MAC_Summary <- data %>% group_by(Name, ID) %>% summarise(n=length(Value[!is.na(Value)]),
                                                           min=min(Value,na.rm = T),
                                                           max=max(Value,na.rm = T),
                                                           median=median(Value,na.rm = T),
                                                           Perc_95=Hazen.Percentile(Value, 95))
  for(i in nrow(MAC_Summary)){

    if(Type == "Freshwater"|Type == "freshwater"|Type == "FW"){


      Band = ifelse(Hazen.Percentile(data$Value, 95)=="Not Enough Data","Not Enough Data",
                    ifelse(Hazen.Percentile(data$Value, 95)<=130,"A",
                           ifelse(Hazen.Percentile(data$Value, 95)<=260,"B",
                                  ifelse(data$Hazen.Percentile(Value, 95)<=550,"C","D"))))

      MAC_Summary$AttributeBand[i]<-Band
    }



    else if (Type == "Marine"|Type == "marine"){


      Band = ifelse(Hazen.Percentile(data$Value, 95)=="Not Enough Data","Not Enough Data",
                    ifelse(Hazen.Percentile(data$Value, 95)<=40,"A",
                           ifelse(Hazen.Percentile(data$Value, 95)<=200,"B",
                                  ifelse(Hazen.Percentile(data$Value, 95)<=500,"C","D"))))
      MAC_Summary$AttributeBand[i]<-Band

    }




  }

  return(MAC_Summary)
}

##################################################################################


FW_Action_Levels <- function(data ,start="",end="", tme=Sys.Date()){

  #if start and end specified subset between them
  if(nchar(start)>1){
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"),tz = "Etc/GMT+12")
    data <- data[with(data, Time> start & Time < end), ]
  }

  names(data) <- c("ID", "Name", "Time", "Value")

  FWAL_Summary <- data %>% group_by(Name)%>% summarise(n=length(data$Value[!is.na(data$Value)]),
                                                       min=min(data$Value,na.rm = T),
                                                       max=max(data$Value,na.rm = T),
                                                       median=median(data$Value,na.rm = T),
                                                       Perc_95=Hazen.Percentile(data$Value, 95))

  for(i in nrow(FWAL_Summary)){
    Percent_Green <- (sum(data$Value <= 260,na.rm = T)/length(data$Value[!is.na(data$Value)]))*100
    Percent_Green<-format(round(Percent_Green,2),nsmall=2)
    Percent_Amber <- (sum(data$Value > 260 & data$Value <= 550,na.rm = T)/length(data$Value[!is.na(data$Value)]))*100
    Percent_Amber<-format(round(Percent_Amber,2),nsmall=2)
    Percent_Red <- (sum(data$Value > 550,na.rm = T)/length(data$Value[!is.na(data$Value)]))*100
    Percent_Red<-format(round(Percent_Red,2),nsmall=2)

    FWAL_Summary$Percent_Green[i]<-Percent_Green
    FWAL_Summary$Percent_Amber[i]<-Percent_Amber
    FWAL_Summary$Percent_Red[i]<-Percent_Red

  }



  return(FWAL_Summary)

}




##################################################################################
#                              SITE FUNCTIONS                                    #
##################################################################################

SSC_Check<-function(site){
  G1<-c("BS961133","BR809582", "BQ723939","BQ708712","BQ711622","BQ739463","BQ966369",
        "CP466747","DP784306","DP281304","DO406909","EO451883","DO686858", "DO712717",
        "DO047598","CO938527","CO543022","GN922883","HN674689", "GN849464","GM781934",
        "JL350292","EL174017","JK491452","EK598179","JI148319","IG265664","FD445529",
        "FC231176")
  G2<-c("EP623312","SO991920","RO629568","ML715056","MK307635")
  G3<-c("GO081642","FO620177","FN834668", "RN123610","QM756918","JM102399","FL356693",
        "FL230406", "KL998150","LK149881","IK555889","OK300616","NK608503","LK082095",
        "GJ662805","QJ471191","LI953392","IG691428")
  G4<-c()

  if(site %in% G1){
    class<-1
  }
  else if(site %in% G2){
    class<-2
  }
  else if(site %in% G3){
    class<-3
  }
  else{
    return("Site not found")
  }

  return(class)
}

##################################################################################

NERMN_GroundWater<- function(){
  return(c("GO204332","FO211523_5","FO211523_1","FO211523_4","FO211523_3","FO211523_2",
           "GO023321_1","GO023321_2","FN053142","GO176312","FN053142","GO176312",
           "FN142982","GN170486","GN605663","GN126544","GN071972","FB471317","FB471317_1",
           "FC188130","GC284749","FC495835","JH108605_47","JH108605_48","JH108605_49",
           "JL624269","JL080754", "IL899951","JM752520","FK779059","FJ656900","FJ442517",
           "GI753566_52","GI753566_53","GI439843","GI156361","EJ981312","FK380119","FJ141820",
           "DL854507","EL141079","FK137414","EL275448","FK317861","DL916315","DK584766",
           "GJ663803","FP328107_73m","FP328107_25m","FP328107_45m","CP689847","CS035256",
           "DP806150","EP650170","BQ995748","BQ625302","CR253391","DP695698","BQ541760_1",
           "BR797412_1","BR949011_1","CQ275568","DP547219","DP547217","DP184539","NL873636",
           "NK967982","NL621629","NL622630","NL552065","NL551065","NL552066","NL891140",
           "NK650949","HN665704","JL100526","IM856109"))
}

###################################################################################

FlowSites<-function(){
  return(c("IG692446","DK982984","OK300616","NJ774061","MI036134","LL080607","JJ552757",
           "JM102399","IL891937","GN562808","FO620177","FN834668","FN448171","FL334833",
           "FL150407","FK014535","BR518499","BQ766632","CP353091","CO923457","CO884445",
           "DO690531","EO564565","HN389361","JL350292","LM970794","MK307635","EK406461",
           "EK630244","FC231176","EK378429","FO153520","DO406909"))
}

##################################################################################

NERMN_Estuary<-function(){
  return(c("GO429546","GO661503","ML081799","ML335393","EP020617","EP118190","CQ947053",
           "CR059778","CR301357","DP547739","DP912601","DP952985","LM227254","KM083686",
           "JM306916","NL493611","ML922670"))
}

##################################################################################

NERMN_River<- function(){
  return(c("BQ966369","RN123610","FN834668","FL356693","FO620177","GO081642","RO629568",
           "DP784306","DO406909","QM756918","QJ471191","CO543022","EL174017","MK307635",
           "FL230406","DO047598","FD445529","OK300616","GN849464","GM781934","GN922883",
           "EK598179","JI148319","JK491452","IG265664","FC231176","JL350292","SO991920",
           "EP623312","JM102399","IK555889","IK564876","GJ662805","LI953392","LK149881",
           "BQ708712","BQ708712","BR809582","BQ723939","BS961133","DO686858","DO712717",
           "NK608503","ML715056","CP466747","DP281304","CO938527","HN674689","EO451883",
           "BQ739463","KL998150","LK082095","IG691428","IL818464","KL919939","IL891937",
           "FO761142", "GI416337"))
}

##################################################################################

Geothermal_Sites<-function(){
  return(c("EK459448","EK512407","EK532195", "EK604303","EK441200"))
}

##################################################################################

NERMN_Lake<-function(){
  intergrated<-list(Intergrated=c("JK448422_INT","HL508168_INT","HL030557_INT","GL051366_INT",
                                  "FL479468_INT","FL274353_INT","EL805168_INT","EK783777_INT",
                                  "GK015589_INT","FK427028_INT",
                                  "FJ164707_INT","FJ295574","FJ988648_INT", "GI084907_INT",
                                  "GI617624_INT","FI680541_INT"))
  hypolimnion<-list(Hypolimnion=c("JK448422_HYP","EL805168_HYP","EK783777_HYP", "FK427028_HYP",
                                  "FJ164707_HYP", "FJ988648_HYP", "GI084907_HYP","FI680541_HYP"))
  bottom<-list(Bottom=c("JK448422_BOT","EL805168_BOT","EK783777_BOT","FK427028_BOT",
                        "FJ164707_BOT","FJ988648_BOT", "GI084907_BOT", "GI617924_BOT",
                        "FI680541_HYP"))
  discrete<-list(Discrete=c("EL805168_DIS","EK783777_DIS"))

  mylist<-list(intergrated,hypolimnion,bottom,discrete)
  return(mylist)
}

###################################################################################

lakelist<-function(){
  return(c("JK448422_INT","HL508168_INT","HL030557_INT","GL051366_INT","FL479468_INT",
           "FL274353_INT","EL805168_INT","EK783777_INT","GK015589_INT","FK427028_INT",
           "FJ164707_INT","FJ295574","FJ988648_INT", "GI084907_INT", "GI617624_INT",
           "FI680541_INT","JK448422_HYP","EL805168_HYP","EK783777_HYP", "FK427028_HYP",
           "FJ164707_HYP", "FJ988648_HYP", "GI084907_HYP","FI680541_HYP",
           "JK448422_BOT","EL805168_BOT","EK783777_BOT","FK427028_BOT","FJ164707_BOT",
           "FJ988648_BOT", "GI084907_BOT", "GI617924_BOT", "FI680541_HYP","EL805168_DIS",
           "EK783777_DIS"))
}




Bathing_River <- function(){
  return(c("EK537123","EL192023","EL143137","EK405487","EK260170","FL230406","EL613536","HL149406","RO629568",
                                "NL683503","RN123610","NK608503","NL517414","LK445461","LK082095","DP768284","DP281304","BQ723939",
                                "BR748451","JL348334","JK491452","IG265664","KM909138","KM083686","GN922883",
                                "FO397216","CO809137","EO564565","BQ708712"))
  }

Bathing_Lake <- function(){
  return(c("HL337241","GL606421","GL314263","FL289316","EK935598","EL224087","EL438512","FK325034","FJ157807",
                "FJ737728","FI660574","GI442508","HL129560"))
}

#################################################################################


##################################################################################
#                               TIME SERIES                                      #
##################################################################################

Continuous_TS <- function(data,parameters,gap){

  names(data) <- c("Site","LocationName","Time","Variable","Value")
  Min_date <- as.Date(Sys.time())
  Max_date <- as.Date(max(data$Time))


  for(i in parameters){

    #get list of dates
    dat_sub <- as.Date(data[data$Variable==i,3])
    #order the list of dates
    dat_sub <- dat_sub[order(dat_sub,decreasing = T)]
    #determine differences between each date and the one previous
    dat_diff <-as.numeric(diff(dat_sub))

    #only calculate for datasets that end within the 5 years of the most recent data, i.e. ignore old datasets.
    if(Max_date - max(dat_sub) < (5*365)){

      #are any date differences greater than the allowable gap
      if(min(dat_diff)<(-gap)){

        #find out the first occurance that is greater than the allowable gap
        first_exceed <- which(dat_diff< (-gap))
        #find out the date this corresponds to
        first_exceed_date <- dat_sub[min(first_exceed)]
        #see if first exceed date is older than the current min date.  If so, take the first exceed date
        Min_date <- if_else(first_exceed_date<Min_date,first_exceed_date,Min_date)

      }else{
        #what is the oldest date of this dataset?
        oldest_date <- min(dat_sub)
        #is the oldest date older than the current min date?  If so, take the oldest date.
        Min_date <- if_else(oldest_date<Min_date,oldest_date,Min_date)

      }
      #move on to the next dataset
    }

  }
  return(Min_date)

}

##################################################################################

##################################################################################
#                              BATHING SEASON                                    #
##################################################################################

Bathing_Season <- function (DateStamp){
  DateStamp<-as.Date(DateStamp)
  Year <- format(DateStamp, format = "%Y")
  Month <- format(DateStamp, format = "%m")
  NewStamp <- character(length(DateStamp))
  for (i in 1:length(DateStamp)) {
    if (Month[i] == "07" | Month[i] == "08" | Month[i] ==
        "09" | Month[i] == "10" | Month[i] == "11" | Month[i] ==
        "12") {
      if (Year[i] == "1999") {
        NewStamp[i] <- "1999/00"
      }
      else {
        NewStamp[i] <- paste(as.numeric(Year[i]), "/",
                             sprintf("%02d", as.numeric(format(DateStamp[i],
                                                               format = "%y")) + 1), sep = "")
      }
    }
    else {
      NewStamp[i] <- paste(as.numeric(Year[i]) - 1, "/",
                           sprintf("%02d", as.numeric(format(DateStamp[i],
                                                             format = "%y"))), sep = "")
    }
  }
  return(NewStamp)
}

##################################################################################


##################################################################################
#                                SFRG TABLE                                      #
##################################################################################

SFRG_Table <- function (data, start = "", end = "",err_mtd="max",output){
  require(dplyr)
  names(data) <- c("ID", "Name", "Time", "Value")
  if (nchar(start) > 1) {
    start <- as.POSIXct(strptime(start, "%Y-%m-%d"),
                        tz = "Etc/GMT+12")
    end <- as.POSIXct(strptime(end, "%Y-%m-%d"), tz = "Etc/GMT+12")
    data <- data[with(data, Time > start & Time < end),
    ]
  }
  ECOLISummary <- data %>% group_by(Name, ID) %>% summarise(
    n = length(Value[!is.na(Value)]),
    Mean = mean(Value,na.rm = TRUE),
    Median = median(Value,na.rm = TRUE),
    Percentile95 = quantile(Value, 0.95, na.rm = TRUE),
    PercentExceed540 = sum(Value >540, na.rm = TRUE)/length(Value[!is.na(Value)]) *100,
    PercentExceed260 = sum(Value >260, na.rm = TRUE)/length(Value[!is.na(Value)]) *100,
    PercentLess540 = sum(Value <=540, na.rm = TRUE)/length(Value[!is.na(Value)]) *100,
    ProposedBand = ifelse(PercentExceed540 <5 & Median <= 130 & Percentile95 <= 540 & PercentExceed260 <20, "A",
                          ifelse(PercentExceed540 <= 10 & Median <= 130 & Percentile95 <= 1000 & PercentExceed260 < 30,"B",
                                 ifelse(PercentExceed540 <= 20 & Median <=130 & Percentile95 <= 1200 & PercentExceed260 < 34, "C",
                                        ifelse(PercentExceed540 <= 30 & Median > 130 & Percentile95 > 1200 & PercentExceed260 > 34, "D",
                                               ifelse(PercentExceed540 > 30 & Median > 260 & Percentile95 > 1200 & PercentExceed260 >50, "E",
                                                      ECOLI_Banding(PercentExceed540,PercentExceed260,Median,Percentile95,err_mtd)))))),
    `ProposedSwimmable?` = ifelse(ProposedBand =="A" || ProposedBand == "B" || ProposedBand =="C", "Swimmable","Not Swimmable"),
    PercentLess280_Marine = sum(Value <=280, na.rm = TRUE)/length(Value[!is.na(Value)]) *100
  )

  ECOLISummary$Comment <- ifelse(ECOLISummary$n >= 50 & ECOLISummary$n < 60,"*",
                                 ifelse(ECOLISummary$n <50 ,"**",""))
  ECOLISummary$FinalBand <- paste(ECOLISummary$ProposedBand,ECOLISummary$Comment,sep="")


  ECOLISummary$Mean <- sprintf("%.1f",round(ECOLISummary$Mean,1))
  ECOLISummary$Median <- sprintf("%.1f",round(ECOLISummary$Median,1))
  ECOLISummary$Percentile95 <- sprintf("%.1f",round(ECOLISummary$Percentile95,1))
  ECOLISummary$PercentLess540 <- sprintf("%.1f",round(ECOLISummary$PercentLess540,1))
  ECOLISummary$PercentLess280_Marine <- sprintf("%.1f",round(ECOLISummary$PercentLess280_Marine,1))

  if(output == "KPI"){
    return(ECOLISummary[,c(1:3,7:8,5:6,10:11)])
  }else{
    return(ECOLISummary[,c(1:6,9,12,14)])
  }
}

###################################################################################



##################################################################################
#                             TIDAL FUNCTIONS                                    #
##################################################################################


TidalFromDate <- function (date,SecondaryPort = "none")
{

  library(lubridate)
  library(tidyverse)


  if(SecondaryPort == "Bowentown"){
    TideData <- readRDS("Tide_Datasets/Bowentown.Rds")
  }else if(SecondaryPort == "Kauri Point"){
    TideData <- readRDS("Tide_Datasets/KauriPoint.Rds")
  }else if(SecondaryPort == "Maketu"){
    TideData <- readRDS("Tide_Datasets/Maketu.Rds")
  }else if(SecondaryPort == "Ohope"){
    TideData <- readRDS("Tide_Datasets/Ohope.Rds")
  }else if(SecondaryPort == "Omokoroa"){
    TideData <- readRDS("Tide_Datasets/Omokoroa.Rds")
  }else if(SecondaryPort == "Opotiki"){
    TideData <- readRDS("Tide_Datasets/Opotiki.Rds")
  }else if(SecondaryPort == "Rangitaiki"){
    TideData <- readRDS("Tide_Datasets/Rangitaiki.Rds")
  }else if(SecondaryPort == "Town Wharf"){
    TideData <- readRDS("Tide_Datasets/TownWharf.Rds")
  }else if(SecondaryPort == "Whakatane"){
    TideData <- readRDS("Tide_Datasets/Whakatane.Rds")
  }else{
    TideData <- readRDS("Tide_Datasets/Tauranga.Rds")
  }

  names(TideData) = c("Date", "Height", "TideHeight")
  TideData$Date <- as.POSIXlt(TideData$Date,tz="etc/GMT+12")

  date <- as.POSIXlt(date,tz="etc/GMT+12")
  tidalsummary <- data.frame(Time = date)

  for (i in 1:nrow(tidalsummary)) {
    hamish <- min(as.numeric(abs(difftime(TideData$Date,
                                          (as.POSIXlt(date[i],tz="etc/GMT+12")), units = "secs"))))
    closestdatedown <- as.POSIXlt((as.POSIXlt(date[i],tz="etc/GMT+12") -
                                     hamish),tz="etc/GMT+12")
    closestdatedown <- as.POSIXlt(closestdatedown,tz="etc/GMT+12")
    closestdatedown <- as.character(closestdatedown)
    closestdateup <- as.POSIXlt((as.POSIXlt(date[i],tz="etc/GMT+12") + hamish),tz="etc/GMT+12")
    closestdateup <- as.POSIXlt(closestdateup,tz="etc/GMT+12")
    closestdateup <- as.character(closestdateup)
    TideData$Date <- as.character(TideData$Date)
    if ((closestdateup[i]) %in% (TideData$Date)) {
      x <- closestdateup[i]
      j <- TideData %>% filter(Date == x) %>% select(TideHeight)
      b <- TideData[which(TideData$Date == (closestdateup[i])) +
                      c(-1:0), ]
      hh <- b[1, 2]
      jj <- b[2, 2]
      first <- as.numeric(difftime(date[i], (as.POSIXlt(b[1,1],tz="etc/GMT+12")), unit = "hours"))
      secondd <- as.numeric(difftime((as.POSIXlt(b[2, 1],tz="etc/GMT+12")),
                                     (as.POSIXlt(b[1, 1],tz="etc/GMT+12")), unit = "hours"))
      phase <- ((first)/(secondd) * pi)
      prop <- ((cos(phase) + 1)/2)
      propo <- format(round(prop, 3), nsmall = 3)
      tidelevel <- prop * (hh - jj) + jj
      tidelevell <- format(round(tidelevel, 3), nsmall = 3)
      tidalsummary$EstimatedWaterLevel[i] <- tidelevell
      tidalsummary$NextTide[i] <- j
      if (j == "H") {
        tidalsummary$Proportion[i] <- (1 - as.numeric(propo))
      }
      if (j == "L") {
        tidalsummary$Proportion[i] <- propo
      }
    }
    if ((closestdatedown[i]) %in% (TideData$Date)) {
      x <- closestdatedown[i]
      b <- TideData[which(TideData$Date == (closestdatedown[i])) +
                      c(0:1), ]
      hh <- b[1, 2]
      jj <- b[2, 2]
      j <- b[2, 3]
      first <- as.numeric(difftime(date[i], (as.POSIXlt(b[1,
                                                          1],tz="etc/GMT+12")), unit = "hours"))
      secondd <- as.numeric(difftime((as.POSIXlt(b[2, 1],tz="etc/GMT+12")),
                                     (as.POSIXlt(b[1, 1],tz="etc/GMT+12")), unit = "hours"))
      phase <- ((first)/(secondd) * pi)
      prop <- ((cos(phase) + 1)/2)
      propo <- format(round(prop, 3), nsmall = 3)
      tidelevel <- prop * (hh - jj) + jj
      tidelevell <- format(round(tidelevel, 3), nsmall = 3)
      tidalsummary$EstimatedWaterLevel[i] <- tidelevell
      tidalsummary$NextTide[i] <- j
      if (j == "H") {
        tidalsummary$Proportion[i] <- (1 - as.numeric(propo))
      }
      if (j == "L") {
        tidalsummary$Proportion[i] <- propo
      }
    }
  }
  return(tidalsummary)
}


##################################################################################





