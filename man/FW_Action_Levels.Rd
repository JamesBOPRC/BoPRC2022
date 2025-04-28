\name{FW_Action_Levels}
\alias{FW_Action_Levels}
\title{
Calculate freshwater action levels
}
\description{
This function will use data from a given dataframe to calculate percentage at each action level for freshwater. This is calculated using Ecoli ("E coli" WQ parameter - AQUARIUS). The time interval for data analysis can be altered using the 'start' and 'end' arguments. The alert levels can be interpretted through the Microbiological Water Quality Guidelines for Marine and Freshwater Recreational Areas. }
\usage{
FW_Action_Levels(data, start = "", end = "")
}
\arguments{
  \item{data}{
Dataframe in the format; (siteid, name, date, value) where value is "E coli"}

 \item{start}{
 Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional

 }
 \item{end}{
 Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional

 }

}


\examples{

#Create dataframe
Df<-AQMultiExtract("RN123610", "E coli")

#Get freshwater action levels
FW_Action_Levels(Df)
