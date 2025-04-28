\name{MAC}
\alias{MAC}
\title{
Do Microbiological Assessment Category Analysis}
\description{
This function will output the Microbiological Assessment Category grade from a given dataframe. This is calculated using either Ecoli or Enterococci ("E coli" or "Enterococci" WQ parameter - AQUARIUS) dependant on water type. Water type can be specifed between freshwater and marine, with default being freshwater. The final grade is calculated using the 95th Hazen percentile. The time interval for data analysis can be altered using the 'start' and 'end' arguments. The final Microbiological Assessment Catergory grade can be interpretted through the Microbiological Water Quality Guidelines for Marine and Freshwater Recreational Areas.  }
\usage{
MAC(data, start, end , Type = "Freshwater")
}
\arguments{
  \item{data}{
  A data frame in the format; (siteid, name, time, value) where value is "E coli" or "Enterococci"
}
 \item{start}{
 Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional

 }
 \item{end}{
 Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional

 }

  \item{Type}{
To specifiy between freshwater and marine
}
}

\examples{
#create a dataframe
df<-AQMultiExtract("EQ065035","Enterococci")

#perform marine MAC analysis
MAC(df, Type="Marine")
