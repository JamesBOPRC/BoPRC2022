\name{NOFLakesRIversNH3}
\alias{NOFLakesRIversNH3}
\title{
NOF Assessment - Ammonia Toxicity (Rivers and Lakes)
}
\description{
This function will output the National Objectives Framework attribute bandings for Ammonia Toxicity from a given dataset. This can be done with either NH3 (adjusted) or NH4-N (unadjusted) total ammoniacal nitrogen values. If the inputted dataframe contains a pH column, the data will automatically be adjusted to NH3. The final attribute state reflects the lowest calculated band of assessed numeric attributes.  This is calculated using ammonia toxicity ("Ammoniacal N" WQ parameter - AQUARIUS). Any negative Ammoniacal N values (sourced from AQUARIUS) are replaced with 0. Subset timeframes can be created using the 'start' and 'end' arguments. The outputted attribute band value can be interpreted through the National Policy Statement for Freshwater Management 2020 or the value sectyion below.
}
\usage{
NOFLakesRiversNH3(data,... start="",end="",adjust=TRUE)
}

\arguments{
  \item{data}{
A data frame in the format; (site ID, name, time, Ammoniacal N, pH).
}



\item{start}{
Allows a subset timeframe to be tested by specifying a start date. This argument must be used in conjunction with an 'end' date.  Optional
}

\item{end}{
Allows a subset timeframe to be tested by specifying an end date. This argument must be used in conjunction with a 'start' date. Optional
}

\item{adjust}{
Transforms NH4-N to NH3 using pH, when adjust = TRUE (default). The data argument (above) must contain the optional pH variable for the transformation to be possible.
}
}


\value{
\item{A}{
99 percent species protection level: No observed effect on any species tested
}

\item{B}{
95 species protection level: Starts impacting occasionally on the 5 percent most sensitive species
}

\item{C}{
80 percent species protection level: starts impacting regularly on the 20 percent most sensitive species (reduced survival of most sensitive species).
}

\item{D}{
Starts approaching acute impact level (that is, risk of death) for sensitive species
}
}

\section{Error Messages}{
"Incorrect data frame"

This error occurs when the inputted data frame does not have 5 columns. Ensure that the data frame has 5 columns; Site ID, Site Name, Time , Ammonical N, pH in that order.

"Ammoniacal N parameter used"

This function uses the AQUARIUS WQ Parameter "Ammoniacal N" when using the AQMultiExtract function. The data frames 4th column should be titled "Ammoniacal N (g/m^3)" . If it does not, this error will occur.

"pH parameter used"

This function uses the AQUARIUS WQ Parameter "pH" when using the AQMultiExtract function. The data frames 5th column should be titled "pH (pH Units)" . If it does not, this error will occur.

}

\examples{
#create a dataframe
Df<-AQMultiExtract("JK491452", c("Ammoniacal N","pH"))
#do a NOF Ammonia Toxicity assessment on the data frame, for the most recent 1 year period jun-july
NOFLakesRiversNH3(Df)
}
