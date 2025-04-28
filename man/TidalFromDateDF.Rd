\name{TidalFromDateDF}
\alias{TidalFromDateDF}
\title{
Add tidal information to exisiting dataframe}
\description{
This function will run the dates in an existing data frame through the TidalFromDate function and add columns with tidal information. . The tidal information provided is 'Next Tide', 'Estimated Water Level' and 'Proportion', these are explained in the value section below. Tidal information is sourced from the LINZ website, and currently (Jan 2022) is only applicable to Tauranga sites from 2012-2024.}
\usage{
TidalFromDateDF(data)
}
\arguments{
  \item{data}{
Dataframe to obtain tidal information for, in quotation marks. Date data must be in the 3rd column.
}
}

\value{
\item{Estimated Water Level}{
The estimated water level value gives an estimate of the water level at a given time. This is calculated using where the time input falls on a cosine curve between the nearest high and low tide.
}

\item{Next Tide}{
The next tide value can be either H (high) or L (low).
}

\item{Proportion}{
The proportion value can be interpreted as 0 = absolute low tide, and 1 = absolute high tide.
}

}

