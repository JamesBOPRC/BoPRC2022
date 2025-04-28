\name{TidalFromDate}
\alias{TidalFromDate}
\title{
Obtain tidal information from date(s)}
\description{
This function will output tidal information from an inputted date. The tidal information provided is 'Next Tide', 'Estimated Water Level' and 'Proportion', these are explained in the value section below. Tidal information is sourced from the LINZ website, and currently (Jan 2022) is only applicable to Tauranga sites from 2012-2024.}
\usage{
TidalFromDate(date)
}
\arguments{
  \item{date}{
Date(s) to obtain tidal information for, in quotation marks. In "yyyy/mm/dd hh:mm:ss" format.
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

\examples{
#find tidal information for 4:23pm on the 12th of March 2014
TidalFromDate("2014/03/12 16:23:00")

#find tidal information for 9:36am and 4:23pm on the 12th of March 2014
TidalFromDate(c("2014/03/12 9:36:00","2014/03/12 16:23:00"))
}
