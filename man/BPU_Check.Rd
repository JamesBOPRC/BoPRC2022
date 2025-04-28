\name{BPU_Check}
\alias{BPU_Check}
\title{Classify biophysical unit of siteID
}
\description{
This function will output the biophysical unit type at a given site, using the site ID. }
\usage{
BPU_Check(SiteID)
}
\arguments{
  \item{SiteID}{
Aquarius SiteID of interest in quotation marks}
}

\value{
\item{Non_VA/Steep}{
Non volcanic steep slope
}

\item{Non_VA/Gentle}{
Non volcanic gentle slope
}

\item{VA/Steep}{
Volcanic steep slope
}

\item{VA/Gentle}{
Volcanic gentle slope
}

}


\section{Error Messages}{
"Unknown biophysical unit"

The inputted site id was not found within the BPU_List function. Ensure you are using the correct site.
}
\examples{
BPU_Check("IG691428")
"VA/Steep"
}
