\name{RosstatMapPlot}
\alias{RosstatMapPlot}
\title{Print map using ggplot2}
\usage{
  RosstatMapPlot(indicator = "average_percapita_income",
    vuosi = 2008, level = "federal_district")
}
\arguments{
  \item{level}{accepts four values \code{federal},
  \code{federal_district}, \code{region} and \code{rayon}}
}
\value{
  shapefile at required level
}
\description{
  Print map using ggplot2
}
\examples{
# shapefile <- GetRusGADM("federal")
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

