\name{GetRusGADM}
\alias{GetRusGADM}
\title{Get Russian map data in GADM format}
\usage{
  GetRusGADM(level)
}
\arguments{
  \item{level}{accepts four values \code{federal},
  \code{federal_district}, \code{region} and \code{rayon}}
}
\value{
  shapefile at required level
}
\description{
  Get Russian map data in GADM format
}
\examples{
# shapefile <- GetRusGADM("federal")
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

