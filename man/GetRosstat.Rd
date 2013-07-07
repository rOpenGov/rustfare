\name{GetRosstat}
\alias{GetRosstat}
\title{Download data from Rosstat Regional Statistics}
\usage{
  GetRosstat(indicator = "mortality_rate",
    level = "federal_district")
}
\arguments{
  \item{indicator}{Character string. Select indicator from
  the \code{indicator} column of data frame returned by
  function \code{RosstatIndicator}. More than one indicator
  is NOT allowed.}

  \item{level}{Character string. Defines the administrative
  level of aggregation \itemize{ \item \code{federal} at
  the federal level \item \code{federal_district} at the
  federal district level (8 regions) \item \code{region} at
  the regional level (83 regions) } More than one level is
  NOT allowed.}
}
\value{
  data frame with 7 columns \itemize{ \item \code{region}
  Region name in Russian \item \code{region_en} Region name
  in English \item \code{year} Year of observation \item
  \code{value} Value of the indicator \item \code{level}
  Aggregation level \item \code{id_shape} id number for
  mergin with shapefile from GADM \item \code{indicator}
  name of the indicator }
}
\description{
  Download data based on indicator and aggregation level
  from Rosstat Regional Statistics
}
\examples{
# dat <- GetRosstat("life_expectancy_men","federal_district")
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

