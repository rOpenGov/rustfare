\name{GetQog}
\alias{GetQog}
\title{Download data from Quality of Governance data}
\usage{
  GetQog(country = "Russia", indicator = "undp_hdi")
}
\arguments{
  \item{country}{Character string. Name of the country}

  \item{indicator}{Character string. Select indicator from
  the \code{indicator} column of data frame returned by
  function \code{QogIndicator()}. More than one country or
  indicator is allowed using
  \code{c("indicator1","indicator2")}}
}
\value{
  data frame with 4 columns \itemize{ \item \code{cname}
  Country name \item \code{year} Year of observation \item
  \code{indicator} Name of the indicator \item
  \code{value}Value of the indicator }
}
\description{
  Download data from Quality of Governance data at
  \url{http://www.qog.pol.gu.se/data/datadownloads/qogstandarddata/}
}
\examples{
# dat <- GetQog(country = c("Russia", "China"), indicator = c("undp_hdi", "fh_polity2"))
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

