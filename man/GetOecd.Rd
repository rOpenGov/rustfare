\name{GetOecd}
\alias{GetOecd}
\title{Download data from OECD Country statistical profile: Russian Federation}
\usage{
  GetOecd(measure = "public_expenditure_on_health")
}
\arguments{
  \item{indicator}{Character string. Select indicator from
  the \code{indicator} column of data frame returned by
  function \code{OecdIndicator}. More than one indicator is
  NOT allowed.}
}
\value{
  data frame with 4 columns \itemize{ \item
  \code{indicator} Name of the indicator \item \code{unit}
  unit the indicator \item \code{value} Value of the
  indicator \item \code{year} Year of observation }
}
\description{
  Download data from OECD Country statistical profile:
  Russian Federation
  \url{http://www.oecd-ilibrary.org/economics/country-statistical-profile-russian-federation_20752288-table-rus}
}
\examples{
# dat <- GetOecd("public_expenditure_on_health")
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

