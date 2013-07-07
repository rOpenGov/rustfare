\name{IndicatorOecd}
\alias{IndicatorOecd}
\title{List the currently available
indicators from OECD Country statistical profile}
\usage{
  IndicatorOecd(urli = "http://research.muuankarski.org/rustfare/data/IndicatorOecd.RData")
}
\arguments{
  \item{urli}{is the url address to the data}
}
\value{
  data frame with indicator names
}
\description{
  List the currently available indicators from OECD Country
  statistical profile at:
  \url{http://www.oecd-ilibrary.org/economics/country-statistical-profile-russian-federation_20752288-table-rus}
}
\examples{
# available.indicators <- IndicatorOecd()
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

