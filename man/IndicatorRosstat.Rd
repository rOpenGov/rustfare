\name{IndicatorRosstat}
\alias{IndicatorRosstat}
\title{List the currently available
indicators from Rosstat Regional Statistics}
\usage{
  IndicatorRosstat(urli = "http://research.muuankarski.org/rustfare/data/RosstatIndicatorData.RData")
}
\arguments{
  \item{urli}{is the url address to the data}
}
\value{
  data frame with indicator name, url, levels of
  aggregation and metadata in Russian and in English
}
\description{
  RosstatIndicator loads data frame that lists currently
  available indicators from Rosstat Regional Statistics
}
\examples{
# available.indicators <- IndicatorRosstat()
}
\author{
  Markus Kainu <markuskainu(at)gmail.com>
}

