\name{mkTrend}
\alias{mkTrend}
\title{
Mann-Kendall modified trend test for serially correlated data
}
\description{
Performs the Modified Mann-Kendall trend test, returning the corrected p-values after accounting for temporal pseudorreplication
}
\usage{
mkTrend(x, ci = 0.95)
}
\arguments{
  \item{x}{
A time series
}
  \item{ci}{
Confidence interval of the statistical test. Default to 0.95
}
}
\details{
If NA values are present in the time series, the function will continue after discarding them, and will give a warning.
}
\value{
A list containing the following elements:
\item{Z}{The original (non corrected) Mann-Kendall test Z statistic}
\item{p.value}{The original (non corrected) Mann-Kendall test p-value}
\item{Zc}{The new Z statistic after applying the correction}
\item{Corrected p.value}{Corrected p-value after accounting for serial autocorrelation}
\item{tau}{Mann-Kendall's tau statistic}
\item{N/n*s}{Value of the correction factor, representing the quotient of the number of samples \emph{N} divided by the effective sample size \emph{(n*s)}}
\item{Sen slope}{The slope of the (linear) trend according to Sen test}
}
\references{
Hamed, K.H. and A.R. Rao, (1998). A modified Mann Kendall trend test for autocorrelated data. Journal of Hydrology 204, 182-196.

Yue, S. and C. Wang (2004). The Mann-Kendall Test Modified by Effective Sample Size to Detect Trend in Serially Correlated Hydrological Series. Water Resources Management 18, 201-218.

Sen, P.K. (1968). Estimates of the Regression Coefficient Based on Kendall's Tau. Journal of the American Statistical Association 63, 1379-1389
}
\author{
Santander Meteorology Group
<\url{http://www.meteo.unican.es}>
}
\note{
A minimum sample size of \emph{N}=40 is recommended 
}
\seealso{
   \code{\link{fwi}} for calculation of fire danger time series
}
\examples{
require(datasets)
require(graphics)

data(sunspot.year) # The yearly sunspot dataset (1700-1988)
mkTrend(as.vector(sunspot.year))
# Original pvalue (0.0036) much lower than corrected pvalue (0.02)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
\keyword{utilities}
