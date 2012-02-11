\name{calibrateProj}
\alias{calibrateProj}
\title{
Calibration of RCM/GCM projections 
}
\description{
The function performs calibration of model simulations using as reference a given dataset. Currently three types of calibration are supported: correction of the mean (i.e. bias correction), correction of the distribution quantile by quantile (quantile-quantile mapping) and a hybrid technique that performs quantile-quantile adjustment considering both distribution and means (quantile quantile adjustment).   
}
\usage{
calibrateProj(obs, pred, sim, method = c("qqadj", "qqmap", "bias"), 
    varcode = c("tas", "hurs", "pr", "wss"), return.par = TRUE)
}
\arguments{
  \item{obs}{
A time series of observations (or any other datasetused as reference for correction, e.g. simulations nested to reanalysis)
}
  \item{pred}{
A time series of model simulations for the same period than observations
}
  \item{sim}{
A time series of future model simulation (or period to be corrected)
}
  \item{method}{
Character string indicating the type of correction to be done. Currently three types of correction are supported: \code{"qqadj"}=quantile-quantile adjustment, \code{"qqmap"}=quantile-quantile mapping and \code{"bias"}=bias correction.
}
  \item{varcode}{
Character string indicating the variable to be corrected. Currently the values implemented are \code{tas}, for temperature, \code{hurs} for relative humidity, \code{pr} for precipitation and \code{wss} for wind velocity. In practice, the function will only give a special treatment to temperature and precipitation.
}
  \item{return.par}{
Logical. Should the "f" and "g" parameters of the correction be returned? default to \code{FALSE}. In practice, this option only makes sense with the quantile-quantile adjustment method, although note that in the case of temperature g=1.
}
}

\details{
All data series used as input must have exactly the same length.
}
\value{
If \code{return.par} is set to \code{FALSE}, a numeric vector with the corrected simulated series. If set to \code{TRUE}, a list with the following elements:
\item{corrvals}{Numeric vector with corrected values}
\item{g}{value of the 'g' parameter}
\item{f}{value of the 'f' parameter}
\item{Mean_delta}{(Mean) Bias of the predicted series wrt the observations}
}
\references{

Amengual, A., Homar, V., Romero, R., Alonso, S. and Ramis, C. A statistical adjustment of regional climate model outputs to local scales: Application to Platja de Palma, Spain. Journal of Climate. In press.

}
\author{
Santander Meteorology Group
<\url{http://www.meteo.unican.es}>
}

\examples{
require(graphics)

rnorm(1000) -> obs
rnorm(1000, mean=.5) -> pred
rnorm(1000, mean=.7) -> sim
calibrateProj(obs, pred, sim, 'qqadj', ret=FALSE) -> corr
plot(ecdf(obs), col='red', ylab='CDF', main='Q-Q adjustment')
lines(ecdf(pred), col='green')
lines(ecdf(sim), col='blue')
lines(ecdf(corr), col='grey20')
legend('topleft', c('Observed','Predicted','Simulated','Corrected'), lty=1, col=c('red','green','blue','grey20'))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
