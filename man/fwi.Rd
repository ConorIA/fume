\name{fwi}
\alias{fwi}

\title{
Canadian Fire Weather Index Computation
}
\description{
Computes the Canadian Fire Weather Index according to the equations presented in van Wagner and Pickett (1985).
}
\usage{
fwi(date, Tm, H, r, W, return.all = FALSE)
}

\arguments{
  \item{date}{
A vector of class \code{date} or \code{chron} with the dates of the records. See details. 
}
  \item{Tm}{
A numeric vector of temperature records
}
  \item{H}{
A numeric vector of relative humidity records
}
  \item{r}{
A numeric vector of precipitation records
}
  \item{W}{
A numeric vector of wind velocity records
}
  \item{return.all}{
Logical. Should all FWI components be returned?. If \code{TRUE}, a complete dataframe is returned with all FWI components. Default to \code{FALSE}, and in this case only FWI is returned.
}
}
\details{
Dates need to be specified in a suitable format, as far as the function computes months and applies day length factor correction.

Some climate products may provide negative rainfall values, relative humidity above 100\% and so on... To avoid errors, the function automatically searches and corrects out-of-range values and gives a warning.

Temperature must be in degree Celsius, relative humidity in \%, precipitation in mm and wind velocity in km/h.

The function will not compute days with missing values for any of the input variables, and will jump to the next day with all complete records.
}
\value{
If \code{return.all} is set to \code{TRUE}, a dataframe is returned, each column corresponding to each of the components. If \code{FALSE}, a numeric vector with FWI values.
}
\references{

Lawson, B. and Armitage, O. (2008). Weather guide for the Canadian Forest Fire Danger Rating System. Canadian Forestry Service Tech. Rep. Edmonton, Canada.

van Wagner, C.E. and T.L. Pickett (1985). Equations and FORTRAN program for the Canadian Forest Fire Weather Index system. Canadian Forestry Service Tech. Rep. 33. Ottawa, Canada.

}
\author{
Santander Meteorology Group
<\url{http://www.meteo.unican.es}>
}


\examples{
require(graphics)
data(fwi.data)
# Observational data (2006-2011) Madrid-Retiro weather station
fwi.data$observed$Madrid -> obsMadrid 
fwi(date=obsMadrid$fecha, Tm=obsMadrid$T, H=obsMadrid$H, 
    r=obsMadrid$P, W=obsMadrid$W, return.all=TRUE) -> fwiMadrid

par(mfrow=c(2,3)) 
for (i in 1:6) {
    plot(obsMadrid$fecha, fwiMadrid[ ,i], ty='l', 
         col=i, xlab='date', ylab='value', 
         main=paste(names(fwiMadrid)[i]))
}
}
\keyword{ utilities }

