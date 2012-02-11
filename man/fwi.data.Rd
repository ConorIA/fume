\name{fwi.data}
\alias{fwi.data}
\docType{data}
\title{
Observational and reanalysis data from 7 locations in Spain.
}
\description{
This dataset contains data of temperature, relative humidity, precipitation and wind velocity from 7 locations in Spain, as well as calculated Fire Weather Index. Data correspond both to observations from the Spanish Meteorological Agency (AEMET, all recorded at noon excepting precipitation, recorded at 07:00 UTC) and NCEP reanalysis (12:00 UTC).
}
\usage{data(fwi.data)}
\format{
A list with two elements: \code{observed} (Observations data) and \code{ncep} (NCEP reanalysis data)
Each element of the list contains 7 dataframes corresponding to weather stations in Spain. For each location, the following elements are included: dates of the records, Temperature (degrees Celsius), Relative humidity (\%), accumulated precipitation (mm), wind velocity (km/h) and Fire Weather Index.
}
\references{
Bedia, J., Herrera, S., Gutierrez. J.M., Zavala, G., Rodriguez, I. and Moreno, J.M. Sensitivity of Fire Weather Index to different Reanalysis Products in the Iberian Peninsula. Natural Hazards and Earth System Sciences. In press. 

Kistler, R., Kalnay, E., Collins,W., Saha, S., White, G.,Woollen, J., Chelliah, M., Ebisuzaki,W., Kanamitsu, M., Kousky, V., van den Dool, H., Jenne, R., and Fiorino, M.: The NCEP-NCAR 50-year reanalysis: Monthly means CD-ROM and documentation, Bull. Amer. Meteorol. Soc., 82, 247-267, 2001.
}
\examples{
data(fwi.data)
require(graphics)
par(mfrow=c(2,3))
for (i in 2:7) {
    plot(fwi.data[[2]][[i]]$fecha, fwi.data[[2]][[i]]$FWI, 
        ty='l', col='red', ylab='FWI', xlab='date')
    title(main=paste(names(fwi.data[[1]])[i]))
    lines(fwi.data[[1]][[i]]$fecha, fwi.data[[1]][[i]]$FWI, ty='l', col='grey')
    legend('topright', c('Observed','NCEP'), lty=1, col=c('red','grey'))
}

}
\keyword{datasets}