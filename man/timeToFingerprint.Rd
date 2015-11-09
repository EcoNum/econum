\name{timeToFingerprint}
\alias{timeToFingerprint}
\alias{fingerprintToTime}

\title{ Calculate fingerprint according to time object, or the reverse }
\description{
  Get an hexadecimal unique identifier corresponding to time an object is
  created. A time object from a hexadecimal fingerprint can be also obtained.
}

\usage{
timeToFingerprint(Time)
fingerprintToTime(hexmode, tz = "")
}

\arguments{
  \item{Time}{ a POSIXct object indicating time an EcoNumData object is
    created.}
  \item{hexmode}{ a hexmode object, or a character string that can be converted
    to a hexmode object. }
  \item{tz}{ the time zone in which to recreate the time object, by default, use
    current time zone for this machine. }
}

\value{
  A character string with hexadecimal representation of the time. This could be
  used as a unique identifier (fingerprint) for objects that are sequencially
  created in time. The reverse function gets time from a fingerprint.
}
\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\examples{
(fp <- timeToFingerprint(Sys.time()))
fingerprintToTime(fp) # Reverse process
rm(fp)
}

\keyword{ utilities }
