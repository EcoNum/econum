\name{wtw}
\alias{wtw.open}
\alias{wtw.close}
\alias{wtw.ready}
\alias{wtw.info}
\alias{wtw.cmd}
\alias{wtw.echo}
\alias{wtw.read}
\alias{wtw.record}
\alias{wtw.sim}

\title{ Control a WTW instrument of series P3, P4, 340, 340i or 197i }
\description{
  Connect to a compatible WTW instrument using a serial (com) port through the
  control cable AK340/B and send commands and/or read values of pH, O2, etc.
}

\usage{
wtw.open(port = getOption("wtw.port"))
wtw.close(port = getOption("wtw.port"))
wtw.ready(port = getOption("wtw.port"))
wtw.echo(port = getOption("wtw.port"))
wtw.cmd(code, port = getOption("wtw.port"))
wtw.sim()
wtw.info(port = getOption("wtw.port"), timeout = 1000)
wtw.read(port = getOption("wtw.port"), timeout = 1000, O2 = FALSE)
wtw.record(file = NULL, interval = 10, Nmax = 10, port = getOption("wtw.port"),
  timeout = 1000, O2 = TRUE, init.data = NULL, graph = TRUE,
  title = "WTW record", ...)
}

\arguments{
  \item{port}{ A serial port as \code{"com1"}, or its number like \code{1} }
  \item{code}{ Code to send to the WTW instrument, i.e., K.1 -> K.19 to simulate
    keystrokes (simple or double), and D.0 -> D.12 to read display items (see
	the WTW communication protocole manual) }
  \item{timeout}{ Maximum time to wait in ms for the answer from the instrument }
  \item{O2}{ For instruments that support it, do we also read O2 values? }
  \item{file}{ The file where to write the records. If \code{NULL}, a default
    name is computed which is 'WTW_<date>.txt' in the temporary directory. If it
    is \code{NA}, nothing is written in a file (but you loose data in case of
	computer crash)! }
  \item{interval}{ Interval between two measurements, in second. }
  \item{Nmax}{ Maximum number of readings to record }
  \item{init.data}{ A data frame with data to start with (append data to it) }
  \item{graph}{ Do we update a graph each time new item is read? }
  \item{title}{ The title of the graph }
  \item{...}{ Further graph arguments }
}

\value{
  \code{wtw.open()} returns \code{TRUE} in case of success, \code{FALSE}
  otherwise. \code{wtw.ready()} does the same if the instrument is ready or not.
  
  \code{wtw.close} returns the com port that was closed.
  
  \code{wtw.info} reads the identifier of the WTW instrument and returns it (if
    it is one of the recognized devices).
  
  \code{wtw.echo()} echo data send by the instrument to the console (mainly for
  debugging purpose). Do use \code{wtw.read()} instead to read measurements.
  
  \code{wtw.sim()} simulates data read from a fake instrument. You can define
  your own function, if you like.
  
  \code{wtw.record()} records a series of measurement on a disk file, and
  possibly plot the data in real time. Note that this is the only function that
  does not require that you open (\code{wtw.open()}) before, and close
  (\code{wtw.close()}) the port before interacting with the instrument.
  
}
\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\note{ A fake instrument for test and set up of control programs is available
  if you specify "com0" as serial port. Note that, for simulations, you can
  speed up globally data acquisition by defining \code{option(econum.speed = X)}.
  If 'X' is higher that 1, it means everything is done faster (X = 10 means ten
  times faster). If 'X' is lower that 1, everything is acquired slower: X = 0.1
  means ten times slower.
  
  Do not forget to close the port after use!
}

\examples{
# Use the fake WTW instrument on com0
options(wtw.port = "com0")	# Change this to connect to a real instrument
wtw.open()
wtw.echo()	# Echo data send by run/enter from the instrument
wtw.read()  # Read data from the instrument
wtw.ready() # Check that instrument is ready
wtw.close()
wtw.ready()	# Not ready any more, because we closed the port

# Record 10 data with an interval of 1 sec and make a graph...
# but speed up things by ten times if we use a fake instrument
if (getOption("wtw.port") == "com0") options(econum.speed = 10)
# Note that you don't have to open/close the port with this function!
wtw.record(interval = 1, Nmax = 10)
# Restore default speed
options(econum.speed = NULL)
}

\keyword{ utilities }
