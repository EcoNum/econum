\name{gce8}
\alias{gce8Port}
\alias{gce8Name}
\alias{gce8Relays}
\alias{gce8List}
\alias{gce8Open}
\alias{gce8Close}
\alias{gce8Ready}
\alias{gce8Cmd}
\alias{gce8Get}
\alias{gce8Set}

\title{ Control a GCE Electronics USB 8 relays card }
\description{
  Switch individual channels on or off in the USB interface card, or
  read its current state.
}

\usage{
gce8Port(port = getOption("gce8Port"), name)
gce8Name(port = gce8Port())
gce8Relays(names, port = gce8Port(), name = gce8Name(port))
gce8List()
gce8Open(port = gce8Port(), name = gce8Name(port))
gce8Close(port = gce8Port(), name = gce8Name(port))
gce8Ready(port = gce8Port(), name = gce8Name(port))
gce8Cmd(relay, on = TRUE, port = gce8Port(), name = gce8Name(port))
gce8Get(port = gce8Port(), name = gce8Name(port))
gce8Set(state = FALSE, port = gce8Port(), name = gce8Name(port))
}

\arguments{
  \item{port}{ The comm port to use (this is a virtual comm port using the
    FTDI virtual port driver available for Windows, Mac OS and Linux). If a
    number (x) is provided, the port is assumed to be 'comx', which is only
    a valid assuption under Windows. Use fully qualified name on Linux or Mac) }
  \item{name}{ The name of the device. With \code{gce8Port()} it is used to
    retrieve the corresponding port; with \code{gce8Open()}, it forces the
    name of the card to use in R }
  \item{names}{ A vector of eight character strings to use to define the name
    of the eight relays. Missing data represent unused relays }
  \item{relay}{ The number, or name of the relay to switch }
  \item{on}{ Do we switch the relay on or off? Can be a boolean, 0/1 or "0"/"1" }
  \item{state}{ The state of the relays. Can be a vector of eight items
    (otherwise, recycling rule applies), either boolean, integers 0/1, or
    anything that automatically coerces into boolean. Missing values are
    accepted and they will match relay that are not modified. The vector can
    also be named, and in this case, names will be match to the names used for
    the 8 relays: R1-R8, or anything else specified using \code{gce8Relays()} }
}

\value{
  Most functions return \code{TRUE} in case of success and \code{FALSE} or a
  'try-error' object in case of failure., except:
  
  \code{gce8Port()} and \code{gce8Name()} return the port and name of a card,
  
  \code{gce8List()} returns the list of currently connected cards,
  
  \code{gce8Relays()} returns a vector of eight character strings with the name
  of the eight relays (dot . substituted for missing values)
  
  \code{gce8Get()} returns a named vector of eight booleans indicating the state
  of the eight relays. Names are the current names of the relays, or the default
  names R1-R8
}
\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\note{ Functions \code{gce8Cmd()}, \code{gce8Get()} and \code{gce8Set()}
  functions use a mechanism to reconnect in case of lost connection. For
  instance, if the USB cord is unplugged and plugged again, connection is lost,
  but these functions can recover it. It only takes about 1 sec more to perform
  the task. If you forgot to connect with \code{gce8Open()}, these functions
  will do this automatically for you. However, you should always use
  \code{gce8Close()} once you have done with the card, in order to free the comm
  port handle. This will works too if the main power of the card in shut off and
  on again, but take care that all relays do return in off state and your
  program is not necessarily aware of that situation, unless you use
  \code{gce8Get()}. For critical applications, do prefer using \code{gce8Set()}
  and always specify all eight relays state instead of using \code{gce8Cmd()}. }

\examples{
\dontrun{
# If the USB interface card is connected to com5, this should work:
options(gce8Port = 5)
# On the Mac, use: options(gce8Port = "/dev/cu.usbserial-A4003CDu")
gce8Open() # Note that this is facultative, since I connect on first use!
gce8Cmd(5, 1)
gce8Get()
gce8Set(FALSE)
gce8Close() # On the other hand, this is required to free the port!
}
}

\keyword{ utilities }
