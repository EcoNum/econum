\name{iks}
\alias{print.IKSData}
\alias{iks.open}
\alias{iks.close}
\alias{iks.process}
\alias{iks.cmd}
\alias{iks.info}
\alias{iks.getAll}
\alias{iks.getName}
\alias{iks.getData}
\alias{iks.setData}
\alias{iks.getConfig}

\title{ Control the IKS Aquastar computer }
\description{
  Connect to an IKS Aquastar through a serial (com) port and control it.
}

\usage{
iks.open(port = getOption("iks.port"), iks.name = "IKS",
    probes.names = paste("p0", 1:8, sep = ""),
    sockets.names = sprintf("s\%02d", 1:16), procfun = iks.process)
iks.close(port = getOption("iks.port"))
iks.process(msg, iks.name = "IKS")
iks.cmd(code, port = getOption("iks.port"), newline = TRUE)
iks.info(port = getOption("iks.port"), wait = 500)

\method{print}{IKSData}(x, \dots)

iks.getAll()
iks.getName(port = getOption("iks.port"))
iks.getData(port = getOption("iks.port"), iks.name = NULL)
iks.setData(data, port = getOption("iks.port"), iks.name = NULL)
iks.getConfig(port = getOption("iks.port"), wait = 500)
}

\arguments{
  \item{port}{ A serial port as \code{"com1"}, or its number like \code{1}. For
    \code{iks.close}, you can also specify \code{"all"} to close all IKS ports }
  \item{iks.name}{ The name you want to use for the IKS Aquastar module }
  \item{probes.names}{ the name to use for the eight IKS probes. }
  \item{sockets.names}{ the name to use for the 16 IKS sockets. }
  \item{procfun}{ A R function used to process data returned by the IKS Aquastar }
  \item{msg}{ The message send back from the IKS Aquastar }
  \item{code}{ Code to send to the IKS Aquastar }
  \item{newline}{ Do we add a newline after the code send (yes by default)? }
  \item{wait}{ Time to wait between characters send to the IKS Aquastar (500ms
    by default). Do not use too low values, or the device will not receive all
	code correctly }
  \item{x}{ an IKSData object. }
  \item{\dots}{ further arguments passed to the \code{print()} method. }
  \item{data}{ The IKS data to record (this is a list with components Name, Port,
    Version, Config, Probes, Units, Triggers, Sockets, SocketsCtrl and Time }
}

\value{
  \code{iks.open()} returns \code{TRUE} in case of success, \code{FALSE}
  otherwise.
  
  \code{iks.close()} returns the com port(s) that was/were closed.
  
  \code{iks.cmd()} and \code{iks.info()} return nothing. They are used for their
  side effect of triggering some response by the IKS Aquastar. That response is
  treated in an asynchronous way by the \code{procfun} provided to
  \code{iks.open()}
  
  \code{iks.getAll()} gets the list of all currently connected IKS Aquastars
  (a vector of characters with corresponding com ports, and with names as
  specified in the \code{iks.name} argument of \code{iks.open()}).
  
  \code{iks.getName()} gets the name of an IKS Aquastar, providing the com port
  it is connectyed to.
  
  \code{iks.getData()} and \code{iks.setData()} are used to retrieve or set the
  current state of the IKS. \code{iks.setData()} should not be used in a
  different context than an \code{iks.process()} and should update the IKS data
  according to what is received from the IKS Aquastar trough the com port.
  
  \code{iks.getConfig()} triggers a dumping of the complete configuration of the
  IKS Aquastar... it takes a couple of seconds to complete! After this commands,
  the whole current config is stored in binary format in the Config component
  and the current state of sockets is updated.
  
  \code{iks.process()} is the default workhorse function to process incoming IKS
  data. It can be replaced by you own custom function, but it is already pretty
  flexible. The default function allows to call custom events in the form of R
  functions that accept two arguments, \code{data} and \code{iks.name}. They must
  be saved in the \code{TempEnv} environment and have predefined names, depending
  on the event that should trigger them: \code{iks.dateProc()} when the date is
  received from the IKS, indicating a complete cycle of data sending,
  \code{iks.socketProc()} on receiving socket config data, and
  \code{iks.configProc()} on receiving IKS config data.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }


\examples{
\dontrun{
iks.getAll()
iks.open(port = "com1") # Change to the port where your IKS is connected!
iks.getAll()
iks.getName()

# To debug exchanges with the IKS aquastar:
options(debug.IKS = TRUE)

iks.getData()   # Current data as recorded in the IKS
iks.getConfig() # Get the whole IKS config
iks.getData()   # Should be visible now!

# Stop the connection with the IKS
iks.close()
iks.getAll()
}
}

\keyword{ utilities }
