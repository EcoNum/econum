\name{para}
\alias{para}

\title{ Control the parallel port interface from R }
\description{
  Switch individual channels on or off in the parallel port interface card, or
  read its current state.
}

\usage{
para(state, change = TRUE, port = getOption("para.port"))
}

\arguments{
  \item{state}{ A logical vector of length 8 indicating the state (on  or off)
    for each of the eight individual channels in the interface card }
  \item{change}{ if \code{TRUE} (by default), change the channels, otherwise
    change nothing but just read the current channels state from the card }
  \item{port}{ The lpt port to which the card is connected. Currently,
	only standard LPT1, LPT2 or LPT3 ports are recognized (use lpt1, lpt2 or
	lpt3, respectively, or their equivalent numbers, 1, 2 or 3). The default
	value is stored in \code{para.port} option, otherwise, it is considered to
	be \code{"lpt1"}.}
}

\value{
  If \code{change = TRUE}, the function returns \code{TRUE} in case of success,
  or \code{FALSE} otherwise (note that fealure to load required dlls, or trying
  to switch to a nonexistant LPT port raise an error).
  
  If \code{change = FALSE}, returns a vector of logical of length 8 indicating
  the current state of the interface card. If \code{state} was provided, its
  names are reused in the returned logical vector.
}
\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\note{ These functions use the \samp{inpout32.dll} and \samp{lpttcl.dll} version
  3.0 that are provided in the \samp{/bin} subdirectory of this package. These
  dlls work only under Windows (95 -> XP) and are probably not compatible with
  Windows Vista or higher. They do not work either under Linux or Mac OS X. You
  must also copy these dlls in the \samp{/windows/system32} directory
  (administrator rights required) before you can use them. }

\examples{
\dontrun{
# If the parallel interface card is connected to LPT1, this should work:
options(para.port = "lpt1")	# Change this to 2 or 3, if needed
para(c(T,T,T,T,T,T,T,T))    # All channels ON
para(c(T,F,T,F,T,F,T,F))    # All odd channels ON
para(c(F,F,F,F,F,F,F,F))    # All channels OFF

# Use a named vector for better tracking of the different channels
st <- c(ch1=T, ch2=F, ch3=F, ch4=T, ch5=T, ch6=F, ch7=F, ch8=T)
st
para(c(F,T,F,T,F,T,F,T))    # All even channels ON
para(st, FALSE) # Read current config, keeping channel names from st
}
}

\keyword{ utilities }
