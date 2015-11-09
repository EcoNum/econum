\name{optionsEcoNum}
\alias{optionsEcoNum}
\alias{getOptEcoNum}
\alias{setOptEcoNum}

\title{ Manage EcoNum options }
\description{
  Examine, set or retrieve EcoNum specific options.
}

\usage{
optionsEcoNum(opt)
getOptEcoNum(key, default = NULL)
setOptEcoNum(key, value)
}

\arguments{
  \item{opt}{ a list with all EcoNum options to set. Optional and if not
    provided, a default list is created at first use of \code{optionsEcoNum()}. }
  \item{key}{ a character string holding an option name. }
  \item{default}{ (optional) default value to return for the option, in case it is
    not defined. }
  \item{value}{ anything to set as an option in the list. }
}

\value{
  For \code{optionsEcoNum()}, the whole list of options is returned.
  For the other functions, associated value (or default value) is returned.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\examples{
## Remote and local EcoNumData repositories
getOptEcoNum("localRepos")
getOptEcoNum("remoteRepos")

## Default general metadata
## Default general metadata
getOptEcoNum("defProject")
getOptEcoNum("defSample")
getOptEcoNum("defSampleDate")
getOptEcoNum("defAuthor")

## Get some non-existing EcoNum option
getOptEcoNum("nokey", default = "my default value")
## Define it
setOptEcoNum("nokey", "some data")
getOptEcoNum("nokey", default = "my default value")
## Eliminate this key
setOptEcoNum("nokey", NULL)
getOptEcoNum("nokey", default = "my default value")
}

\keyword{ utilities }
