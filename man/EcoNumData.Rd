\name{newEcoNumData}
\alias{newEcoNumData}
\alias{print.EcoNumData}
\alias{summary.EcoNumData}
\alias{print.summary.EcoNumData}
\alias{validate}
\alias{validate.EcoNumData}
\alias{reposSave}
\alias{reposSave.EcoNumData}
\alias{reposLoad}

\title{ Create, manipulate, save and load EcoNumData objects }
\description{
  EcoNumData objects are data frames with companion metadata that are saved
  in a specific way in both local and remote EcoNumData repositories. They
  are basic building blocks for a simplified, file-based LIMS (Laboratory
  Information Management System) as it is using at UMONS EcoNum.
}

\usage{
newEcoNumData(x, metadata = list(project = "", sample = "",
    sampledate = getOptEcoNum("defSampleDate"), author = "", date = Sys.time(),
    comment = NULL, topic = NULL), class = NULL)
\method{print}{EcoNumData}(x, \dots)
\method{summary}{EcoNumData}(object, \dots)
\method{print}{summary.EcoNumData}(x, \dots)

validate(object, \dots)
\method{validate}{EcoNumData}(object, file = NULL, \dots)

reposSave(object, envir = .GlobalEnv, local = TRUE, remote = TRUE, \dots)
\method{reposSave}{EcoNumData}(object, envir = .GlobalEnv, local = TRUE, remote = TRUE, \dots)

reposLoad(file, backup.existing = FALSE, \dots)
}

\arguments{
  \item{x}{ some data. }
  \item{metadata}{ a list of various metadata, with, as a minimum: 'project',
    'sample', 'sampledate' and 'author'. }
  \item{class}{ (optional) the class of the EcoNumData object to create (it will
    subclass both EcoNumData and data.frame). }
  \item{object}{ any object, but for EcoNumData methods, it is an EcoNumData
    object. }
  \item{file}{ the file path to where the object is stored in either the local
    or the remote repository. }
  \item{envir}{ an environment where to assign the EcoNumData object
    (\code{.GlobalEnv} by default). }
  \item{local}{ logical (\code{TRUE} by default). Should we save the object in
    the local repository, defined by \code{getOptEcoNum("localRepos")}. }
  \item{remote}{ logical (\code{TRUE} by default). Should we save the object in
    the remote repository, defined by \code{getOptEcoNum("remoteRepos")}. }
  \item{backup.existing}{ logical (\code{FALSE} by default). Should we backup
    an object of the same name as the one that is loaded (it will become
    <obj>2)? }
  \item{\dots}{ further arguments to pass to the function. }
}

\value{
  For \code{newEcoNumData()}, the object that is just created. It is a data frame
  with a 'metadata' attribute, as a minimum, and it inherits from both
  EcoNumData and data.frame.
  
  For \code{validate()}, the object is returned possibly corrected by using a
  series of validation rules (and warnings are usually issued if something is
  changed).
  
  For \code{reposSave()}, a vector of three named character strings, with
  'object' being the name of the object containing the item in the global
  workspace, 'localFile' being the path to the .RData file in the local
  repository (or \code{NA} if it was not saved there) and 'remoteFile' being
  the path to the .Rdata file as saved in the remote repository (or \code{NA} if
  it was not saved there). This is returned invisibly.
  
  For \code{reposLoad()}, the name of the object that is reloaded is returned
  invisibly.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\examples{
\dontrun{
## Save some generic EcoNumData
res <- reposSave(newEcoNumData(data.frame(x = 1, y = 2)), remote = FALSE)
## The data is in .GlobalEnv
EcoNumData
summary(EcoNumData)

## Can be erased and reloaded from the repository later on
rm(EcoNumData)
get(reposLoad(res["localFile"]))

## Delete the object from memory
rm(EcoNumData)
## ... and from the disk
unlink(res["localFile"])
## If the project subdirectory is empty, remove it
localDir <- dirname(res["localFile"])
if (length(dir(localDir)) == 0) unlink(localDir, recursive = TRUE)
rm(localDir)
}
}

\keyword{ utilities }
