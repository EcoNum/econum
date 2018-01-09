# EcoNumData object creation (default function)
newEcoNumData <- function(x, metadata = list(project = "", sample = "",
sampledate = getOptEcoNum("defSampleDate"), author = "", date = Sys.time(),
comment = NULL, topic = NULL), class = NULL) {
  # Convert a data frame into a minimal EcoNumData object
  m <- metadata
  if (m$project == "") m$project <- getOptEcoNum("defProject")
  if (m$sample == "") m$sample <- getOptEcoNum("defSample")
  if (m$author == "") m$author <- getOptEcoNum("defAuthor")
  # Create clean metadata list
  metadata <- list(project = m$project, sample = m$sample,
    sampledate = m$sampledate, author = m$author, date = m$date,
    comment = m$comment, topic = m$topic)
  x <- as.data.frame(x)
  attr(x, "metadata") <- metadata
  class(x) <- c(class, "EcoNumData", "data.frame")
  x
}

# EcoNumData object methods
print.EcoNumData <- function(x, ...) {
  cat("An EcoNumData object with:\n")
  dat <- as.data.frame(x) # Eliminate all attributes
  print(dat)
  invisible(x)
}

# Just create an object summary.EcoNumData
summary.EcoNumData <- function(object, ...) {
  class(object) <- c("summary.EcoNumData", "data.frame")
  object
}

# Print the content of the data frame and then of metadata attribute
print.summary.EcoNumData <- function(x, ...) {
  cat("An EcoNumData object with:\n")
  dat <- as.data.frame(x) # Eliminate all attributes
  print(dat)
  cat("\nWith following metadata:\n")
  print(attr(x, "metadata"))
  invisible(x)
}

# Define the validate method  (in econum package)
validate <- function(object, ...)
  UseMethod("validate")

# Check and possibly correct an EcoNumData object (in econum package)
validate.EcoNumData <- function(object, file = NULL, ...) {
  # Basic validation method for general EcoNumData object
  # TODO: basic validate.EcoNumData()
  # For the moment, just return the object
  object
}

# Define the reposSave method (file path/name determined from metadata)
reposSave <- function(object, envir = .GlobalEnv, local = TRUE,
remote = TRUE, ...)
  UseMethod("reposSave")

# Save an EcoNumData object to the repository
# TODO: check if objects with same fingerprints already exist in repositories
# TODO: an option to replace a file or not!
reposSave.EcoNumData <- function(object, envir = .GlobalEnv, local = TRUE,
remote = TRUE, ...) {
  # Get metadata
  m <- attr(object, "metadata")
  # Construct context according to last class name
  cl <- class(object)[1]
  if (cl == "EcoNumData") {
    cl <- NULL
    objName <- "EcoNumData"
    subPath <- m$project
    fileExt <- ".RData"
  } else {
    objName <- paste("EcoNumData", cl, sep = "_")
    subPath <- file.path(m$project, cl)
    fileExt <- paste("_", cl, ".RData", sep = "")
  }
  # If there is a 'topic' in metadata, append it to objName
  if (!is.null(m$topic)) objName <- paste(objName, m$topic, sep = ".")
  # Get project, sample, sampledate and date to construct the filename
  # Fingerprint identifies a unique EcoNumData object
  fingerprint <- paste("_", timeToFingerprint(m$date), fileExt, sep = "")
  fileName <- paste(m$sample, "_", format(m$sampledate, "%Y-%m-%d_%H.%M.%S"),
    fingerprint, sep = "")
  # filePattern is the regular expression used to track replicated EcoNumData
  # objects (based on fingerprint) along the repository
  filePattern <- paste(fingerprint, "$", sep = "")
  filePattern <- gsub("\\.", "\\\\.", fingerprint)

  # Create an object named objName in envir
  assign(objName, object, envir = envir)
  res <- objName

  # Save in local repository
  localRepos <- getOptEcoNum("localRepos")
  if (isTRUE(local) && file.exists(localRepos)) {
    # Make sure subPath exists
    Path <- file.path(localRepos, subPath)
    if (!file.exists(Path))
      dir.create(Path, showWarnings = FALSE, recursive = TRUE)

    # Check possible duplicates in other projects
    # TODO: what to do in case of duplicates???

    # Save the object in the repository
    localFile <- file.path(Path, fileName)
    ret <- try(save(list = objName, file = localFile), silent = TRUE)
    if (inherits(ret, "try-error")) {
      stop("Error while saving results in local repository:\n", ret)
    } else res[2] <- localFile
  } else res[2] <- NA

  # If available, save in remote repository
  remoteRepos <- getOptEcoNum("remoteRepos")
  if (isTRUE(remote) && file.exists(remoteRepos)) {
    # Make sure subPath exists
    Path <- file.path(remoteRepos, subPath)
    if (!file.exists(Path))
      dir.create(Path, showWarnings = FALSE, recursive = TRUE)

    # TODO: look for already existing objects with same fingerprint

    # Save the object in the repository
    remoteFile <- file.path(Path, fileName)
    ret <- try(save(list = objName, file = remoteFile), silent = TRUE)
    if (inherits(ret, "try-error")) {
      stop("Error while saving results in remote repository:\n", ret)
    } else res[3] <- remoteFile
  } else res[3] <- NA
  names(res) <- c("object", "localFile", "remoteFile")
  # Return object name and path to both repositories files
  invisible(res)
}

# reposReload() cannot be a method... because the object is not created yet
# and thus, its class is unknown when the function is called!
#  (in econum package)
reposLoad <- function(file, backup.existing = FALSE, ...) {
  # Try loading the file...
  res <- load(file)

  # TODO: allow for EcoNumCalib objects too!
  # There should be only one object and it should start with EcoNumData
  if (length(res) > 1)
    stop("Incorrect EcoNumData object: more than one object found")
  if (!grepl("^EcoNumData.*(_.+){0,1}(\\..+){0,1}$", res))
    stop("Incorrect EcoNum object: must be EcoNumData(_<class>)(.<topic>)")
  # Get the specific class of the EcoNumData object from its name
  class <- sub("^EcoNumData_([^.]+)(\\..+){0,1}$", "\\1", res)
  if (class == res) class <- NULL

  # Validate the object
  object <- validate(get(res), file = file)

  if (isTRUE(backup.existing)) {
    # If res object already exists in .GlobalEnv, resave as res2
    if (exists(res, envir = .GlobalEnv, inherits = FALSE))
      assign(paste(res, "2", sep = ""), get(res, envir = .GlobalEnv),
        envir = .GlobalEnv)
  }

  # Save this object in .GlobalEnv
  assign(res, object, envir = .GlobalEnv)
  invisible(res)
}
