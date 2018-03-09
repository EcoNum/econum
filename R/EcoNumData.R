#' Create, manipulate, save and load EcoNumData objects
#'
#' `EcoNumData` objects are data frames with metadata that are saved in a
#' specific way in both local and remote EcoNumData repositories. They are basic
#' building blocks for a simplified, file-based LIMS (Laboratory Information
#' Management System) as it is using at UMONS EcoNum.
#'
#' @param x Some data.
#' @param metadata A list of various metadata, with, as a minimum: 'project',
#' 'sample', 'sample_date' and 'author'.
#' @param class The (optional) class of the EcoNumData object to create (it will
#' subclass both `EcoNumData` and `data.frame`).
#' @param object Any object, but for `EcoNumData` methods, it must be an
#' `EcoNumData` object.
#' @param file The file path to where the object is stored in either the local
#' or the remote repository.
#' @param envir An environment where to assign the `EcoNumData` object
#' (`.GlobalEnv` by default).
#' @param local Logical (`TRUE` by default). Should we save the object in the
#' local repository, defined by `get_opt_econum("local_repos")`.
#' @param remote Logical (`TRUE` by default). Should we save the object in the
#' remote repository, defined by `get_opt_econum("remote_repos")`.
#' @param backup_existing Logical (`FALSE` by default). Should we backup an
#' object of the same name as the one that is loaded (it will become  `<obj>2`)?
#' @param ... Further arguments to pass to the function.
#' @return For `new_econum_data()`, the object that is just created. It is a
#' `data.frame` with a 'metadata' attribute, as a minimum, and it inherits from
#' both `EcoNumData` and `data.frame`.
#'
#' For `validate()`, the object is returned possibly corrected by using a series
#' of validation rules (and warnings are usually issued if something is
#' changed).
#'
#' For `repos_save()`, a vector of three named character strings, with 'object'
#' being the name of the object containing the item in the global workspace,
#' 'local_file' being the path to the `.RData` file in the local repository (or
#' `NA` if it was not saved there) and 'remote_file' being the path to the
#' `.Rdata` file as saved in the remote repository (or `NA` if it was not saved
#' there). This is returned invisibly.
#'
#' For `repos_load()`, the name of the object that is reloaded is returned
#' invisibly.
#' @author Philippe Grosjean \email{Philippe.Grosjean@@umons.ac.be}
#' @export
#' @name EcoNumData
#' @seealso [options_econum()], [time_to_fingerprint()]
#' @keywords misc
#' @concept GUI API implementation
#' @examples
#' \dontrun{
#' # Save some generic EcoNumData
#' res <- repos_save(new_ecomum_data(data.frame(x = 1, y = 2)), remote = FALSE)
#' # The data is in .GlobalEnv
#' EcoNumData
#' summary(EcoNumData)
#'
#' # Can be erased and reloaded from the repository later on
#' rm(EcoNumData)
#' get(repos_load(res["local_file"]))
#'
#' # Delete the object from memory
#' rm(EcoNumData)
#' # ... and from the disk
#' unlink(res["local_file"])
#' # If the project subdirectory is empty, remove it
#' local_dir <- dirname(res["local_file"])
#' if (length(dir(local_dir)) == 0) unlink(local_dir, recursive = TRUE)
#' rm(local_dir)
#' }
new_econum_data <- function(x, metadata = list(project = "", sample = "",
sample_date = get_opt_econum("def_sample_date"), author = "", date = Sys.time(),
comment = NULL, topic = NULL), class = NULL) {
  # Convert a data frame into a minimal EcoNumData object
  m <- metadata
  if (is.null(m$project) || m$project == "")
    m$project <- get_opt_econum("def_project")
  if (is.null(m$sample) || m$sample == "")
    m$sample <- get_opt_econum("def_sample")
  if (is.null(m$author) || m$author == "")
    m$author <- get_opt_econum("def_author")
  if (is.null(m$sample_date) || m$sample_date == "")
    m$sample_date <- get_opt_econum("def_sample_date")
  if (is.null(m$date) || m$date == "")
    m$date <- Sys.time()
  # Collect custom metadata at the end
  m2 <- m
  m2$project <- NULL
  m2$sample <- NULL
  m2$sample_date <- NULL
  m2$author <- NULL
  m2$date <- NULL
  m2$comment <- NULL
  m2$topic <- NULL
  # Create clean metadata list
  metadata <- c(list(project = m$project, sample = m$sample,
    sample_date = m$sample_date, author = m$author, date = m$date,
    comment = m$comment, topic = m$topic), m2)
  x <- as.data.frame(x)
  attr(x, "metadata") <- metadata
  class(x) <- c(class, "EcoNumData", "data.frame")
  x
}

#' @export
#' @rdname EcoNumData
print.EcoNumData <- function(x, ...) {
  cat("An EcoNumData object with:\n")
  dat <- as.data.frame(x) # Eliminate all attributes
  print(dat)
  invisible(x)
}

#' @export
#' @rdname EcoNumData
summary.EcoNumData <- function(object, ...) {
  class(object) <- c("summary.EcoNumData", "data.frame")
  object
}

#' @export
#' @rdname EcoNumData
print.summary.EcoNumData <- function(x, ...) {
  cat("An EcoNumData object with:\n")
  dat <- as.data.frame(x) # Eliminate all attributes
  print(dat)
  cat("\nWith following metadata:\n")
  print(attr(x, "metadata"))
  invisible(x)
}

#' @export
#' @rdname EcoNumData
validate <- function(object, ...)
  UseMethod("validate")

#' @export
#' @rdname EcoNumData
validate.EcoNumData <- function(object, file = NULL, ...) {
  # Basic validation method for general EcoNumData object
  # TODO: basic validate.EcoNumData()
  # For the moment, just return the object
  object
}

#' @export
#' @rdname EcoNumData
repos_save <- function(object, envir = .GlobalEnv, local = TRUE,
remote = TRUE, ...)
  UseMethod("repos_save")

#' @export
#' @rdname EcoNumData
repos_save.EcoNumData <- function(object, envir = .GlobalEnv, local = TRUE,
remote = TRUE, ...) {
  # TODO: check if objects with same fingerprints already exist in repositories
  # TODO: an option to replace a file or not!

  # Get metadata
  m <- attr(object, "metadata")
  # Construct context according to last class name
  cl <- class(object)[1]
  if (cl == "EcoNumData") {
    cl <- NULL
    obj_name <- "EcoNumData"
    sub_path <- m$project
    file_ext <- ".RData"
  } else {
    obj_name <- paste("EcoNumData", cl, sep = "_")
    sub_path <- file.path(m$project, cl)
    file_ext <- paste("_", cl, ".RData", sep = "")
  }
  # If there is a 'topic' in metadata, append it to obj_name
  if (!is.null(m$topic)) obj_name <- paste(obj_name, m$topic, sep = ".")
  # Get project, sample, sample_date and date to construct the filename
  # Fingerprint identifies a unique EcoNumData object
  fingerprint <- paste("_", time_to_fingerprint(m$date), file_ext, sep = "")
  file_name <- paste(m$sample, "_", format(m$sample_date, "%Y-%m-%d_%H.%M.%S"),
    fingerprint, sep = "")
  # file_pattern is the regular expression used to track replicated EcoNumData
  # objects (based on fingerprint) along the repository
  file_pattern <- paste(fingerprint, "$", sep = "")
  file_pattern <- gsub("\\.", "\\\\.", fingerprint)

  # Create an object named obj_name in envir
  assign(obj_name, object, envir = envir)
  res <- obj_name

  # Save in local repository
  local_repos <- get_opt_econum("local_repos")
  if (isTRUE(local) && file.exists(local_repos)) {
    # Make sure sub_path exists
    path <- file.path(local_repos, sub_path)
    if (!file.exists(path))
      dir.create(path, showWarnings = FALSE, recursive = TRUE)

    # Check possible duplicates in other projects
    # TODO: what to do in case of duplicates???

    # Save the object in the repository
    local_file <- file.path(path, file_name)
    ret <- try(save(list = obj_name, file = local_file), silent = TRUE)
    if (inherits(ret, "try-error")) {
      stop("Error while saving results in local repository:\n", ret)
    } else res[2] <- local_file
  } else res[2] <- NA

  # If available, save in remote repository
  remote_repos <- get_opt_econum("remote_repos")
  if (isTRUE(remote) && file.exists(remote_repos)) {
    # Make sure subPath exists
    path <- file.path(remote_repos, sub_path)
    if (!file.exists(path))
      dir.create(path, showWarnings = FALSE, recursive = TRUE)

    # TODO: look for already existing objects with same fingerprint

    # Save the object in the repository
    remote_file <- file.path(path, file_name)
    ret <- try(save(list = obj_name, file = remote_file), silent = TRUE)
    if (inherits(ret, "try-error")) {
      stop("Error while saving results in remote repository:\n", ret)
    } else res[3] <- remote_file
  } else res[3] <- NA
  names(res) <- c("object", "local_file", "remote_file")
  # Return object name and path to both repositories files
  invisible(res)
}

#' @export
#' @rdname EcoNumData
repos_load <- function(file, backup_existing = FALSE, ...) {
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
  object <- validate(get(res), file = file, ...)

  if (isTRUE(backup_existing)) {
    # If res object already exists in .GlobalEnv, resave as res2
    if (exists(res, envir = .GlobalEnv, inherits = FALSE))
      assign(paste(res, "2", sep = ""), get(res, envir = .GlobalEnv),
        envir = .GlobalEnv)
  }

  # Save this object in .GlobalEnv
  assign(res, object, envir = .GlobalEnv)
  invisible(res)
}
