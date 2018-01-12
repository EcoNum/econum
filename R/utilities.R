# General EcoNum options

# Note: we need these functions, currently defined in titrationX.Y-Z.R
# but need to move these functions here! These 3 functions are not exported
# to the NAMESPACE yet!
sdev_pHmeter <- function(name, id = "pH01", type = "serial", port = "com1",
mode = "4800,n,8,1", buffering = "none", ...) {
  obj <- list(...)
  obj$name <- name
  obj$id <- id
  obj$type <- type
  obj$port <- port
  obj$mode <- mode
  obj$buffering <- buffering
  classes <- rev(strsplit(name, " ", fixed = TRUE)[[1]])
  classes <- paste("pHmeter", classes, sep = "")
  class(obj) <- c(classes, "pHmeter", "sdev")
  obj
}

sdev_titrator <- function(name, id = "T01", type = "serial", port = "com1",
mode = "4800,n,8,1", buffering = "line", ...) {
  obj <- list(...)
  obj$name <- name
  obj$id <- id
  obj$type <- type
  obj$port <- port
  obj$mode <- mode
  obj$buffering <- buffering
  classes <- rev(strsplit(name, " ", fixed = TRUE)[[1]])
  classes <- paste("titrator", classes, sep = "")
  class(obj) <- c(classes, "titrator", "sdev")
  obj
}

sdev_sampler <- function(name, id = "S01", type = "serial",
port = "\\\\\\\\.\\\\com10", mode = "4800,n,8,1", buffering = "line",
maxpos = 16, ...) {
  obj <- list(...)
  obj$name <- name
  obj$id <- id
  obj$type <- type
  obj$port <- port
  obj$mode <- mode
  obj$buffering <- buffering
  obj$maxpos <- maxpos
  classes <- rev(strsplit(name, " ", fixed = TRUE)[[1]])
  classes <- paste("sampler", classes, sep = "")
  class(obj) <- c(classes, "sampler", "sdev")
  obj
}

#' Manage EcoNum options
#'
#' Examine, set or retrieve EcoNum-specific options.
#'
#' @param opt A list with all EcoNum options to set. Optional and if not
#' provided, a default list is created at first use of `options_econum()`.
#' @param key A character string with an option name.
#' @param default Default (optional) value to return for the option, in case it
#' is not defined.
#' @param value Anything to set as an option in the list.
#' @return For `options_econum()`, the whole list of options is returned. For
#' the other functions, associated value (or default value) is returned.
#' @author Philippe Grosjean \email{Philippe.Grosjean@@umons.ac.be}
#' @export
#' @seealso [EcoNumData], [time_to_fingerprint()]
#' @keywords utilities
#' @examples
#' # Remote and local EcoNumData repositories
#' get_opt_econum("local_repos")
#' get_opt_econum("remote_repos")
#'
#' # Default general metadata
#' get_opt_econum("def_project")
#' get_opt_econum("def_sample")
#' get_opt_econum("def_sample_date")
#' get_opt_econum("def_author")
#'
#' # Get some non-existing EcoNum option
#' get_opt_econum("nokey", default = "my default value")
#' # Define it
#' set_opt_econum("nokey", "some data")
#' get_opt_econum("nokey", default = "my default value")
#' # Eliminate this key
#' set_opt_econum("nokey", NULL)
#' get_opt_econum("nokey", default = "my default value")
options_econum <- function(opt) {
  if (missing(opt)) { # Create it
    opt <- getOption("econum", default = list())

    # Repositories options
    # Default mapping for Windows is t:/EcoNumData
    # For Linux, it is /media/EcoNumDataPublic/EcoNumData
    # and for Mac OS X, it is /Volumes/Public/EcoNumData
    if (.Platform$OS.type == "windows") {
      opt$localRepos <- "d:/EcoNumData"
      opt$remoteRepos <- "t:/EcoNumData"
    } else {
      opt$localRepos <- "~/EcoNumData"
      if (Sys.info()["sysname"] == "Darwin" ||
        file.exists("/Volumes/Public/EcoNumData")) {
        opt$remoteRepos <- "/Volumes/Public/EcoNumData"
      } else opt$remoteRepos <- "/media/EcoNumPublic/EcoNumData"
    }

    # Default general metadata
    opt$def_project <- "project00"
    opt$def_sample <- "test00"
    opt$def_sample_date <- Sys.time()
    opt$def_author <- "student"

    # List of possible devices
    opt$titrators <- list(
      SchottUniversal = sdev_titrator("Schott TitronicUniversal",
        id = "TU01", type = "serial", port = "com6",
        mode = "4800,n,7,1", buffering = "line"),
      SchottT200 = sdev_titrator("Schott TitronicT200",
        id = "T20001", type = "serial", port = "com5",
        mode = "2400,n,8,2", buffering = "line")
    )
    opt$pHmeters <- list(
      ConsortC3010 = sdev_pHmeter("Consort C3010",
        id = "C3010", type = "serial", port = "\\\\\\\\.\\\\com14",
        mode = "19200,n,8,2", buffering = "none"),
      ConsortP602 = sdev_pHmeter("Consort P602",
        id = "P602", type = "serial", port = "com9",
        mode = "2400,n,8,2", buffering = "none"),
        WTW340i = sdev_pHmeter("WTW 340i",
        id = "WTW340i", type = "serial", port = "com9",
        mode = "4800,n,8,2", buffering = "none")
    )
    opt$samplers <- list(
      SchottTWalphaPlus = sdev_sampler("Schott TWalphaPlus",
        id = "TW01", type = "serial", port = "\\\\\\\\.\\\\com11",
        mode = "4800,n,7,2", buffering = "line", maxpos = 16)
    )

    # Default devices
    opt$titrator <- sdev_titrator("Schott TitronicUniversal", id = "TU01",
      type = "serial", port = "com6", mode = "4800,n,7,1",
      #type = "serial", port = "/dev/cu.PL2303-00002006", mode = "4800,n,7,1",
      buffering = "line")
    opt$pHmeter <- sdev_pHmeter("Consort C3010", id = "C3010",
      type = "serial", port = "\\\\\\\\.\\\\com14", mode = "19200,n,8,2",
      #type = "serial", port = "/dev/cu.PL2303-00001004", mode = "19200,n,8,2",
      buffering = "none")
    opt$sampler <- sdev_sampler("Schott TWalphaPlus", id = "TW01",
      type = "serial", port = "\\\\\\\\.\\\\com11", mode = "4800,n,7,2",
      buffering = "line", maxpos = 16)
  }

  # Save these options in EcoNum
  options(econum = opt)
  opt
}

#' @export
#' @rdname options_econum
get_opt_econum <- function(key, default = NULL) {
  opt <- getOption("econum", default = options_econum())
  if (missing(key)) {
    return(opt)
  } else {
    value <- opt[[key]]
    if (is.null(value) && !missing(default))
      value <- default
    return(value)
  }
}

#' @export
#' @rdname options_econum
set_opt_econum <- function(key, value) {
  opt <- get_opt_econum()
  opt[[key]] <- value
  options_econum(opt)
  value
}


#' Calculate fingerprint from time object, or the opposite
#'
#' Get an hexadecimal unique identifier corresponding to time an object is
#' created. A time object from a hexadecimal fingerprint can be also obtained.
#'
#' @param Time A POSIXct object indicating time an `EcoNumData` object is
#' created.
#' @param hexmode An hexmode object, or a character string that can be converted
#' to an hexmode object.
#' @param tz The time zone in which to recreate the time object, by default, use
#' current time zone for this machine.
#' @return A character string with hexadecimal representation of the time. This
#' could be used as a unique identifier (fingerprint) for objects that are
#' sequencially created in time. The reverse function gets time from a
#' fingerprint.
#' @author \email{Philippe.Grosjean@@umons.ac.be}
#' @export
#' @seealso [new_econum_data()], [options_econum()]
#' @keywords utilities
#' @examples
#' (fp <- time_to_fingerprint(Sys.time()))
#' fingerprint_to_time(fp) # Reverse process
#' rm(fp)
time_to_fingerprint <- function(time, tz = "GMT") {
  # Calculate an hexadecimal "fingerprint" based on time (rounding down to sec)
  # Make sure time is expressed in UTC first
  time <- as.POSIXct(as.character(time), tz = tz)
  toupper(as.hexmode(as.integer(time)))
}

#' @export
#' @rdname time_to_fingerprint
fingerprint_to_time <- function(hexmode, tz = "GMT") {
  # Like with Sys.time(), it produces a POSIXct object with NULL tzone attribute
  # Fingerprint is always produced in UTC time!
  res <- as.POSIXct(strtoi(as.hexmode(hexmode), 16L),
    origin = "1970-01-01 00:00.00", tz = tz)
  res
}
