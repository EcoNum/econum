# Functions for WTW instruments (pH-meter, oxymeter, ...)
# TODO: calibration + calibration report
# TODO: read pressure in mbar + set/get salinity
# TODO: synch date and time with computer
# TODO: measure conductivity/salinity + mV
.wtw.port <- function(port) {
  # Make sure port is correct
  # Default value is "com1"
  if (is.null(port)) return("com1")
  # If only a number is provided, prepend "com"
  if (regexpr("^com[0-9]{1,2}$", tolower(port[1])) < 1) {
    return(paste("com", as.integer(port[1]), sep = ""))
  } else {
    # Make sure it is in lowercase and without attributes
    return(tolower(as.character(port[1])))
  }
}

wtw.open <- function(port = getOption("wtw.port")) {
  # Make sure the com port is closed
  port <- wtw.close(port)
  # Allow for a fake port "com0"
  if (port == "com0") {
    # Simulate port opening
    assign(".wtw.com0", TRUE, envir = .GlobalEnv)
    return(TRUE)
  }
  # Open and configure the port (no error handling yet!)
  .Tcl(paste("set wtw_", port, " [open \"", port, "\" r+]", sep = ""))
  .Tcl(paste("fconfigure $wtw_", port,
    " -mode \"4800,n,8,2\" -buffering none -blocking 0", sep = ""))
  # Check that the WTW instrument is responding
  wtw.ready(port)
}

wtw.close <- function(port = getOption("wtw.port")) {
  port <- .wtw.port(port)
  # Allow for a fake port "com0"
  if (port == "com0") {
    if (exists(".wtw.com0", envir = .GlobalEnv, inherits = FALSE))
      rm(list = ".wtw.com0", envir = .GlobalEnv)
  } else {
    try(.Tcl(paste("close $wtw_", port, sep = "")), silent = TRUE)
  }
  invisible(port)
}

wtw.ready <- function(port = getOption("wtw.port")) {
  port <- .wtw.port(port)
  if (port == "com0")
    return(exists(".wtw.com0", envir = .GlobalEnv))
  !is.null(wtw.read(port = port))
}

wtw.sim <- function() {
  # Simulate data from a WTW device (default function)
  pH <- round(runif(1, min = 7, max = 8), 2)
  O2 <- round(runif(1, min = 4, max = 5), 2)
  T <- round(runif(1, min = 20, max = 21), 1)
  c(pH = pH, O2 = O2, T = T)
}

wtw.cmd <- function(code, port = getOption("wtw.port")) {
  # Send a command to the device
  port <- .wtw.port(port)
  # Fake instrument
  if (port == "com0") return()
  # Real instrument
  try(.Tcl(paste("puts -nonewline $wtw_", port, " {", code, "\r}", sep = "")),
    silent = TRUE)
}

wtw.echo <- function(port = getOption("wtw.port")) {
  port <- .wtw.port(port)

  if (port == "com0") {
    # Echo of fake data
    if (!exists(".wtw.com0", envir = .GlobalEnv)) {
      cat("Error communicating with the WTW instrument\n")
      flush.console()
      return(invisible(character(0)))
    }
    dat <- get("wtw.sim", envir = .GlobalEnv, inherits = TRUE)()
    res <- "WTW fake instrument on port com0\n"
    cat(res)
    date <- format(Sys.Date(), "%d.%m.%y")
    time <- format(Sys.time(), "%H:%M")
    res2 <- paste(" ", date, "        ", time, "\n", sep = "")
    cat(res2)
    res <- paste(res, res2, collapse = "")
    res2 <- paste("pH ", dat["pH"], "         ", dat["T"], "  oC\n", sep = "")
    cat(res2)
    res <- paste(res, res2, collapse = "")
    res2 <- paste(" ", dat["O2"],  "  mg/l O2\n \n", sep = "")
    cat(res2)
    res <- paste(res, res2, collapse = "")
    flush.console()
  } else {
    # Echo of true data
    line <- "  "
    i <- 1
    res <- character(0)
    while (line != "" || i < 20) {
      Sys.sleep(0.1)
      i <- i + 1
      line <- try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))),
        silent = TRUE)
      if (inherits(line, "try-error")) {
        cat("Error communicating with the WTW instrument\n")
        flush.console()
        return(invisible(res))
      }
      if (line != "") {
        i <- 1
        res <- c(res, line)
        cat(line, "\n")
        flush.console()
      }
    }
  }
  invisible(res)
}

wtw.info <- function(port = getOption("wtw.port"), timeout = 1000) {
  # Read the signature of the WTW instrument
  port <- .wtw.port(port)

  if (port == "com0") {
    return("WTW fake")
  } else {
    # This is a real instrument
    # Make sure no old data is remaining in the input buffer
    res <- " "
    while (res != "" && !inherits(res, "try-error"))
      res <- try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))),
        silent = TRUE)
    try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))), silent = TRUE)
    # Send the command
    wtw.cmd("K.18", port = port)
    # Wait for WTW instrument response (wait until timeout max)
    tmax <- proc.time()[3] + timeout / 1000
    # We have to read several lines here: read until the line starting with 'pH'
    while (regexpr("^[0-9]{2}$", res) < 1 && !inherits(res, "try-error") &&
      proc.time()[3] < tmax)
      code <- try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))),
        silent = TRUE)
    if (regexpr("^[0-9]{2}$", res) > -1) {
      # Decrypt code into device name
      res <- switch(code,
        "10" = "WTW pH340",
        "11" = "WTW pH340/ION",
        "20" = "WTW OXI340",
        "30" = "WTW LF340",
        "40" = "WTW MultiLine P4",
        "41" = "WTW MultiLine P3 pH/Oxi",
        "42" = "WTW MultiLine P3 pH/LF",
        "18" = "WTW pH340i",
        "19" = "WTW pH/ION340i",
        "24" = "WTW OXI340i",
        "35" = "WTW Cond340i",
        "45" = "WTW pH/Oxi340i",
        "49" = "WTW pH/Cond340i",
        "44" = "WTW Multi340i",
        "60" = "WTW pH197i",
        "70" = "WTW Cond197i",
        "80" = "WTW Cond197i",
        "90" = "WTW Multi197i",
        paste("Unknown WTW with code", code))
      return(res)
    } else return(NULL)
  }
}

wtw.read <- function(port = getOption("wtw.port"), timeout = 1000, O2 = FALSE) {
  port <- .wtw.port(port)

  if (port == "com0") {
    # Read fake data
    if (!exists(".wtw.com0", envir = .GlobalEnv)) {
      return(c(pH = NA, O2 = NA, T = NA))
    }
    res <- get("wtw.sim", envir = .GlobalEnv, inherits = TRUE)()
    if (!isTRUE(O2)) res["O2"] <- NA # Do not read O2 data
    return(res)
  } else {
    # Read real data
    # Make sure no old data is remaining in the input buffer
    res <- " "
    res2 <- " "
     while (res != "" && !inherits(res, "try-error"))
      res <- try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))),
        silent = TRUE)
    try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))), silent = TRUE)
    # Send the command
    wtw.cmd("K.7", port = port)
    # Wait for WTW instrument response (wait until timeout max)
    tmax <- proc.time()[3] + timeout / 1000
    # We have to read several lines here: read until the line starting with 'pH'
    while (regexpr("^pH", res) < 1 && !inherits(res, "try-error") &&
      proc.time()[3] < tmax)
      res <- try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))),
        silent = TRUE)
    # Do we read also O2?
    if (isTRUE(O2)) {
      # Next line is O2 (on pH/Oxi 340i)
      while (regexpr("mg", res2) < 1 && !inherits(res2, "try-error") &&
        proc.time()[3] < tmax)
        res2 <- try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))),
          silent = TRUE)
    }
    # Flush one empty line send at the end
    try(tclvalue(.Tcl(paste("gets $wtw_", port, sep = ""))), silent = TRUE)
    # Extract data from the result
    if (regexpr("^pH", res) > -1) {
      pH <- as.numeric(substr(res, 4, 10))
      temp <- as.numeric(substr(res, 15, 20))
      if (regexpr("mg", res2) > -1) {
        O2 <- as.numeric(substr(res2, 1, 7))
      } else O2 <- NA
      # Note, we have also the temperature as measured by O2 probe
      # but ignored for the moment...
      return(c(pH = pH, O2 = O2, T = temp))
    } else return(c(pH = NA, O2 = NA, T = NA))
  }
}

# Collect data every interval seconds, write on disk and update a graph
wtw.record <- function(file = NULL, interval = 10, Nmax = 10,
port = getOption("wtw.port"), timeout = 1000, O2 = TRUE, init.data = NULL,
graph = TRUE, title = "WTW record", ...) {
  # If file is NULL, create a default name
  if (is.null(file)) {
    file <- paste("WTW_", format(Sys.Date(), "%Y-%m-%d"), ".txt", sep = "")
    file <- file.path(dirname(tempdir()), file)
  }
  # Make sure the port is (re)opened properly
  port <- wtw.close(port = port)
  wtw.open(port = port)
  on.exit(wtw.close(port))
  interval <- as.numeric(interval[1]) # Interval in seconds
  Nmax <- as.integer(Nmax[1]) # Maximum number of observations to collect

  cat("Collecting data from WTW instrument\nHit escape to stop\n")
  flush.console()
  if (!is.null(init.data)) {
    dat <- init.data
    # Check that these data are correct
    if (!inherits(dat, "data.frame"))
      stop("init.data must be adata frame!")
    if (is.null(names(dat) ||
      any(names(dat) != c("Date", "Time", "pH", "O2", "T"))))
      stop("init.data must have Date/Time/pH/O2/T columns")
  } else {
    dat <- data.frame(Date = character(0), Time = character(0),
    pH = numeric(0), O2 = numeric(0), T = numeric(0),
    stringsAsFactors = FALSE)
  }
  attr(dat, "file") <- file

  # Loop for the number of observations we want
  for (i in 1:Nmax) {
    res <- wtw.read(port = port, timeout = timeout, O2 = O2)
    # Add date and time
    date <- format(Sys.Date(), "%Y-%m-%d")
    time <- format(Sys.time(), "%H:%M:%S")
    cat(time, ": pH = ", res[1], ", O2 = ", res[2], " mg/l, T = ", res[3],
      "deg.C\n", sep = "")
    flush.console()
    dat <- rbind(dat, data.frame(Date = date, Time = time,
      pH = as.numeric(res[1]), O2 = as.numeric(res[2]),
      T = as.numeric(res[3]), stringsAsFactors = FALSE))

    # Save a copy in .GlobalEnv
    assign("wtw.dat", dat, envir = .GlobalEnv)

    # Write data to file
    if (!is.na(file)) {
      if (!file.exists(file))
        cat("Date\tTime\tpH\tO2\tT\n", file = file)
      cat(paste(date, "\t", time, "\t", res[1], "\t", res[2], "\t", res[3],
        "\n", sep = ""), file = file, append = TRUE)
    }

    if (isTRUE(graph)) {
      # Update the graph
      #tt <- as.POSIXct(strptime(as.character(dat$Time),
      #  format = "%H:%M:%S"))

      tt <- as.POSIXct(strptime(paste(as.character(dat$Date),
        as.character(dat$Time)), format = "%Y-%m-%d %H:%M:%S"))
      print(tt)
      print(dat$pH)
      # Depending if we have pH and/or O2 data, label differs
      if (!is.null(dat$pH) && any(!is.na(dat$pH)))
        isPh <- TRUE else isPh <- FALSE
      if (!is.null(dat$O2) && any(!is.na(dat$O2)))
        isO2 <- TRUE else isO2 <- FALSE
      # Get graph limits
      Ylim <- range(dat[, c("pH", "O2")], na.rm = TRUE)
      # Range must be at least 2 units
      span <- Ylim[2] - Ylim[1]
      if (span < 2) {
        enlarge <- (2 - span) / 2
        Ylim[1] <- Ylim[1] - enlarge/2
        Ylim[2] <- Ylim[2] + enlarge/2
      }
      # Determine what to plot
      if (isTRUE(isPh)) {
        if (isTRUE(isO2)) {
          Ylab <- expression(paste("pH - ", O[2], " (mg/L)"))
          plot(tt, dat$pH, ylim = Ylim, type = "l", xlab = "Time",
            ylab = Ylab, xaxt = "n")
          lines(tt, dat$O2, col = 2)
        } else {
          Ylab <- "pH"
          plot(tt, dat$pH, ylim = Ylim, type = "l", xlab = "Time",
            ylab = Ylab, xaxt = "n")
        }
      } else {
        if (isTRUE(isO2)) {
          Ylab <- expression(paste(O[2], " (mg/L)"))
          plot(tt, dat$O2, ylim = Ylim, type = "l", xlab = "Time",
            ylab = Ylab, xaxt = "n")
        } else {
          Ylab <- NULL	# Nothing to plot
        }
      }
      # Do we plot also temperature (possibly on a second axis)?
      if (!is.null(dat$T) && any(!is.na(dat$T))) {
        if (is.null(Ylab)) {
          # Plot ONLY temperature
          plot(tt, dat$T, type = "l", xlab = "Time", xaxt = "n",
            ylab = expression(paste("Temperature (", degree, "C)")))
        } else {
          # Add temperature on the second axis
          # TODO: rescale correctly
          lines(tt, dat$T - 20, col = 4)
          # Add second y-axis
          axis(side = 4, at = (4:20)/2, labels = (44:60)/2)
          mtext(expression(paste("Temperature (", degree, "C)")),
            side = 4, line = 2)
        }
      }
      # Add the title
      title("title")
      # Add x-axis
      axis(side = 1, at = tt, labels = format(tt, "%H:%M:%S"))
      grid()

      # Create legend
      # TODO: calculate legend according to what to plot
      # TODO: options cols, lwds and ltys for the three curves
      legend(tt[1], 10, c("pH", "O2", "T"), col = c(1, 2, 4), lty = 1)
    }
    # Wait until time interval
    # Do we need to change speed?
    speed <- getOption("econum.speed")
    if (!is.null(speed)) {
      speed <- as.numeric(speed[1])
    } else speed <- 1
    Sys.sleep(interval / speed)
  }
  invisible(dat)
}
