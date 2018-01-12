#' Control the IKS Aquastar computer from within R
#'
#' Connect to an IKS Aquastar through a serial (com) port and control it.
#'
#' @param port A serial port such as `"com1"`, or its number such as `1`. For
#' `iks_close()`, you can also specify `"all"` to close all IKS ports.
#' @param iks_name The name you want to use for the IKS Aquastar module.
#' @param probes_names The name to use for the eight IKS probes.
#' @param sockets_names The name to use for the 16 IKS sockets.
#' @param procfun An \R function used to process data returned by the IKS
#' Aquastar.
#' @param msg The message send back from the IKS Aquastar.
#' @param code Code to send to the IKS Aquastar.
#' @param new_line Do we add a newline after the code send (yes by default)?
#' @param wait Time to wait between characters send to the IKS Aquastar (500ms
#' by default). Do not use too low values, or the device will not receive all
#' code correctly.
#' @param x An `iksdata` object.
#' @param ... Further arguments passed to the `print()` method.
#' @param data The IKS data to record (this is a list with components name,
#' port, version, config, probes, units, triggers, sockets, sockets_ctrl and time.
#' @return `iks_open()` returns `TRUE` in case of success, `FALSE` otherwise.
#'
#' `iks_close()` returns the com port(s) that was/were closed.
#'
#' `iks_cmd()` and `iks_info()` return nothing. They are used for their side
#' effects of triggering some response by the IKS Aquastar. That response is
#' treated in an asynchronous way by the `procfun` provided to `iks_open()`.
#'
#' `iks_get_all()` gets the list of all currently connected IKS Aquastars (a
#' vector of characters with corresponding com ports, and with names as
#' specified in the `iks_name` argument of `iks_open()`).
#'
#' `iks_get_name()` gets the name of an IKS Aquastar, providing the com port it
#' is connected to.
#'
#' `iks_get_data()` and `iks_set_data()` are used to retrieve or set the current
#' state of the IKS. `iks_set_data()` should not be used in a different context
#' than an `iks_process()` and should update the IKS data according to what is
#' received from the IKS Aquastar trough the com port.
#'
#' `iks_get_config()` triggers a dumping of the complete configuration of the
#' IKS Aquastar... it takes a couple of seconds to complete! After this
#' command, the whole current config is stored in binary format in the `config`
#' component and the current state of sockets is updated.
#'
#' `iks_process()` is the default workhorse function to process incoming IKS
#' data. It can be replaced by you own custom function, but it is already pretty
#' flexible. The default function allows to call custom events in the form of R
#' functions that accept two arguments, `data` and `iks_name`. They must be
#' saved in the `TempEnv` environment and have predefined names, depending on
#' the event that should trigger them: `iks_date_proc()` when the date is
#' received from the IKS, indicating a complete cycle of data sending,
#' `iks_socket_proc()` on receiving socket config data, and `iks_config_proc()`
#' on receiving IKS config data.
#' @author Philippe Grosjean \email{Philippe.Grosjean@@umons.ac.be}
#' @name iks
#' @export
#' @seealso [wtw_open()], [gce8_open()]
#' @keywords utilities
#' @concept hardware control
#' @examples
#' \dontrun{
#' iks_get_all()
#' iks_open(port = "com1") # Change to the port where your IKS is connected!
#' iks_get_all()
#' iks_get_name()
#'
#' # To debug exchanges with the IKS aquastar:
#' options(debug_iks = TRUE)
#'
#' iks_get_data()   # Current data as recorded in the IKS
#' iks_get_config() # Get the whole IKS config
#' iks_get_data()   # Should be visible now!
#'
#' # Stop the connection with the IKS
#' iks_close()
#' iks_get_all()
#' }
print.iksdata <- function(x, ...) {
  # Print IKS state in a nice way
  cat(x$name, ": IKS aquastar on ", x$port, "\n", sep = "")
  if (!is.null(x$version))
    cat("(reported as: ", x$version, ")\n", sep = "")

  i_time <- x$time["iks"]
  p_time <- x$time["probes"]
  s_time <- x$time["sockets"]

  if (!is.null(p_time) && !is.na(p_time)) {
    if (!is.null(i_time) && !is.na(i_time)) {
      # Check time lag between IKS and computer
      time_diff <- as.double(p_time - i_time, units = "secs")
      if (time_diff < 61 & time_diff > -61) {
        cat("(computer and IKS times are synchrone: less than 1 min difference)\n")
      } else {
        cat("Computer and IKS times are different:\n")
        print(time_diff)
      }
    }
    # Now, print the probes data
    cat("\nProbes, as on ", format(p_time), ":\n", sep = "")
    # Align on 8 characters
    #Name :     p01     p02     p03     p04     p05     p06     p07     p08
    #Type :      pH      pH      pH      Te      Te      Ox      Ox      Ox
    #Value:    3.81    3.84    3.76    18.5    19.7   100.3    99.1    98.6
    #Unit :                              °C      °C    %sat    %sat    %sat
    #Trig.:               *               +       -
    cat("Name :", format(abbreviate(x$names_probes, 8), width = 8,
      justify = "right"), "\n")
    types <- as.character(x$probes$type)
    types[is.na(types)] <- ""
    cat("Type :", format(abbreviate(types, 8), width = 8,
      justify = "right"), "\n")
    cat("Value:", format(abbreviate(x$probes$value, 8), width = 8,
      justify = "right"), "\n")
    units <- as.character(x$probes$unit)
    units[is.na(units)] <- ""
    cat("Units:", format(abbreviate(units, 8), width = 8,
      justify = "right"), "\n")
    triggers <- as.character(x$probes$trigger)
    triggers[is.na(triggers)] <- ""
    cat("Trig.:", format(abbreviate(triggers, 8), width = 8,
      justify = "right"), "\n")
  }
  if (!is.null(s_time) && !is.na(s_time)) {
    # Also print the sockets state
    cat("\nSockets, as on ", format(s_time), ":\n", sep = "")
    # Align on 8 characters too
    #Name :
    #Ctrl :
    #State: indicate between brackets if unplugged
    state <- paste(x$sockets$state0, "%", sep = "")
    state[state == "0%"] <- "off"
    state[state == "100%"] <- "ON"
    state[!x$sockets$plugged] <- NA
    #Alarm:
    alarm <- rep("", 17)
    alarm[x$sockets$alarm] <- "*"
    # Take care that first socket is at line #2 in the table!
    cat("Name :", format(abbreviate(x$names_sockets[1:8], 8), width = 8,
      justify = "right"), "\n")
    cat("Ctrl :", format(abbreviate(x$sockets$control[2:9], 8), width = 8,
      justify = "right"), "\n")
    cat("State:", format(abbreviate(state[2:9], 8), width = 8,
      justify = "right"), "\n")
    cat("Alarm:", format(abbreviate(alarm[2:9], 8), width = 8,
      justify = "right"), "\n")
    cat("\n")
    cat("Name :", format(abbreviate(x$names_sockets[9:16], 8), width = 8,
      justify = "right"), "\n")
    cat("Ctrl :", format(abbreviate(x$sockets$control[10:17], 8), width = 8,
      justify = "right"), "\n")
    cat("State:", format(abbreviate(state[10:17], 8), width = 8,
      justify = "right"), "\n")
    cat("Alarm:", format(abbreviate(alarm[10:17], 8), width = 8,
      justify = "right"), "\n")
  }
  invisible(x)
}

.iks_port <- function(port) {
  # Make sure port is correct
  if (!is.null(port) && port == "all") return("all")
  # Default value is "com1"
  if (is.null(port)) return("com1")
  # If only a number is provided, prepend "com"
  if (grepl("^[0-9]+$", port[1])) {
    num_port <- as.integer(port[1])
  } else num_port <- NA
  if (!is.na(num_port)) {
    # If port is higher than 9,
    # name must be \\\\\\\\.\\\\comXX under Windows
    if (num_port > 9) {
      return(paste("\\\\\\\\.\\\\com", num_port, sep = ""))
    } else {
      return(paste("com", num_port, sep = ""))
    }
  } else {
    return(as.character(port[1]))
  }
}

.iks_make_names <- function(port) {
  # Get a clean version of the port (something that can decently be used as proc name)
  gsub("\\.", "_", make.names(gsub("^[\\.]+", "", port)))
}

#' @export
#' @rdname iks
iks_process <- function(msg, iks_name = "iks") {
  # Do we receive the date?
  is_date <- FALSE
  # Do we receive socket data?
  is_socket <- FALSE
  # Do we receive config data?
  is_config <- FALSE
  # The default R function that processes the text send by an IKS aquastar
  #cat(iks_name, "> ", msg, "\n", sep = "")
  data <- iks_get_data(iks_name = iks_name)
  # and update corresponding data in Data
  if (regexpr("^E[1-8] ", msg) > 0) {

    # It is some probe data
    probes <- data$probes
    if (is.null(probes)) {
      # Create a data frame with default data
      type <- factor(rep(NA, 8),
        levels = c("Cd", "Lv", "O2", "Pa", "pH", "Rx", "Te"))
      value <- as.numeric(rep(NA, 8))
      unit <- as.character(rep(NA, 8))
      trigger <- factor(rep(NA, 8), levels = c("-", " ", "+", "*"))
      probes <- data.frame(type = type, value = value,
        unit = unit, trigger = trigger, stringsAsFactors = FALSE)
      rownames(probes) <- paste("p", 1:8, sep = "")
    }

    # Not calibrate
    if (regexpr("^E[1-8] Not calibrate", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      probes$value[chan] <- NaN # This indicates a not calibrated probe
      # In comparison to NA that indicates no probe on this port
    }

    # pH
    if (regexpr("^E[1-8] [(]pH", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(]pH.[)]([^ ]*) .*$", "\\1", msg)
      dat <- try(as.numeric(dat), silent = TRUE)
      probes$type[chan] <- "pH"
      probes$value[chan] <- dat
      probes$unit[chan] <- ""
      trigger <- sub("^E[1-8] [(]pH(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Oxygen
    if (regexpr("^E[1-8] [(][Oo]x", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(][Oo]x.[)]([0-9. ]*)[^0-9].*$", "\\1", msg)
      dat <- try(as.numeric(dat), silent = TRUE)
      probes$type[chan] <- "O2"
      probes$value[chan] <- dat
      # Determines unit used
      if (regexpr("% *$", msg) > 0)
        probes$unit[chan] <- "%sat" else probes$unit[chan] <- "mg/L"
      trigger <- sub("^E[1-8] [(][Oo]x(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Temperature
    if (regexpr("^E[1-8] [(][Tt]e", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(][Tt]e.[)] ([^ ]*) .*$", "\\1", msg)
      dat <- try(as.numeric(dat), silent = TRUE)
      probes$type[chan] <- "Te"
      probes$value[chan] <- dat
      if (regexpr("C *$", msg) > 0)
        probes$unit[chan] <- "degC" else probes$unit[chan] <- "degF"
      trigger <- sub("^E[1-8] [(][Tt]e(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Pressure (both English and French version)
    if (regexpr("^E[1-8] [(](AP|Ld|ap|ld)", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(]...[)] *([^ ]*) .*$", "\\1", msg)
      dat <- try(as.numeric(dat), silent = TRUE)
      probes$type[chan] <- "Pa"
      probes$value[chan] <- dat
      probes$unit[chan] <- "mB"
      trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Water level (both English and French version)
    if (regexpr("^E[1-8] [(](Lv|Pe|lv|pe)", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(]...[)] *([^ ]*).*$", "\\1", msg)
      if (tolower(dat) == "air") dat <- 0 else dat <- 1
      probes$type[chan] <- "Lv"
      probes$value[chan] <- dat
      probes$unit[chan] <- ""
      trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Conductivity (English)
    if (regexpr("^E[1-8] [(](Co|Le|co|le)", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(]...[)] *([^ ]*).*$", "\\1", msg)
      dat <- try(as.numeric(dat), silent = TRUE)
      probes$type[chan] <- "Cd"
      probes$value[chan] <- dat
      if (regexpr("uS *$", msg) > 0)
        probes$unit[chan] <- "uS" else if (regexpr("mS *$", msg) > 0)
        probes$unit[chan] <- "mS" else probes$unit[chan] <- ""
      trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Redox (English and untested!)
    if (regexpr("^E[1-8] [(](Re|Rx|rH|re|rx|rh)", msg) == 1) {
      chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
      dat <- sub("^E[1-8] [(]...[)] *([^ ]*).*$", "\\1", msg)
      dat <- try(as.numeric(dat), silent = TRUE)
      probes$type[chan] <- "Rx"
      probes$value[chan] <- dat
      if (regexpr("mV *$", msg) > 0)
        probes$unit[chan] <- "mV" else probes$unit[chan] <- "rH"
      trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
      probes$trigger[chan] <- trigger
    }

    # Record these results back into Data
    data$probes <- probes
  } else if (regexpr("^E[0-9]", msg) > 0) {
    # This must be the time and date as used by the IKS aquastar
    is_date <- TRUE
    # Computer time is:
    c_date <- Sys.time()
    # IKS date is decrypted
    time <- sub("^E([0-9]+:[0-9]+).*$", "\\1", msg)
    date <- sub("^E.*, ([0-9]+)\\.([0-9]+).*$", "\\2-\\1", msg)
    # Create a complete date with this (assume "00" for sec, and same year)
    iks_date <- paste(format(Sys.time(), "%Y"), "-", date, " ", time, ":00",
      sep = "")
    iks_date <- as.POSIXct(iks_date)
    # Record the Time info
    time <- data$time
    if (is.null(time)) {
      time <- as.POSIXct(rep(NA, 3))
      names(time) <- c("iks", "probes", "sockets")
    }
    time[1] <- iks_date
    time[2] <- c_date
    data$time <- time
  } else if (regexpr("^aquastar", msg) > 0) {
    # This is the identifier, as obtained with iks.info()
    data$version <- msg
  } else if (regexpr("End RAM$", msg) > 0) {
    # This is probably the dumping of RAM data (IKS config)
    is_config <- TRUE
    data$config <- sub("End RAM$", "", msg)
  } else if (regexpr("Socket End$", msg) > 0) {
    # This is info about the state of the sockets
    is_socket <- TRUE
    # Record the Time we got sockets state
    time <- data$time
    if (is.null(time)) {
      time <- as.POSIXct(rep(NA, 3))
      names(time) <- c("iks", "probes", "sockets")
    }
    time[3] <- Sys.time()
    data$time <- time
    # Decrypt and record socket state
    sockstr <- sub("Socket End$", "", msg)
    # Split the socket into numbers (two digits each time) in hexmode
    sockhex <- gsub("(..)", "-\\1", sockstr)
    sockhex <- substr(sockhex, 2, 357)
    sockhex <- strsplit(sockhex, "-", fixed = TRUE)[[1]]
    sockint <- apply(matrix(sockhex), 1, as.hexmode)
    sockint <- matrix(sockint, ncol = 7, byrow = TRUE)
    sockdata <- as.data.frame(sockint, stringsAsFactors = FALSE)
    names(sockdata) <- c("plugged", "control", "state0", "state1", "max_val",
      "min_val", "alarm")
    rownames(sockdata) <- sprintf("s%02d", 0:16)
    sockdata$plugged <- as.logical(sockdata$plugged)
    # Rem: here I am not sure that redox control is in this position!
    sockdata$control <- c("none", "pH", "temper", "redox", "conduct", "05",
      "oxygen", "level", "08", "09", "0A", "0B", "interval", "timer",
      "current", "curr.ht", "lunar", "day/nite",
      "alarm")[sockdata$control + 1]
    sockdata$alarm <- as.logical(sockdata$alarm)
    data$sockets <- sockdata
  }
  # Save the modified data
  iks_set_data(data, iks_name = iks_name)
  # If date or socket data received, possibly trigger another function
  if (is_date && !is.null(getTemp("iks_date_proc")))
    getTemp("iks_date_proc")(data = data, iks_name = iks_name)
  if (is_socket && !is.null(getTemp("iks_socket_proc")))
    getTemp("iks_socket_proc")(data = data, iks_name = iks_name)
  if (is_config && !is.null(getTemp("iks_config_proc")))
    getTemp("iks_config_proc")(data = data, iks_name = iks_name)
  invisible(msg)
}

#' @export
#' @rdname iks
iks_get_all <- function() {
  # Get the list of currently connected IKS aquastars
  getTemp("IKSs")$devices
}

#' @export
#' @rdname iks
iks_get_name <- function(port = getOption("iks_port")) {
  # Return the name of a given IKS
  port <- .iks_port(port)
  IKSs <- getTemp("IKSs")$devices
  sport <- .iks_make_names(port)
  if (!(sport %in% IKSs)) return(NULL)
  iks_names <- names(IKSs)
  iks_names[IKSs == sport]
}

#' @export
#' @rdname iks
iks_get_data <- function(port = getOption("iks_port"), iks_name = NULL) {
  if (missing(iks_name)) iks_name <- iks_get_name(port)
  if (is.null(iks_name)) return(NULL)
  IKSs <- getTemp("IKSs")
  IKSs[[iks_name]]
}

#' @export
#' @rdname iks
iks_set_data <- function(data, port = getOption("iks_port"), iks_name = NULL) {
  if (!inherits(data, "iksdata")) stop("'data' must be an 'iksdata' object")
  if (missing(iks_name)) iks_name <- iks_get_name(port)
  if (is.null(iks_name)) return(FALSE)
  IKSs <- getTemp("IKSs")
  IKSs[[iks_name]] <- data
  assignTemp("IKSs", IKSs)
  TRUE
}

#' @export
#' @rdname iks
iks_open <- function(port = getOption("iks_port"), iks_name = "iks",
probes_names = paste("p0", 1:8, sep = ""),
sockets_names = sprintf("s%02d", 1:16), procfun = iks_process) {
  # This is the main function that starts the connection to one IKS aquastar
  # iks_proc here under is the R workhorse function that do the computation

  iks_name <- as.character(iks_name[1])

  probes_names <- as.character(probes_names)
  if (length(probes_names) != 8)
    stop("'probes_names' must be a vector of eight names")
  names(probes_names) <- paste("p0", 1:8, sep = "")

  sockets_names <- as.character(sockets_names)
  if (length(sockets_names) != 16)
    stop("'sockets_names' must be a vector of 16 names")
  names(sockets_names) <- sprintf("s%02d", 1:16)

  is.function(procfun) || stop("'procfun' must be a function!")
  # Note: the data send by the IKS must be read from the Tcl $::IKSMsg variable

  # Make sure the com port is closed
  port <- .iks_port(port)
  item <- .iks_make_names(port)
  iks_close(port)

  # We need Tcl to be able to call an R functions to process IKS data
  tcl_proc_exists <- function(proc) {
    proc <- as.character(proc[1])
    return(length(as.character(tcl("info", "commands", proc))) == 1)
  }

  if (!tcl_proc_exists("iks_proc")) {
    # Create the callback when a client sends data
    iks_proc <- function() {
      # Note: I don't know how to pass arguments here.
      # So, I use Tcl global variables instead:
      # - the server port from $::iks_port,
      # - and the message from $::iks_msg
      tcl_get_value_ <- function(name) {
        # Get the value stored in a plain Tcl variable
        if (!is.character(name)) stop("'name' must be a character!")

        # Create a temporary dual variable with tclVar()
        temp <- tclVar(init = "")

        # Copy the content of the var of interest to it
        .Tcl(paste("catch {set ", as.character(temp), " $", name, "}",
          sep = ""))

        # Get the content of the temporary variable
        tclvalue(temp) # (is destroyed when the function exits)
      }

      port <- tcl_get_value_("::iks_port")
      if (port == "") return(FALSE) # The connection with this IKS is closed
      msg <- tcl_get_value_("::iks_msg")
      if (msg == "") return(FALSE) # No message!

      # Make sure this message is not processed twice
      .Tcl("set ::iks_msg {}")

      # Do we have to debug IKS transactions
      if (isTRUE(getOption("debug_iks")))
        cat("IKS on ", port, ": ", msg, "\n", sep = "")

      # The function that processes the client request is iks_proc_<item>
      proc <- getTemp(paste("iks_proc", item, sep = "_"),
        mode = "function")
      if (is.null(proc)) return(FALSE) # The server is probably closed
      # Call this function
      res <- proc(msg = msg, iks_name = iks_get_name(port))
      return(TRUE) # The command is processed
    }

    # This is a copy of tclFun from tcltk2, to avoid a Depends: tcltk2
    tcl_fun_ <- function(f, name = deparse(substitute(f))) {
      # Register a simple R function (without arguments) as a callback
      # in Tcl, and give it the same name)
      # Indeed, .Tcl.callback(f) in tcltk package does the job...
      # but it gives cryptic names like R_call 0x13c7168
      # Check that 'f' is a function with no arguments
      # (cannot handle them, currently)
      if (!is.function(f))
        stop("'f' must be a function!")
      if (!is.null(formals(f)))
        stop("The function used cannot (yet) have arguments!")
      # Make sure the name of the function is valid
      if (!is.character(name)) {
        stop("'name' must be a character string!")
      } else {
        name <- make.names(name[1])
      }
      res <- .Tcl.callback(f)
      # Make sure this is correct (R_call XXXXXXXX)
      if (length(grep("R_call ", res) > 0)) {
        # Create a proc with the same name in Tcl
        .Tcl(paste("proc ", name, " {} {", res, "}", sep = ""))
      }
      # Return the R_call XXXXXXXX string, as .Tcl.callback() does
      res
      # Rem: if you delete the R 'f' function,
      # the Tcl 'f' function still works (?!)
    }
    tcl_fun_(iks_proc)
  }

  # Copy procfun into TempEnv as IKSProc_<item>
  assignTemp(paste("iks_proc", item, sep = "_"), procfun)

  # Create the Tcl function that retrieves data from the IKS
  cmd <- paste(c(paste("proc iks_handler_", item, " {iks} {", sep = ""),
    "if {[eof $iks] == 1 || [catch {gets $iks line}]} {",
    "    # end of file or abnormal connection drop",
    "    fileevent $iks readable {}",
    "    close $iks",
    paste("    #puts \"Close $iks_", item, "($iks)\"", sep = ""),
    # I've got once: can't unset ...: no such variable after a USB4
    # deconnection => protect myself against this!
    paste("    catch { unset iks_", item, "($iks) }", sep = ""),
    "} else {",
    "    global iks_port",
    "    global iks_msg",
    paste("    set ::iks_port", port),
    "    set ::iks_msg $line",
    "    iks_proc    ;# process the command in R",
    "}\n}"), collapse = "\n")
  # if {[gets $port line] < 0} {return} # To handle incomplete lines!
  .Tcl(cmd)

  # Connect to an IKS aquastar
  # TODO: catch possible error here, due to wrong com port and test IKS presence
  .Tcl(paste("set iks_", item, " [open \"", port, "\" r+]", sep = ""))
  .Tcl(paste("fconfigure $iks_", item,
    " -mode \"9600,n,8,1\" -buffering line -blocking 0", sep = ""))
  .Tcl(paste("fileevent $iks_", item,
    " readable [list iks_handler_", item, " $iks_", item, "]", sep = ""))

  # Add this port in the TempEnv variable 'IKSs$Devices'
  IKSs <- getTemp("IKSs")
  dev <- IKSs$devices
  names_dev <- names(dev)
  if (!(item %in% dev)) {
    dev <- c(dev, item)
    names(dev) <- c(names_dev, iks_name)
    dev <- sort(dev)
    IKSs$devices <- dev
    # Record basic info about this IKS device
    info <- list(name = iks_name, port = port, names_probes = probes_names,
      names_sockets = sockets_names)
    class(info) <- c("iksdata", "list")
    IKSs[[iks_name]] <- info
    assignTemp("IKSs", IKSs)
  } else stop("Com port already registered as an IKS device... Should never occur!")
  # Get info about this IKS
  iks_info(port = port)
  TRUE # Humm! Only if it succeed... TODO: check this!
}

#' @export
#' @rdname iks
iks_close <- function(port = getOption("iks_port")) {
  # Stop one or more connections to IKS aquastars
  dev <- iks_get_all()
  port <- .iks_port(port)
  if (port == "all") port <- dev
  any_closed <- FALSE
  for (i in 1:length(port)) {
    iks_port <- port[i]
    item <- .iks_make_names(iks_port)
    if (item %in% dev) { # This port is open
      any_closed <- TRUE
      name <- names(dev)[dev == item]
      dev <- dev[dev != item]
      if (length(dev) == 0) {
        rmTemp("IKSs")
      } else {
        # Clean up IKSs data
        IKSs <- getTemp("IKSs")
        IKSs$devices <- dev
        IKSs[[name]] <- NULL
        assignTemp("IKSs", IKSs)
      }

      # Eliminate the processing function from TempEnv
      rmTemp(paste("iks_proc", item, sep = "_"))

      # Close the connection to the given IKS
      try(.Tcl(paste("close $iks_", item, sep = "")), silent = TRUE)
    }
  }
  invisible(any_closed)
}

#' @export
#' @rdname iks
iks_cmd <- function(code, port = getOption("iks_port"), new_line = TRUE) {
  # Send the given code to one or more IKS aquastars through their comm port
  port <- .iks_port(port)
  if (port == "all") port <- iks_get_all()
  if (!is.null(port) && length(port) > 0) {
    for (i in 1:length(port)) {
      if (isTRUE(new_line)) {
        .Tcl(paste("puts -nonewline $iks_", .iks_make_names(port[i]),
          " {", code, "}", sep = ""))
      } else {
        .Tcl(paste("puts $iks_", .iks_make_names(port[i]),
          " {", code, "}", sep = ""))
      }
    }
  }
}

#' @export
#' @rdname iks
iks_info <- function(port = getOption("iks_port"), wait = 500) {
  port <- .iks_port(port)
  item <- .iks_make_names(port)
  .Tcl(paste("puts $iks_", item, " \10", sep = ""))
  .Tcl(paste("after", wait))
  .Tcl(paste("puts $iks_", item, " D", sep = ""))
  .Tcl(paste("after", wait))
  .Tcl(paste("puts $iks_", item, " V", sep = ""))
  .Tcl(paste("after", wait))
  .Tcl(paste("puts $iks_", item, " \10", sep = ""))
}

#' @export
#' @rdname iks
iks_get_config <- function(port = getOption("iks_port"), wait = 500) {
  port <- .iks_port(port)
  item <- .iks_make_names(port)
  .Tcl(paste("puts $iks_", item, " \10", sep = ""))
  .Tcl(paste("after", wait))
  .Tcl(paste("puts $iks_", item, " D", sep = ""))
  .Tcl(paste("after", wait))
  .Tcl(paste("puts $iks_", item, " R", sep = ""))
}

##' @export
##' @rdname iks
#iks_ready <- function (port = getOption("iks_port")) {
#  port <- .iks_port(port)
#  # TODO: how to test this?
#  TRUE
#  #!is.null(iks_version(port = port))
#}

##' @export
##' @rdname iks
#iks_read <- function(port = getOption("iks_port"), wait = 500) {
#  port <- .iks_port(port)
#  item <- .iks_make_names(port)
#  .Tcl(paste("puts $iks_", item, " \10", sep = ""))
#  .Tcl(paste("after", wait))
#  .Tcl(paste("puts $iks_", item, " D", sep = ""))
#  .Tcl(paste("after", wait))
#  .Tcl(paste("puts $iks_", item, " A", sep = ""))
#}

##' @export
##' @rdname iks
#iks_header <- function(port = getOption("iks_port"), wait = 500) {
#  port <- .iks_port(port)
#  item <- .iks_make_names(port)
#  .Tcl(paste("puts $iks_", item, " \10", sep = ""))
#  .Tcl(paste("after", wait))
#  .Tcl(paste("puts $iks_", item, " D", sep = ""))
#  .Tcl(paste("after", wait))
#  .Tcl(paste("puts $iks_", item, " E", sep = ""))
#}

##' @export
##' @rdname iks
#iks_load_data <- function(port = getOption("iks_port"), wait = 500) {
#  port <- .iks_port(port)
#  item <- .iks_make_names(port)
#  .Tcl(paste("puts $iks_", item, " \10", sep = ""))
#  .Tcl(paste("after", wait))
#  .Tcl(paste("puts $iks_", item, " D", sep = ""))
#  .Tcl(paste("after", wait))
#  .Tcl(paste("puts $iks_", item, " S", sep = ""))
#}

#.iks_action <- function() {
#  iks_file <- paste("iks_nano_", format(Sys.Date(), "%Y-%m-%d"), ".txt",
#    sep = "")
#  odir <- getwd()
#  setwd("c:/temp")
#  if (!file.exists(iks_file))
#    cat(iks_header, "\n", sep = "", file = iks_file)
#  cat(paste(as.vector(entry[-1]), collapse = "\t"), "\n",
#    file = iks_file, sep = "", append = TRUE)
#  setwd(odir)
#
#  # Get the table in, memory and update it
#  if (exists(".iks_table_nano", envir = .GlobalEnv)) {
#    table <- get(".iks_table_nano", envir = .GlobalEnv)
#    # Keep only last 400 points
#    n <- nrow(table)
#    if (n > 399) table <- table[(n - 399):n, ]
#    table <- rbind(table, entry)
#  } else table <- entry   # Create a new table
#  .iks_table_nano <<- table
#
#  # Update the graph
#  if (nrow(table) > 2) {
#    opar <- par(no.readonly = TRUE)
#    on.exit(par(opar))
#    par(mar = c(5.1, 4.1, 4.1, 4.1))
#    matplot(table[, 1], table[, -c(1:3, 10:12)], type = "l", lty = 1,
#      xlab = "Time", ylab = "pH/Ox", xaxt = "n", ylim = c(2, 9))
#    # Add temperatures, but put them at same scale
#    matlines(table[, 1], table[, 10:11] - 20, lty = 2)
#    # Add second y-axis
#    axis(side = 4, at = (12:18)/2, labels = (52:58)/2)
#    mtext("Temperature (degC)", side = 4, line = 2)
#    # Add x-axis
#    axis(side = 1, at = table[, 1], labels = format(table[, 1],"%H:%M:%S"))
#    title("Nanoculture monitoring")
#    legend(table[1, 1], 9, c(iks_col[3:8], "T1", "T2"),
#      col = c(1:6, 1:2), lty = c(rep(1, 6), rep(2, 2)))
#  }
#
#  # Create a new entry
#  # Computer date and time are used instead!
#  date_time <- Sys.time()
#  date <- format(Sys.Date(), "%Y-%m-%d")
#  time <- format(date_time, "%H:%M:%S")
#  entry <- data.frame(date_time, date, time, v1 = NA, v2 = NA, v3 = NA,
#    v4 = NA, v5 = NA, v6 = NA, temp1 = NA, temp2 = NA, term = "",
#    stringsAsFactors = FALSE)
#  names(entry) <- c("date_time", iks_col)
#}

#.Last.lib <- function(libpath) {
#  # Make sure that all connections are closed
#  IKSs <- iks_get_all()
#  if (is.null(IKSs) || length(IKSs) < 1) return()
#  if (length(IKSs) == 1) {
#    cat("Closing connection with the IKS aquastar\n")
#  } else {
#    cat("Closing connections with all IKS aquastars\n")
#  }
#  iks_close("all")
#}


#try(iks_close("all"), silent = TRUE)
#try(winMenuDel("IKS"), silent = TRUE)
##unlink("c:/temp/iksdata.txt")
#if (exists(".iks_table")) rm(".iks_table")
#if (exists(".iks_entry")) rm(".iks_entry")

#winMenuAdd("IKS")
#winMenuAddItem("IKS", "Display ON", "options(display_iks = TRUE)")
#winMenuAddItem("IKS", "Display OFF", "options(display_iks = FALSE)")
#winMenuAddItem("IKS", "Clear data", 'unlink("c:/temp/iks_nano.txt"); try(rm(".iks_table"), silent = TRUE)')
#winMenuAddItem("IKS", "-", "")
#winMenuAddItem("IKS", "Stop", 'iks_close("all"); winMenuDel("IKS")')

#cat("Recording IKS data...\n")
#setWindowTitle(title = "IKS recording")
#iks.open(port = "com1", procfun = myProcess_iks)
