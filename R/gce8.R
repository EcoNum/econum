#' Control a GCE Electronics USB 8 relays card
#'
#' Switch individual channels on or off in the USB interface card, or read its
#' current state.
#'
#' @param port The comm port to use (this is a virtual comm port using the FTDI
#' virtual port driver available for Windows, Mac OS and Linux). If a number (x)
#' is provided, the port is assumed to be 'comx', which is only a valid
#' assuption under Windows. Use fully qualified name on Linux or Mac).
#' @param name The name of the device. With `gce8_port()` it is used to retrieve
#' the corresponding port; with `gce8_open()`, it forces the name of the card to
#' use in R.
#' @param names A vector of eight character strings to use to define the name of
#' the eight relays. Missing data represent unused relays.
#' @param relay The number, or name of the relay to switch.
#' @param on Do we switch the relay on or off? Can be a boolean, 0/1 or "0"/"1".
#' @param state The state of the relays. Can be a vector of eight items
#' (otherwise, recycling rule applies), either boolean, integers 0/1, or
#' anything that automatically coerces into boolean. Missing values are accepted
#' and they will match relay that are not modified. The vector can also be
#' named, and in this case, names will be match to the names used for the 8
#' relays: R1-R8, or anything else specified using `gce8_relays()`.`
#' @return Most functions return `TRUE` in case of success and `FALSE` or a
#' `try-error` object in case of failure., except:
#'
#' `gce8_port()` and `gce8_name()` return the port and name of a card,
#'
#' `gce8_list()` returns the list of currently connected cards,
#'
#' `gce8_relays()` returns a vector of eight character strings with the name of
#' the eight relays (dot . substituted for missing values)
#'
#' `gce8_get()` returns a named vector of eight booleans indicating the state
#' of the eight relays. Names are the current names of the relays, or the
#' default names R1-R8.
#' @author Philippe Grosjean \email{Philippe.Grosjean@@umons.ac.be}
#' @note Functions `gce8_cmd()`, `gce8_get()` and `gce8_set()` functions use a
#' mechanism to reconnect in case of lost connection. For instance, if the USB
#' cord is unplugged and plugged again, connection is lost, but these functions
#' can recover it. It only takes about 1 sec more to replug the card. If you
#' forgot to connect with `gce8_open()`, these functions will do this
#' automatically for you. However, you should always use `gce8_close()` once you
#' have done with the card, in order to free the comm port handle. This will
#' works too if the main power of the card in shut off and on again, but take
#' care that all relays do return in off state and your program is not
#' necessarily aware of that situation, unless you use `gce8_get()`. For
#' critical applications, do prefer using `gce8_set()` and always specify all
#' eight relays state instead of using `gce8_cmd()`.
#'
#' Those cards come from [GCE Electronics](http://gce-electronics.com/fr/11-usb).
#' @export
#' @name gce8
#' @seealso [wtw_open()], [iks_open()]
#' @keywords utilities
#' @concept Hardware control
#' @examples
#' \dontrun{
#' # If the USB interface card is connected to com5, this should work:
#' options(gce8_port = 5)
#' # On the Mac, use: options(gce8_port = "/dev/cu.usbserial-A4003CDu")
#' gce8_open() # Note that this is facultative, since I connect on first use!
#' gce8_cmd(5, 1)
#' gce8_get()
#' gce8_set(FALSE)
#' gce8_close() # On the other hand, this is required to free the port!
#' }
gce8_port <- function(port = getOption("gce8_port"), name) {
  # Control GCE USB 8 relays card card (gce8) from GCE Electronics
  # If name is not missing, get the port for a device with a given name
  if (!missing(name)) {
    name <- make.names(as.character(name)[1])
    name <- gsub("\\.", "_", name)
    dev <- getOption("gce8_devices", default = list())
    if (any(dev == name)) {
      return(names(dev)[dev == name][1])
    } else return(NULL) # No port associated with this name!
  } else {
    # Make sure port is correct
    # Default value is "com1" (under Windows only... TODO: reasonable default for other OSes)
    if (is.null(port)) return("com1")
    # If only a number is provided, prepend "com"
    if (is.numeric(port)) { # Again, only viable for Windows => use other defaults too
      return(paste("com", as.integer(port[1]), sep = ""))
    } else {
      # Make sure it is a single string without attributes
      return(as.character(port[1]))
    }
  }
}

#' @export
#' @rdname gce8
gce8_name <- function(port = gce8_port()) {
  # Is this port open?
  dev <- getOption("gce8_devices", default = list())
  if (!is.null(dev[[port]])) return(dev[[port]])

  # Create a valid name for this device
  name <- make.names(paste("gce8_", port, sep = ""))
  name <- gsub("\\.", "_", name)
  name
}

#' @export
#' @rdname gce8
gce8_relays <- function(names, port = gce8_port(), name = gce8_name(port)) {
  force(port)
  # Get the gce8_relays option list
  rel <- getOption("gce8_relays", default = list())

  # Set or get custom names for the 8 relays
  if (missing(names)) {
    res <- rel[[name]]
    if (is.null(res)) res <- paste("R", 1:8, sep = "")
    return(res)
  }

  # We want to change the name of the relays
  # If is NULL, reset default names
  if (is.null(names)) {
    rel[[name]] <- NULL
    options(gce8_elays = rel)
    return(paste("R", 1:8, sep = ""))
  }
  # Must be a character vector of eight strings
  if (!is.character(names) || length(names) != 8)
    stop("'names' must be a vector of eight character strings")
  rel[[name]] <- names
  options(gce8_relays = rel)
  names
}

#' @export
#' @rdname gce8
gce8_list <- function() {
  # List all currently opened GCE USB 8 relay cards
  getOption("gce8_devices", default = list())
}

#' @export
#' @rdname gce8
gce8_open <- function(port = gce8_port(), name = gce8_name(port)) {
  force(port)
  # If name is provided, make sure it is valid
  if (!missing(name)) {
    name <- make.names(as.character(name)[1])
    name <- gsub("\\.", "_", name)
  }
  # Make sure ::gce namespace exists in Tcl
  if (!identical(tclvalue(.Tcl("namespace exists ::gce")), "1"))
    .Tcl("namespace eval ::gce { variable desc \"GCE Electronic cards\"}")

  # Make sure the com port is closed
  try(.Tcl(paste("close $::gce::", name, sep = "")), silent = TRUE)

  # Open and configure the port (no error handling yet!)
  res <- try(.Tcl(paste("set ::gce::", name, " [open \"", port, "\" r+]",
    sep = "")), silent = TRUE)
	if (inherits(res, "try-error")) return(res)
  res <- try(.Tcl(paste("fconfigure $::gce::", name,
    " -mode \"9600,n,8,1\" -buffering none -blocking 0 -handshake none",
    sep = "")), silent = TRUE)
  if (inherits(res, "try-error")) return(res)

  # Create an entry in gce8_devices
  dev <- getOption("gce8_devices", default = list())
  dev[[port]] <- name
  options(gce8_devices = dev)

  # Check that the GCE USB 8 relays card is responding
  gce8_ready(port)
}

#' @export
#' @rdname gce8
gce8_close <- function(port = gce8_port(), name = gce8_name(port)) {
  force(port)
  res <- try(.Tcl(paste("close $::gce::", name, sep = "")), silent = TRUE)
  if (inherits(res, "try-error")) {
    return(FALSE)
  } else {
    # Eliminate this item from the list
    dev <- getOption("gce8_devices", default = list())
    dev[[port]] <- NULL
    options(gce8_devices = dev)
    return(TRUE)
  }
}

#' @export
#' @rdname gce8
gce8_ready <- function(port = gce8_port(), name = gce8_name(port)) {
  force(port)
  # Test response of the GCE USB 8 relays card
  isTRUE(gce8_cmd(0, name = name))
}

#' @export
#' @rdname gce8
gce8_cmd <- function(relay, on = TRUE, port = gce8_port(), name = gce8_name(port)) {
  # Use relay = NULL to change memory state!

  force(port)
  # Relay is supposed to be 1-8, or the name of a relay
  if (is.character(relay)) {
    # Get the correcponding relay number
    relaynum <- match(relay, gce8_relays(port = port, name = name))
    if (is.na(relaynum)) stop("Relay '", relay, "' not found for this device")
  } else relaynum <- as.integer(relay[1]) # We got integer(0) if relay was NULL

  if (!is.logical(on)) on <- on == "1"

  if (!length(relaynum)) {
    # Change memory state
    # Note: 'G0', is not a correct instruction => should return "?" and this is
    # a mean to check that the card is still responding!
    code <- paste("M", relaynum, as.numeric(isTRUE(on)), "G0", sep = "")
  } else {
    # Change relay state
    # Note: 'G0', is not a correct instruction => should return "?" and this is
    # a mean to check that the card is still responding!
    code <- paste("RLY", relaynum, as.numeric(isTRUE(on)), "G0", sep = "")
  }
  # Send the command to the GCE USB 8 relays card
  res <- try(.Tcl(paste("puts -nonewline $::gce::", name, " {", code, "}",
    sep = "")), silent = TRUE)
  # Try reading from the card
  if (!inherits(res, "try-error")) {
    res <- try(tclvalue(.Tcl(paste("gets $::gce::", name, sep = ""))),
      silent = TRUE)
  }
  if (inherits(res, "try-error")) {
    # Try reconnect to the card
    res <- gce8_open(port = port, name = name)
    if (!isTRUE(res)) return(res)
    # Try to resend the command
    res <- try(.Tcl(paste("puts -nonewline $::gce::", name, " {", code, "}",
      sep = "")), silent = TRUE)
    if (inherits(res, "try-error")) return(res)
    res <- ""
  }

  # Wait for the card response (wait 0.5 sec max)
  tmax <- proc.time()[3] + 0.5
  while (res != "?" && !inherits(res, "try-error") && proc.time()[3] < tmax)
    res <- try(tclvalue(.Tcl(paste("gets $::gce::", name, sep = ""))),
      silent = TRUE)
  return(res == "?")
}

#' @export
#' @rdname gce8
gce8_get <- function(port = gce8_port(), name = gce8_name(port)) {
  force(port)
  # Ask the current config of the GCE USB 8 relays card
  res <- try(.Tcl(paste("puts -nonewline $::gce::", name, " {M0?RLY}",
    sep = "")), silent = TRUE)
  # Try reading from the card
  if (!inherits(res, "try-error")) {
    res <- try(tclvalue(.Tcl(paste("gets $::gce::", name, sep = ""))),
      silent = TRUE)
  }

  if (inherits(res, "try-error")) {
    # Try reconnect to the card
    res <- gce8_open(port = port, name = name)
    if (!isTRUE(res)) return(res)
    # Try to resend the command
    res <- try(.Tcl(paste("puts -nonewline $::gce::", name, " {M0?RLY}",
      sep = "")), silent = TRUE)
    if (inherits(res, "try-error")) return(res)
    res <- ""
  }

  # Wait for the card response (wait one sec max)
  tmax <- proc.time()[3] + 1
  while ((res == "" || res == "?") && !inherits(res, "try-error") &&
    proc.time()[3] < tmax) {
    res <- try(tclvalue(.Tcl(paste("gets $::gce::", name, sep = ""))),
      silent = TRUE)
  }

  # Decrypt the code received
  if (!grepl("^[01]{8}$", res)) return(structure(paste("wrong code:", res),
    class = "try-error"))
  res <- (charToRaw(res) == 49)
  lbl <- gce8_relays(port = port, name = name)
  lbl[is.na(lbl)] <- "."
  names(res) <- lbl
  res
}

#' @export
#' @rdname gce8
gce8_set <- function(state = FALSE, port = gce8_port(), name = gce8_name(port)) {
  # Given a vector of eight booleans, make sure the card is configurated
  # that way (note, recycling rule applies if size is different)
  # Missing values are allowed and corresponding relays will not be changed

  # If state is a character string, convert into a vector of booleans
  if (is.character(state)) state <- (charToRaw(state[1]) == 49)

  # If this is a named vector, try to match names to relays names
  relays <- names(state)
  if (!is.null(relays)) {
    rel <- gce8_relays(port = port, name = name)
    # Construct a default vector of 8 missing booleans
    s <- as.logical(rep(NA, 8))
    names(s) <- rel
    s[relays] <- state
    # Keep only first eight items and convert to logical
    state <- as.logical(s[1:8])
  } else {
    # Convert to boolean and set to 8 elements
    state <- rep(as.logical(state), length.out = 8)
  }

  # Get the current state of the card
  curstate <- gce8_get(port = port, name = name)
  if (inherits(curstate, "try-error")) return(curstate)

  # For each relay, compare curstate and state
  for (i in 1:8) {
    if (!is.na(state[i]) && state[i] != curstate[i])
      gce8_cmd(i, state[i])
  }

  # Return success
  TRUE
}
