#' Control the parallel port interface from R under Windows
#'
#' Switch individual channels on or off in the parallel port interface card, or
#' read its current state.
#'
#' @param state A logical vector of length 8 indicating the state (on  or off)
#' for each of the eight individual channels in the parallel interface card.
#' @param change If `TRUE` (by default), change the channels, otherwise
#' change nothing but just read the current channels state from the card.
#' @param port The lpt port to which the card is connected. Currently, only
#' standard LPT1, LPT2 or LPT3 ports are recognized (use lpt1, lpt2 or lpt3,
#' respectively, or their equivalent numbers, 1, 2 or 3). The default value is
#' stored in the `para_port` option, otherwise, it is considered to be `"lpt1"`.
#' @return If `change = TRUE`, the function returns `TRUE` in case of success,
#' or `FALSE` otherwise (note that failure to load required dlls, or trying to
#' switch to a nonexistant LPT port raise an error).
#'
#' If `change = FALSE`, returns a vector of logical of length 8 indicating
#' the current state of the interface card. If `state` was provided, its names
#' are reused in the returned logical vector.
#' @author Philippe Grosjean \email{Philippe.Grosjean@@umons.ac.be}
#' @note These functions use the `inpout32.dll` and `lpttcl.dll` version 3.0
#' that are provided in the `/bin` subdirectory of this package. These dlls work
#' only under Windows (95 -> XP) and are probably not compatible with Windows
#' Vista or higher. They do not work either under Linux or Mac OS X. You must
#' also copy these dlls in the `/windows/system32` directory (administrator
#' rights required) before you can use them.
#' @export
#' @seealso [wtw_open()], [iks_open()]
#' @keywords utilities
#' @concept Hardware interface
#' @examples
#' \dontrun{
#' # If the parallel interface card is connected to LPT1, this should work:
#' options(para_port = "lpt1")	# Change this to 2 or 3, if needed
#' para(c(T, T, T, T, T, T, T, T))    # All channels ON
#' para(c(T, F, T, F, T, F, T, F))    # All odd channels ON
#' para(c(F, F, F, F, F, F, F, F))    # All channels OFF
#'
#' # Use a named vector for better tracking of the different channels
#' st <- c(ch1 = TRUE, ch2 = FALSE, ch3 = FALSE, ch4 = TRUE,
#'         ch5 = TRUE, ch6 = FALSE, ch7 = FALSE, ch8 = TRUE)
#' st
#' para(c(F, T, F, T, F, T, F, T))    # All even channels ON
#' para(st, FALSE) # Read current config, keeping channel names from st
#' }
para <- function(state, change = TRUE, port = getOption("para_port")) {
  # Access the parallel port on Windows 95 -> XP and control the parallel interface
  # Change or read the parallel interface channels (ON/OFF) on a given lpt port
  # Default value for port
  if (is.null(port)) port <- "lpt1"
  .paraRequire()  # Make sure dlls are loaded
  .paraPort(port) # Make sure we pilot the correct lpt port

  if (isTRUE(change)) {
    # state is a vector of eight logical values indicating which channel
    # to switch ON or OFF
    state <- as.logical(state)
    if (is.null(state) || length(state) != 8)
      stop("'state' must be a vector of eight logical values")
    # Construct a numeric value from the vector of logicals
    val <- state[1] + 2 * state[2] + 4 * state[3] + 8 * state[4] +
      16 * state[5] + 32 * state[6] + 64 * state[7] + 128 * state[8]
    # Set the parallel interface to this value
    .Tcl("lpt_wrctrl 1")
    .Tcl(paste("lpt_wrdata", val))
    .Tcl("lpt_wrctrl 0")
    # Check the value
    res <- as.numeric(.Tcl("lpt_rddata"))
    return(res == val)
    # Note: to read or write the status register (not used here):
    #lpt_rdstat
    #lpt_wrstat <val>
  } else {
    # Do not change the state, just read it from the parallel interface card
    res <- as.numeric(.Tcl("lpt_rddata"))
    # Reconstitute the vector of logical values from this integer
    res <- as.logical(intToBits(res)[1:8])
    # Possibly reuse names from the provided state vector
    if (!missing(state) && !is.null(names(state)) &&
        length(names(state)) == 8)
      names(res) <- names(state)
    res
  }
}

.paraRequire <- function() {
  # Make sure tcl/tk lpttcl is operational
  if (.Platform$OS.type != "windows")
    stop("This is a Windows-specific function!")
  if (!capabilities("tcltk"))
    stop("This version of R cannot use Tcl/Tk!")
  # You have to install inpout32.dll and lpttcl v. 3.0 in c:\windows\system32!
  res <- tclRequire("lpttcl", warn = FALSE)
  if (inherits(res, "tclObj")) res <- tclvalue(res)
  if (res[1] == FALSE) {
    # Try loading the dlls and the package
    .Tcl("load lpttcl")
    res <- tclvalue(.Tcl("set ver [package require lpttcl]"))
    # Recheck now
    if (res[1] == FALSE)
      stop("Unable to find the lpttcl.dll and inpout32.dll libraries!")
  }
  res	# The package version number
}

.paraPort <- function(port) {
  # If port is referred as "lptX", extract "X"
  port <- sub("^lpt", "", tolower(port))
  .Tcl(paste("lpt_setport", port))
  prt <- as.numeric(.Tcl("lpt_getport"))
  # Note: to set base address (possible future enhancement), you use:
  #lpt_setba <addr>
  addr <- as.numeric(.Tcl("lpt_getba"))
  # Returns -1 if pointing on a nonexisting parallel port in lpt_setport
  if (addr < 0)
    stop("Nonexistant parallel port")
  list(port = prt, address = addr)
}
