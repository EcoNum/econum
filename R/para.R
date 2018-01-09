# Access the parallel port on Windows 95 -> XP and control the parallel interface
".paraRequire" <- function() {
  # Make sure tcl/tk lpttcl is operational
  if (.Platform$OS.type != "windows")
    stop("This is a Windows-specific function!")
  if (!capabilities("tcltk"))
    stop("This version of R cannot use Tcl/Tk!")
  # You have to install inpout32.dll and lpttcl v. 3.0 in c:\windows\system32!
  # TODO: make them accessible form their initial location in the R package...
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

# Set port to use
".paraPort" <- function(port) {
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

# Change or read the parallel interface channels (ON/OFF) on a given lpt port
para <- function(state, change = TRUE, port = getOption("para.port")) {
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
    val <- state[1] + 2*state[2] + 4*state[3] + 8*state[4] +
      16*state[5] + 32*state[6] + 64*state[7] + 128*state[8]
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
