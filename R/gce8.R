# Control GCE USB 8 relays card card (gce8)
# from GCE Electronics
gce8Port <- function (port = getOption("gce8Port"), name) {
	# If name is not missing, get the port for a device with a given name
	if (!missing(name)) {
		name <- make.names(as.character(name)[1])
		name <- gsub("\\.", "_", name)
		dev <- getOption("gce8Devices", default = list())
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

gce8Name <- function (port = gce8Port()) {
	# Is this port open?
	dev <- getOption("gce8Devices", default = list())
	if (!is.null(dev[[port]])) return(dev[[port]])

	# Create a valid name for this device
	name <- make.names(paste("gce8_", port, sep = ""))
	name <- gsub("\\.", "_", name)
	return(name)
}

gce8Relays <- function (names, port = gce8Port(), name = gce8Name(port)) {
	force(port)
	# Get the gce8Relays option list
	rel <- getOption("gce8Relays", default = list())

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
		options(gce8Relays = rel)
		return(paste("R", 1:8, sep = ""))
	}
	# Must be a character vector of eight strings
	if (!is.character(names) || length(names) != 8)
		stop("'names' must be a vector of eight character strings")
	rel[[name]] <- names
	options(gce8Relays = rel)
	return(names)
}

gce8List <- function () {
	# List all currently opened GCE USB 8 relay cards
	return(getOption("gce8Devices", default = list()))
}

gce8Open <- function (port = gce8Port(), name = gce8Name(port)) {
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

	# Create an entry in gce8Devices
	dev <- getOption("gce8Devices", default = list())
	dev[[port]] <- name
	options(gce8Devices = dev)

	# Check that the GCE USB 8 relays card is responding
	return(gce8Ready(port))
}

gce8Close <- function (port = gce8Port(), name = gce8Name(port)) {
	force(port)
	res <- try(.Tcl(paste("close $::gce::", name, sep = "")), silent = TRUE)
	if (inherits(res, "try-error")) {
		return(FALSE)
	} else {
		# Eliminate this item from the list
		dev <- getOption("gce8Devices", default = list())
		dev[[port]] <- NULL
		options(gce8Devices = dev)
		return(TRUE)
	}
}

gce8Ready <- function (port = gce8Port(), name = gce8Name(port)) {
	force(port)
	# Test response of the GCE USB 8 relays card
	return(isTRUE(gce8Cmd(0, name = name)))
}

gce8Cmd <- function (relay, on = TRUE, port = gce8Port(), name = gce8Name(port)) {
	## Use relay = NULL to change memory state!
	
	force(port)
	# Relay is supposed to be 1-8, or the name of a relay
	if (is.character(relay)) {
		# Get the correcponding relay number
		relaynum <- match(relay, gce8Relays(port = port, name = name))
		if (is.na(relaynum)) stop("Relay '", relay, "' not found for this device")
	} else relaynum <- as.integer(relay[1]) # We got integer(0) if relay was NULL

	if (!is.logical(on)) on <- on == "1"
	
	if (!length(relaynum)) {
		## Change memory state
		## Note: 'G0', is not a correct instruction => should return "?" and this is
		## a mean to check that the card is still responding!
		code <- paste("M", relaynum, as.numeric(isTRUE(on)), "G0", sep = "")
	} else {
		## Change relay state
		## Note: 'G0', is not a correct instruction => should return "?" and this is
		## a mean to check that the card is still responding!
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
		res <- gce8Open(port = port, name = name)
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

gce8Get <- function (port = gce8Port(), name = gce8Name(port)) {
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
		res <- gce8Open(port = port, name = name)
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
	lbl <- gce8Relays(port = port, name = name)
	lbl[is.na(lbl)] <- "."
	names(res) <- lbl
	return(res)
}

gce8Set <- function (state = FALSE, port = gce8Port(), name = gce8Name(port)) {
	# Given a vector of eight booleans, make sure the card is configurated
	# that way (note, recycling rule applies if size is different)
	# Missing values are allowed and corresponding relays will not be changed

	# If state is a character string, convert into a vector of booleans
	if (is.character(state)) state <- (charToRaw(state[1]) == 49)

	# If this is a named vector, try to match names to relays names
	relays <- names(state)
	if (!is.null(relays)) {
		rel <- gce8Relays(port = port, name = name)
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
	curstate <- gce8Get(port = port, name = name)
	if (inherits(curstate, "try-error")) return(curstate)

	# For each relay, compare curstate and state
	for (i in 1:8) {
		if (!is.na(state[i]) && state[i] != curstate[i])
			gce8Cmd(i, state[i])
	}

	# Return success
	return(TRUE)
}
