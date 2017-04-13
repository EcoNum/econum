# Create and manage connections to IKS aquastar using tcltk
print.IKSData <- function (x, ...) {
	# Print IKS state in a nice way
	cat(x$Name, ": IKS aquastar on ", x$Port, "\n", sep = "")
	if (!is.null(x$Version))
		cat("(reported as: ", x$Version, ")\n", sep = "")

	Itime <- x$Time["IKS"]
	Ptime <- x$Time["Probes"]
	Stime <- x$Time["Sockets"]

	if (!is.null(Ptime) && !is.na(Ptime)) {
		if (!is.null(Itime) && !is.na(Itime)) {
			# Check time lag between IKS and computer
			Tdiff <- as.double(Ptime - Itime, units = "secs")
			if (Tdiff < 61 & Tdiff > -61) {
				cat("(computer and IKS time are synchrone: less than 1 min difference)\n")
			} else {
				cat("Computer and IKS times are different:\n")
				print(Tdiff)
			}
		}
		# Now, print the probes data
		cat("\nProbes, as on ", format(Ptime), ":\n", sep = "")
		# Align on 8 characters
		#Name :     p01     p02     p03     p04     p05     p06     p07     p08
		#Type :      pH      pH      pH      Te      Te      Ox      Ox      Ox
		#Value:    3.81    3.84    3.76    18.5    19.7   100.3    99.1    98.6
		#Unit :                              °C      °C    %sat    %sat    %sat
		#Trig.:               *               +       -
		cat("Name :", format(abbreviate(x$NamesProbes, 8), width = 8,
			justify = "right"), "\n")
		Types <- as.character(x$Probes$Type)
		Types[is.na(Types)] <- ""
		cat("Type :", format(abbreviate(Types, 8), width = 8,
			justify = "right"), "\n")
		cat("Value:", format(abbreviate(x$Probes$Value, 8), width = 8,
			justify = "right"), "\n")
		Units <- as.character(x$Probes$Unit)
		Units[is.na(Units)] <- ""
		cat("Units:", format(abbreviate(Units, 8), width = 8,
			justify = "right"), "\n")
		Triggers <- as.character(x$Probes$Trigger)
		Triggers[is.na(Triggers)] <- ""
		cat("Trig.:", format(abbreviate(Triggers, 8), width = 8,
			justify = "right"), "\n")
	}
	if (!is.null(Stime) && !is.na(Stime)) {
		# Also print the sockets state
		cat("\nSockets, as on ", format(Stime), ":\n", sep = "")
		# Align on 8 characters too
		#Name :
		#Ctrl :
		#State: indicate between brackets if unplugged
		State <- paste(x$Sockets$State0, "%", sep = "")
		State[State == "0%"] <- "off"
		State[State == "100%"] <- "ON"
		State[!x$Sockets$Plugged] <- NA
		#Alarm:
		Alarm <- rep("", 17)
		Alarm[x$Sockets$Alarm] <- "*"
		# Take care that first saocket is at line #2 in the table!
		cat("Name :", format(abbreviate(x$NamesSockets[1:8], 8), width = 8,
			justify = "right"), "\n")
		cat("Ctrl :", format(abbreviate(x$Sockets$Control[2:9], 8), width = 8,
			justify = "right"), "\n")
		cat("State:", format(abbreviate(State[2:9], 8), width = 8,
			justify = "right"), "\n")
		cat("Alarm:", format(abbreviate(Alarm[2:9], 8), width = 8,
			justify = "right"), "\n")
		cat("\n")
		cat("Name :", format(abbreviate(x$NamesSockets[9:16], 8), width = 8,
			justify = "right"), "\n")
		cat("Ctrl :", format(abbreviate(x$Sockets$Control[10:17], 8), width = 8,
			justify = "right"), "\n")
		cat("State:", format(abbreviate(State[10:17], 8), width = 8,
			justify = "right"), "\n")
		cat("Alarm:", format(abbreviate(Alarm[10:17], 8), width = 8,
			justify = "right"), "\n")
	}
	return(invisible(x))
}

.iks.port <- function (port) {
	## Make sure port is correct
	if (!is.null(port) && port == "all") return("all")
	## Default value is "com1"
	if (is.null(port)) return("com1")
	## If only a number is provided, prepend "com"
	if (grepl("^[0-9]+$", port[1])) {
		numport <- as.integer(port[1])
	} else numport <- NA
	if (!is.na(numport)) {
		## If port is higher than 9,
		## name must be \\\\\\\\.\\\\comXX under Windows
		if (numport > 9) {
			return(paste("\\\\\\\\.\\\\com", numport, sep =""))
		} else {
			return(paste("com", numport, sep = ""))
		}
	} else {
		return(as.character(port[1]))
	}
}

.iks.make.names <- function (port) {
	## Get a clean version of the port (something that can decently be used as proc name)
	return(gsub("\\.", "_", make.names(gsub("^[\\.]+", "", port))))
}

iks.process <- function (msg, iks.name = "IKS") {
    # Do we receive the date?
	isDate <- FALSE
	# Do we receive socket data?
	isSocket <- FALSE
	# Do we receive config data?
	isConfig <- FALSE
	# The default R function that processes the text send by an IKS aquastar
	#cat(iks.name, "> ", msg, "\n", sep = "")
	Data <- iks.getData(iks.name = iks.name)
	# and update corresponding data in Data
	if (regexpr("^E[1-8] ", msg) > 0) {
		# It is some probe data
		Probes <- Data$Probes
		if (is.null(Probes)) {
			# Create a data frame with default data
			Type <- factor(rep(NA, 8),
				levels = c("Cd", "Lv", "O2", "Pa", "pH", "Rx", "Te"))
			Value <- as.numeric(rep(NA, 8))
			Unit <- as.character(rep(NA, 8))
			Trigger <- factor(rep(NA, 8), levels = c("-", " ", "+", "*"))
			Probes <- data.frame(Type = Type, Value = Value,
				Unit = Unit, Trigger = Trigger, stringsAsFactors = FALSE)
			rownames(Probes) <- paste("p", 1:8, sep = "")
		}
		# Not calibrate
		if (regexpr("^E[1-8] Not calibrate", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			Probes$Value[chan] <- NaN # This indicates a not calibrated probe
			# In comparison to NA that indicates no probe on this port
		}
		# pH
		if (regexpr("^E[1-8] [(]pH", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(]pH.[)]([^ ]*) .*$", "\\1", msg)
			dat <- try(as.numeric(dat), silent = TRUE)
			Probes$Type[chan] <- "pH"
			Probes$Value[chan] <- dat
			Probes$Unit[chan] <- ""
			trigger <- sub("^E[1-8] [(]pH(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
        # Oxygen
		if (regexpr("^E[1-8] [(][Oo]x", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(][Oo]x.[)]([0-9. ]*)[^0-9].*$", "\\1", msg)
			dat <- try(as.numeric(dat), silent = TRUE)
			Probes$Type[chan] <- "O2"
			Probes$Value[chan] <- dat
			# Determines unit used
			if (regexpr("% *$", msg) > 0)
				Probes$Unit[chan] <- "%sat" else Probes$Unit[chan] <- "mg/L"
			trigger <- sub("^E[1-8] [(][Oo]x(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
		# Temperature
        if (regexpr("^E[1-8] [(][Tt]e", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(][Tt]e.[)] ([^ ]*) .*$", "\\1", msg)
			dat <- try(as.numeric(dat), silent = TRUE)
			Probes$Type[chan] <- "Te"
			Probes$Value[chan] <- dat
			if (regexpr("C *$", msg) > 0)
				Probes$Unit[chan] <- "degC" else Probes$Unit[chan] <- "degF"
			trigger <- sub("^E[1-8] [(][Tt]e(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
		# Pressure (both English and French version)
        if (regexpr("^E[1-8] [(](AP|Ld|ap|ld)", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(]...[)] *([^ ]*) .*$", "\\1", msg)
			dat <- try(as.numeric(dat), silent = TRUE)
			Probes$Type[chan] <- "Pa"
			Probes$Value[chan] <- dat
			Probes$Unit[chan] <- "mB"
			trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
		# Water level (both English and French version)
        if (regexpr("^E[1-8] [(](Lv|Pe|lv|pe)", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(]...[)] *([^ ]*).*$", "\\1", msg)
			if (tolower(dat) == "air") dat <- 0 else dat <- 1
			Probes$Type[chan] <- "Lv"
			Probes$Value[chan] <- dat
			Probes$Unit[chan] <- ""
			trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
		# Conductivity (English)
        if (regexpr("^E[1-8] [(](Co|Le|co|le)", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(]...[)] *([^ ]*).*$", "\\1", msg)
			dat <- try(as.numeric(dat), silent = TRUE)
			Probes$Type[chan] <- "Cd"
			Probes$Value[chan] <- dat
			if (regexpr("uS *$", msg) > 0)
				Probes$Unit[chan] <- "uS" else if (regexpr("mS *$", msg) > 0)
				Probes$Unit[chan] <- "mS" else Probes$Unit[chan] <- ""
			trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
		# Redox (English and untested!)
        if (regexpr("^E[1-8] [(](Re|Rx|rH|re|rx|rh)", msg) == 1) {
			chan <- as.numeric(sub("^E([1-8]).*$", "\\1", msg))
			dat <- sub("^E[1-8] [(]...[)] *([^ ]*).*$", "\\1", msg)
			dat <- try(as.numeric(dat), silent = TRUE)
			Probes$Type[chan] <- "Rx"
			Probes$Value[chan] <- dat
			if (regexpr("mV *$", msg) > 0)
				Probes$Unit[chan] <- "mV" else Probes$Unit[chan] <- "rH"
			trigger <- sub("^E[1-8] [(]..(.)[)].*$", "\\1", msg)
			Probes$Trigger[chan] <- trigger
		}
		# Record these results back into Data
		Data$Probes <- Probes
	} else if (regexpr("^E[0-9]", msg) > 0) {
		# This must be the time and date as used by the IKS aquastar
		isDate <- TRUE
		# Computer time is:
		Cdate <- Sys.time()
		# IKS date is decrypted
		time <- sub("^E([0-9]+:[0-9]+).*$", "\\1", msg)
		date <- sub("^E.*, ([0-9]+)\\.([0-9]+).*$", "\\2-\\1", msg)
		# Create a complete date with this (assume "00" for sec, and same year)
		IKSdate <- paste(format(Sys.time(), "%Y"), "-", date, " ", time, ":00",
			sep = "")
		IKSdate <- as.POSIXct(IKSdate)
		# Record the Time info
		Time <- Data$Time
		if (is.null(Time)) {
			Time <- as.POSIXct(rep(NA, 3))
			names(Time) <- c("IKS", "Probes", "Sockets")
		}
		Time[1] <- IKSdate
		Time[2] <- Cdate
		Data$Time <- Time
	} else if (regexpr("^aquastar", msg) > 0) {
		# This is the identifier, as obtained with iks.info()
		Data$Version <- msg
	} else if (regexpr("End RAM$", msg) > 0) {
		# This is probably the dumping of RAM data (IKS config)
		isConfig <- TRUE
		Data$Config <- sub("End RAM$", "", msg)
	} else if (regexpr("Socket End$", msg) > 0) {
		# This is info about the state of the sockets
		isSocket <- TRUE
		# Record the Time we got sockets state
		Time <- Data$Time
		if (is.null(Time)) {
			Time <- as.POSIXct(rep(NA, 3))
			names(Time) <- c("IKS", "Probes", "Sockets")
		}
		Time[3] <- Sys.time()
		Data$Time <- Time
		# Decrypt and record socket state
		sockstr <- sub("Socket End$", "", msg)
		# Split the socket into numbers (two digits each time) in hexmode
		sockhex <- gsub("(..)", "-\\1", sockstr)
		sockhex <- substr(sockhex, 2, 357)
		sockhex <- strsplit(sockhex, "-", fixed = TRUE)[[1]]
		sockint <- apply(matrix(sockhex), 1, as.hexmode)
		sockint <- matrix(sockint, ncol = 7, byrow = TRUE)
		sockdata <- as.data.frame(sockint, stringsAsFactors = FALSE)
		names(sockdata) <- c("Plugged", "Control", "State0", "State1", "MaxVal",
			"MinVal", "Alarm")
		rownames(sockdata) <- sprintf("s%02d", 0:16)
		sockdata$Plugged <- as.logical(sockdata$Plugged)
		# Rem: here I am not sure that redox control is in this position!
		sockdata$Control <- c("none", "pH", "temper", "redox", "conduct", "05",
			"oxygen", "level", "08", "09", "0A", "0B", "interval", "timer",
			"current", "curr.ht", "lunar", "day/nite",
			"alarm")[sockdata$Control + 1]
		sockdata$Alarm <- as.logical(sockdata$Alarm)
		Data$Sockets <- sockdata
	}
	# Save the modified data
	iks.setData(Data, iks.name = iks.name)
	# If date or socket data received, possibly trigger another function
	if (isDate && !is.null(getTemp("iks.dateProc")))
		getTemp("iks.dateProc")(data = Data, iks.name = iks.name)
	if (isSocket && !is.null(getTemp("iks.socketProc")))
		getTemp("iks.socketProc")(data = Data, iks.name = iks.name)
	if (isConfig && !is.null(getTemp("iks.configProc")))
		getTemp("iks.configProc")(data = Data, iks.name = iks.name)
	return(invisible(msg))
}

iks.getAll <- function () {
	# Get the list of currently connected IKS aquastars
	return(getTemp("IKSs")$Devices)
}

iks.getName <- function (port = getOption("iks.port")) {
	# Return the name of a given IKS
	port <- .iks.port(port)
	IKSs <- getTemp("IKSs")$Devices
	sport <- .iks.make.names(port)
	if (!(sport %in% IKSs)) return(NULL)
	IKSNames <- names(IKSs)
	return(IKSNames[IKSs == sport])
}

iks.getData <- function (port = getOption("iks.port"), iks.name = NULL) {
	if (missing(iks.name)) iks.name <- iks.getName(port)
	if (is.null(iks.name)) return(NULL)
	IKSs <- getTemp("IKSs")
	return(IKSs[[iks.name]])
}

iks.setData <- function (data, port = getOption("iks.port"), iks.name = NULL) {
	if (!inherits(data, "IKSData")) stop("'data' must be an 'IKSData' object")
	if (missing(iks.name)) iks.name <- iks.getName(port)
	if (is.null(iks.name)) return(FALSE)
	IKSs <- getTemp("IKSs")
	IKSs[[iks.name]] <- data
	assignTemp("IKSs", IKSs)
	return(TRUE)
}

iks.open <- function (port = getOption("iks.port"), iks.name = "IKS",
probes.names = paste("p0", 1:8, sep = ""),
sockets.names = sprintf("s%02d", 1:16), procfun = iks.process) {
	# This is the main function that starts the connection to one IKS aquastar
 	# IKSproc is the R workhorse function that do the computation

	iks.name <- as.character(iks.name[1])

	probes.names <- as.character(probes.names)
	if (length(probes.names) != 8)
		stop("'probes.names' must be a vector of eight names")
	names(probes.names) <- paste("p0", 1:8, sep = "")

	sockets.names <- as.character(sockets.names)
	if (length(sockets.names) != 16)
		stop("'sockets.names' must be a vector of 16 names")
	names(sockets.names) <- sprintf("s%02d", 1:16)

	is.function(procfun) || stop("'procfun' must be a function!")
    # Note: the data send by the IKS must be read from the Tcl $::IKSMsg variable

	# Make sure the com port is closed
	port <- .iks.port(port)
	item <- .iks.make.names(port)
	iks.close(port)

	# We need Tcl to be able to call an R functions to process IKS data
	"tclProcExists" <- function (proc) {
		proc <- as.character(proc[1])
		return(length(as.character(tcl("info", "commands", proc))) == 1)
	}

    if (!tclProcExists("IKSProc")) {
	    # Create the callback when a client sends data
		"IKSProc" <- function () {
			# Note: I don't know how to pass arguments here.
			# So, I use Tcl global variables instead:
			# - the server port from $::IKSPort,
			# - and the message from $::IKSMsg
            "tclGetValue_" <- function (name) {
    			# Get the value stored in a plain Tcl variable
    			if (!is.character(name)) stop("'name' must be a character!")

    			# Create a temporary dual variable with tclVar()
    			Temp <- tclVar(init = "")

    			# Copy the content of the var of interest to it
    			.Tcl(paste("catch {set ", as.character(Temp), " $", name, "}",
					sep = ""))

    			# Get the content of the temporary variable
    			Res <- tclvalue(Temp) # (is destroyed when the function exits)
    			return(Res)
			}

			port <- tclGetValue_("::IKSPort")
			if (port == "") return(FALSE) # The connection with this IKS is closed
			msg <- tclGetValue_("::IKSMsg")
			if (msg == "") return(FALSE) # No message!

			# Make sure this message is not processed twice
			.Tcl("set ::IKSMsg {}")

			# Do we have to debug IKS transactions
			if (isTRUE(getOption("debug.IKS")))
				cat("IKS on ", port, ": ", msg, "\n", sep = "")

			# The function that processes the client request is IKSProc_<item>
			proc <- getTemp(paste("IKSProc", item, sep = "_"),
				mode = "function")
			if (is.null(proc)) return(FALSE) # The server is probably closed
			# Call this function
			res <- proc(msg = msg, iks.name = iks.getName(port))
			return(TRUE) # The command is processed
		}
		# This is a copy of tclFun from tcltk2, to avoid a Depends: tcltk2
		"tclFun_" <-
			function(f, name = deparse(substitute(f))) {
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
    		return(res)
    		# Rem: if you delete the R 'f' function,
			# the Tcl 'f' function still works (?!)
		}
		tclFun_(IKSProc)
	}

	# Copy procfun into TempEnv as IKSProc_<item>
	assignTemp(paste("IKSProc", item, sep ="_"), procfun)

 	# Create the Tcl function that retrieves data from the IKS
    cmd <- paste(c(paste("proc IKSHandler_", item, " {iks} {", sep = ""),
        "if {[eof $iks] == 1 || [catch {gets $iks line}]} {",
        "    # end of file or abnormal connection drop",
        "    fileevent $iks readable {}",
		"    close $iks",
		paste("    #puts \"Close $IKS_", item, "($iks)\"", sep = ""),
		## I've got once: can't unset ...: no such variable after a USB4
		## deconnection => protect myself against this!
		paste("    catch { unset IKS_", item, "($iks) }", sep = ""),
		"} else {",
  		"    global IKSPort",
  		"    global IKSMsg",
		paste("    set ::IKSPort", port),
		"    set ::IKSMsg $line",
		"    IKSProc    ;# process the command in R",
		"}\n}"), collapse = "\n")
    # if {[gets $port line] < 0} {return} # To handle incomplete lines!
    .Tcl(cmd)

    # Connect to an IKS aquastar
	# TODO: catch possible error here, due to wrong com port and test IKS presence
	.Tcl(paste("set IKS_", item, " [open \"", port, "\" r+]", sep = ""))
	.Tcl(paste("fconfigure $IKS_", item,
		" -mode \"9600,n,8,1\" -buffering line -blocking 0", sep = ""))
	.Tcl(paste("fileevent $IKS_", item,
		" readable [list IKSHandler_", item, " $IKS_", item, "]", sep = ""))

	# Add this port in the TempEnv variable 'IKSs$Devices'
	IKSs <- getTemp("IKSs")
	Dev <- IKSs$Devices
	namesDev <- names(Dev)
	if (!(item %in% Dev)) {
		Dev <- c(Dev, item)
		names(Dev) <- c(namesDev, iks.name)
		Dev <- sort(Dev)
		IKSs$Devices <- Dev
		# Record basic info about this IKS device
		Info <- list(Name = iks.name, Port = port, NamesProbes = probes.names,
			NamesSockets = sockets.names)
		class(Info) <- c("IKSData", "list")
		IKSs[[iks.name]] <- Info
		assignTemp("IKSs", IKSs)
	} else stop("Com port already registered as an IKS device... Should never occur!")
	# Get info about this IKS
	iks.info(port = port)
	return(TRUE) # Humm! Only if it succeed... TODO: check this!
}

iks.close <- function (port = getOption("iks.port")) {
	# Stop one or more connections to IKS aquastars
	Dev <- iks.getAll()
	port <- .iks.port(port)
	if (port == "all") port <- Dev
	anyclosed <- FALSE
	for (i in 1:length(port)) {
		Port <- port[i]
		Item <- .iks.make.names(Port)
		if (Item %in% Dev) { # This port is open
			anyclosed <- TRUE
			Name <- names(Dev)[Dev == Item]
			Dev <- Dev[Dev != Item]
			if (length(Dev) == 0) {
		    	rmTemp("IKSs")
			} else {
				# Clean up IKSs data
				IKSs <- getTemp("IKSs")
				IKSs$Devices <- Dev
				IKSs[[Name]] <- NULL
				assignTemp("IKSs", IKSs)
  			}

			# Eliminate the processing function from TempEnv
			rmTemp(paste("IKSProc", Item, sep = "_"))

			# Close the connection to the given IKS
			try(.Tcl(paste("close $IKS_", Item, sep = "")), silent = TRUE)
		}
	}
	return(invisible(anyclosed))
}

iks.cmd <- function (code, port = getOption("iks.port"), newline = TRUE) {
	# Send the given code to one or more IKS aquastars through their comm port
	port <- .iks.port(port)
	if (port == "all") port <- iks.getAll()
	if (!is.null(port) && length(port) > 0) {
		for (i in 1:length(port)) {
		    if (isTRUE(newline)) {
				.Tcl(paste("puts -nonewline $IKS_", .iks.make.names(port[i]),
					" {", code, "}", sep = ""))
		    } else {
    			.Tcl(paste("puts $IKS_", .iks.make.names(port[i]),
					" {", code, "}", sep = ""))
			}
		}
	}
}

iks.info <- function(port = getOption("iks.port"), wait = 500) {
	port <- .iks.port(port)
	item <- .iks.make.names(port)
	.Tcl(paste("puts $IKS_", item, " \10", sep = ""))
	.Tcl(paste("after", wait))
    .Tcl(paste("puts $IKS_", item, " D", sep = ""))
    .Tcl(paste("after", wait))
    .Tcl(paste("puts $IKS_", item, " V", sep = ""))
    .Tcl(paste("after", wait))
    .Tcl(paste("puts $IKS_", item, " \10", sep = ""))
}

iks.getConfig <- function(port = getOption("iks.port"), wait = 500) {
	port <- .iks.port(port)
	item <- .iks.make.names(port)
	.Tcl(paste("puts $IKS_", item, " \10", sep = ""))
	.Tcl(paste("after", wait))
    .Tcl(paste("puts $IKS_", item, " D", sep = ""))
    .Tcl(paste("after", wait))
    .Tcl(paste("puts $IKS_", item, " R", sep = ""))
}

#iks.ready <- function (port = getOption("iks.port")) {
#	port <- .iks.port(port)
#	# TODO: how to test this?
#	return(TRUE)
#	#return(!is.null(iks.version(port = port)))
#}

#iks.read <- function(port = getOption("iks.port"), wait = 500) {
#	port <- .iks.port(port)
# 	item <- .iks.make.names(port)
#	.Tcl(paste("puts $IKS_", item, " \10", sep = ""))
#	.Tcl(paste("after", wait))
#    .Tcl(paste("puts $IKS_", item, " D", sep = ""))
#    .Tcl(paste("after", wait))
#    .Tcl(paste("puts $IKS_", item, " A", sep = ""))
#}

#iks.header <- function(port = getOption("iks.port"), wait = 500) {
#	port <- .iks.port(port)
#	item <- .iks.make.names(port)
#	.Tcl(paste("puts $IKS_", item, " \10", sep = ""))
#	.Tcl(paste("after", wait))
#    .Tcl(paste("puts $IKS_", item, " D", sep = ""))
#    .Tcl(paste("after", wait))
#    .Tcl(paste("puts $IKS_", item, " E", sep = ""))
#}

#iks.loadData <- function(port = getOption("iks.port"), wait = 500) {
#	port <- .iks.port(port)
#	item <- .iks.make.names(port)
#	.Tcl(paste("puts $IKS_", item, " \10", sep = ""))
#	.Tcl(paste("after", wait))
#    .Tcl(paste("puts $IKS_", item, " D", sep = ""))
#    .Tcl(paste("after", wait))
#    .Tcl(paste("puts $IKS_", item, " S", sep = ""))
#}

#.iks.action <- function () {
#	IKSfile <- paste("IKSnano_", format(Sys.Date(), "%Y-%m-%d"), ".txt",
#		sep = "")
#	odir <- getwd()
#	setwd("c:/temp")
#	if (!file.exists(IKSfile))
#		cat(IKSheader, "\n", sep = "", file = IKSfile)
#	cat(paste(as.vector(entry[-1]), collapse = "\t"), "\n",
#		file = IKSfile, sep = "", append = TRUE)
#	setwd(odir)
#
#	# Get the table in, memory and update it
#	if (exists(".IKS.table.nano", envir = .GlobalEnv)) {
#		Table <- get(".IKS.table.nano", envir = .GlobalEnv)
#		# Keep only last 400 points
#		n <- nrow(Table)
#		if (n > 399) Table <- Table[(n - 399):n, ]
#		Table <- rbind(Table, entry)
#	} else Table <- entry   # Create a new table
#	.IKS.table.nano <<- Table
#
#	# Update the graph
#	if (nrow(Table) > 2) {
#		opar <- par(no.readonly = TRUE)
#		on.exit(par(opar))
#		par(mar = c(5.1, 4.1, 4.1, 4.1))
#		matplot(Table[, 1], Table[, -c(1:3, 10:12)], type = "l", lty = 1,
#			xlab = "Time", ylab = "pH/Ox", xaxt = "n", ylim = c(2, 9))
#		# Add temperatures, but put them at same scale
#		matlines(Table[, 1], Table[, 10:11] - 20, lty = 2)
#		# Add second y-axis
#		axis(side = 4, at = (12:18)/2, labels = (52:58)/2)
#	    mtext("Temperature (degC)", side = 4, line = 2)
#		# Add x-axis
#		axis(side = 1, at = Table[, 1], labels = format(Table[, 1],"%H:%M:%S"))
#		title("Nanoculture monitoring")
#	    legend(Table[1, 1], 9, c(IKScol[3:8], "T1", "T2"),
#			col = c(1:6, 1:2), lty = c(rep(1, 6), rep(2, 2)))
#	}
#
#	# Create a new entry
#	# Computer date and time are used instead!
#	datetime <- Sys.time()
#	date <- format(Sys.Date(), "%Y-%m-%d")
#	time <- format(datetime, "%H:%M:%S")
#	entry <- data.frame(datetime, date, time, v1 = NA, v2 = NA, v3 = NA,
# 		v4 = NA, v5 = NA, v6 = NA, temp1 = NA, temp2 = NA, term = "",
#		stringsAsFactors = FALSE)
#	names(entry) <- c("datetime", IKScol)
#}



#.Last.lib <- function (libpath) {
#	# Make sure that all connections are closed
#	IKSs <- iks.getAll()
#	if (is.null(IKSs) || length(IKSs) < 1) return()
#	if (length(IKSs) == 1) {
#		cat("Closing connection with the IKS aquastar\n")
#	} else {
#		cat("Closing connections with all IKS aquastars\n")
#	}
#	iks.close("all")
#}


#try(iks.close("all"), silent = TRUE)
#try(winMenuDel("IKS"), silent = TRUE)
##unlink("c:/temp/IKSData.txt")
#if (exists(".IKS.table")) rm(".IKS.table")
#if (exists(".IKS.entry")) rm(".IKS.entry")

#winMenuAdd("IKS")
#winMenuAddItem("IKS", "Display ON", "options(display.IKS = TRUE)")
#winMenuAddItem("IKS", "Display OFF", "options(display.IKS = FALSE)")
#winMenuAddItem("IKS", "Clear data", 'unlink("c:/temp/IKSnano.txt"); try(rm(".IKS.table"), silent = TRUE)')
#winMenuAddItem("IKS", "-", "")
#winMenuAddItem("IKS", "Stop", 'iks.close("all"); winMenuDel("IKS")')

#cat("Recording IKS data...\n")
#setWindowTitle(title = "IKS recording")
#iks.open(port = "com1", procfun = myProcessIKS)
