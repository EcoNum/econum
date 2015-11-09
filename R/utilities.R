## General EcoNum options

## Note: we need these functions, currently defined in titrationX.Y-Z.R
## but need to move these functions here! These 3 functions are not exported
## to the NAMESPACE yet!
sdev_pHmeter <- function (name, id = "pH01", type = "serial", port = "com1",
mode = "4800,n,8,1", buffering = "none", ...)
{
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
	return(obj)
}

sdev_titrator <- function (name, id = "T01", type = "serial", port = "com1",
mode = "4800,n,8,1", buffering = "line", ...)
{
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
	return(obj)
}

sdev_sampler <- function (name, id = "S01", type = "serial",
port = "\\\\\\\\.\\\\com10", mode = "4800,n,8,1", buffering = "line",
maxpos = 16, ...)
{
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
	return(obj)
}

optionsEcoNum <- function (opt)
{
	if (missing(opt)) { # Create it
		opt <- getOption("EcoNum", default = list())

		## Repositories options
		## Default mapping for Windows is t:/EcoNumData
		## For Linux, it is /media/EcoNumDataPublic/EcoNumData
		## and for Mac OS X, it is /Volumes/Public/EcoNumData
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

		## Default general metadata
		opt$defProject <- "project00"
		opt$defSample <- "test00"
		opt$defSampleDate <- Sys.time()
		opt$defAuthor <- "student"

		## List of possible devices
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

		## Default devices
		opt$titrator <- sdev_titrator("Schott TitronicUniversal", id = "TU01",
			type = "serial", port = "com6", mode = "4800,n,7,1",
			#type = "serial", port = "/dev/cu.PL2303-00002006", mode = "4800,n,7,1",
			buffering = "line")
		opt$pHmeter <- sdev_pHmeter("Consort C3010", id = "C3010",
			type = "serial", port = "\\\\\\\\.\\\\com14", mode = "19200,n,8,2",
		#	#type = "serial", port = "/dev/cu.PL2303-00001004", mode = "19200,n,8,2",
			buffering = "none")
		opt$sampler <- sdev_sampler("Schott TWalphaPlus", id = "TW01",
			type = "serial", port = "\\\\\\\\.\\\\com11", mode = "4800,n,7,2",
			buffering = "line", maxpos = 16)
	}

	## Save these options in EcoNum
	options(EcoNum = opt)
	return(opt)
}

getOptEcoNum <- function (key, default = NULL)
{
	opt <- getOption("EcoNum", default = optionsEcoNum())
	if (missing(key)) {
		return(opt)
	} else {
		value <- opt[[key]]
		if (is.null(value) && !missing(default))
			value <- default
		return(value)
	}
}

setOptEcoNum <- function (key, value)
{
	opt <- getOptEcoNum()
	opt[[key]] <- value
	optionsEcoNum(opt)
	return(value)
}

## Calculate an hexadecimal "fingerprint" based on time (rounding down to sec)
timeToFingerprint <- function (Time)	
	return(toupper(as.hexmode(as.integer(Time))))

## Like with Sys.time(), it produces a POSIXct object with NULL tzone attribute
fingerprintToTime <- function (hexmode, tz = "") {
	## Fingerprint is always produced in UTC time!
	res <- as.POSIXct(strtoi(as.hexmode(hexmode), 16L),
		origin = "1970-01-01 00:00.00", tz = "UTC")
	## Eliminate tzone attribute to use default one instead
	attr(res, "tzone") <- NULL
	return(res)
}
