# Calibration of chemical determination curves and prediction of unknowns
# OO-oriented version of the functions in the chemCal package
# Needs the chemCal and MASS packages

#chemCal <- function (conc, resp, data, intercept = TRUE, robust = FALSE,
#weights = NULL, ...)
#{
#	cl <- match.call()
#	if (missing(data)) {
#		if (isTRUE(intercept)) form <- resp ~ conc else form <- resp ~ conc + 0
#		if (isTRUE(robust)) { # Use robust rlm() function from MASS
#			obj <- rlm(form, weights = weights, ...)
#		} else { # Use classical lm()
#			obj <- lm(form, weights = weights, ...)
#		}
#	} else { # data is provided, conc and resp are the name of the variables
#		conc <- sub('^"(.*)"$', "\\1", deparse(substitute(conc)))
#		resp <- sub('^"(.*)"$', "\\1", deparse(substitute(resp)))
#		form <- paste(resp, "~", conc)
#		if (!isTRUE(intercept)) form <- paste(form, "+ 0")
#		form <- as.formula(form)
#		if (isTRUE(robust)) { # Use robust rlm() function from MASS
#			obj <- rlm(form, data = data, weights = weights, ...)
#		} else { # Use classical lm()
#			obj <- lm(form, data = data, weights = weights, ...)
#		}
#	}
#	# Change the call element of the object
#	obj$call <- cl
#	# Return a 'chemCal' object
#	class(obj) <- c("chemCal", class(obj))
#	return(obj)
#}
#
## print(), summary(), coef() methods are those of lm() or rlm() functions
## plot() and predict() are specific
## lod() and loq() methods are defined in the chemCal package
#plot.chemCal <- function (x, which = 0L, ...)
#{
#	if (which == 0L) { # Use calplot()
#		calplot(x, ...)
#	} else { # Use the regular plot.lm() function
#		# Not possible to mix both
#		if (0 %in% which)
#			stop("Cannot mix calibration plot (which == 0) and residual analysis graphs (which > 0)")
#		plot.lm(x, which = which, ...)
#	}
#}
#
#predict.chemCal <- function (object, newdata, samples, ..., ws = "auto",
#alpha = 0.05, var.s = "auto")
#{
#	# On the contrary to inverse.predict, this function allows to perform the
#	# calculation for many samples at a time
#	## TODO...
#}
