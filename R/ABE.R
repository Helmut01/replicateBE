################################################
# Conventional (unscaled) ABE by ANOVA         #
# fixed: sequence, subjects, period, treatment #
################################################
ABE <- function(alpha = 0.05, path.in = NULL, path.out = NULL,
                file, set, ext, header = 0, na = ".", sep = ",",
                dec = ".", logtrans = TRUE, print = TRUE,
                details = FALSE, verbose = FALSE, ask = FALSE,
                data = NULL) {
  exec <- strftime(Sys.time(), usetz=TRUE)
  ret  <- CV.calc(alpha=alpha, path.in=path.in, path.out=path.out,
                  file=file, set=set, ext=ext, header=header, na=na,
                  sep=sep, dec=dec, logtrans=logtrans, ola=FALSE,
                  print=print, verbose=verbose, ask=ask,
                  data=data)
  if (print) { # change from "ABEL" to "ABE"
    results <- paste0(substr(ret$res.file, 1,
                     which(strsplit(ret$res.file, "")[[1]]=="_")),
                     "ABE.txt")
  }
  # generate variables for internal data based on MD5 hash
  # 2nd condition: Otherwise, the header from a CSV file will be overwritten
  if (!is.null(data) | missing(ext)) {
    id    <- which.data(data)
    md5   <- digest::digest(data)
    file  <- id[id$checksum == md5, "file"]
    set   <- id[id$checksum == md5, "set"]
    descr <- id[id$checksum == md5, "descr"]
    ext   <- ""
  }
  logtrans <- ret$transf
  os <- Sys.info()[[1]] # get OS for line-endings in output (Win: CRLF)
  ow <- options()       # save options
  options(digits=12)    # dealing with anova(): increase digits!
  if (logtrans) { # use the raw data and log-transform internally
    mod <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
                        data=ret$data)
  } else {        # use the already log-transformed data
    mod <- lm(logPK ~ sequence + subject%in%sequence + period + treatment,
                      data=ret$data)
  }
  if (verbose) {
    cat("\nData set", paste0(file, set), "by ABE",
        paste0("\n", paste0(rep("\u2500", 20), collapse="")), "\n")
    print(stats::anova(mod)) # otherwise summary of lmerTest is used
    cat("\ntreatment T \u2013 R:\n")
    print(signif(summary(mod)$coefficients["treatmentT", ]), 7)
    cat(anova(mod)["Residuals", "Df"], "Residual Degrees of Freedom\n\n")
  }
  PE  <- exp(coef(mod)[["treatmentT"]])
  CI  <- as.numeric(exp(confint(mod, "treatmentT", level=1-2*alpha)))
  DF  <- aov(mod)[[8]]
  res <- data.frame(ret$type, "ABE", ret$n, ret$nTT, ret$nRR,
                    paste0(ret$Sub.Seq, collapse="|"),
                    paste0(ret$Miss.seq, collapse="|"),
                    paste0(ret$Miss.per, collapse="|"), alpha,
                    sprintf("%8.3f", DF), ret$CVwT, ret$CVwR, 80,
                    125, CI[1], CI[2], PE, "fail")
  names(res)<- c("Design", "Method", "n", "nTT", "nRR", "Sub/seq",
                 "Miss/seq", "Miss/per", "alpha", "DF", "CVwT(%)",
                 "CVwR(%)", "BE.lo(%)", "BE.hi(%)", "CI.lo(%)",
                 "CI.hi(%)", "PE(%)", "BE")
  # Convert CVs, limits, PE, and CI (till here as fractions) to percent
  res$"CVwT(%)"  <- 100*res$"CVwT(%)"
  res$"CVwR(%)"  <- 100*res$"CVwR(%)"
  res$"PE(%)"    <- 100*res$"PE(%)"
  res$"CI.lo(%)" <- 100*res$"CI.lo(%)"
  res$"CI.hi(%)" <- 100*res$"CI.hi(%)"
  if (round(res$"CI.lo(%)", 2) >= 80 &
      round(res$"CI.hi(%)", 2) <= 125)
    res$BE <- "pass" # CI within acceptance range
  options(ow) # restore options
  if (details) { # results in default (7 digits) precision
    ret <- res   # and remove superfluous columns
    return(ret)
  }
  # Round percents to two decimals according to the GL
  res$"CVwT(%)"  <- round(res$"CVwT(%)", 2)
  res$"CVwR(%)"  <- round(res$"CVwR(%)", 2)
  res$"BE.lo(%)" <- round(res$"BE.lo(%)", 2)
  res$"BE.hi(%)" <- round(res$"BE.hi(%)", 2)
  res$"PE(%)"    <- round(res$"PE(%)", 2)
  res$"CI.lo(%)" <- round(res$"CI.lo(%)", 2)
  res$"CI.hi(%)" <- round(res$"CI.hi(%)", 2)
  overwrite <- TRUE # default
  if (print) { # to file in UTF-8
    if (ask & file.exists(results)) {
      answer <- tolower(readline("Results already exists. Overwrite the file [y|n]? "))
      if(answer != "y") overwrite <- FALSE
    }
    if (overwrite) { # either the file does not exist or should be overwritten
      # only binary mode supports UTF-8 and different line endings
      res.file <- file(description=results, open="wb")
      res.str  <- env.info(fun="ABE", option=NA, path.in, path.out,
                           file, set, ext, exec, data)
      if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
      if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
      # Note: Selecting the /first/ element is a workaround!
      # Don't know why /two/ are returned for the internal rds03.
      writeBin(charToRaw(res.str[1]), res.file)
      close(res.file)
    }
  }
  txt <- paste0(ret$txt,
                "\nConfidence interval: ", sprintf("%6.2f%% ... %.2f%%",
                res$"CI.lo(%)", res$"CI.hi(%)"), " (", res$BE, ")",
                "\nPoint estimate     : ", sprintf("%6.2f%%", res$"PE(%)"))
  if (res$Design == "RTR|TRR") {
    txt <- paste0(txt, "\nNote: The extra-reference design assumes lacking period effects. ",
                  "The treatment\ncomparison will be biased in the presence of a ",
                  "true period effect.")
  }
  txt <- paste0(txt, "\n")
  if (print & overwrite) {
    res.file <- file(description=results, open="ab")
    res.str  <- txt # UNIXes LF
    if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
    if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
    writeBin(charToRaw(res.str), res.file)
    close(res.file)
  }
} # end of function ABE()
