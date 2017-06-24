################################################
# EMA's 'Method A' (ANOVA)                     #
# fixed: sequence, subjects, period, treatment #
################################################
method.A <- function(alpha = 0.05, path.in = NULL, path.out = NULL,
                     file, set, ext, header = 0, na = ".", sep = ",",
                     dec = ".", logtrans = TRUE, ola = FALSE,
                     print = TRUE, details = FALSE, adjust = FALSE,
                     verbose = FALSE, ask = FALSE, plot.bxp = FALSE,
                     data = NULL) {
  exec <- strftime(Sys.time(), usetz=TRUE)
  ret  <- CV.calc(alpha=alpha, path.in=path.in, path.out=path.out,
                  file=file, set=set, ext=ext, header=header, na=na,
                  sep=sep, dec=dec, logtrans=logtrans, ola=ola,
                  print=print, verbose=verbose, ask=ask,
                  plot.bxp=plot.bxp, data=data)
  results  <- paste0(ret$res.file, "_A.txt")
  logtrans <- ret$transf
  os <- Sys.info()[[1]] # get OS for line-endings in output (Win: CRLF)
  ow <- options()       # save options
  options(digits=12)    # dealing with anova(): increase digits!
  if (logtrans) { # use the raw data and log-transform internally
    modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
                         data=ret$data)
  } else {        # use the already log-transformed data
    modA <- lm(logPK ~ sequence + subject%in%sequence + period + treatment,
                       data=ret$data)
  }
  if (!is.null(data) & missing(ext)) {
    id    <- which.data(data)
    md5   <- digest::digest(data)
    file  <- id[id$checksum == md5, "file"]
    set   <- id[id$checksum == md5, "set"]
    descr <- id[id$checksum == md5, "descr"]
    ext   <- ""
  }
  if (verbose) {
    cat("\nData set", paste0(file, set), "by Method A",
        paste0("\n", paste0(rep("\u2500", 25), collapse="")), "\n")
    print(stats::anova(modA)) # otherwise summary of lmerTest is used
    cat("\ntreatment T \u2013 R:\n")
    print(signif(summary(modA)$coefficients["treatmentT", ]), 7)
    cat(summary(modA)$df[2], "Residual Degrees of Freedom\n\n")
  }
  PE  <- exp(coef(modA)[["treatmentT"]])
  CI  <- as.numeric(exp(confint(modA, "treatmentT", level=1-2*alpha)))
  DF  <- aov(modA)[[8]]
  res <- data.frame(ret$type, "A", ret$n, ret$nTT, ret$nRR,
                    paste0(ret$Sub.Seq, collapse="|"),
                    paste0(ret$Miss.seq, collapse="|"),
                    paste0(ret$Miss.per, collapse="|"), alpha,
                    sprintf("%8.3f", DF), ret$CVwT, ret$CVwR, ret$BE1,
                    ret$BE2, CI[1], CI[2], PE, "fail", "fail", "fail",
                    log(CI[2])-log(PE), paste0(ret$ol, collapse="|"),
                    ret$CVwR.new, ret$BE.new1, ret$BE.new2,
                    "fail", "fail", "fail")
  names(res)<- c("Design", "Method", "n", "nTT", "nRR", "Sub/seq",
                 "Miss/seq", "Miss/per", "alpha", "DF", "CVwT(%)",
                 "CVwR(%)", "EL.lo(%)", "EL.hi(%)", "CI.lo(%)",
                 "CI.hi(%)", "PE(%)", "CI", "GMR", "BE", "log.half-width",
                 "outlier", "CVwR.new(%)", "EL.new.lo(%)", "EL.new.hi(%)",
                 "CI.new", "GMR.new", "BE.new")
  if (ret$BE2 == 1.25) { # change column names if necessary
    colnames(res)[which(names(res) == "EL.lo(%)")] <- "BE.lo(%)"
    colnames(res)[which(names(res) == "EL.hi(%)")] <- "BE.hi(%)"
  }
  if (!is.na(ret$BE.new2) & ret$BE.new2 == 1.25) { # change column names if necessary
    colnames(res)[which(names(res) == "EL.new.lo(%)")] <- "BE.new.lo(%)"
    colnames(res)[which(names(res) == "EL.new.hi(%)")] <- "BE.new.hi(%)"
  }
  # Convert CVs, limits, PE, and CI (till here as fractions) to percent
  res$"CVwT(%)" <- 100*res$"CVwT(%)"
  res$"CVwR(%)" <- 100*res$"CVwR(%)"
  if ("BE.lo(%)" %in% names(res)) { # conventional limits
    res$"BE.lo(%)" <- 100*res$"BE.lo(%)"
    res$"BE.hi(%)" <- 100*res$"BE.hi(%)"
  } else {                          # expanded limits
    res$"EL.lo(%)" <- 100*res$"EL.lo(%)"
    res$"EL.hi(%)" <- 100*res$"EL.hi(%)"
  }
  if (!is.na(res$"CVwR.new(%)")) {
    res$"CVwR.new(%)" <- 100*res$"CVwR.new(%)"
    if ("BE.new.lo(%)" %in% names(res)) { # conventional limits
      res$"BE.new.lo(%)" <- 100*res$"BE.new.lo(%)"
      res$"BE.new.hi(%)" <- 100*res$"BE.new.hi(%)"
    } else {                              # expanded limits
      res$"EL.new.lo(%)" <- 100*res$"EL.new.lo(%)"
      res$"EL.new.hi(%)" <- 100*res$"EL.new.hi(%)"
    }
  }
  res$"PE(%)"    <- 100*res$"PE(%)"
  res$"CI.lo(%)" <- 100*res$"CI.lo(%)"
  res$"CI.hi(%)" <- 100*res$"CI.hi(%)"
  if (round(res$"CI.lo(%)", 2) >= 100*ret$BE1 &
      round(res$"CI.hi(%)", 2) <= 100*ret$BE2)
    res$CI <- "pass"  # CI within acceptance range
  if (round(res$"PE(%)", 2) >= 80 & round(res[["PE(%)"]], 2) <= 125)
    res$GMR <- "pass" # PE within 80.00-125.00%
  if (res$CI == "pass" & res$GMR == "pass")
    res$BE <- "pass"  # if passing both, conclude BE
  if (!is.na(res$"CVwR.new(%)")) {
    if (round(res$"CI.lo(%)", 2) >= 100*ret$BE.new1 &
        round(res$"CI.hi(%)", 2) <= 100*ret$BE.new2)
      res$CI.new <- "pass"  # CI within acceptance range
    res$GMR.new <- res$GMR
    if (res$CI.new == "pass" & res$GMR.new == "pass")
      res$BE.new <- "pass"  # if passing both, conclude BE
  }
  options(ow) # restore options
  if (details) { # results in default (7 digits) precision
    ret <- res
    if (as.character(res$outlier) == "NA") {
      # remove superfluous columns if ola=FALSE or
      # ola=TRUE and no outlier(s) detected
      ret <- ret[ , !names(ret) %in% c("outlier", "CVwR.new(%)",
                                       "EL.new.lo(%)", "EL.new.hi(%)",
                                       "CI.new", "GMR.new", "BE.new")]
    }
    return(ret)
  }
  # Round percents to two decimals according to the GL
  res$"CVwT(%)" <- round(res$"CVwT(%)", 2)
  res$"CVwR(%)" <- round(res$"CVwR(%)", 2)
  if ("BE.lo(%)" %in% names(res)) { # conventional limits
    res$"BE.lo(%)" <- round(res$"BE.lo(%)", 2)
    res$"BE.hi(%)" <- round(res$"BE.hi(%)", 2)
  } else {                          # expanded limits
    res$"EL.lo(%)" <- round(res$"EL.lo(%)", 2)
    res$"EL.hi(%)" <- round(res$"EL.hi(%)", 2)
  }
  res$"PE(%)"    <- round(res$"PE(%)", 2)
  res$"CI.lo(%)" <- round(res$"CI.lo(%)", 2)
  res$"CI.hi(%)" <- round(res$"CI.hi(%)", 2)
  if (!is.na(res$"CVwR.new(%)")) {
  res$"CVwR.new(%)" <- round(res$"CVwR.new(%)", 2)
    if ("BE.new.lo(%)" %in% names(res)) { # conventional limits
      res$"BE.new.lo(%)" <- round(res$"BE.new.lo(%)", 2)
      res$"BE.new.hi(%)" <- round(res$"BE.new.hi(%)", 2)
    } else {                          # expanded limits
      res$"EL.new.lo(%)" <- round(res$"EL.new.lo(%)", 2)
      res$"EL.new.hi(%)" <- round(res$"EL.new.hi(%)", 2)
    }
  }
  overwrite <- TRUE # default
  if (print) { # to file in UTF-8
    if (ask & file.exists(results)) {
      answer <- tolower(readline("Results already exists. Overwrite the file [y|n]? "))
      if(answer != "y") overwrite <- FALSE
    }
    if (overwrite) { # either the file does not exist or should be overwritten
      res.file <- file(description=results, open="wb")
      res.str  <- env.info(fun="method.A", option=NA, path.in, path.out,
                           file, set, ext, exec, data)
      if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
      if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
      writeBin(charToRaw(res.str), res.file)
      close(res.file)
    }
  }
  # insert DFs and alpha into the text generated by CV.calc()
  cut.pos    <- unlist(gregexpr(pattern="Switching CV", ret$txt))
  left.str   <- substr(ret$txt, 1, cut.pos-1)
  right.str  <- substr(ret$txt, cut.pos, nchar(ret$txt))
  insert.str <- paste0("Degrees of freedom : ", sprintf("%3i", DF),
                "\nalpha              : ", alpha,
                " (", 100*(1-2*alpha), "% CI)\n")
  txt <- paste0(left.str, insert.str, right.str)
  if (!is.na(res$"CVwR.new(%)")) {
    txt1 <- paste0("\n\nAssessment based on original CVwR",
                   sprintf(" %.2f%%", res$"CVwR(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-2), collapse=""))
  }
  txt <- paste0(txt,
                "\nConfidence interval: ", sprintf("%6.2f%% ... %.2f%%",
                res$"CI.lo(%)", res$"CI.hi(%)"), " (", res$CI, ")",
                "\nPoint estimate     : ", sprintf("%6.2f%%", res$"PE(%)"),
                " (", res$GMR, ")",
                "\nOverall conclusion : ", res$BE)
  if (!is.na(res$"CVwR.new(%)")) {
    txt1 <- paste0("\n\nAssessment based on recalculated CVwR",
                 sprintf(" %.2f%%", res$"CVwR.new(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-2), collapse=""))
    txt <- paste0(txt, "\nConfidence interval: ", res$CI.new,
                  "\nPoint estimate     : ", res$GMR.new,
                  "\nOverall conclusion : ", res$BE.new)
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
  # Optional: Assess whether there is an inflation of the Type I Error
  # with the specified alpha. If yes, iteratively adjust alpha to
  # preserve the consumer risk.
  if (adjust) {
    if (!ret$type %in% c("RTRT|TRTR", "RTR|TRT", "RRT|RTR|TRR")) {
      message("Assessment of the Type I Error for this design is not implemented.")
    } else {
      if (PE > 0.80 & PE < 1.25) { # PE must be within limits!
        if (ret$type == "RTRT|TRTR")   des <- "2x2x4"
        if (ret$type == "RTR|TRT")     des <- "2x2x3"
        if (ret$type == "RRT|RTR|TRR") des <- "2x3x3"
        if (print) {
          adj <- capture.output(PowerTOST::scABEL.ad(theta0=PE, CV=ret$CVwR,
                                          design=des, n=ret$Sub.Seq,
                                          alpha.pre=alpha, details=TRUE))
          txt <- paste0("\nSim\u2019s based on ANOVA; ",
                        "1,000,000 studies in each iteration simulated.",
                        "\n", substr(adj[18], 1, 39), "\n", adj[20])
          if (adj[20] != "TIE not > nominal alpha; no adjustment of alpha is required.") {
            txt <- paste0(txt, "\n", adj[21])
          }
          if (overwrite) {
            res.file <- file(results, open="ab")
            res.str  <- txt # UNIXes LF
            if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
            if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
            writeBin(charToRaw(res.str), res.file)
            close(res.file)
          }
        } else { # to console
          scABEL.ad(theta0=PE, CV=ret$CVwR, design=des, n=ret$Sub.Seq,
                    alpha.pre=alpha, details=TRUE)
        }
      } else {
        message("PE must be within 80.00-125.00% for the assessment of the Type I Error.")
      }
    }
  } # end of iteratively adjusting alpha
} # end of function method.A()