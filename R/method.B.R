#################################################
# EMA's 'Method B' (linear mixed effects)       #
# fixed:  sequence, period, treatment           #
# random: subjects                              #
# option=1: nlme/lme (Satterthwaite's DF)       #
# option=2: lmerTest/lmer (CONTAIN/Residual DF) #
#################################################
method.B <- function(alpha = 0.05, path.in, path.out, file,
                     set = "", ext, na = ".", sep = ",", dec = ".",
                     logtrans = TRUE, ola = FALSE, print = TRUE,
                     details = FALSE, verbose = FALSE, ask = FALSE,
                     plot.bxp = FALSE, fence = 2, data = NULL,
                     option = 2) {
  exec <- strftime(Sys.time(), usetz=TRUE)
  if (!missing(ext)) ext <- tolower(ext) # case-insensitive
  ret  <- CV.calc(alpha=alpha, path.in=path.in, path.out=path.out,
                  file=file, set=set, ext=ext, na=na, sep=sep,
                  dec=dec, logtrans=logtrans, ola=ola,
                  print=print, verbose=verbose, ask=ask,
                  plot.bxp=plot.bxp, fence=fence, data=data)
  logtrans <- ret$logtrans
  # Add description of the degrees of freedom to the result file
  if (option == 1) DF.suff <- "Satt"
  if (option == 2) DF.suff <- "GL"
  if (option == 3) DF.suff <- "KR"
  results <- paste0(ret$res.file, "_MethodB_DF_", DF.suff, ".txt")
  # generate variables based on the attribute
  # 2nd condition: Otherwise, the header from a CSV file will be overwritten
  if (!is.null(data) & missing(ext)) {
    info  <- info.data(data)
    file  <- info$file
    set   <- info$set
    ref   <- info$ref
    descr <- info$descr
    ext   <- ""
  }
  os <- Sys.info()[[1]]   # get OS for line-endings in output (Win: CRLF)
  ow <- options("digits") # save options
  options(digits=12)      # increase digits for anova()
  on.exit(ow)             # ensure that options are reset if an error occurs
  if (option == 2) {      # by nlme (Residual DF)
    options(contrasts=c("contr.treatment", "contr.poly"))
    if (logtrans) {       # use the raw data and log-transform internally
      modB <- lme(log(PK) ~ sequence + period + treatment,
                            random = ~1|subject, na.action=na.omit,
                            data=ret$data)
    } else {              # use the already log-transformed data
      modB <- lme(logPK ~ sequence + period + treatment,
                          random = ~1|subject, na.action=na.omit,
                          data=ret$data)
    }
    EMA.B <- summary(modB)
    PE    <- EMA.B$tTable["treatmentT", "Value"]
    CI    <- exp(as.numeric(intervals(modB, which="fixed",
                          level=1-2*alpha)[[1]]["treatmentT", c(1, 3)]))
    DF    <- EMA.B$tTable["treatmentT", "DF"]
    if (verbose) {
      name <- paste0(file, set)
      cat("\nData set", paste0(name, ": Method B (option = 2) by lme()"),
          paste0("\n", paste0(rep("\u2500", 41+nchar(name)), collapse="")), "\n")
      if (logtrans) cat("Response: log(PK)\n") else cat("\nResponse: logPK\n")
      print(anova(modB), digits=6, signif.stars=FALSE)
      cat("\ntreatment T \u2013 R:\n")
      print(signif(EMA.B$tTable["treatmentT", c(1:2, 4:5)], 5))
      cat(DF, "Degrees of Freedom (equivalent to SAS\u2019 DDFM=CONTAIN)\n\n")
    }
  } else {              # by lmer/lmerTest (Satterthwaite's or Kenward-Roger DF)
    if (logtrans) {     # use the raw data and log-transform internally
      modB <- lmer(log(PK) ~ sequence + period + treatment + (1|subject),
                             data=ret$data)
    } else {            # use the already log-transformed data
      modB <- lmer(logPK ~ sequence + period + treatment + (1|subject),
                           data=ret$data)
    }
    if (option == 1) {
      EMA.B <- summary(modB, ddf="Satterthwaite")
    } else {
      EMA.B <- summary(modB, ddf="Kenward-Roger")
    }
    PE <- EMA.B$coefficients["treatmentT", "Estimate"]
    CI <- exp(PE + c(-1, +1) *
              qt(1-alpha, EMA.B$coef["treatmentT", "df"]) *
              EMA.B$coef["treatmentT", "Std. Error"])
    DF <- EMA.B$coefficients["treatmentT", "df"]
    if (verbose) {
      if (option == 1) {
        cat("\nData set", paste0(file, set, ": Method B (option = 1) by lmer()"),
            paste0("\n", paste0(rep("\u2500", 46), collapse="")), "\n")
        if (logtrans) cat("Response: log(PK)\n") else cat("\nResponse: logPK\n")
        print(anova(modB, ddf="Satterthwaite"), digits=6, signif.stars=FALSE)
        cat("\ntreatment T \u2013 R:\n")
        print(signif(EMA.B$coefficients["treatmentT", c(1:2, 4:5)], 5))
        cat(signif(DF, 6), "Degrees of Freedom (equivalent to SAS\u2019 DDFM=SATTERTHWAITE)\n\n")
      } else {
        df.txt <- "3; equivalent to Stata\u2019s dfm=Kenward Roger EIM)"
        cat("\nData set", paste0(file, set, ": Method B (option = 3) by lmer()"),
            paste0("\n", paste0(rep("\u2500", 46), collapse="")), "\n")
        if (logtrans) cat("Response: log(PK)\n") else cat("\nResponse: logPK\n")
        print(anova(modB, ddf="Kenward-Roger"), digits=6, signif.stars=FALSE)
        cat("\ntreatment T \u2013 R:\n")
        print(signif(EMA.B$coefficients["treatmentT", c(1:2, 4:5)], 5))
        cat(signif(DF, 6), "Degrees of Freedom (equivalent to Stata\u2019s dfm=Kenward Roger EIM)\n\n")
      }
    }
  } # end of evaluation by option=2 (lme) or option=1/3 (lmer)
  PE  <- exp(PE)
  res <- data.frame(ret$type, paste0("B-", option), ret$n, ret$nTT, ret$nRR,
                    paste0(ret$Sub.Seq, collapse="|"),
                    paste0(ret$Miss.seq, collapse="|"),
                    paste0(ret$Miss.per, collapse="|"), alpha,
                    DF, ret$CVwT, ret$CVwR, ret$sw.ratio,
                    ret$sw.ratio.upper, ret$BE1, ret$BE2, CI[1], CI[2],
                    PE, "fail", "fail", "fail", log(CI[2])-log(PE),
                    paste0(ret$ol, collapse="|"), ret$CVwR.rec,
                    ret$sw.ratio.rec, ret$sw.ratio.rec.upper, ret$BE.rec1,
                    ret$BE.rec2, "fail", "fail", "fail",
                    stringsAsFactors=FALSE)
  names(res)<- c("Design", "Method", "n", "nTT", "nRR", "Sub/seq",
                 "Miss/seq", "Miss/per", "alpha", "DF", "CVwT(%)",
                 "CVwR(%)", "sw.ratio", "sw.ratio.CL", "L(%)",
                 "U(%)", "CL.lo(%)", "CL.hi(%)", "PE(%)",
                 "CI", "GMR", "BE", "log.half-width", "outlier",
                 "CVwR.rec(%)", "sw.ratio.rec", "sw.ratio.rec.CL",
                 "L.rec(%)", "U.rec(%)",
                 "CI.rec", "GMR.rec", "BE.rec")
  if (ret$BE2 == 1.25) { # change column names if not scaling
    colnames(res)[which(names(res) == "L(%)")] <- "BE.lo(%)"
    colnames(res)[which(names(res) == "U(%)")] <- "BE.hi(%)"
  }
  if (!is.na(ret$BE.rec2) & ret$BE.rec2 == 1.25) { # change column names if not scaling
    colnames(res)[which(names(res) == "L.rec(%)")] <- "BE.rec.lo(%)"
    colnames(res)[which(names(res) == "U.rec(%)")] <- "BE.rec.hi(%)"
  }
  # Convert CVs, limits, PE, and CI (up to here as fractions) to percent
  res$"CVwT(%)" <- 100*res$"CVwT(%)"
  res$"CVwR(%)" <- 100*res$"CVwR(%)"
  if ("BE.lo(%)" %in% names(res)) { # conventional limits
    res$"BE.lo(%)" <- 100*res$"BE.lo(%)"
    res$"BE.hi(%)" <- 100*res$"BE.hi(%)"
  } else {                          # expanded limits
    res$"L(%)" <- 100*res$"L(%)"
    res$"U(%)" <- 100*res$"U(%)"
  }
  if (!is.na(res$"CVwR.rec(%)")) { # only if recalculated CVwR
    res$"CVwR.rec(%)" <- 100*res$"CVwR.rec(%)"
    if ("BE.rec.lo(%)" %in% names(res)) { # conventional limits
      res$"BE.rec.lo(%)" <- 100*res$"BE.rec.lo(%)"
      res$"BE.rec.hi(%)" <- 100*res$"BE.rec.hi(%)"
    } else {                              # expanded limits
      res$"L.rec(%)" <- 100*res$"L.rec(%)"
      res$"U.rec(%)" <- 100*res$"U.rec(%)"
    }
  }
  res$"PE(%)"    <- 100*res$"PE(%)"
  res$"CL.lo(%)" <- 100*res$"CL.lo(%)"
  res$"CL.hi(%)" <- 100*res$"CL.hi(%)"
  if (round(res$"CL.lo(%)", 2) >= 100*ret$BE1 &
      round(res$"CL.hi(%)", 2) <= 100*ret$BE2)
    res$CI <- "pass"  # CI within acceptance range
  if (round(res$"PE(%)", 2) >= 80 & round(res[["PE(%)"]], 2) <= 125)
    res$GMR <- "pass" # PE within 80.00-125.00%
  if (res$CI == "pass" & res$GMR == "pass")
    res$BE <- "pass"  # if passing both, conclude BE
  if (!is.na(res$"CVwR.rec(%)")) {
    if (round(res$"CL.lo(%)", 2) >= 100*ret$BE.rec1 &
        round(res$"CL.hi(%)", 2) <= 100*ret$BE.rec2)
      res$CI.rec <- "pass"  # CI within acceptance range
    res$GMR.rec <- res$GMR
    if (res$CI.rec == "pass" & res$GMR.rec == "pass")
      res$BE.rec <- "pass"  # if passing both, conclude BE
  }
  if (details) { # results in default (7 digits) precision
    ret <- res
    if (as.character(res$outlier) == "NA") {
      # remove superfluous columns if ola=FALSE or ola=TRUE
      # and no outlier(s) detected
      ret <- ret[ , !names(ret) %in% c("outlier", "CVwR.rec(%)",
                                       "sw.ratio.rec", "L.rec(%)",
                                       "U.rec(%)", "CI.rec",
                                       "GMR.rec", "BE.rec")]
    }
    #class(ret) <- "repBE"
    return(ret)
  }
  # Round percents to two decimals according to the GL
  res$"CVwT(%)" <- round(res$"CVwT(%)", 2)
  res$"CVwR(%)" <- round(res$"CVwR(%)", 2)
  if ("BE.lo(%)" %in% names(res)) { # conventional limits
    res$"BE.lo(%)" <- round(res$"BE.lo(%)", 2)
    res$"BE.hi(%)" <- round(res$"BE.hi(%)", 2)
  } else {                          # expanded limits
    res$"L(%)" <- round(res$"L(%)", 2)
    res$"U(%)" <- round(res$"U(%)", 2)
  }
  res$"PE(%)"    <- round(res$"PE(%)", 2)
  res$"CL.lo(%)" <- round(res$"CL.lo(%)", 2)
  res$"CL.hi(%)" <- round(res$"CL.hi(%)", 2)
  if (!is.na(res$"CVwR.rec(%)")) { # only if recalculated CVwR
  res$"CVwR.rec(%)" <- round(res$"CVwR.rec(%)", 2)
    if ("BE.rec.lo(%)" %in% names(res)) { # conventional limits
      res$"BE.rec.lo(%)" <- round(res$"BE.rec.lo(%)", 2)
      res$"BE.rec.hi(%)" <- round(res$"BE.rec.hi(%)", 2)
    } else {                          # expanded limits
      res$"L.rec(%)" <- round(res$"L.rec(%)", 2)
      res$"U.rec(%)" <- round(res$"U.rec(%)", 2)
    }
  }
  overwrite <- TRUE # default
  if (print) { # to file in UTF-8
    if (ask & file.exists(results)) {
      answer <- tolower(readline("Results already exists. Overwrite the file [y|n]? "))
      if(answer != "y") overwrite <- FALSE
    }
    if (overwrite) { # either the file does not exist or should be overwritten
      # only binary mode supports UTF-8 and different line endings
      res.file <- file(description=results, open="wb")
      res.str  <- info.env(fun="method.B", option=option, path.in, path.out,
                           file, set, ext, exec, data)
      if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
      if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
      writeBin(charToRaw(res.str), res.file)
      close(res.file)
    }
  }
  # insert DF of treatment difference and alpha into the text generated by CV.calc()
  cut.pos    <- unlist(gregexpr(pattern="Switching CV", ret$txt))
  left.str   <- substr(ret$txt, 1, cut.pos-1)
  right.str  <- substr(ret$txt, cut.pos, nchar(ret$txt))
  insert.str <- "Degrees of freedom : "
  if (option == 1) insert.str <- paste0(insert.str, sprintf("%7.3f (Satterthwaite)", DF))
  if (option == 2) insert.str <- paste0(insert.str, sprintf("%3i", DF))
  if (option == 3) insert.str <- paste0(insert.str, sprintf("%7.3f (Kenward-Roger)", DF))
  insert.str <- paste0(insert.str, "\nalpha              :   ", alpha,
                " (", 100*(1-2*alpha), "% CI)\n")
  txt <- paste0(left.str, insert.str, right.str)
  if (!is.na(res$"CVwR.rec(%)")) {
    txt1 <- paste0("\n\nAssessment based on original CVwR",
                   sprintf(" %.2f%%", res$"CVwR(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-2), collapse=""))
  }
  txt <- paste0(txt,
                "\nConfidence interval: ", sprintf("%6.2f%% ... %6.2f%%",
                                                   res$"CL.lo(%)", res$"CL.hi(%)"),
                "  ", res$CI,
                "\nPoint estimate     : ", sprintf("%6.2f%%", res$"PE(%)"),
                "              ", res$GMR,
                "\nMixed (CI & PE)    :                      ", res$BE, "\n")
  txt <- paste0(txt,
                repBE.draw.line(called.from="ABEL", L=ret$BE1, U=ret$BE2,
                                lo=CI[1], hi=CI[2], PE=PE), "\n")
  if (!is.na(res$"CVwR.rec(%)")) {
    txt1 <- paste0("\nAssessment based on recalculated CVwR",
                   sprintf(" %.2f%%", res$"CVwR.rec(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-1), collapse=""))
    txt <- paste0(txt, "\nConfidence interval: ", res$CI.rec,
                  "\nPoint estimate     : ", res$GMR.rec,
                  "\nMixed (CI & PE)    : ", res$BE.rec, "\n")
    txt <- paste0(txt,
                  repBE.draw.line(called.from="ABEL", L=ret$BE.rec1, U=ret$BE.rec2,
                                  lo=CI[1], hi=CI[2], PE=PE), "\n")
  }
  if (res$Design == "TRR|RTR")
    txt <- paste0(txt, "Note: The extra-reference design assumes lacking period effects. ",
                  "The treatment\ncomparison will be biased in the presence of a ",
                  "true period effect.\n")
  if (res$Design %in% c("TRTR|RTRT|TRRT|RTTR", "TRRT|RTTR|TTRR|RRTT"))
    txt <- paste0(txt, "Note: Confounded effects; design not recommended.\n")
  if (print & overwrite) {
    res.file <- file(results, open="ab")                        # line endings
    res.str  <- txt                                             # LF (UNIXes, Solaris)
    if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF (Windows)
    if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR (OSX)
    writeBin(charToRaw(res.str), res.file)
    close(res.file)
  }
} # end of function method.B()
