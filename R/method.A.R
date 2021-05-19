#########################################################
# EMA's 'Method A' (ANOVA)                              #
# fixed: sequence, subject(sequence), period, treatment #
#########################################################
method.A <- function(alpha = 0.05, path.in, path.out, file,
                     set = "", ext, na = ".", sep = ",", dec = ".",
                     logtrans = TRUE, regulator = "EMA", ola = FALSE,
                     print = TRUE, details = FALSE, adjust = FALSE,
                     verbose = FALSE, ask = FALSE, plot.bxp = FALSE,
                     fence = 2, data = NULL) {
  exec <- strftime(Sys.time(), usetz=TRUE)
  if (!missing(ext)) ext <- tolower(ext) # case-insensitive
  ret  <- CV.calc(alpha=alpha, path.in=path.in, path.out=path.out,
                  file=file, set=set, ext=ext, na=na, sep=sep,
                  dec=dec, logtrans=logtrans, regulator=regulator, ola=ola,
                  print=print, verbose=verbose, ask=ask,
                  plot.bxp=plot.bxp, fence=fence, data=data)
  results <- paste0(ret$res.file, "_MethodA.txt")
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
  logtrans <- ret$logtrans
  os <- Sys.info()[[1]] # get OS for line-endings in output (Win: CRLF)
  ow <- options()       # save options
  options(digits=12)    # increase digits for anova()
  on.exit(ow)           # ensure that options are reset if an error occurs
  if (logtrans) {       # use the raw data and log-transform internally
    modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
                         data = ret$data)
  } else {              # use the already log-transformed data
    modA <- lm(logPK ~ sequence + subject%in%sequence + period + treatment,
                       data = ret$data)
  }
  if (verbose) {
    name <-  paste0(file, set)
    len  <- max(27+nchar(name), 35)
    cat(paste0("\nData set ", name, ": Method A by lm()"),
        paste0("\n", paste0(rep("\u2500", len), collapse = "")), "\n")
    # change from type I (default as in versions up to 1.0.17)
    # to type III to get the correct carryover test
    typeIII <- stats::anova(modA) # otherwise summary of lmerTest is used
    attr(typeIII, "heading")[1] <- "Type III Analysis of Variance Table\n"
    MSdenom <- typeIII["sequence:subject", "Mean Sq"]
    df2     <- typeIII["sequence:subject", "Df"]
    fvalue  <- typeIII["sequence", "Mean Sq"] / MSdenom
    df1     <- typeIII["sequence", "Df"]
    typeIII["sequence", 4] <- fvalue
    typeIII["sequence", 5] <- pf(fvalue, df1, df2, lower.tail = FALSE)
    print(typeIII, digits = 6, signif.stars = FALSE)
    cat("\ntreatment T \u2013 R:\n")
    print(signif(summary(modA)$coefficients["treatmentT", ]), 6)
    cat(summary(modA)$df[2], "Degrees of Freedom\n\n")
  }
  PE  <- exp(coef(modA)[["treatmentT"]])
  CI  <- as.numeric(exp(confint(modA, "treatmentT", level=1-2*alpha)))
  DF  <- aov(modA)[[8]]
  res <- data.frame(ret$type, "A", ret$n, ret$nTT, ret$nRR,
                    paste0(ret$Sub.Seq, collapse="|"),
                    paste0(ret$Miss.seq, collapse="|"),
                    paste0(ret$Miss.per, collapse="|"), alpha,
                    DF, ret$CVwT, ret$CVwR, ret$swT, ret$swR, ret$sw.ratio,
                    ret$sw.ratio.upper, ret$BE1, ret$BE2, CI[1], CI[2],
                    PE, "fail", "fail", "fail", log(CI[2])-log(PE),
                    paste0(ret$ol, collapse="|"), ret$CVwR.rec,
                    ret$swR.rec, ret$sw.ratio.rec,
                    ret$sw.ratio.rec.upper, ret$BE.rec1,
                    ret$BE.rec2, "fail", "fail", "fail",
                    stringsAsFactors=FALSE)
  names(res)<- c("Design", "Method", "n", "nTT", "nRR", "Sub/seq",
                 "Miss/seq", "Miss/per", "alpha", "DF", "CVwT(%)",
                 "CVwR(%)", "swT", "swR", "sw.ratio", "sw.ratio.CL",
                 "L(%)", "U(%)", "CL.lo(%)", "CL.hi(%)", "PE(%)",
                 "CI", "GMR", "BE", "log.half-width", "outlier",
                 "CVwR.rec(%)", "swR.rec", "sw.ratio.rec",
                 "sw.ratio.rec.CL", "L.rec(%)", "U.rec(%)",
                 "CI.rec", "GMR.rec", "BE.rec")
  if (ret$BE2 == 1.25) { # change column names if necessary
    colnames(res)[which(names(res) == "L(%)")] <- "BE.lo(%)"
    colnames(res)[which(names(res) == "U(%)")] <- "BE.hi(%)"
  }
  if (!is.na(ret$BE.rec2) & ret$BE.rec2 == 1.25) { # change column names if necessary
    colnames(res)[which(names(res) == "L.rec(%)")] <- "BE.rec.lo(%)"
    colnames(res)[which(names(res) == "U.rec(%)")] <- "BE.rec.hi(%)"
  }
  # Convert CVs, limits, PE, and CI (till here as fractions) to percent
  res$"CVwT(%)" <- 100*res$"CVwT(%)"
  res$"CVwR(%)" <- 100*res$"CVwR(%)"
  if ("BE.lo(%)" %in% names(res)) { # conventional limits
    res$"BE.lo(%)" <- 100*res$"BE.lo(%)"
    res$"BE.hi(%)" <- 100*res$"BE.hi(%)"
  } else {                          # expanded limits
    res$"L(%)" <- 100*res$"L(%)"
    res$"U(%)" <- 100*res$"U(%)"
  }
  if (!is.na(res$"CVwR.rec(%)")) {
    res$"CVwR.rec(%)"    <- 100*res$"CVwR.rec(%)"
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
  if (details) { # results in full precision
    ret <- res
    if (as.character(res$outlier) == "NA") {
      # remove superfluous columns if ola=FALSE or ola=TRUE
      # and no outlier(s) detected
      ret <- ret[, !names(ret) %in% c("outlier", "CVwR.rec(%)",
                                      "swR.rec", "sw.ratio.rec",
                                      "L.rec(%)", "U.rec(%)",
                                      "CI.rec", "GMR.rec", "BE.rec")]
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
  if (!is.na(res$"CVwR.rec(%)")) {
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
      res.str  <- info.env(fun="method.A", option=NA, path.in, path.out,
                           file, set, ext, exec, data)
      if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
      if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
      writeBin(charToRaw(res.str), res.file)
      close(res.file)
    }
  }
  # insert DF and alpha into the text generated by CV.calc()
  cut.pos    <- unlist(gregexpr(pattern="Regulator", ret$txt))
  left.str   <- substr(ret$txt, 1, cut.pos-1)
  right.str  <- substr(ret$txt, cut.pos, nchar(ret$txt))
  insert.str <- paste0("Degrees of freedom : ", sprintf("%3i", DF),
                "\nalpha              :   ", alpha,
                " (", 100*(1-2*alpha), "% CI)\n")
  txt <- paste0(left.str, insert.str, right.str)
  if (!is.na(res$"CVwR.rec(%)")) {
    txt1 <- paste0("\n\nAssessment based on original CVwR",
                   sprintf(" %.2f%%", res$"CVwR(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-2), collapse=""))
  }
  txt <- paste0(txt,
                "\nConfidence interval: ", sprintf("%6.2f%% ... %6.2f%%",
                res$"CL.lo(%)", res$"CL.hi(%)"), "  ", res$CI,
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
  if (print & overwrite) {
    res.file <- file(description=results, open="ab")
    res.str  <- txt                                             # UNIXes LF
    if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF
    if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR
    writeBin(charToRaw(res.str), res.file)
    close(res.file)
  }
  # Optional: Assess whether there is an inflation of the Type I Error
  # with the specified alpha. If yes, iteratively adjust alpha to
  # preserve the consumer risk.
  if (adjust) {
    if (!ret$type %in% c("TRTR|RTRT", "TRT|RTR", "TRR|RTR|RRT")) {
      message("Assessment of the Type I Error for this design is not implemented.")
    } else {
      if (PE > 0.80 & PE < 1.25) { # PE must be within limits!
        if (ret$type == "TRTR|RTRT")   des <- "2x2x4"
        if (ret$type == "TRT|RTR")     des <- "2x2x3"
        if (ret$type == "TRR|RTR|RRT") des <- "2x3x3"
        if (print) {
          txt <- paste0("\n", paste0(rep("\u2500", 74), collapse=""))
          adj <- scABEL.ad(theta0=PE, CV=ret$CVwR, design=des,
                           n=ret$Sub.Seq, alpha.pre=alpha, print=FALSE)
          if (!is.na(ret$CVwR.rec)) { # 2nd run for recalculated CVwR
            adj1 <- scABEL.ad(theta0=PE, CV=ret$CVwR.rec, design=des,
                              n=ret$Sub.Seq, alpha.pre=alpha, print=FALSE)
          }
          no.infl <- "  TIE not > nominal 0.05; consumer risk is controlled.\n"
          ifelse (is.na(ret$CVwR.rec),
            txt <- paste0(txt, "\nAssessment of the empiric Type I Error (TIE); "),
            txt <- paste0(txt, "\nAssessment of the empiric Type I Error (TIE) based on original CVwR;\n"))
            iter <- (adj$sims - adj$sims %% 1e6) / 1e6
            ifelse (iter == 1,
              txt <- paste0(txt, "1,000,000 studies simulated.\n"),
              txt <- paste0(txt, "1,000,000 studies in each of the ", iter,
                            " iterations simulated.\n"))
          if (is.na(adj$alpha.adj)) {
            txt <- paste0(txt, no.infl)
          } else {
            txt <- paste0(txt, "  TIE for alpha",
                          sprintf(" %1.6f         : %1.5f",
                                  alpha, adj$TIE.unadj), "\n")
            txt <- paste0(txt, "  TIE for adjusted alpha",
                          sprintf(" %1.6f: %1.5f",
                                  adj$alpha.adj, adj$TIE.adj), "\n")
          } # EO orginal CVwR
          if (!is.na(ret$CVwR.rec)) {
            txt <- paste0(txt, "Assessment of the empiric Type I Error (TIE) based on recalculated CVwR;")
            iter <- (adj1$sims - adj1$sims %% 1e6) / 1e6
            ifelse (iter == 1,
              txt <- paste0(txt, "\n1,000,000 studies simulated.\n"),
              txt <- paste0(txt, "\n1,000,000 studies in each of the ", iter,
                                  " iterations simulated.\n"))
            if (is.na(adj1$alpha.adj)) {
              txt <- paste0(txt, no.infl)
            } else {
              if (alpha)
              txt <- paste0(txt, "  TIE for alpha",
                            sprintf(" %1.6f         : %1.5f",
                                    alpha, adj1$TIE.unadj), "\n")
              if (round(adj1$TIE.adj, 5) != round(adj1$TIE.unadj, 5)) { # this should not happen...
                txt <- paste0(txt, "  TIE for adjusted alpha",
                              sprintf(" %1.6f: %1.5f", adj1$alpha.adj, adj1$TIE.adj), "\n")
              } else {
                txt <- paste0(txt, no.infl)
              }
            }
          } # EO recalculated CVwR
          if (overwrite) {
            res.file <- file(results, open="ab")                        # line endings
            res.str  <- txt                                             # LF (UNIXes, Solaris)
            if (os == "Windows") res.str <- gsub("\n", "\r\n", res.str) # CRLF (Windows)
            if (os == "Darwin")  res.str <- gsub("\n", "\r", res.str)   # CR (OSX)
            writeBin(charToRaw(res.str), res.file)
            close(res.file)
          }
        } else { # to console
          scABEL.ad(theta0=PE, CV=ret$CVwR, design=des, n=ret$Sub.Seq,
                    alpha.pre=alpha, details=TRUE)
          if (!is.na(ret$CVwR.rec)) { # 2nd run for recalculated CVwR
            scABEL.ad(theta0=PE, CV=ret$CVwR.rec, design=des, n=ret$Sub.Seq,
                      alpha.pre=alpha, details=TRUE)
          }
        }
      } else {
        message("PE must be within 80.00\u2013125.00% for assessment of the Type I Error.")
      }
    }
  } # end of iteratively adjusting alpha
} # end of function method.A()
