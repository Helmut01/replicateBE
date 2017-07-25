#########################################################
# EMA's 'Method A' (ANOVA)                              #
# fixed: sequence, subject(sequence), period, treatment #
#########################################################
method.A <- function(alpha = 0.05, path.in = NULL, path.out = NULL,
                     file, set = "", ext, na = ".", sep = ",",
                     dec = ".", logtrans = TRUE, ola = FALSE,
                     print = TRUE, details = FALSE, adjust = FALSE,
                     verbose = FALSE, ask = FALSE, plot.bxp = FALSE,
                     fence = 2, data = NULL) {
  exec <- strftime(Sys.time(), usetz=TRUE)
  ret  <- CV.calc(alpha=alpha, path.in=path.in, path.out=path.out,
                  file=file, set=set, ext=ext, na=na, sep=sep,
                  dec=dec, logtrans=logtrans, ola=ola,
                  print=print, verbose=verbose, ask=ask,
                  plot.bxp=plot.bxp, fence=fence, data=data)
  results  <- paste0(ret$res.file, "_MethodA.txt")
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
  logtrans <- ret$transf
  os <- Sys.info()[[1]] # get OS for line-endings in output (Win: CRLF)
  ow <- options()       # save options
  options(digits=12)    # increase digits for anova()
  if (logtrans) { # use the raw data and log-transform internally
    modA <- lm(log(PK) ~ sequence + subject%in%sequence + period + treatment,
                         data=ret$data)
  } else {        # use the already log-transformed data
    modA <- lm(logPK ~ sequence + subject%in%sequence + period + treatment,
                       data=ret$data)
  }
  if (verbose) {
    name <-  paste0(file, set)
    cat("\nData set", name, "by Method A",
        paste0("\n", paste0(rep("\u2500", 21+nchar(name)), collapse="")), "\n")
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
                    sprintf("%8.3f", DF), ret$CVwT, ret$CVwR, ret$sw.ratio,
                    ret$BE1, ret$BE2, CI[1], CI[2], PE, "fail", "fail",
                    "fail", log(CI[2])-log(PE), paste0(ret$ol, collapse="|"),
                    ret$CVwR.new, ret$sw.ratio.new, ret$BE.new1,
                    ret$BE.new2, "fail", "fail", "fail")
  names(res)<- c("Design", "Method", "n", "nTT", "nRR", "Sub/seq",
                 "Miss/seq", "Miss/per", "alpha", "DF", "CVwT(%)",
                 "CVwR(%)", "sw.ratio", "EL.lo(%)", "EL.hi(%)", "CI.lo(%)",
                 "CI.hi(%)", "PE(%)", "CI", "GMR", "BE", "log.half-width",
                 "outlier", "CVwR.new(%)", "sw.ratio.new", "EL.new.lo(%)",
                 "EL.new.hi(%)", "CI.new", "GMR.new", "BE.new")
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
    res$"CVwR.new(%)"    <- 100*res$"CVwR.new(%)"
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
      # remove superfluous columns if ola=FALSE or ola=TRUE
      # and no outlier(s) detected
      ret <- ret[, !names(ret) %in% c("outlier", "CVwR.new(%)",
                                      "sw.ratio.new", "EL.new.lo(%)",
                                      "EL.new.hi(%)", "CI.new",
                                      "GMR.new", "BE.new")]
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
  cut.pos    <- unlist(gregexpr(pattern="Switching CV", ret$txt))
  left.str   <- substr(ret$txt, 1, cut.pos-1)
  right.str  <- substr(ret$txt, cut.pos, nchar(ret$txt))
  insert.str <- paste0("Degrees of freedom : ", sprintf("%3i", DF),
                "\nalpha              :   ", alpha,
                " (", 100*(1-2*alpha), "% CI)\n")
  txt <- paste0(left.str, insert.str, right.str)
  if (!is.na(res$"CVwR.new(%)")) {
    txt1 <- paste0("\n\nAssessment based on original CVwR",
                   sprintf(" %.2f%%", res$"CVwR(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-2), collapse=""))
  }
  txt <- paste0(txt,
                "\nConfidence interval: ", sprintf("%6.2f%% ... %6.2f%%",
                res$"CI.lo(%)", res$"CI.hi(%)"), "  ", res$CI,
                "\nPoint estimate     : ", sprintf("%6.2f%%", res$"PE(%)"),
                "              ", res$GMR,
                "\nMixed (CI & PE)    :                      ", res$BE, "\n")
  txt <- paste0(txt,
                draw.line(called.from="ABEL",
                          L=ret$BE1, U=ret$BE2, lo=CI[1], hi=CI[2], PE=PE), "\n")
  if (!is.na(res$"CVwR.new(%)")) {
    txt1 <- paste0("\nAssessment based on recalculated CVwR",
                 sprintf(" %.2f%%", res$"CVwR.new(%)"))
    txt <- paste0(txt, txt1, "\n", paste0(rep("\u2500", nchar(txt1)-1), collapse=""))
    txt <- paste0(txt, "\nConfidence interval: ", res$CI.new,
                  "\nPoint estimate     : ", res$GMR.new,
                  "\nMixed (CI & PE)    : ", res$BE.new, "\n")
    txt <- paste0(txt,
                  draw.line(called.from="ABEL",
                            L=ret$BE.new1, U=ret$BE.new2, lo=CI[1], hi=CI[2], PE=PE), "\n")
  }
  if (res$Design == "TRR|RTR")
    txt <- paste0(txt, "Note: The extra-reference design assumes lacking period effects. ",
                  "The treatment\ncomparison will be biased in the presence of a ",
                  "true period effect.\n")
  if (res$Design %in% c("TRTR|RTRT|TRRT|RTTR", "TRRT|RTTR|TTRR|RRTT"))
    txt <- paste0(txt, "Note: Confounded effects; design not recommended.\n")
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
          if (!is.na(ret$CVwR.new)) { # 2nd run for recalculated CVwR
            adj1 <- scABEL.ad(theta0=PE, CV=ret$CVwR.new, design=des,
                              n=ret$Sub.Seq, alpha.pre=alpha, print=FALSE)
          }
          no.infl <- "  TIE not > nominal 0.05; consumer risk is controlled.\n"
          ifelse (is.na(ret$CVwR.new),
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
          if (!is.na(ret$CVwR.new)) {
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
          if (!is.na(ret$CVwR.new)) { # 2nd run for recalculated CVwR
            scABEL.ad(theta0=PE, CV=ret$CVwR.new, design=des, n=ret$Sub.Seq,
                      alpha.pre=alpha, details=TRUE)
          }
        }
      } else {
        message("PE must be within 80.00\u2013125.00% for assessment of the Type I Error.")
      }
    }
  } # end of iteratively adjusting alpha
} # end of function method.A()
