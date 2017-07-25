################################################
# Information about the computing environment, #
# packages, data set, and method.              #
################################################
info.env <- function(fun, option=NA, path.in, path.out,
                     file, set, ext, exec, data) {
  if (!is.null(data) & missing(ext)) {
    info  <- info.data(data)
    file  <- info$file
    set   <- info$set
    ref   <- info$ref
    descr <- info$descr
    ext   <- ""
  }
  ext.xls <- c("XLS", "xls", "XLSX", "xlsx")
  system <- Sys.info()
  node   <- system[["nodename"]]
  user   <- system[["user"]]
  OS     <- system[["sysname"]]
  OSrel  <- system[["release"]]
  OSver  <- system[["version"]]
  rver   <- sessionInfo()$R.version$version.string
  rver   <- substr(rver, which(strsplit(rver, "")[[1]]=="n")+2, nchar(rver))
  ryear  <- substr(rver, which(strsplit(rver, "")[[1]]=="(")-1, nchar(rver))
  rver   <- substr(rver, 1, which(strsplit(rver, "")[[1]]=="(")-2)
  cit    <- citation("replicateBE")
  year   <- paste0(" (", substr(cit, regexpr("year", cit)+8,
                                regexpr("year", cit)+11), ")")
  cit    <- citation("readxl")
  year1  <- paste0(" (", substr(cit, regexpr("year", cit)+8,
                                regexpr("year", cit)+11), ")")
  cit    <- citation("PowerTOST")
  year2  <- paste0(" (", substr(cit, regexpr("year", cit)+8,
                                regexpr("year", cit)+11), ")")
  cit    <- citation("nlme")
  year3  <- paste0(" (", substr(cit, regexpr("year", cit)+8,
                                regexpr("year", cit)+11), ")")
  cit    <- citation("lmerTest")
  year4  <- paste0(" (", substr(cit, regexpr("year", cit)+8,
                                regexpr("year", cit)+11), ")")
  lic    <- paste0("This code is copyright \u00A9 by Helmut Sch\u00FCtz, Michael Tomashevskiy, Detlew Labes.\n",
                   "This code is open source; you can redistribute it and/or modify it under the\n",
                   "terms of the GNU General Public License as published by the Free Software Foun-\n",
                   "dation; either version 3, or (at your option) any later version. See the GNU\n",
                   "GPL for more details. Copies of the GPL-3 versions are available at:\n",
                   "http://www.gnu.org/licenses/gpl-3.0.html")
  discl  <- paste0("\n\u2554", paste0(rep("\u2550", 76), collapse=""), "\u2557\n",
                   "\u2551 Program offered for Use without any Guarantees and Absolutely No Warranty. \u2551\n",
                   "\u2551 No Liability is accepted for any Loss and Risk to Public Health Resulting  \u2551\n",
                   "\u2551 from Use of this R-Code.                                                   \u2551\n",
                   "\u255A", paste0(rep("\u2550", 76), collapse=""), "\u255D")
  if (fun == "model.B") {
    ifelse (option == 1, hr.len <- 62+nchar(exec), hr.len <- 57+nchar(exec))
  } else {
    hr.len <- 79
  }
  hr     <- paste0(rep("\u2500", hr.len), collapse="")
  if (!is.null(data)) { # internal data
    info <- paste(lic, discl, "\nReference data set :", set, "(internal data)")
  } else {              # CSV or XLS(X)
    if (missing(path.in) |
        regexpr("/library/replicateBE/extdata/", path.in)[1] >= 1) { # internal CSV
      info <- paste(lic, discl, "\nReference data set :", set, "(internal CSV)")
    } else {                                                         # external CSV or XLS(X)
      info <- paste(lic, discl, "\nInput from         : ")
      if (is.null(path.in)) {
        info <- paste0(info, getwd(), "/")
      } else {
        info <- paste0(info, path.in)
      }
      if (ext %in% ext.xls) {
        info <- paste0(info, "\nFile [sheet]       : ", file, ".", ext,
                       " [", set, "]")
      } else {
        info <- paste0(info, "\nFile               : ", file, set, ".", ext)
      }
    }
  }
  info <- paste(info, "\nOutput to          : ")
  if (is.null(path.out)) {
    info <- paste0(info, getwd(), "/")
  } else {
    info <- paste0(info, path.out)
  }
  info <- paste0(info, "\nSystem             : ", node,
                 "\nUser               : ", user,
                 "\nOperating System   : ", OS, " ", OSrel)
  if (OS == "Darwin") { # special treatment (long system[["version"]])
    tmp <- strwrap(OSver, width = 79, prefix="\n                     ")
    for (j in 1:length(tmp)) {
      info <- paste0(info, tmp[[j]])
    }
  } else {
    info <- paste0(info, " ", OSver)
  }
  info <- paste0(info, "\nR version          : ",
                       sprintf("%-10s", rver), ryear)
  if (ext %in% ext.xls) {
    info <- paste0(info, "\nreadxl version     : ",
                         sprintf("%-10s", packageVersion("readxl")), year1)
  }
  info <- paste0(info, "\nPowerTOST version  : ",
                 sprintf("%-10s", packageVersion("PowerTOST")), year2)
  if (fun == "method.B") {
    if (option == 1) {
      info <- paste0(info, "\nlmerTest version   : ",
                     sprintf("%-10s", packageVersion("lmerTest")), year4)
    } else {
      info <- paste0(info, "\nnlme version       : ",
                     sprintf("%-10s", packageVersion("nlme")), year3)
    }
  }
  info <- paste0(info, "\nreplicateBE version: ",
                 sprintf("%-10s", packageVersion("replicateBE")), year)
  info <- paste0(info, "\n", hr,
                       "\nFunction           : CV.calc(): stats:lm() executed ", exec,
                       "\n  Fixed effects    : sequence, subject(sequence), period",
                       "\n  Data             : treatment = R")
  info <- paste0(info, "\nFunction           : ", fun, "(")
  if (is.na(option)) {
    info <- paste0(info, "): stats:lm()")
    info <- paste0(info, " executed ", exec)
  } else {
    info <- paste0(info, "option=", option, "): ")
    ifelse (option == 1,
      info <- paste0(info, "lmerTest:lmer()\n"),
      info <- paste0(info, "nlme:lme()\n"))
    info <- paste(info, "                    executed", exec)
  }
  if (fun %in% c("ABE", "method.A")) {
    info <- paste0(info, "\n  Fixed effects    : sequence, ",
                         "subject(sequence), period, treatment",
                         "\n  Data             : all")
  } else {
    info <- paste0(info, "\n  Fixed effects    : sequence, period, treatment",
                         "\n  Random effect    : subject(sequence)",
                         "\n  Data             : all")
  }
  info <- paste0(info, "\n", hr, "\n")
  return(info)
} # end of function env.info()
