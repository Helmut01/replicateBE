#################################
# Get the data from the file or #
# internal data and generate    #
# output common to all methods. #
#################################
get.data <- function(path.in, path.out, file, set = "",
                     ext, na = ".", sep = ",", dec = ".",
                     logtrans = TRUE, print, plot.bxp, data) {
  graphics.off()
  transf <- logtrans # default
  if (is.null(data)) { # checking external data
    ext.csv <- c("CSV", "csv")
    ext.xls <- c("XLS", "xls", "XLSX", "xlsx")
    if (missing(file))
      stop("Argument 'file' must be given.")
    if (is.numeric(file))
      stop("Argument 'file' must be a string (i.e., enclosed in single or double quotes).")
    if (is.numeric(set))
      stop("Argument 'set' must be a string (i.e., enclosed in single or double quotes).")
    if (missing(ext))
      stop("Argument 'ext' (file-extension) must be given.")
    if (!ext %in% c(ext.csv, ext.xls))
      stop("Data format not supported (must be Character Separated Variables or Excel.")
    if (ext %in% ext.csv) {
      if (!sep %in% c(";", ",", "\t"))
        stop(paste0("Reading CSV-file\n       Argument 'sep' (variable separator) must be any of",
                    "\n       ',' (comma = default), ';' (semicolon), or '\\t' (tab)."))
      if (!dec %in% c(".", ","))
        stop("Reading CSV-file: Argument 'dec' (decimal separator) must be\n'.' (period = default) or ',' (comma).")
      if (sep == dec)
        stop("Reading CSV-file\n       Arguments 'sep' and decimal 'dec' must be different.")
    }
    if (ext %in% ext.xls & (set == ""))
      stop("Reading Excel\n       Argument 'set' (name of worksheet) must be given.")
    if (is.null(path.in) | missing(path.in)) {
      home.path <- getwd()
      setwd(home.path)
      warning("'path.in' not given; home folder'", home.path, "' used.")
      path.in <- home.path
    }
    if (!dir.exists(path.in)) {
      home.path <- getwd()
      setwd(home.path)
      warning("Folder given in 'path.in' does not exist; home folder\n  '", home.path, "' used.")
      path.in <- home.path
    } # Adds trailing '/' to path if missing
    path.in <- ifelse(regmatches(path.in, regexpr(".$", path.in)) == "/",
                      path.in, paste0(path.in, "/"))
  } # EO checking external data
  if (print | plot.bxp) { # check only if necessary
    if (missing(path.out)) {
      home.path <- getwd()
      setwd(home.path)
      warning("'path.out' not given; output to home folder\n  '", home.path, "'.")
      path.out <- home.path
    }
    if (is.null(path.out)) {
      home.path <- getwd()
      setwd(home.path)
      warning("'path.out' not given; output to home folder\n  '", home.path, "'.")
      path.out <- home.path
    }
    if (!dir.exists(path.out)) {
      home.path <- getwd()
      setwd(home.path)
      warning("Folder given in 'path.out' does not exist; output to home folder\n  '", home.path, "'.")
      path.out <- home.path
    } # Adds trailing '/' to path if missing
    path.out <- ifelse(regmatches(path.out, regexpr(".$", path.out)) == "/",
                       path.out, paste0(path.out, "/"))
  } # EO print/plot checks
  if (is.null(data)) {      # read data from file
    if (ext %in% ext.csv) full.name <- paste0(path.in, file, set, ".", ext)
    if (ext %in% ext.xls) full.name <- paste0(path.in, file, ".", ext)
    if (!file.exists(full.name)) {
      setwd(dirname(file.choose()))
      path.in <- paste0(getwd(), "/")
      full.name <- paste0(path.in, file, ".", ext)
    }
    # Read the entire content
    if (ext %in% ext.xls) { # read from Excel to the data frame
      datawithdescr <- as.data.frame(read_excel(path=full.name, sheet=set,
                                                na=c("NA", "ND", ".", "", "Missing"),
                                                skip=0, col_names=FALSE))
    } else {
      datawithdescr <- read.csv(file=full.name, sep=sep, dec=dec, quote="", header=FALSE,
                                strip.white=TRUE, na.strings=c("NA", "ND", ".", "", "Missing"),
                                stringsAsFactors=FALSE)
    }
    namesvector = c("subject", "period", "sequence", "treatment")
    # Looking for a row with namesvector, summing all its members and
    # if all are there, mark as TRUE
    Nnamesdf <- c(t(apply(datawithdescr, 1, function(row, table) {
      sum(match(tolower(row), table=table), na.rm=TRUE)}, table=namesvector)) == 10)
    if (sum(Nnamesdf) == 0)
      stop("Column names must be given as 'subject', 'period', 'sequence', 'treatment'.")
    if (sum(Nnamesdf) > 1) {
      err.msg <- paste("More than 1 row with column names 'subject', 'period'",
                       "\n       'sequence', 'treatment' detected.")
      stop(err.msg)
    }
    # If there are some # comments in datafiles, collapse them
    if (which(Nnamesdf == TRUE)-1) {
      # Selecting rows before names of dataset
      if (!ext %in% ext.xls) {
        descr <- scan(file=full.name, what=character(), quiet = TRUE, sep = "\n",
                      nlines = (which(Nnamesdf == TRUE)-1))
        descr <- paste0(descr[startsWith(descr, "#")], collapse="\n")
        descr <- gsub("#", "", descr)
      } else {
        descrdf <- datawithdescr[1:(which(Nnamesdf == TRUE)-1), ]
        descr <- unname(apply(descrdf, 1, function(row){paste0(row[!is.na(row)], collapse=" ")}))
      }
      descr <- paste0(strwrap(descr, width = 78), collapse="\n")
    } else {
      descr <- ""
    }
    data <- datawithdescr[(which(Nnamesdf == TRUE)+1):nrow(datawithdescr), ]
    names(data) <- lapply(datawithdescr[which(Nnamesdf == TRUE), ], as.character)
    # Convert eventual mixed or upper case variable names to lower case
    facs  <- which(!names(data) %in% c("PK", "logPK")) # will be factors later
    names(data)[facs] <- tolower(names(data)[facs])
    # from demo(error.catching)
    tryCatch.W.E <- function(expr) {
      W <- NULL
      w.handler <- function(w){ # warning handler
        W <<- w
        invokeRestart("muffleWarning")
      }
      list(value=withCallingHandlers(tryCatch(expr, error=function(e) e),
                                     warning=w.handler), warning=W)
    }
    PKcols <- which(names(data) %in% c("PK", "logPK")) # PK columns
    for (j in seq_along(PKcols)) {                     # transform to numeric
      msg1 <- tryCatch.W.E(data[, PKcols[j]] <- as.numeric(data[, PKcols[j]]))$warning
      invisible(grepl("NAs introduced by coercion", msg1))
      if (!is.null(msg1)) { # NAs as specified in "na" or complete data
        if (!grepl("NAs introduced by coercion", msg1)) {
          msg2 <- paste0("Import: Missing data according to your specifier na='", na, "'")
          msg2 <- paste0(msg2, " not found\n  in column ", names(data)[PKcols[j]], ".")
          msg2 <- paste0(msg2, " Any other non-numeric value was converted to NA.")
          warning(msg2)
        } else {
          print(msg1) # Problem
        }
      }
    }
    # data[, PKcols] <- lapply(data[, PKcols], as.numeric) # throws warnings
    # EO reading file
    if (print) res.file <- paste0(path.out, file, set, "_ABEL")
    if (plot.bxp) png.path <- paste0(path.out, file, set, "_boxplot.png")
    # If the user erroneously asks for analysis of logPK - which does not
    # exist in the data set - change to internal log-transformation.
    if (logtrans == FALSE & !"logPK" %in% colnames(data)) {
      warn.msg <- paste0("Requested analysis of already transformed data ('logtrans = FALSE')\n",
                         "  not possible since column 'logPK' does not exist in the dataset.\n",
                         "  Analysis of log-transformed column 'PK' instead.")
      warning(warn.msg)
      logtrans <- TRUE
      transf <- TRUE
    } # factorize variables except response(s)
    cols       <- c("subject", "period", "sequence", "treatment")
    data[cols] <- lapply(data[cols], factor)
    if (sum(!unique(data$treatment) %in% c("R", "T")) !=0)
      stop("treatments must be given as 'R' and 'T'.")
  } else { # EO reading external data
    if (missing(ext)) { # get information of internal data set
      info  <- info.data(data)
      file  <- info$file
      set   <- info$set
      ref   <- info$ref
      descr <- info$descr
      if (logtrans == FALSE & !"logPK" %in% colnames(data)) {
        warn.msg <- paste0("Requested analysis of already transformed data ('logtrans = FALSE')\n",
                           "  not possible since column 'logPK' does not exist in the dataset.\n",
                           "  Analysis of log-transformed column 'PK' instead.")
        warning(warn.msg)
        logtrans <- TRUE
        transf <- TRUE
      }
    }
  }
  if (print) res.file <- paste0(path.out, file, set, "_ABEL")
  if (plot.bxp) png.path <- paste0(path.out, "/", file, set, "_boxplot.png")
  subjs  <- unique(data$subject)          # Subjects
  seqs   <- levels(unique(data$sequence)) # Sequences
  design <- info.design(seqs=seqs)        # fetch info
  seqs   <- design$reordered              # preferred reordered sequences (T first)
  Npers  <- design$periods                # Number of periods
  Nseqs  <- design$sequences              # Number of sequences
  type   <- design$type                   # Nice identifier string
  design <- design$design                 # "full" or "partial"
  if (nchar(type) == 19) {  # 4-period 4-sequence full replicate designs
    if (Npers != 4) stop("4 periods required in this full replicate design.")
    if (Nseqs != 4) stop("4 sequences required in this full replicate design.")
  }
  if (nchar(type) == 9) {  # 4-period full replicate designs
    if (Npers != 4) stop("4 periods required in this full replicate design.")
    if (Nseqs != 2) stop("2 sequences required in this full replicate design.")
  }
  if (nchar(type) == 7) {  # 3-period replicates
    if (type %in% c("TRT|RTR", "TRR|RTT")) {
      if (Npers != 3) stop("3 periods required in this full replicate design.")
      if (Nseqs != 2) stop("2 sequences required in this full replicate design.")
    }
    if (type == "TRR|RTR") {
      if (Npers != 3) stop("3 periods required in the extra-reference design.")
      if (Nseqs != 2) stop("2 sequences required in the extra-reference design.")
    }
  }
  if (nchar(type) == 11) { # 3-sequence partial replicate or Balaam's design
    if (!type == "TR|RT|TT|RR") {
      if (Npers != 3) stop("3 periods required in this partial replicate design.")
      if (Nseqs != 3) stop("3 sequences required in this partial replicate design.")
    } else {
      if (Npers != 2) stop("2 periods required in Balaam's design.")
      if (Nseqs != 4) stop("4 sequences required in Balaam's design.")
    }
  }
  # next line introduced for DS24 where all data of subject 16 are NA
  # Given that: Do we need na.action=na.omit in lme() any more?
  data <- na.omit(data)
  Nsub.seq <- table(data$sequence[!duplicated(data$subject)])
  # adapt to reordered sequences
  Nsub.seq <- Nsub.seq[order(match(names(Nsub.seq), seqs))]
  ref   <- data[data$treatment == "R", ]
  test  <- data[data$treatment == "T", ]
  if (type == "TR|RT|TT|RR") {
    T.subj <- test[(test$sequence == "RT" | test$sequence == "TR"), "subject"]
    R.subj <- ref[(ref$sequence == "RT" | ref$sequence == "TR"), "subject"]
    NTR <- length(unique(T.subj) %in% unique(R.subj))
  } else {
    NTR <- length(unique(test$subject) %in% unique(ref$subject)) #  >= 1 T & >= 1 R
  }
  n     <- length(unique(data$subject))
  # Get the wide data frame subject+sequence\period
  compl <- reshape(data[c("subject", "sequence", "period", "PK")],
                   idvar=c("subject", "sequence"), timevar=c("period"),
                   direction="wide")
  uncompletedata <- compl[!complete.cases(compl), ] # exclude complete
  if (nrow(uncompletedata)) {
    # Select NAs in period columns
    Miss.per <- data.frame(sapply(compl, function(y) sum(is.na(y))))
    Miss.per <- t(Miss.per[!rownames(Miss.per) %in% c("subject", "sequence"), ])
    colnames(Miss.per) <- paste0("PK.", 1:ncol(Miss.per))
    # Reformat uncomplete
    uncompletedeshaped <- reshape(uncompletedata, direction="long")
    # Exclude periods with data
    deshapeduncomplete <- uncompletedeshaped[!complete.cases(uncompletedeshaped), ]
    Miss.seq <- table(deshapeduncomplete$sequence)
  } else {
    Miss.seq <- rep(0, Nseqs)
    names(Miss.seq) <- seqs
    Miss.per <- rep(0, Npers)
  }
  Miss.seq <- Miss.seq[order(match(names(Miss.seq), seqs))]
  # Data of subjects with two R treatments
  RR   <- ref[duplicated(ref$subject, fromLast=TRUE)|
              duplicated(ref$subject, fromLast=FALSE), ]
  RR   <- RR[!is.na(RR$PK), ]        # exclude NAs
  nRR  <- length(unique(RR$subject)) # number of subjects
  nTT  <- NA
  if (design == "full") { # only full replicates
    # Data of subjects with two T treatments
    TT  <- test[duplicated(test$subject, fromLast=TRUE)|
                duplicated(test$subject, fromLast=FALSE), ]
    TT  <- TT[!is.na(TT$PK), ]        # exclude NAs
    nTT <- length(unique(TT$subject)) # number of subjects
  }
  if (!is.na(descr) & !is.null(descr) & length(descr) >= 1)
    txt <- paste0(strwrap(descr, width = 78), collapse="\n")
  if (logtrans) {
    txt <- paste0(txt,
                  "\nAnalysis performed on column \u2018PK\u2019 ",
                  "(data internally log-transformed)")
  } else {
    txt <- paste0(txt,
                  "\nAnalysis performed on column \u2018logPK\u2019 ",
                  "(data already log-transformed)")
  }
  txt <- paste0(txt, "\nSequences (design) : ", type)
  if (nchar(type) == 19) txt <- paste(txt, "(4-period 4-sequence full replicate)")
  if (nchar(type) == 9) txt <- paste(txt, "(4-period full replicate)")
  if (type %in% c("TRT|RTR", "TRR|RTT")) txt <- paste(txt, "(3-period full replicate)")
  if (type == "TR|RT|TT|RR") txt <- paste(txt, "(Balaam\u2019s 2-period 4-sequence replicate)")
  if (type == "TRR|RTR|RRT") txt <- paste(txt, "(partial replicate)")
  if (type == "TRR|RTR") txt <- paste(txt, "(partial replicate; extra-reference)")
  x <- paste0(Nsub.seq, collapse = "|")
  if (sum(Miss.seq) > 0) x <- c(x, paste0(Miss.seq, collapse = "|"))
  if (sum(Miss.per) > 0) x <- c(x, paste0(Miss.per, collapse = "|"))
  x.len <- nchar(x)
  x.max <- max(x.len)
  txt <- paste0(txt, "\nSubjects / sequence: ", x[1])
  if (length(unique(Nsub.seq)) == 1) {
    txt <- paste0(txt, paste0(rep(" ", x.max-x.len[1]+1), collapse=""),
                  "(balanced)")
  } else {
    txt <- paste0(txt, paste0(rep(" ", x.max-x.len[1]+1), collapse=""),
                  "(unbalanced)")
  }
  if (sum(Miss.seq) > 0) {
    txt <- paste0(txt, "\nMissings / sequence: ", paste0(x[2],
                  paste0(rep(" ", x.max-x.len[2]+1), collapse=""),
                  "(incomplete)"))
  }
  if (sum(Miss.per) > 0) {
    txt <- paste0(txt, "\nMissings / period  : ", paste0(x[3],
                  paste0(rep(" ", x.max-x.len[3]+1), collapse=""),
                  "(incomplete)"))
  }
  txt <- paste0(txt,
                "\nSubjects (total)   : ", sprintf("%3i", n),
                "\nSubj\u2019s with T and R: ", sprintf("%3i", NTR),
                " (calculation of the CI)")
  if (NTR < 12) {
    txt <- paste0(txt, "\n                     ",
                  "Less than 12 as required acc. to the BE-GL.")
  }
  if (design == "full") {
    txt <- paste0(txt, "\nSubj\u2019s with two Ts : ", sprintf("%3i", nTT))
  }
  txt <- paste0(txt, "\nSubj\u2019s with two Rs : ", sprintf("%3i", nRR))
  if ((type == "TRT|RTR" | type == "TRR|RTT") & nRR < 12) {
    txt <- paste(txt, "(uncertain CVwR acc. to Q&A Rev. 12)")
  }
  ret <- list(data=data, ref=ref, RR=RR, test=test, type=type, n=n,
              nTT=ifelse(design == "full", nTT, NA), nRR=nRR,
              design=design, txt=txt, Sub.Seq=Nsub.seq,
              Miss.seq=Miss.seq, Miss.per=Miss.per, logtrans=transf,
              res.file=ifelse(print, res.file, NA),
              png.path=ifelse(plot.bxp, png.path, NA))
  return(ret)
} # end of function get.data()
