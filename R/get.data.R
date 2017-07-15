#################################
# Get the data from the file or #
# internal data and generate    #
# output common to all methods. #
#################################
get.data <- function(path.in = NULL, path.out = NULL, file, set = "",
                     ext, na = ".", sep = ",", dec = ".",
                     logtrans = TRUE, print, plot.bxp, data) {
  graphics.off()
  if (is.null(data)) { # checking external data
    ext.csv <- c("CSV", "csv")
    ext.xls <- c("XLS", "xls", "XLSX", "xlsx")
    if (missing(file))
      stop("Argument \'file\' must be given.")
    if (is.numeric(file))
      stop("Argument \'file\' must be a string (i.e., enclosed in single or double quotes).")
    if (is.numeric(set))
      stop("Argument \'set\' must be a string (i.e., enclosed in single or double quotes).")
    if (missing(ext))
      stop("Argument \'ext\' (extension of file) must be given.")
    if (!ext %in% c(ext.csv, ext.xls))
      stop("Data format not supported (must be Character Separated Variables or Excel.")
    if (ext %in% ext.csv) {
      if (!sep %in% c(";", ",", "\t"))
        stop(paste0("Reading CSV-file: Argument \'sep\' (variable separator) must be any of",
                    "\n\';\' (semicolon), \',\' (comma), or \'\\t\' (tab)."))
      if (!dec %in% c(".", ","))
        stop("Reading CSV-file: Argument \'dec\' (decimal separator) must be \'.\' (period) or \',\' (comma).")
      if (sep == dec)
        stop("Reading CSV-file: Arguments \'sep\' and decimal \'dec\' must be different.")
    }
    if (ext %in% ext.xls & (set == ""))
      stop("Reading Excel: Argument \'set\' (name of sheet) must be given.")
    if (is.null(path.in) | missing(path.in)) {
      setwd("~/")
      cat("No path for input given; home folder", getwd(), "used.\n")
      path.in <- getwd()
    }
    if (!dir.exists(path.in)) {
      setwd("~/")
      cat("Path for input does not exist; home folder", getwd(), "used.\n")
      path.in <- getwd()
    } # Adds trailing '/' to path if missing
    path.in <- ifelse(regmatches(path.in, regexpr(".$", path.in)) == "/",
                      path.in, paste0(path.in, "/"))
  } # EO checking external data
  if (print | plot.bxp) { # check only if necessary
    if (missing(path.out)) {
      setwd("~/")
      cat("No path for output given; output to", getwd(), "\n")
      path.out <- getwd()
    }
    if (is.null(path.out)) {
      setwd("~/")
      cat("No path for output given; output to", getwd(), "\n")
      path.out <- getwd()
    }
    if (!dir.exists(path.out)) {
      setwd("~/")
      cat("Path for output does not exist; output to", getwd(), "\n")
      path.out <- getwd()
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
    }  # The entire content
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
    # looking for a row with namesvector, summing all its members, if all are there, marking as TRUE
    Nnamesdf <- c(t(apply(datawithdescr, 1, function(row, table) {
      sum(match(tolower(row), table=table), na.rm=TRUE)}, table=namesvector)) == 10)
    if(sum(Nnamesdf) == 0)
      stop("Column names must be given as \'subject\', \'period\', \'sequence\', \'treatment\'.")
    if(sum(Nnamesdf) > 1)
      stop("More than 1 row with column names as \'subject\', \'period\', \'sequence\', \'treatment\' detected.")
    # selecting the rows before names of dataset
    descrdf <- datawithdescr[0:(which(Nnamesdf == TRUE)-1), ]
    # if there are some # comments in CSV-files, collapse them
    if(nrow(descrdf)) {
      descr <- unname(apply(descrdf, 1, function(row){paste0(row[!is.na(row)], collapse=" ")}))
      if (!ext %in% ext.xls) {
        descr <- paste0(descr[startsWith(descr, "#")], collapse="\n")
        descr <- gsub("# ", "", descr)
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
    PKcols <- which(names(data) %in% c("PK", "logPK")) # PK columns
    for (j in seq_along(PKcols)) {                    # transform to numeric
      data[, PKcols[j]] <- as.numeric(data[, PKcols[j]])
    }
    # data[, PKcols] <- lapply(data[, PKcols], as.numeric) # throws warnings
    # EO reading file
    if (print) res.file <- paste0(path.out, file, set, "_ABEL")
    if (plot.bxp) png.path <- paste0(path.out, file, set, "_boxplot.png")
    # If the user erroneously asks for analysis of logPK - which does not
    # exist in the data set - change to internal log-transformation.
    if (logtrans == FALSE & !"logPK" %in% colnames(data)) {
      warning(paste0("Column \'logPK\' does not exist in the data set.",
                     "\nLog-transformed column \'PK'\ internally."))
      logtrans <- TRUE
    } # factorize variables except response(s)
    cols       <- c("subject", "period", "sequence", "treatment")
    data[cols] <- lapply(data[cols], factor)
    if (sum(!unique(data$treatment) %in% c("R", "T")) !=0)
      stop("treatments must be given as \'R\' and \'T\'.")
  } else { # EO reading external data
    if (missing(ext)) { # get information of internal data set
      info  <- info.data(data)
      file  <- info$file
      set   <- info$set
      ref   <- info$ref
      descr <- info$descr
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
  if (nchar(type) == 11) { # 3-sequence partial replicate
    if (Npers != 3) stop("3 periods required in this partial replicate design.")
    if (Nseqs != 3) stop("3 sequences required in this partial replicate design.")
  }
  # next line introduced for DS24 where all data of subject 16 are NA
  # Given that: Do we need na.action=na.omit in lme() any more?
  data <- na.omit(data)
  Nsub.seq <- table(data$sequence[!duplicated(data$subject)])
  # adapt to reordered sequences
  Nsub.seq <- Nsub.seq[order(match(names(Nsub.seq), seqs))]
  ref   <- data[data$treatment == "R", ]
  test  <- data[data$treatment == "T", ]
  NTR   <- length(unique(test$subject) %in% unique(ref$subject)) #  >= 1 T & >= 1 R
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
  if (nchar(type) == 11) txt <- paste(txt, "(partial replicate)")
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
              Miss.seq=Miss.seq, Miss.per=Miss.per, transf=logtrans,
              res.file=ifelse(print, res.file, NA),
              png.path=ifelse(plot.bxp, png.path, NA))
  return(ret)
} # end of function get.data()
