#########################################
# Sequences of known and tested designs #
# according to the preferred order.     #
# Sort sequences of unknown design (T   #
# first) and throw a message that the   #
# design is untested.                   #
#########################################
info.design <- function(seqs = NA) {
  sequences <- length(seqs)
  if (sequences < 2)
    stop("At least 2 sequences required.")
  if (!is.character(seqs))
    stop("Sequences must be given as strings, not numbers.")
  if (sequences != length(unique(seqs)))
    stop(paste("The", sequences,"sequences must be unique."))
  periods <- unique(nchar(seqs))
  if (periods < 2)
    stop("Not a crossover design.")
  if (periods == 2 & sequences == 2) {
    stop("Not a replicate design.")
  }
  if (length(periods) > 1)
    stop("Each sequence must have the same number of periods.")
  reordered <- NA

  # 4-period 4-sequence full replicates
  if (periods == 4 & sequences == 4 & is.na(reordered[1])) {
    if (sum(seqs %in% c("RTRT", "RTTR", "TRRT", "TRTR")) == 4) {
      reordered <- seqs[order(match(seqs, c("TRTR", "RTRT", "TRRT", "RTTR")))]
    }
    if (sum(seqs %in% c("RRTT", "RTTR", "TRRT", "TTRR")) == 4 & is.na(reordered[1])) {
      reordered <- seqs[order(match(seqs, c("TRRT", "RTTR", "TTRR", "RRTT")))]
    }
    if (is.na(reordered[1])) {
      message("Untested design.")
      reordered <- rev(seqs) # at least T first
    }
    design <- "full"
  }

  # 4-period 2-sequence full replicates
  if (periods == 4 & sequences == 2 & is.na(reordered[1])) {
    if (sum(seqs %in% c("RTRT", "TRTR")) == 2 & is.na(reordered[1])) {
      reordered <- seqs[order(match(seqs, c("TRTR", "RTRT")))]
    }
    if (sum(seqs %in% c("RTTR", "TRRT")) == 2 & is.na(reordered[1])) {
      reordered <- seqs[order(match(seqs, c("TRRT", "RTTR")))]
    }
    if (sum(seqs %in% c("TTRR", "RRTT")) == 2 & is.na(reordered[1])) {
      reordered <- seqs[order(match(seqs, c("TTRR", "RRTT")))]
    }
    if (is.na(reordered[1])) {
      message("Untested design.")
      reordered <- rev(seqs) # at least T first
    }
    design <- "full"
  }

  # 3-period 2-sequence full replicates or partial replicate
  if (periods == 3 & sequences == 2 & is.na(reordered[1])) {
    if (sum(seqs %in% c("RTR", "TRT")) == 2 & is.na(reordered[1])) { # full
      reordered <- seqs[order(match(seqs, c("TRT", "RTR")))]
      design <- "full"
    }
    if (sum(seqs %in% c("RTT", "TRR")) == 2 & is.na(reordered[1])) { # full
      reordered <- seqs[order(match(seqs, c("TRR", "RTT")))]
      design <- "full"
    }
    if (sum(seqs %in% c("RTR", "TRR")) == 2 & is.na(reordered[1])) { # extra-reference
      reordered <- seqs[order(match(seqs, c("TRR", "RTR")))]
      design <- "partial"
    }
    if (is.na(reordered[1])) {
      message("Untested design.")
      reordered <- rev(seqs) # at least T first
      design <- "partial" # likely...
    }
  }

  # 3-period 3-sequence partial replicate
  if (periods == 3 & sequences == 3 & is.na(reordered[1])) {
    if (sum(seqs %in% c("RRT", "RTR", "TRR")) == 3 & is.na(reordered[1])) {
      reordered <- seqs[order(match(seqs, c("TRR", "RTR", "RRT")))]
    }
    if (is.na(reordered[1])) {
      message("Untested design.")
      reordered <- rev(seqs) # at least T first
    }
    design <- "partial"
  }

  # 2-period 4-sequence (Balaam's)
  if (periods == 2 & sequences == 4 & is.na(reordered[1])) {
    if (sum(seqs %in% c("RR", "RT", "TR", "TT")) == 4 & is.na(reordered[1])) {
      reordered <- seqs[order(match(seqs, c("TR", "RT", "TT", "RR")))]
    }
    if (is.na(reordered[1])) {
      message("Untested design.")
      reordered <- rev(seqs) # at least T first
    }
    design <- "full"
  }
  design <- list(reordered, paste0(reordered, collapse="|"),
                 sequences, periods, design)
  names(design) <- c("reordered", "type", "sequences", "periods", "design")
  return(design)
}
