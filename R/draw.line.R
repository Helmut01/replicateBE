#########################
# function to show the  #
# BE-limits, CI, and PE #
#########################
draw.line <- function(called.from, L, U, lo, hi, PE, theta1, theta2) {
  # unicode symbols:
  # confidence interval:   filled black square
  #                        ]max.range[ left and/or right triangle
  # point estimate:        white rhombus
  # expanded limits:       double vertical line
  # BE-limits, GMR-restr.: single vertical line
  # spaghetti Viennese to catch all possible combinations
  # the 'resolution' is ca. 0.5%
  # the CI and PE have presedence over the limits
  if(missing(theta1)) theta1 <- 0.8
  if(missing(theta2)) theta2 <- 1/theta1
  s         <- c("\u256B", "\u255F", "\u2562",
                 "\u253C", "\u251C", "\u2524",
                 "\u25CA", "\u25A0", "\u25C4", "\u25BA",
                 "\u2500", "\u00A0")
  names(s) <- c("EX", "EX1", "EX2",
                "BE", "BE1", "BE2",
                "PE", "CI", "CL.lo", "CL.hi",
                "li", "sp")
  sf   <- 107.6     # scaling factor to get a 79 character string
  L.0  <- 0.6983678 # max. lower expansion
  U.0  <- 1.4319102 # max. upper expansion
  repl <- function(l, sf, L.0, loc, sym) {
    substr(l, sf*(loc-L.0)+1, sf*(loc-L.0)+1) <- sym
    return(l)
  }
  l <- paste0(rep(s["sp"], sf*(U.0-L.0)+1), collapse="")
  l <- repl(l, sf, L.0, 1, s["BE"])
  if (!called.from == "ABE") {
    ifelse (L == 0.8  & lo > 0.8, sym <- s["BE1"], sym <- s["BE"])
    l <- repl(l, sf, L.0, 0.80, sym)
    ifelse (U == 1.25 & hi < 1.25, sym <- s["BE2"], sym <- s["BE"])
    l <- repl(l, sf, L.0, 1.25, sym)
    if (L != 0.8) { # scaled
      ifelse (lo >= L, sym <- s["EX1"], sym <- s["EX"])
      l <- repl(l, sf, L.0, L, sym)
    } else {        # unscaled
      ifelse (lo >= 0.8, sym <- s["BE1"], sym <- s["BE"])
      l <- repl(l, sf, L.0, 0.8, sym)
    }
    if (U != 1.25) {
      ifelse (hi <= U, sym <- s["EX2"], sym <- s["EX"])
      l <- repl(l, sf, L.0, U, sym)
    } else {
      ifelse (hi >= 1.25, sym <- s["BE"], sym <- s["BE2"])
      l <- repl(l, sf, L.0, 1.25, sym)
    }
  } else {
    ifelse (lo < theta1, sym <- s["BE"], sym <- s["BE1"])
    l <- repl(l, sf, L.0, theta1, sym)
    ifelse (hi > theta2, sym <- s["BE"], sym <- s["BE2"])
    l <- repl(l, sf, L.0, theta2, sym)
  }
  l <- repl(l, sf, L.0, PE, s["PE"])
  ifelse (lo < L.0, l <- repl(l, sf, L.0, L.0, s["CL.lo"]),
          l <- repl(l, sf, L.0, lo, s["CI"]))
  ifelse (hi > U.0, l <- repl(l, sf, L.0, U.0, s["CL.hi"]),
          l <- repl(l, sf, L.0, hi, s["CI"]))
  last <- sf*(U.0-L.0)+1
  while (last <= sf*(U.0-L.0)+1) { # last non-space character
    last <- last - 1
    if (substr(l, last, last) != s["sp"]) break
  }
  if (substr(l, sf*(U.0-L.0)+1, sf*(U.0-L.0)+1) != s["sp"]) # special case
    last <- sf*(U.0-L.0)+1
  first <- 0
  while (first < last) {          # first non-space character
    first <- first + 1
    if (substr(l, first, first) != s["sp"]) break
  }
  while (first < last) { # replace space with line
    if (substr(l, first, first) == s["sp"])
      substr(l, first, first) <- s["li"]
    first <- first + 1
  }
  return(l) # TODO: trim trailing whitespace. How for unicode string?
}
