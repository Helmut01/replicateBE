#########################
# function to show the  #
# BE-limits, CI, and PE #
#########################
draw.line <- function(called.from, L, U, lo, hi, PE, theta1, theta2) {
  # unicode symbols:
  # confidence interval:   filled black square
  #                        ]max.range[: left and/or right triangle
  # point estimate:        white rhombus
  # expanded limits:       double vertical line
  # BE-limits, GMR-restr.: single vertical line
  # spaghetti Viennese to catch all possible combinations
  # the 'resolution' is ca. 0.5%
  # the CI and PE have presedence over the limits, i.e.
  # they are alway shown
  s         <- c("\u256B", "\u255F", "\u2562",
                 "\u253C", "\u251C", "\u2524",
                 "\u25CA", "\u25A0", "\u25C4", "\u25BA",
                 "\u2500", "\u00A0")
  names(s) <- c("EX", "EX1", "EX2",
                "BE", "BE1", "BE2",
                "PE", "CI", "CL.lo", "CL.hi",
                "li", "sp")
  L.0 <- 0.6983678 # max. lower expansion
  U.0 <- 1.4319102 # max. upper expansion
  if(missing(theta1)) theta1 <- 0.8
  if(missing(theta2)) theta2 <- 1/theta1
  f   <- 104       # scaling factor to get a 78 character string
  # the empty string
  l   <- paste0(rep(s["sp"], f*(U.0-L.0)+2), collapse="")
  # lower CL
  if (lo < L.0) { # below drawing range: left triangle
    substr(l, 1, 1) <- s["CL.lo"]
  } else {
    substr(l, f*(lo-L.0)+2, f*(lo-L.0)+2) <- s["CI"]
  }
  # point estimate
  substr(l, f*(PE-L.0)+2, f*(PE-L.0)+2) <- s["PE"]
  # 100% mark
  if (substr(l, f*(1-L.0)+2,  f*(1-L.0)+2) == s["sp"]) {
    # only if not already 'occupied'
  substr(l, f*(1-L.0)+2, f*(1-L.0)+2) <- s["BE"]
  }
  # upper CL
  if (hi > U.0) { # above drawing range: right triangle
    substr(l, f*(U.0-L.0)+2, f*(U.0-L.0)+2) <- s["CL.hi"]
  } else {
    substr(l, f*(hi-L.0)+2, f*(hi-L.0)+2) <- s["CI"]
  }
  if (!called.from == "ABE") {
    # lower limit
    if (substr(l, f*(L-L.0)+2, f*(L-L.0)+2) == s["sp"] & L != 0.8) {
      # only if not already 'occupied'
      if (lo > L) { # there will be a line to the left
        substr(l, f*(L-L.0)+2, f*(L-L.0)+2) <- s["EX1"]
      } else {      # no line to the left
        substr(l, f*(L-L.0)+2, f*(L-L.0)+2) <- s["EX"]
      }
    } else { # unscaled
      if (lo < 0.8) { # there will be a line to the left
        substr(l, f*(0.8-L.0)+2, f*(theta1-L.0)+2) <- s["BE"]
      } else {        # no line to the left
        substr(l, f*(0.8-L.0)+2, f*(theta1-L.0)+2) <- s["BE1"]
      }
    }
    # upper limit
    if (substr(l, f*(U-L.0)+2,  f*(U-L.0)+2) == s["sp"] & U != 1.25) {
      # only if not already 'occupied'
      if (hi < U) { # there will be a line to the right
        substr(l, f*(U-L.0)+2, f*(U-L.0)+2) <- s["EX2"]
      } else {      # no line to the right
        substr(l, f*(U-L.0)+2, f*(U-L.0)+2) <- s["EX"]
      }
    } else { # unscaled
      if (hi > 1.25) { # there will be a line to the right
        substr(l, f*(1.25-L.0)+2, f*(1.25-L.0)+2) <- s["BE"]
      } else {        # no line to the right
        substr(l, f*(1.25-L.0)+2, f*(1.25-L.0)+2) <- s["BE2"]
      }
    }
    if (substr(l, f*(0.8-L.0)+2,  f*(0.8-L.0)+2) == s["sp"]) {
      # only if not already 'occupied'
      if (L == 0.8 & lo > 0.8) {   # lower GMR restriction
        substr(l, f*(0.8-L.0)+2, f*(0.8-L.0)+2) <- s["BE1"]
      } else {                     # lower expanded limit
        substr(l, f*(0.8-L.0)+2, f*(0.8-L.0)+2) <- s["BE"]
      }
    }
    if (substr(l, f*(1.25-L.0)+2, f*(1.25-L.0)+2) == s["sp"]) {
      # only if not already 'occupied'
      if (U == 1.25 & hi < 1.25) { # upper GMR restriction
        substr(l, f*(1.25-L.0)+2, f*(1.25-L.0)+2) <- s["BE2"]
        if (is.na(last)) last <- f*(0.8-L.0)+2 # for ABE
      } else {                     # upper expanded limit
        substr(l, f*(1.25-L.0)+2, f*(1.25-L.0)+2) <- s["BE"]
      }
    }
  } else { # ABE
    # lower limit
    if (substr(l, f*(theta1-L.0)+2,  f*(theta1-L.0)+2) == s["sp"]) {
      # only if not already 'occupied'
      if (lo < theta1) { # there will be a line to the left
        substr(l, f*(theta1-L.0)+2, f*(theta1-L.0)+2) <- s["BE"]
      } else {        # no line to the left
        substr(l, f*(theta1-L.0)+2, f*(theta1-L.0)+2) <- s["BE1"]
      }
    }
    # upper limit
    if (substr(l, f*(theta2-L.0)+2, f*(theta2-L.0)+2) == s["sp"]) {
      # only if not already 'occupied'
      if (hi > theta2) { # there will be a line to the right
        substr(l, f*(theta2-L.0)+2, f*(theta2-L.0)+2) <- s["BE"]
      } else {        # no line to the right
        substr(l, f*(theta2-L.0)+2, f*(theta2-L.0)+2) <- s["BE2"]
      }
    }
  }
  last <- f*(U.0-L.0)+2
  while (last <= f*(U.0-L.0)+2) { # last non-space character
    last <- last - 1
    if (substr(l, last, last) != s["sp"]) break
  }
  if (substr(l, f*(U.0-L.0)+2, f*(U.0-L.0)+2) != s["sp"]) # special case
    last <- f*(U.0-L.0)+2
  first <- 0
  while (first < last) {          # first non-space character
    first <- first + 1
    if (substr(l, first, first) != s["sp"]) break
  }
  while (first < last) {          # fill to the right with a line
    if (substr(l, first, first) == s["sp"])
      substr(l, first, first) <- s["li"]
    first <- first + 1
  }
  # TODO: trim trailing whitespace. How for unicode string?
  return(l)
}
