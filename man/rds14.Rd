\encoding{utf-8}
\name{rds14}
\alias{rds14}
\docType{data}
\title{
Reference Data Set 14 (RTRT|TRTR)
}

\description{
Data set simulated with high variability and dropouts as a hazard function growing with period. 77 subjects.
}

\usage{rds14}
\format{
  A data frame with 273 observations on the following 6 variables:
  \describe{
    \item{\code{subject}}{a factor with 77 levels: 1, 2, \ldots, 78}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTRT, TRTR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
    \item{\code{logPK}}{a numeric vector of the natural logarithms of {\code{PK}}}
  }
}

\details{
Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (17 missings in sequence RTRT and 18 in sequence TRTR). Missings / period: 0/1, 4/2, 12/3, 19/4. No outliers.
}

\author{
Michael Tomashevskiy
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
str(rds14)
head(rds14, 8)
}
\keyword{datasets}
