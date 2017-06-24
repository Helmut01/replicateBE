\encoding{utf-8}
\name{rds12}
\alias{rds12}
\docType{data}
\title{
Reference data set 12 (RTRT|TRTR)
}

\description{
Data set simulated with extreme intrasubject variability. 77 subjects.
}

\usage{data("RTRT.TRTR"); rds12}
\format{
  A data frame with 298 observations on the following 6 variables:
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
Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (3 missings in sequence RTRT and 7 in sequence TRTR). Missings / period: 0/1, 1/2, 7/3, 2/4. No outliers.
}

\author{
Michael Tomashevskiy
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds12)
head(rds12, 8)
}
\keyword{datasets}
