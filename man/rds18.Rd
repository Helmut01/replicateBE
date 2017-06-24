\encoding{utf-8}
\name{rds18}
\alias{rds18}
\docType{data}
\title{
Reference data set 18 (RTRT|TRTR)
}

\description{
Data set based on \code{rds14}. Removed T data of subjects 63–78. 77 subjects.
}

\usage{data("RTRT.TRTR"); rds18}
\format{
  A data frame with 245 observations on the following 6 variables:
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
Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (31 missings in sequence RTRT and 32 in sequence TRTR). Missings / period: 8/1, 12/2, 18/3, 25/4. No outliers.
}

\author{
Michael Tomashevskiy, Helmut \enc{Schütz}{Schuetz}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds18)
head(rds18, 8)
}
\keyword{datasets}
