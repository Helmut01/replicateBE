\encoding{utf-8}
\name{rds20}
\alias{rds20}
\docType{data}
\title{
Reference data set 20 (RTRT|TRTR)
}

\description{
Data set based on \code{rds19}. Removed data of subjects 63–78. 61 subjects.
}

\usage{data("RTRT.TRTR"); rds20}
\format{
  A data frame with 216 observations on the following 6 variables:
  \describe{
    \item{\code{subject}}{a factor with 61 levels: 1, 2, \ldots, 62}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTRT, TRTR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
    \item{\code{logPK}}{a numeric vector of the natural logarithms of {\code{PK}}}
  }
}

\details{
Unbalanced (30 subjects in sequence RTRT and 31 in TRTR) and incomplete (14 missings in both sequences). Missings / period: 0/1, 4/2, 9/3, 15/4. No outliers.
}

\author{
Michael Tomashevskiy, Helmut \enc{Schütz}{Schuetz}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds20)
head(rds20, 8)
}
\keyword{datasets}
