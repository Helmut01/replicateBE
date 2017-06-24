\encoding{utf-8}
\name{rds13}
\alias{rds13}
\docType{data}
\title{
Reference data set 13 (RTRT|TRTR)
}

\description{
Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data deleted). 222 subjects.
}

\usage{data("RTRT.TRTR"); rds13}
\format{
  A data frame with 776 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 222 levels: 1, 2, \ldots, 222}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTRT, TRTR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Balanced (111 subjects in both sequences) and incomplete (56 missings in both sequences). Missings / period: 0/1, 0/2, 0/3, 112/4. No outliers.
}

\author{
Detlew Labes, Helmut \enc{Schütz}{Schuetz}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds13)
row <- c(605:612)
rds13[row, ]
}
\keyword{datasets}
