\encoding{utf-8}
\name{rds09}
\alias{rds09}
\docType{data}
\title{
Reference data set 09 (RTRT|TRTR)
}

\description{
Based on \code{rds08}. Wide numeric range (data of last 37 subjects multiplied by 1,000,000). 222 subjects.
}

\usage{data("RTRT.TRTR"); rds09}
\format{
  A data frame with 888 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 222 levels: 1, 2, \ldots, 222}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTRT, TRTR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Balanced (222 subjects in both sequences) and complete. No outliers.
}

\author{
Detlew Labes, Helmut \enc{Schütz}{Schuetz}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds09)
row <- c(737:744)
rds09[row, ]
}
\keyword{datasets}
