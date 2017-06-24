\encoding{utf-8}
\name{rds08}
\alias{rds08}
\docType{data}
\title{
Reference data set 08 (RTRT|TRTR)
}

\description{
Data set simulated with \ifelse{html}{\out{C<sub>wT</sub>}}{\eqn{C\textsubscript}{wT}} = 70\%, \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} = 80\%, \ifelse{html}{\out{C<sub>bT</sub>}}{\eqn{C\textsubscript}{bT}} = \ifelse{html}{\out{C<sub>bT</sub>}}{\eqn{C\textsubscript}{bT}} = 150\%, GMR = 0.85. 222 subjects.
}

\usage{data("RTRT.TRTR"); rds08}
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
Balanced (222 subjects in both sequences) and complete. No outliers.\cr
The extreme sample size results from high variability, an assumed true GMR 0.85, and target power 90\%.
}

\author{
Detlew Labes
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds08)
row <- c(1:4, 445:448)
rds08[row, ]
}
\keyword{datasets}
