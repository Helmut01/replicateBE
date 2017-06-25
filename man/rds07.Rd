\encoding{utf-8}
\name{rds07}
\alias{rds07}
\docType{data}
\title{
Reference Data Set 07 (RRT|RTR|TRR)
}

\description{
Data set simulated with \ifelse{html}{\out{C<sub>wT</sub>}}{\eqn{C\textsubscript}{wT}} = \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} = 35\%, GMR 0.90. 360 subjects.
}

\usage{rds07}
\format{
  A data frame with 1,080 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 360 levels: 1, 2, \ldots, 360}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 3 levels: RRT, RTR, TRR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Balanced (120 subjects in each of the three sequences) and complete. No outliers.
}

\author{
Detlew Labes
}

\seealso{
  {\link[replicateBE]{RRT.RTR.TRR}}
}

\examples{
str(rds07)
row <- c(1:3, 361:363, 721:723)
rds07[row, ]
}
\keyword{datasets}
