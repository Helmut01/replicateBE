\encoding{utf-8}
\name{rds22}
\alias{rds22}
\docType{data}
\title{
Reference Data Set 22 (RTR|TRR)
}

\description{
Data set simulated with \ifelse{html}{\out{C<sub>wT</sub>}}{\eqn{C\textsubscript}{wT}} = \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} = 40\%, GMR 0.90. 36 subjects.
}

\usage{rds21}
\format{
  A data frame with 108 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 36 levels: 1, 2, \ldots, 36}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 2 levels: RTR, TRR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Balanced (18 subjects in each of the sequences) and complete (no missing data). No outliers.
}

\note{
This partial replicate design is also known as the \sQuote{extra-reference design}. Since T is not administered in all periods, lacking period effects must be assumed. In the presence of a \emph{true} period effect the treatment comparison will be biased. Hence, this design is not recommended.
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\references{
Chow SC, Shao J, Wang H. \emph{Individual bioequivalence testing under 2×3 designs.}\cr
Stat Med. 2002;21:629--48. \doi{10.1002/sim.1056}
}

\seealso{
  {\link[replicateBE]{RTR.TRR}}
}

\examples{
str(rds22)
rds22[52:57, ]
}

\keyword{datasets}
