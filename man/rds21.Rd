\encoding{utf-8}
\name{rds21}
\alias{rds21}
\docType{data}
\title{
Reference Data Set 21 (RTRT|TRTR)
}

\description{
Based on \code{ds01}. 77 subjects.
}

\usage{rds21}
\format{
  A data frame with 298 observations (2 NA) on the following 6 variables:
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
Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (5 missings in sequence RTRT and 7 in sequence TRTR). Missings / period: 1/1, 1/2, 8/3, 2/4. One extreme result of subjects 45 & 52 set to NA. No outliers.
}

\source{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
str(rds21)
row <- c(172:175, 200:203)
rds21[row, ]
}
\keyword{datasets}
