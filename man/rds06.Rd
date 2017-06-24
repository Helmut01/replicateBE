\encoding{utf-8}
\name{rds06}
\alias{rds06}
\docType{data}
\title{
Reference data set 06 (RTRT|TRTR)
}

\description{
Based on \code{rds01}. 77 subjects. Responses of T and R switched.
}

\usage{data("RTRT.TRTR"); rds06}
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
Unbalanced (39 subjects in sequence RTRT and 38 in TRTR) and incomplete (7 missings in sequence RTRT and 3 in sequence TRTR). Missings / period: 0/1, 1/2, 7/3, 2/4. No outliers.
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
data(RTRT.TRTR)
str(rds06)
head(rds06, 8)
}
\keyword{datasets}
