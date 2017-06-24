\encoding{utf-8}
\name{rds01}
\alias{rds01}
\docType{data}
\title{
Reference data set 01 (RTRT|TRTR)
}

\description{
Data set I of the EMA’s Q&A document. 77 subjects.
}

\usage{data("RTRT.TRTR"); rds01}
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
Unbalanced (38 subjects in sequence RTRT and 39 in TRTR) and incomplete (3 missings in sequence RTRT and 7 in sequence TRTR). Missings / period: 0/1, 1/2, 7/3, 2/4. Two extreme outliers (subjects 45 and 52) after the reference treatment in sequence RTRT.\cr\cr
In the source evaluated by \acronym{SAS} v9.1 for \acronym{ABEL}. Reported results:
  \tabular{ll}{
    \code{CVwR} \tab 47.0\%\cr
    \code{PE} \tab 115.66\% (Method A)\cr\tab 115.73\% (Method B)\cr
    \code{90\% CI} \tab 107.11\% – 124.89\% (Method A)\cr\tab 107.17\% – 124.97\% (Method B)
  }
}

\source{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\seealso{
  {\link[replicateBE]{RTRT.TRTR}}
}

\examples{
data("RTRT.TRTR")
str(rds01)
head(rds01, 8)
}
\keyword{datasets}
