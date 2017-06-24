\encoding{utf-8}
\name{rds02}
\alias{rds02}
\docType{data}
\title{
Reference data set 02 (RRT|RTR|TRR)
}

\description{
Data set II of the EMA’s Q&A document. 24 subjects.
}

\usage{data("RRT.RTR.TRR"); rds02}
\format{
  A data frame with 72 observations on the following 6 variables:
  \describe{
    \item{\code{subject}}{a factor with 24 levels: 1, 2, \ldots, 24}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 3 levels: RRT, RTR, TRR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
    \item{\code{logPK}}{a numeric vector of the natural logarithms of {\code{PK}}}
  }
}

\details{
Balanced (8 subjects in each of the three sequences) and complete (no missing data). No outliers.\cr\cr
In the source evaluated by \acronym{SAS} v9.1 for \acronym{ABEL}. Reported results:
  \tabular{ll}{
    \code{CVwR} \tab 11.2\%\cr
    \code{PE} \tab 102.26\% (Method A)\cr\tab 102.26\% (Method B)\cr
    \code{90\% CI} \tab 97.32\% – 107.46\% (Method A)\cr\tab 97.32\% – 107.46\% (Method B)
  }
}

\source{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\seealso{
  {\link[replicateBE]{RRT.RTR.TRR}}
}

\examples{
data("RRT.RTR.TRR")
str(rds02)
row <- c(1:3, 10:12, 16:18)
rds02[row, ]
}

\keyword{datasets}
