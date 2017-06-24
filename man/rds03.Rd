\encoding{utf-8}
\name{rds03}
\alias{rds03}
\docType{data}
\title{
Reference data set 03 (RTR|TRT)
}

\description{
Based on \code{ds01}. Removed all data of period 4. 77 subjects.
}

\usage{data("RTR.TRT"); rds03}
\format{
  A data frame with 223 observations on the following 6 variables:
  \describe{
    \item{\code{subject}}{a factor with 77 levels: 1, 2, \ldots, 78}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 2 levels: RTR, TRT}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
    \item{\code{logPK}}{a numeric vector of the natural logarithms of {\code{PK}}}
  }
}

\details{
Unbalanced (38 subjects in sequence RTR and 39 in TRT) and incomplete (2 missings in sequence RTR and 6 in sequence TRT). Missings / period: 0/1, 1/2, 7/3. Two extreme outliers (subjects 45 and 52) after the reference treatment in sequence RTR.
}

\source{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\author{
Helmut \enc{Schütz}{Schuetz}
}

\seealso{
  {\link[replicateBE]{RTR.TRT}}
}

\examples{
data("RTR.TRT")
str(rds03)
head(rds03, 6)
}

\keyword{datasets}
