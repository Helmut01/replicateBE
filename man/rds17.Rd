\encoding{utf-8}
\name{rds17}
\alias{rds17}
\docType{data}
\title{
Reference Data Set 17 (RTR|TRT)
}

\description{
Based on \code{ds03}. 19 subjects.
}

\usage{rds17}
\format{
  A data frame with 56 observations on the following 6 variables:
  \describe{
    \item{\code{subject}}{a factor with 19 levels: 1, 2, \ldots, 22}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 2 levels: RTR, TRT}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses acceptable for reference-scaling (generally \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
    \item{\code{logPK}}{a numeric vector of the natural logarithms of {\code{PK}}}
  }
}

\details{
Unbalanced (12 subjects in sequence RTR and 7 in TRT) and incomplete (1 missing in sequence TRT). Missings / period: 0/1, 0/2, 1/3. No outliers.
}

\note{
Acceptable according to the \acronym{EMA}’s Q&A since 12 subjects in sequence RTR.
}

\references{
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
str(rds17)
head(rds17, 6)
}

\keyword{datasets}
