\encoding{utf-8}
\name{rds05}
\alias{rds05}
\docType{data}
\title{
Reference data set 05 (RTTR|TRRT)
}

\description{
Data set given in the Appendix of Metzler & Shumaker. 26 subjects.
}

\usage{data("RTTR.TRRT"); rds05}
\format{
  A data frame with 104 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 26 levels: 1, 2, \ldots, 26}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTTR, TRRT}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Balanced (13 subjects in both sequences) and complete. No outliers.\cr\cr
In the source evaluated by SAS for the FDA’s mixed effects model (\sQuote{Method C}; \strong{not} acceptable for the \acronym{EMA}). Reported results:
  \tabular{ll}{
    \code{CVwR} \tab 5.47\%\cr
    \code{CVwT} \tab 6.75\%\cr
    \code{PE} \tab 107.90\%\cr
    \code{90\% CI} \tab 103.66\% – 112.2\%
  }
}

\source{
Metzler CM, Shumaker RC. \emph{The Phenytoin Trial is a Case Study of \sQuote{Individual} Bioequivalence}\cr
Drug Inf J. 1998;32:1063--72.
}

\seealso{
  {\link[replicateBE]{RTTR.TRRT}}
}

\examples{
data("RTTR.TRRT")
str(rds05)
row <- c(1:4, 9:12)
rds05[row, ]
}

\keyword{datasets}
