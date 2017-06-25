\encoding{utf-8}
\name{rds04}
\alias{rds04}
\docType{data}
\title{
Reference Data Set 04 (RRT|RTR|TRR)
}

\description{
Data set of Table II given by Patterson & Jones. 51 subjects.
}

\usage{rds04}
\format{
  A data frame with 153 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 51 levels: 1, 2, \ldots, 56}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 3 levels: RRT, RTR, TRR}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Balanced (17 subjects in each of the three sequences) and complete. No outliers.\cr\cr
In the source evaluated by SAS 9.1 for the FDA’s mixed effects model (\sQuote{Method C}; \strong{not} acceptable for the \acronym{EMA}). Reported results:
  \tabular{ll}{
    \code{CVwR} \tab 61\%\cr
    \code{PE} \tab 137\%\cr
    \code{90\% CI} \tab 119\% – 159\%
  }
}

\source{
Patterson SD, Jones B. \emph{Viewpoint: observations on scaled average bioequivalence}\cr
Pharm Stat. 2012;11(1):1--7. \doi{10.1002/pst.498}
}

\seealso{
  {\link[replicateBE]{RRT.RTR.TRR}}
}

\examples{
str(rds04)
row <- c(1:3, 52:54, 103:105)
rds04[row, ]
}

\keyword{datasets}
