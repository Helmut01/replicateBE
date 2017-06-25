\encoding{utf-8}
\name{rds10}
\alias{rds10}
\docType{data}
\title{
Reference Data Set 10 (TRR|RRT)
}

\description{
Data set given in Table 9.3.3 of Chow & Liu. 18 subjects.
}

\usage{rds10}
\format{
  A data frame with 54 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 18 levels: 1, 2, \ldots, 18}
    \item{\code{period}}{a factor with 3 levels: 1, 2, 3}
    \item{\code{sequence}}{a factor with 2 levels: TRR, RTT}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses (here AUC)}
  }
}

\details{
Balanced (9 subjects in both sequences) and complete. No outliers.
}

\note{
In analogy to the \acronym{EMA}’s Q&A: Uncertain estimate of \code{CVwR} since less than 12 subjects in sequence TRR.
}

\source{
Chow SC, Liu JP. \emph{Design and Analysis of Bioavailability and Bioequivalence Studies.}\cr
Boca Raton: CRC Press; \ifelse{html}{\out{3<sup>rd</sup>}}{\eqn{3\textsuperscript}{rd}} edition 2009. p275.
}

\seealso{
  {\link[replicateBE]{TRR.RRT}}
}

\examples{
str(rds10)
row <- c(1:3, 28:30)
rds10[row, ]
}

\keyword{datasets}
