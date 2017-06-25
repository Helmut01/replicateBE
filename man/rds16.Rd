\encoding{utf-8}
\name{rds16}
\alias{rds16}
\docType{data}
\title{
Reference Data Set 16 (RTTR|TRRT)
}

\description{
Data set given by the \acronym{FDA}. 38 subjects.
}

\usage{rds16}
\format{
  A data frame with 152 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 38 levels: 1, 2, \ldots, 38}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTTR, TRRT}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Unbalanced (20 subjects in sequence RTTR and 18 in TRRT) and complete. No outliers.
}

\source{
\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip}{bioequivalence study files}. MAO inhibitor - IR. Drug 14a: CMAX.
}

\seealso{
  {\link[replicateBE]{RTTR.TRRT}}
}

\examples{
str(rds16)
row <- c(1:4, 9:12)
rds16[row, ]
}

\keyword{datasets}
