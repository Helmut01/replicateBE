\encoding{utf-8}
\name{RTRT.TRTR.RTTR.TRRT}
\alias{RTRT.TRTR.RTTR.TRRT}
\alias{rds23}
\docType{data}
\title{
Reference Data Set for RTRT|TRTR|RTTR|TRRT Designs
}

\description{
Data set from the literature to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\details{
  \tabular{crcl}{
    \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
    \code{rds23} \tab 22 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\format{
  \itemize{
    \item Reference Data Set 23 (\code{rds23})\cr
    22 subjects.
    \describe{
      A data frame with 88 observations on the following 5 variables:
      \tabular{ll}{
        \code{subject} \tab a factor with 22 levels: 1, 2, \ldots, 27\cr
        \code{period} \tab a factor with 4 levels: 1, 2, 3, 4\cr
        \code{sequence} \tab a factor with 4 levels: RTRT, RTTR, TRRT, TRTR\cr
        \code{treatment} \tab a factor with 2 levels: R, T\cr
        \code{PK} \tab a numeric vector of pharmacokinetic responses acceptable for reference-scaling (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})
        }
      Unbalanced (4 subjects in sequence RTRT and 6 in each of the other three) and complete. One extreme outlier (subject 17) after the reference treatment in sequence TRTR.
    }
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds23} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 7.
  }
}

\references{
\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip}{bioequivalence study files}
}

\examples{
str(rds23)
row <- c(5:8, 25:28, 1:4, 9:12)
rds23[row, ]
}

\keyword{datasets}
