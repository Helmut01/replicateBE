\encoding{utf-8}
\name{rds11}
\alias{rds11}
\docType{data}
\title{
Reference Data Set 11 (RTTR|TRRT)
}

\description{
Data set given in Table 9.6 of Hauschke \emph{et al.} 37 subjects.
}

\usage{rds11}
\format{
  A data frame with 148 observations on the following 5 variables:
  \describe{
    \item{\code{subject}}{a factor with 37 levels: 1, 2, \ldots, 37}
    \item{\code{period}}{a factor with 4 levels: 1, 2, 3, 4}
    \item{\code{sequence}}{a factor with 2 levels: RTTR, TRRT}
    \item{\code{treatment}}{a factor with 2 levels: R, T}
    \item{\code{PK}}{a numeric vector of pharmacokinetic responses (here \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}})}
  }
}

\details{
Unbalanced (19 subjects in sequence RTTR and 18 in TRRT) and complete. No outliers.\cr\cr
In the source evaluated by the FDA's mixed effects model (\sQuote{Method C}; \strong{not} acceptable for the \acronym{EMA}). Reported results:
  \tabular{ll}{
    \code{PE} \tab 90.0\%\cr
    \code{90\% CI} \tab 79.6\% – 101.7\%
  }
}

\source{
Hauschke D, Steinijans VW, Pigeot I. \emph{Bioequivalence Studies in Drug Development.}\cr
Chichester: John Wiley; 2007. p216.

\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip}{bioequivalence study files}. Antihypertensive - Patch. DRUG #17(a): CMAX.
}

\seealso{
  {\link[replicateBE]{RTTR.TRRT}}
}

\examples{
str(rds11)
head(rds11, 8)
}

\keyword{datasets}
