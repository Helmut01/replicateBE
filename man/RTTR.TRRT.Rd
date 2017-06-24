\encoding{utf-8}
\name{rds05+rds11+rds16}
\alias{RTTR.TRRT}
\docType{data}
\title{
Reference data sets for RTTR|TRRT replicate designs
}

\description{
Data sets from the literature to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{lll}{
  \code{rds05} \tab N=26 \tab \code{CVwR} <30\%: Evaluate by \code{method.A()}, \code{method.B()}, or \code{ABE()}\cr
  \code{rds11} \tab N=37 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or\code{method.B()}\cr
  \code{rds16} \tab N=38 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}
  }
}

\source{
  \tabular{ll}{
  \code{rds05} \tab Metzler & Shumaker. 1998. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in the Appendix.\cr
  \code{rds11} \tab Hauschke \emph{et al.} 2007. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in Table 9.6.\cr
  \code{rds16} \tab FDA. 1997. \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 14a.
  }
}

\references{
Metzler CM, Shumaker RC. \emph{The Phenytoin Trial is a Case Study of \sQuote{Individual} Bioequivalence.}\cr
Drug Inf J. 1998;32:1063--72

Hauschke D, Steinijans VW, Pigeot I. \emph{Bioequivalence Studies in Drug Development.}\cr
Chichester: John Wiley; 2007. p216.

\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research. \emph{Bioequivalence Studies.}\cr
Rockville, 1997. \href{https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip}{bioequivalence study files}
}

\examples{
data("RTTR.TRRT")
str(rds05)
head(rds11, 8)
}

\keyword{datasets}
