\encoding{utf-8}
\name{rds05+rds11+rds16}
\alias{RTTR.TRRT}
\docType{data}
\title{
Reference Data Sets for RTTR|TRRT Replicate Designs
}

\description{
Data sets from the literature to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  \code{rds05} \tab 26 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  \code{rds11} \tab 37 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds16} \tab 38 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds05} \tab \ifelse{html}{\out{Metzler&nbsp;&amp;&nbsp;Shumaker}}{Metzler~&~Shumaker} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in the Appendix.\cr
  \code{rds11} \tab Hauschke \emph{et al.} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data given in Table 9.6.\cr
  \code{rds16} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Drug 14a.
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
str(rds05)
head(rds11, 8)
}

\keyword{datasets}
