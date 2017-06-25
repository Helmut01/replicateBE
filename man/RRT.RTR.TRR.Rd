\encoding{utf-8}
\name{rds02+rds04+rds07}
\alias{RRT.RTR.TRR}
\docType{data}
\title{
Reference data sets for RRT|RTR|TRR (partial) replicate designs
}

\description{
Data sets from the literature or obtained by simulations to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  \code{rds02} \tab 24 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  \code{rds04} \tab 51 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds07} \tab 360 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds02} \tab EMA, CHMP \tab Data set II.\cr
  \code{rds04} \tab \ifelse{html}{\out{Patterson&nbsp;&amp;&nbsp;Jones}}{Patterson~&~Jones} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C\textsubscript}{max}} data of Table II.\cr
  \code{rds07} \tab R \tab Large simulated data set with homoscedasticity.
  }
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}

Patterson SD, Jones B. \emph{Viewpoint: observations on scaled average bioequivalence.}\cr
Pharm Stat. 2012;11(1):1--7. \doi{10.1002/pst.498}
}

\examples{
data("RRT.RTR.TRR")
str(rds02)
row <- c(1:3, 10:12, 16:18)
rds02[row, ]
}

\keyword{datasets}
