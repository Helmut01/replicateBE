\encoding{utf-8}
\name{rds01+rds06+rds08+rds09+rds12+rds13+rds14+rds15+rds18+rds19+rds20+rds21}
\alias{RTRT.TRTR}
\docType{data}
\title{
Reference data sets for RTRT|TRTR replicate designs
}

\description{
Data sets from the literature, edited, or obtained by simulations to be evaluated by \code{method.A()} and/or \code{method.B()}.
}

\details{
   \tabular{crcl}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab N \tab \ifelse{html}{\out{C<sub>wR</sub>}}{\eqn{C\textsubscript}{wR}} (\%) \tab Evaluation\cr
  \code{rds01} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds06} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds08} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds09} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds12} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds13} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds14} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds15} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds18} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds19} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds20} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  \code{rds21} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
}

\source{
  \tabular{cll}{
  \ifelse{html}{\out{Data&nbsp;set}}{Data~set} \tab Origin \tab Description\cr
  \code{rds01} \tab EMA, CHMP \tab Data set I.\cr
  \code{rds06} \tab \code{rds01} edited \tab T and R switched.\cr
  \code{rds08} \tab R \tab Large simulated data set with slight heteroscedasticity.\cr
  \code{rds09} \tab \code{rds08} \tab Wide numeric range (data of last 37 subjects multiplied by 1,000,000).\cr
  \code{rds12} \tab Phoenix NLME \tab Simulated data set with extreme intrasubject variability.\cr
  \code{rds13} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data deleted).\cr
  \code{rds14} \tab Phoenix NLME \tab Simulated data set with high intrasubject variability and number of dropouts increasing with period.\cr
  \code{rds15} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}).\cr
  \code{rds18} \tab \code{rds14} edited \tab Removed T data of subjects 63–78.\cr
  \code{rds19} \tab \code{rds18} edited \tab Removed data of subjects 63–78.\cr
  \code{rds20} \tab \code{rds19} edited \tab Outlier of R (subject 1) introduced: original value ×100.\cr
  \code{rds21} \tab \code{rds01} edited \tab One extreme result of subjects 45 & 52 set to NA.
  }
}

\references{
European Medicines Agency, Committee for Medicinal Products for Human Use. \emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf}{EMA/618604/2008 Rev. 13}
}

\examples{
data("RTRT.TRTR")
str(rds01)
}

\keyword{datasets}
