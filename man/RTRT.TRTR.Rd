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
  \tabular{lll}{
  \code{rds01} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds06} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds08} \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds09} \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds12} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds13} \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds14} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds15} \tab N=222 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds18} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds19} \tab N=61 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds20} \tab N=61 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}\cr
  \code{rds21} \tab N=77 \tab \code{CVwR} >30\%: Evaluate by \code{method.A()} or \code{method.B()}
  }
}

\source{
  \tabular{ll}{
  \code{rds01} \tab EMA, CHMP. 2015. Data set I.\cr
  \code{rds06} \tab Based on \code{rds01} (T and R switched).\cr
  \code{rds08} \tab Large simulated data set with slight heteroscedasticity.\cr
  \code{rds09} \tab Based on \code{rds08}. Wide numeric range (data of last 37 subjects multiplied by 1,000,000).\cr
  \code{rds12} \tab Simulated data set with extreme intrasubject variability.\cr
  \code{rds13} \tab Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data deleted).\cr
  \code{rds14} \tab Simulated data set with high intrasubject variability and number of dropouts increasing with period.\cr
  \code{rds15} \tab Based on \code{rds08}. Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'}).\cr
  \code{rds18} \tab Based on \code{rds14}. Removed T data of subjects 63–78.\cr
  \code{rds19} \tab Based on \code{rds18}. Removed data of subjects 63–78.\cr
  \code{rds20} \tab Based on \code{rds19}. Outlier of R (subject 1) introduced: original value ×100.\cr
  \code{rds21} \tab Based on \code{rds01}. One extreme result of subjects 45 & 52 set to NA.
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
