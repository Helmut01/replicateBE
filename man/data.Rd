\encoding{utf-8}
\name{refdata}
\alias{data}
\docType{data}

\title{
Reference Datasets
}

\description{
Datasets of replicate designs from the public domain, edited, or obtained by simulations to be evaluated by \code{method.A()}, \code{method.B()}, or \code{ABE()}.
}

\details{
  \tabular{llcrcl}{
  Design \tab Specification \tab Dataset \tab N \tab \ifelse{html}{\out{CV<sub>wR</sub>}}{\eqn{CV_{wR}}} (\%) \tab Evaluation\cr
  TRTR|RTRT \tab full \tab \code{rds01} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds06} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds12} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds14} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds18} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds21} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds19} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds20} \tab 61 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds08} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds09} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds13} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds15} \tab 222 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRTR|RTRT \tab full \tab \code{rds25} \tab 70 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR \tab full \tab \code{rds26} \tab 54 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR \tab full \tab \code{rds05} \tab 26 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TRRT|RTTR \tab full \tab \code{rds11} \tab 37 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR \tab full \tab \code{rds16} \tab 38 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TTRR|RRTT \tab full \tab \code{rds28} \tab 64 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TRTR|RTRT|TRRT|RTTR \tab full \tab \code{rds23} \tab 22 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRRT|RTTR|TTRR|RRTT \tab full \tab \code{rds24} \tab 39 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRT|RTR \tab full \tab \code{rds03} \tab 77 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRT|RTR \tab full \tab \code{rds17} \tab 19 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTT \tab full \tab \code{rds10} \tab 18 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TR|RT|TT|RR \tab Balaam’s \tab \code{rds27} \tab 312 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTR|RRT \tab partial \tab \code{rds02} \tab 24 \tab <30 \tab \code{method.A()}, \code{method.B()}, \code{ABE()}\cr
  TRR|RTR|RRT \tab partial \tab \code{rds04} \tab 51 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTR|RRT \tab partial \tab \code{rds07} \tab 360 \tab >30 \tab \code{method.A()}, \code{method.B()}\cr
  TRR|RTR \tab partial \tab \code{rds22} \tab 36 \tab >30 \tab \code{method.A()}, \code{method.B()}
  }
In full replicate designs \emph{both} R and T are administered twice (in 3-period designs to \enc{½}{1/2} of the subjects).\cr
Balaam’s design is a mixture of a conventional crossover (\enc{½}{1/2} of the subjects) and a replicate design (\enc{¼}{1/4} of the subjects receive \emph{either} R \emph{or} T twice).\cr
In partial replicate designs \emph{only} R is administered twice.
}

\source{
  \tabular{cll}{
    Dataset \tab Origin \tab Description\cr
    \code{rds01} \tab EMA, CHMP \tab Data set I\cr
    \code{rds06} \tab \code{rds01} edited \tab T and R switched\cr
    \code{rds12} \tab Phoenix NLME \tab Simulated with extreme variability\cr
    \code{rds14} \tab Phoenix NLME \tab Simulated with high variability and number of dropouts increasing with period\cr
    \code{rds18} \tab \code{rds14} edited \tab Removed T data of subjects 63–78\cr
    \code{rds21} \tab \code{rds01} edited \tab One extreme result of subjects 45 & 52 set to NA\cr
    \code{rds19} \tab \code{rds18} edited \tab Removed data of subjects 63–78\cr
    \code{rds20} \tab \code{rds19} edited \tab Outlier of R (subject 1) introduced: original value ×100\cr
    \code{rds08} \tab R \tab Simulated with slight heteroscedasticity\cr
    \code{rds09} \tab \code{rds08} \tab Wide numeric range (data of last 37 subjects multiplied by 1,000,000)\cr
    \code{rds13} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data deleted)\cr
    \code{rds15} \tab \code{rds08} edited \tab Highly incomplete (approx. 50\% of period 4 data coded as missing \code{'NA'})\cr
    \code{rds25} \tab R \tab Simulated with heteroscedasticity\cr
    \code{rds26} \tab Patterson & Jones 2016 \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data given in Tables 4.30 & 4.31\cr
    \code{rds05} \tab Shumaker & Metzler \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data given in the Appendix\cr
    \code{rds11} \tab Hauschke \emph{et al.} \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data given in Table 9.6.\cr
    \code{rds16} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Drug 14a\cr
    \code{rds28} \tab R \tab Simulated with homoscedasticity\cr
    \code{rds23} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Drug \cr
    \code{rds24} \tab FDA, CDER \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Drug 1\cr
    \code{rds03} \tab \code{rds01} edited \tab Period 4 removed\cr
    \code{rds17} \tab \code{rds03} edited \tab Highly unbalanced (twelve subjects in RTR and seven in TRT)\cr
    \code{rds10} \tab Chow & Liu \tab AUC data given in Table 9.3.3.\cr
    \code{rds27} \tab R \tab Simulated with homoscedasticity\cr
    \code{rds02} \tab EMA, CHMP \tab Data set II\cr
    \code{rds04} \tab Patterson & Jones 2012 \tab \ifelse{html}{\out{C<sub>max</sub>}}{\eqn{C_{max}}} data of Table II\cr
    \code{rds07} \tab R \tab Simulated with homoscedasticity\cr
    \code{rds22} \tab R \tab Simulated with homoscedasticity
  }
}
\references{
European Medicines Agency, Committee for Medicinal Products for Human Use.
\emph{Questions & Answers: positions on specific questions addressed to the Pharmacokinetics Working Party (PKWP).}\cr
London, 19 November 2015. \href{https://www.ema.europa.eu/en/documents/scientific-guideline/questions-answers-positions-specific-questions-addressed-pharmacokinetics-working-party_en.pdf}{EMA/618604/2008 Rev. 13}

Patterson SD, Jones B.
\emph{Viewpoint: observations on scaled average bioequivalence.}\cr
Pharm Stat. 2012;11(1):1--7. \doi{10.1002/pst.498}

Shumaker RC, Metzler CM.
\emph{The Phenytoin Trial is a Case Study of \sQuote{Individual} Bioequivalence.}\cr
Drug Inf J. 1998;32(4):1063--72. \doi{10.1177/009286159803200426}

Chow SC, Liu JP.
\emph{Design and Analysis of Bioavailability and Bioequivalence Studies.}\cr
Boca Raton: CRC Press; 3\ifelse{html}{\out{<sup>rd</sup>}}{\eqn{^{rd}}} edition 2009. p275.

Hauschke D, Steinijans VW, Pigeot I.
\emph{Bioequivalence Studies in Drug Development.}\cr
Chichester: John Wiley; 2007. p216.

Patterson SD, Jones B.
\emph{Bioequivalence and Statistics in Clinical Pharmacology.}\cr
Boca Raton: CRC Press; 2\ifelse{html}{\out{<sup>nd</sup>}}{\eqn{^{nd}}} edition 2016. p105--6.

\acronym{U.S.} Food and Drug Administration, Center for Drug Evaluation and Research.
\emph{Bioequivalence Studies.}\cr
Rockville, 1997.
\href{https://web.archive.org/web/20170723175533/https://www.fda.gov/downloads/Drugs/ScienceResearch/UCM301481.zip
}{bioequivalence study files} (archived 2017-07-23)
}

\seealso{
  4-period full replicates\cr
  {\link[replicateBE]{TRTR.RTRT}}, {\link[replicateBE]{TRRT.RTTR}}, {\link[replicateBE]{TTRR.RRTT}}, {\link[replicateBE]{TRTR.RTRT.TRRT.RTTR}}, {\link[replicateBE]{TRRT.RTTR.TTRR.RRTT}}\cr
  2-period replicate (Balaam’s design)\cr
  {\link[replicateBE]{TR.RT.TT.RR}}\cr
  3-period full replicates\cr
  {\link[replicateBE]{TRT.RTR}}, {\link[replicateBE]{TRR.RTT}}\cr
  3-period partial replicates\cr
  {\link[replicateBE]{TRR.RTR.RRT}}, {\link[replicateBE]{TRR.RTR}}
}

\author{
Helmut \enc{Schütz}{Schuetz} (R-code for simulations by Detlew Labes), Michael Tomashevskiy (simulations in Phoenix NLME)
}

\examples{
# show structure of all data sets
ds <- substr(grep("rds", unname(unlist(data(package = "replicateBE"))),
                  value = TRUE), start = 1, stop = 5)
for (i in seq_along(ds)) {
  cat(ds[i], "\n")
  str(eval(parse(text = ds[i])))
}
}

\keyword{datasets}
