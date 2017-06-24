# replicateBE
Comparative BA-calculation for the EMA's Average Bioequivalence with Expanding Limits (ABEL)

Methods as given in the EMA's Q&A document are available at
http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf
Method A:
  log(response) ~ sequence + subject(sequence) + period + treatment
  where all effects are fixed (i.e., ANOVA)
Implemented with function lm() of library stats.
Method B:
    log(response) ~ sequence + subject(sequence) + period + treatment
    where subject(sequence) is a random effect.
    Two options:
      1:  Implemented with function lmer() of library lmerTest.
          Satterthwaite's degrees of freedom
      2:  Implemented with function lme() of library nlme.
          Degrees of freedom equivalent to SAS DDFM=CONTAIN and
          Phoenix/WinNonlin DF Residual.
Conventional (unscaled) Average Bioequivalence (ABE) can be calculated as well.
