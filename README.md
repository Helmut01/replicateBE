# replicateBE
## Comparative BA-calculation for the EMA’s Average Bioequivalence with Expanding Limits (ABEL)

The library supports the methods given in the EMA’s [Q&A document](http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2009/09/WC500002963.pdf) according to the EMA’s [Guideline on the Investigation of Bioequivalence](http://www.ema.europa.eu/docs/en_GB/document_library/Scientific_guideline/2010/01/WC500070039.pdf).


- Method A
  
  A linear model of log-transformed PK reponses and effects _sequence_, _subject(sequence)_, _period_, and _treatment_ – where all effects are fixed (_i.e._, ANOVA). Estimated via function ```lm()``` of library ```stats```.
  ```Rscript
  log(PK) ~ sequence + subject(sequence) + period + treatment, data = data
  ```
  
- Method B
  
  A linear mixed model of log-transformed PK reponses and effects _sequence_, _subject(sequence)_, _period_, and _treatment_ – where _subject(sequence)_ is a random effect and all others are fixed.
  ```RScript
  log(PK) ~ sequence + period + treatment, random = ~1|subject, data = data
  ```
  Two options
    - Estimated via function ```lme()``` of library ```nlme```. Uses degrees of freedom equivalent to SAS’ ```DDFM=CONTAIN``` and Phoenix/WinNonlin’s ```DF Residual```. Implicitely preferred according to the Q&A document.
    - Estimated via function ```lmer()``` of library ```lmerTest```. Uses Satterthwaite’s degrees of freedom.


- Conventional (unscaled) Average Bioequivalence (ABE) can be calculated as well.

**Program offered for Use without any Guarantees and Absolutely No Warranty. No Liability is accepted for any Loss and Risk to Public Health Resulting from Use of this R-Code.**
