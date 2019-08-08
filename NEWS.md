# replicateBE 1.0.10.9000

Published on Github 2019-08-08

## Minor changes

  * Changed default path-variables from `NULL` to the user’s home folder `"~/"`.
  * `print(..., , digits=7)` if `verbose=TRUE`. No need for more significant digits; results in the result data.frame are given in full precision anyway.

## Minor changes

  * README.Rmd knitted to README.md

# replicateBE 1.0.10

Published on Github 2019-07-24

  * On CRAN.

## Issues

  * Seemingly the LaTeX-Installation on CRAN is corrupt (THX to Duncan Murdoch on r-package-devel). Either the file `hyperref.sty` is corrupt or missing (local rendering worked on Windows and Linux). Therefore, URLs in references are converted to truncated (*i.e.*, wrong) URLs in footnotes in the PDF-manual. Notified Uwe Ligges.

## Bug fixes

  * Stopped import when *no* missings according to the specifier `na` where found (THX to Igor Rubets).

## Minor changes

  * README.md included.
  * NEWS.md instead of NEWS.
  * Removed numbered references since CRAN converts URLs to footnotes in the PDF manual.
  * Updated links to the EMA’s datasets in `info.data()`.

# replicateBE 1.0.9

Published on Github 2019-07-22

  * On CRAN.

# replicateBE 1.0.8.9000

Published on Github 2019-07-20

## Bug fixes

  * `importFrom(pbkrtest, getKR)` since on win-builder.r-project.org error `'pbkrtest package required for Kenward-Roger's method'` (not locally!)
  * Forces `logtrans=TRUE` if a user asks for an internal dataset which does not have the column `logPK`.

## Major changes

  * Added Kenward-Roger degrees of freedom to `method.B (..., option=3)`.

## Minor changes

  * Updated links to EMA’s documents (again...) in man-pages. Numbered references.
  * More informative warning in `get.data()` if na-specifier does not match the data in columns `PK` and/or `logPK`.
  * Depends now on R >= 3.5.0 (since .rda saved with 3.6.0).
  * Kept column `logPK` only in rds01 and rds02 because given by the EMA. Removed in rds06, rds12, rds14, rds18, rds19, rds20, rds21 (TRTR|RTRR) and rds03, rds17 (TRT|RTR). Man-pages corrected. Reduces the footprint of the library by 5%.
  * If `path.in` and/or `path.out` not given or specified folder does not exist: Changed `cat()` to `warning()`.
  * Cosmetic changes in verbose output of `method.A()`.
  * `suppressMessages()` no more needed (vignette and testthat). Issue orginating in `rlang` and `ggplot2` resolved. Both packages don't contain a NEWS file. THX Hadley!
  * `stringsAsFactors=FALSE` in res-list of all methods.
  * Removed superfluous `options(ow)` after models (THX to DL).
  * Updated man-page of `method.A()` and the vignette.

# replicateBE 1.0.8

Published on Github 2019-06-14

## Bug fixes

  * THX to Uwe Ligges @CRAN. Resubmitted to CRAN.

# replicateBE 1.0.7

Published on Github 2019-06-14

## Bug fixes

  * As suggested by <cran-submissions@r-project.org>. Resubmitted to CRAN.

# replicateBE 1.0.6

Published on Github 2019-06-12

  * Submitted to CRAN (passed checks at R-release and R-devel).

# replicateBE 1.0.5.9003

Published on GitHub 2019-06-12

## Bug fixes

  * LaTeX problems partly fixed (THX to Duncan Murdoch on r-help). Final solution: Text between `\list{}{}`. Note that this is discouraged acc. to the R-Extension Manual 2.1.1!

## Minor changes

  * Simplified and reordered sources of man-pages.
  * Updated WHO-reference.
  * Changed `{\eqn{foo\textsubscript}{bar}}` to simple `{\eqn{foo_{bar}}` in two man pages.
  * `StartupWarnings` seemingly unavoidable until `rlang` will be corrected.
  * Small correction in text of box plot of `CV.calc()`. Studentized/standardized does not exactly agree with SAS (uses `type=2` by default).
  * First version of vignette.
  * Added `\tests` for `testthat()`.

# replicateBE 1.0.5.9002

Published on GitHub 2019-05-30

## Issues

  * Renaming S3 method doesn’t help.

## Minor changes

  * Updated links in man-pages to reflect changes in the FDA’s site.
  * Updated links to reference data in `info.data()`.
  * Updated links in CSV-files.
  * Renamed S3 methods (prefixed with `repBE`) acc. to <https://github.com/r-lib/rlang/issues/669>. THX to MT!
  * The warning in CHECK dissappeared in R 3.6.0. Throws other ones in examples which change the working directory. All changed to `\dontrun{}`.

# replicateBE 1.0.5.9001

Published on GitHub 2019-04-23

## Minor changes

  * Updated links in man-pages to reflect changes in the EMA’s site.

# replicateBE 1.0.5.9000

Published on GitHub 2017-11-25

## Issues

  * CHECK throws a WARNING:
    
    `checking for unstated dependencies in examples ... WARNING`
    `Warning in parse(file = files, n = -1L) :`
    `  invalid input found on input connection 'replicateBE-Ex.R'`
    
    `parse()` is used in the man-pages of `method.B` and `data`. Neither
    `\dontrun{}`, commenting the code out, or completelly removing the
    examples resolves the issue.

## Bug fixes

  * Import from XLS with a header.
  * `get.data()` CSV-file: Issue with characters in the header which were `== sep` resolved. [MT]
  * `get.data()` Sub/seq in data.frame was empty for complete sequences.
  * `info.env()` gave 'path.out' as 'path.in'.
  * Mixed model was given in the result file of `method.A()`.
  * Adapt the No. of subjects/sequence and No. of missings/sequence to the reordered sequences in `get.data()`. Calculation was correct but output in result-files wrong.
  * nTT was NA in `get.data()` for TRTR|RTRT. Now design (full/replicate) is obtained from `info.design()`.
  * In `CV.calc(..., ole=TRUE)` if an outlier is detected not in both methods (studentized, standardized).
  * `method.B(option=1)` output to file.
  * In package `PowerTOST` T is always first. The order is only relevant in `method.A(..., adjust=TRUE)` and an unbalanced RTR|TRT-design. In this case the order of subjects/sequence is reversed in calling
    `scABEL(...,  n=rev(ret$Sub.Seq))`. THX to DL for discovering this bug.

## Major changes

  * Added data set 27 for Balaam’s design (TR|RT|TT|RR) and data set 28 (TTRR|RRTT); both simulated with homoscedasticy. The former only for completeness (poor power). The latter might be useful for steady state studies. Man-pages for both.
  * Balaam’s design implemented.
  * Compare variabilites according to the FDA’s warfarin guidance.
  * Added data set 26 (TRTR|RTRT). One of the few where we have results for comparison.
  * Added data set 25 (TRTR|RTRT) simulated with heteroscedasticy.
  * Shows a 'graphical' presentation of the CI, PE, and BE-limits in the result file.
  * Added data set 24 (TRRT|RTTR|TTRR|RRTT) from the FDA. Subject 16 completely missing. Adapted `get.data()` for such a case.
  * Added Data Set 23 as an example of a 4-period 4-sequence design (though not recommended by the FDA).
  * Added data set 22 for the extra-reference design (RTR|TRR).
  * Calculate the sw-ratio (useful for the WHO’s scaling of AUC).
  * Assessment for NTIDs implemented. `ABE()` has new arguments `theta1` and `theta2`. Default to 0.80 and 1.25.
  * New function `info.design()`. Sorts sequences according to the preferred rder (T first) and throws a message if the design is untested.
  * Changed the names of result files reflecting the Method used rather the internals (`lme`/`lmer`).
  * `Lazy data: true` in DESCRIPTION allows direct access of objects within `data()`.

## Minor changes

  * Rewrite of reading from file.
    * The header is automatically identified (removed as an argument from the calling functions).
    * Subject must no more be the first column. The word 'subject' is allowed in the header. [MT]
  * Argument `set` can be an empty string `""` to support reading from CSV- files. Only required for XLS (the name of the sheet) now.
  * Argument `fence` for box plots instead of the hard-coded 3 as a multiplier of IQR. The default 1.5 in most (all?) software packages detects *a lot* of outliers. 3 seems to be liberal. 2 is a compromise.
  * Changed TRTR|TRRT|RTTR|RTRT to TRTR|RTRT|TRRT|RTTR. More logical (stacking sequences of the other full replicates). Adapted scripts and man pages accordingly.
  * Changed the lexical order of sequences (T before R) in conformity with package `PowerTOST` and Q&A DSII.
  * Introduced aliases in man pages for all data sets. Otherwise, warnings in CHECK about undocumented objects.
  * Identify internal data sets based on their attribute. Uses now `info.data().` THX to DL. Function `which.data()` removed.
  * Man pages reworked. [DL]
  * `method.A(adjust=TRUE)` assesses additionally the TIE based on the
    recalculated `CVwR` (if applicable).
  * Changed TRR.RTT to RTT.TRR for consistancy (R always first).

# replicateBE 1.0.5

Released to collaborators 2017-06-24

## Issues

  * CSV-files: Characters in the header which are equal to `sep` are lost.
  * `method.B(option=1)` can't print to file with internal data.

## Minor changes

  * Unified data sets, updated man-pages, and changed `which.data()`: `ref` to `rds'. Warnings in CHECK: `'rdsXX' not found`.

# replicateBE 1.0.4

Released to collaborators 2017-06-23

## Issues

  * A user calls `foo(path.in=path.in, ...)` but the variable `path.in` is is not defined before. Gives `object 'path.in' not found in CV.calc()`.

## Bug fixes

  * If 'descr' was read from an external file and the code stopped for data not matching the id in `which.data()`.
  * Warning in man-pages: Header must not contain the word 'subject'.
  * If the specified file is not found in `path.in` the file browser opens.
  * If `verbose=TRUE` the anova was not shown in `ABE()`.

## Major changes

  * New function `which.data()`: returns data.frame 'id' of checksum (MD5), file "DS", set ("01", ... "XX"), ref ("refXX"), descr. Used in `get.data()` to obtain required variables of the currently attached internal data set based on its MD5-checksum.
  * Changed the default variable separator from `";"` to `","` (more common). Suggested by DL.

## Minor changes  
  
  * Removed `CV.calc` from `NAMESPACE`. Man-page not needed any more. THX DL!
  * Replaced `subset()` in `get.data()` and `CV.calc()` by direct assignment. No need for the hacks any more.
  * Removed DS.xls and adapted man-examples accordingly. CRAN policy: Data sets should not be larger than 1 MB. BTW, why is the zip so much larger than the tarball? Before removing DS.xls their sizes were similar.
  * F.i. data(ref02) followed by `print(ABE(details=T, print=F, data=data))` works. Problems: The descriptive header (variable `descr`) doesn't exist in the internal data set as well as its name. Hence, the same with the default `print=TRUE` in all functions gives an error. Stupid: The data set must be chosen outside of the function call. `foo(data=data(ref02))` gives an error.
  * Added argument `data=NULL` (default `NULL`) to top-level functions.
  * `method.B()`: Changed `print(anova.lm(modB))` to `print(anova(modB))`. [DL]
  * Modified imports in `NAMESPACE`. [DL]
  * Working on `\data` and the documentation. All data sets observe the pattern `refXX.rda` in order to come last in the man-page.

# replicateBE 1.0.3

Released to collaborators and beta-testers 2016-06-19

## Major changes

  * Renamed package from `ABEL` to `replicateBE` since ABE is now supported as well.
  * Added function `ABE()` for conventional (unscaled) ABE.

## Minor changes

  * Added `\data\ref01.rda` (already factorized data.frame) and man-page.
  * Changed `enf.info()`: If path(s) `NULL`, show the user’s home folder in the lines `Input from` and `Output to`.

# replicateBE 1.0.2

Released to collaborators and beta-tester Mahmoud Teaima 2016-06-19

## Minor changes

  * New argument `plot.bxp`. If `FALSE` (default) the boxplot will be shown in the default graphics device. If `TRUE` the boxplot will be saved in PNG format to `path.out`. Argument `ask` is also observed.
  * If `path.in` and/or `path.out` not given or not existing, the user’s home folder will be used. Updated the man pages accordingly.

# replicateBE 1.0.1

Released to collaborators and beta-tester Mahmoud Teaima 2016-06-18

## Minor changes

  * Argument `ask` (default `FALSE`). If `TRUE` and a result file already exists, the user is asked whether it should be overwritten.
  * Removed `lme4` from listed packages in `env.inf()`. `lmer`, `summary`, and `anova` obtained from `lmerTest`.
  * Changed in `get.data()` name of file from `"_results_"` to `"_ABEL_"`.
  * Changed `get.data()`: `path.out` only required if `print=TRUE`. Default `path.out` in `method.A()` and `method.B()` set to `NULL`.
  * Improved man-pages. Added an example comparing Method B with Method A.

# replicateBE 1.0.0

Released to collaborators and beta-tester Mahmoud Teaima 2016-06-16

## Major changes

  * First version of package built.

## Minor changes

  * Simplified output in `method.A()` if `adjust=TRUE`.
  * Housekeeping routine removed.
  * The former variable `path` (used for in- /and/ output) substituted by variables `path.in` and `path.out`.
  * Functions `env.info()`, `get.data()`, `CV.calc()`, `method.A()`, `method.B()` stand-alone in order to support package-building.
  * `method.A()`: Changed  
     `  lm(foo ~ subject + period + treatment, data)`  
     to the subject decomposition acc. to the Q&A document  
     `  lm(foo ~ sequence + subject%in%sequence + period + treatment, data)`  
     Note: PE, MSE, DF are identical to the simple model, but less confusing if the ANOVA is shown (`verbose=TRUE`). [suggested by Mahmoud Teaima]

# replicateBE 0.95

Released to collaborators 2016-06-11

## Bug fixes

  * print corrected. Line-endings CRLF (Windows), LF (UNIXes), CR (MacOS). Tested on Windows [HS] and on macOS Sierra 10.12.5 (THX to Mahmoud Teaima, Faculty of Pharmacy, Cairo University).
  * Reading xls(x)-files corrected. Converts the tibble-object to a data.frame.

## Minor changes

  * List in `env.inf()` only the packages which are used by the respective functions.
  * y-axis in boxplot symmetrical around zero. I think that this is more informative than the default.
  * Vector of na-strings supported since v1.0.0 of package `readxl`. Throws a message if the installed version is <1.0.0. Is there a method to *automatically* update a package?

# replicateBE 0.94

Released to collaborators and beta-tester Mahmoud Teaima 2016-06-01

## Minor changes

  * Outlier analysis to console if `verbose=TRUE`.
  * Moved `DF` and `alpha` up after the number of subjects. More logical to me.
  * If outlier(s) detected, `CVwR` is recalculated and BE is *additionally* assessed based on the new limits.
  * Added optional outlier assessment (`ola=TRUE`) for the reference. Defaults to `FALSE`. Studentized and standardized residuals are calculated and shown in box plots. Note: Only standardized (a.k.a internally studentized) residual are available in Phoenix WinNonlin.

# replicateBE 0.93

Released to collaborators 2016-12-16

## Minor changes

  * Variable and decimal separators for reading CSV-files can be specified. Defaults `";"` and `"."`
  * Result-file UTF-8 encoded (important for OSX).
  * Added license info and disclaimer (paranoia).
  * Added a statement if `nTR <12`. Suggested by MT.
  * Added a statement about 'uncertain' CVwR if nRR <12 in one of the full replicate 3-period designs (acc. to the EMA's Q&A Rev. 12).
  * Added argument `verbose` (default `FALSE`) to `method.A()`/`method.B()` to support detailed information without debugging.
  * Moved check for trailing `'/'` in the path-argument from `method.A()`/ `method.B()` to `get.data()`.
  * Restored the evaluation by `lmer`/`lmerTest` from v0.83 to support comparing the performance of packages. Wish of DL.

# replicateBE 0.92

Released to collaborators 2016-12-14

## Bug fixes

  * Wrong `Miss.per` for data sets with `NA='.'` fixed. [MT]

## Minor changes

  * If path does not exist, R’s working directory is used with warning. [MT]

# replicateBE 0.91

Released to collaborators 2016-12-14

## Minor changes

  * The line `names(Miss.per)` is removed (not used in output). [MT]
  * Vectorized `Miss.seq` and `Miss.per` in `get.data()`, unnecessary function `complete()` removed. [MT]

# replicateBE 0.90

Released to collaborators 2016-12-14

## Minor changes

  * Removed `lmer`/`lmerTest` (was called by `option=1` in previous versions), since Satterthwaite’s DF are not compliant with the Q&A document.
  * Added DF of the treatment difference to the output.

# replicateBE 0.83

Released to collaborators 2016-12-11

## Major changes

  * Added function `complete()` to construct a data.frame of complete data based on the subjects and design. Called by `get.data()` to calculate the number of missing values independent whether the DS contains this information (NAs in 'PK') or not. Not vectorized yet.
  * New variable `Miss.seq`: Missings / sequence.
  * New variable `Miss.per`: Missings / period.

# replicateBE 0.82

Released to collaborators 2016-12-10

## Bug fixes

  * Added `na.action=na.omit` to `lme()` since its default (`na.fail`) stopped the model.

## Major changes

  * Changed the default in `method.B()` from `option=1` to `option=2`. No DDFM applied. `lme4`/`lmerTest` (`option=1`) - like Phoenix by default - uses Satterthwaite’s DF.
  * Input checking:
    * Converts variable names (except `PK` and `logPK`) to *lower* case. Stops if variables are not coded as `subject`, `period`, `sequence`, and `treatment`.
    * Stops if treatments are not coded as `R` and `T`.
    * Stops if the number of sequences and/or periods does not match the specified design.

## Minor changes

  * Returns number of missings / sequence (currently only for NAs).

# replicateBE 0.81

Released to collaborators 2016-12-09

## Bug fixes

  * Checking packages fixed. [MT]

## Minor changes

* `(.)` removed before names of variables. [MT]

# replicateBE 0.8

Released to collaborators 2016-12-09

## Bug fixes

  * Calculation of `Nsub.seq` fixed. [MT]

## Minor changes

  * `/` is added to the path if not present. [MT]
  * The name of used package is added to the file name for `method.B()` (`_lmer` or `_lme`). [MT]
  * `lme4` is added to the list of packages (it was an implied subroutine call from `lmerTest`). [MT]

# replicateBE 0.7

Released to collaborators and beta-tester Jiri Hofmann 2016-12-07

## Bug fixes

  * Calculation of `Nsub.seq`! DA12 (with N=77) gave 149 in both sequences.
  * BE-assessment corrected (testing both `res$CI` and `res$GMR` instead of `unique()`.

# replicateBE 0.6

Released to collaborators 2016-12-02

## Major changes

  * `sequence+subject%in%sequence+period according` to the Q&A and the setup in SAS and Phoenix. Note that nested subjects are superfluous. The more simple model `subject+period` gives exactly the same residual error!

# replicateBE 0.5

Released to collaborators 2016-11-28

## Major changes

  * Added import format XLS(X). Requires package `readxl`. Cave: If the sheet(s) contains a commentary header, in the argument `skip` the number of rows *not* to be used must be given.

# replicateBE 0.4

Released to collaborators 2016-11-27

## Major changes

  * New function `CV.calc()` (suggested by DL). Moved the calculations from `get.data()`. Adapted calls in `method.A()` and `method.B()`.

## Minor changes

  * Vectorized `Nsub.seq` [MT].

# replicateBE 0.3

Released to collaborators 2016-11-26

## Bug fixes

  * Bug corrected in selecting subjects with RR [MT].

## Minor changes

  * Added number of subjects/sequence and check for balance (not vectorized yet).
  * Results of iteratively adjusted alpha printed. Throws a warning if `adjust=TRUE` for an unsupported design.

# replicateBE 0.2

Released to collaborators 2016-11-24

## Major changes

  * Use package `PowerTOST` for assessment of the Type I Error and (optional) adjustment of alpha (only `Method A`). New argument `adjust` (default `FALSE`). Results only to the console. Changed hard-coded parts to the respective functions of `PowerTOST`.
  * New argument `logtrans` (default `TRUE`). If `TRUE`, the raw data (`PK`) are internally log-transformed. If `FALSE` the already log-transformed data (`logPK`) will be used.

## Minor changes

  * Data can be provided *without* the column logPK. If erroneously a call is made with `logtrans=FALSE`, the code switches to internal log-transformation (as if `logtrans=TRUE` would have been called) and throws a warning.
  * Added log half-width to the data.frame `res`.

# replicateBE 0.1

Released to collaborators 2016-11-23

## Major changes

  * New argument `option` (`1` or `2`) in `method.B()`. `1` (default) evaluates by `lmerTest' and `2` by `nlme`.
  * New arguments `print` (default `TRUE`) and `details` (default `FALSE`).
    `  x <- foo(..., print=FALSE, details=TRUE)` provides a data.frame of results with 7 significant digits.
  * Using `intervals()` instead of a hard-coded confidence interval [DL].
  * Added coding for Method B by `nlme`. [MT]
  * All changes done by [HS] if not stated otherwise.

# replicateBE 0.0

Released to collaborators 2016-11-22

  * New [HS].
