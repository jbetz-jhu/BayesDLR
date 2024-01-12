BayesDLR: Bayesian Inference for the Diagnostic Likelihood Ratio
================
Josh Betz - Biostatistics Consulting Center (<jbetz@jhu.edu>)
2024-01-12 05:20

- [Using BayesDLR:](#using-bayesdlr)
  - [Installation:](#installation)
- [The Workflow](#the-workflow)
  - [Step 1. Supply Raw Data File](#step-1-supply-raw-data-file)
  - [Step 2. Configure the Analysis](#step-2-configure-the-analysis)
  - [Step 3: Process Data and Run
    Analyses](#step-3-process-data-and-run-analyses)
- [Try it For Yourself](#try-it-for-yourself)
- [The Analysis](#the-analysis)
  - [2x2 Table](#2x2-table)
  - [Statistical Model](#statistical-model)
  - [The Role of Sample Sizes](#the-role-of-sample-sizes)

This repository is for fitting Empirical Bayes Beta Binomial models to
prevalence values from two populations. Currently, a single component
model and a two-component mixture model are implemented, and are fit
using maximum marginal likelihood. Estimation is done using the
[`optimx`
package](https://cran.r-project.org/web/packages/optimx/index.html).

------------------------------------------------------------------------

# Using BayesDLR:

## Installation:

First, install [the R environment for statistical computing and the
Rstudio IDE](https://posit.co/download/rstudio-desktop/). In RStudio,
install the following packages, or check for updates if these packages
are already installed. This can be done using `install.packages()` and
`update.packages()` or using the `Packages` tab of the RStudio IDE.

In Rstudio, go to `File` \> `New Project` \> `Version Control` \> `Git`.
In the repository URL, enter <https://github.com/jbetz-jhu/BayesDLR>.
Leave the project directory name as `BayesDLR`. Select a directory to
‘clone’ (i.e. download) the repository: this directory will become the
“project root directory”, denoted as `project_root` in code. Click the
“Create Project” button. RStudio will download the repository from
GitHub into the folder you specified. In this folder, you should see the
following folder structure:

- (project_root)
  - BayesDLR
    - analyses
    - code
    - data
    - results

------------------------------------------------------------------------

# The Workflow

## Step 1. Supply Raw Data File

Related data and analyses for a research project are grouped together in
subfolders, specified by the variable `data_subfolder`. There is a
project for demonstration entitled `"example_1"` that is built into the
repository. In the `"\BayesDLR\data"` subfolder, you’ll see an
`"example_1"` subfolder: inside this is a `"raw_data"` subfolder with
two example datasets in .CSV format:

- `BayesDLR\data\example_1\raw_data\`
  - `example_1_component_v1_240110.csv`
  - `example_1_component_v2_240110.csv`

## Step 2. Configure the Analysis

Once raw data has been put in the
`\BayesDLR\data\(project name)\raw_data` subfolder, go to the
`\BayesDLR` folder and open `1_configure_analysis.r`.

- Set `data_subfolder` to `(project name)`
  - For the first example, `data_subfolder <- "example_1"`.
- Set `input_data_file` to the file name that will be used as input. If
  the input is an Excel sheet (.xls or .xlsx) formats, specify the sheet
  to be loaded using `input_data_sheet`.
  - For the first example,
    `input_data_file <- "example_1_component_v1_240110.csv"` - since
    this is a .csv and not an Excel format, `input_sheet_name` is
    ignored.
- Set the data version name: this is a shortened name that is used to
  name processed data and analyses. Since this helps link the raw file
  with the processed data and analyses, make sure this label is
  descriptive.
  - For the first example, `data_version_name <- "V1-240110"` for
    **V**ersion **1** of the data, created on 20**24-01-10**.
- Set the threshold for relative sample sizes ($r_k$): for more
  information on this, see [the role of sample sizes in the Beta
  Binomial Model](#the-role-of-sample-sizes)
  - For the first example, `r_k_threshold <- 0.10` - The threshold is
    10%
- Set the names of the columns in the input file: this includes the
  `row_id`, a label for the data in each row, and the numerator and
  denominator for cases and controls.
  - For the first example:
    - `row_id <- "event"`
    - `case_denominator <- "n_1"`
    - `case_count <- "y_1"`
    - `control_denominator <- "n_0"`
    - `control_count <- "y_0"`
- (Optional) Set a name for the variable which flags data for inclusion,
  and the values indicating inclusion. If the data do not have such a
  flag, set to `NULL`.
  - For the first example: `flag_column <- "flag"` and
    `flags_include <- "include"` - Any rows where `flag` does not match
    `"include"` are removed from analytic data.
- Specify random number generator (RNG) seeds: this allows you to
  reproduce the same random number streams used in an earlier analysis.
  If set to `NULL`, a new RNG seed is generated from the current state
  of the RNG, and saved so analyses can be reproduced.

Once these parameters have been set, run the code. If successful, you
should see a message similar to:

`Successfully wrote V1-240110_r_k_10_pct.yml to ..\BayesDLR\analyses\example_1.`
`Proceed to 2_run_analysis.R`

This indicates that the configuration file `V1-240110_r_k_10_pct.yml`
was created: This is a plain-text file that the workflow uses to run and
summarize analyses.

------------------------------------------------------------------------

## Step 3: Process Data and Run Analyses

Once the configuration file is created for the analysis, open
`2_run_analysis.r`. Set the same values of `data_subfolder`,
`data_version_name`, and `r_k_threshold` supplied earlier: This tells
the workflow the path to the configuration file:
`..\BayesDLR\analysis\(data_subfolder)\(data_version_name)_r_k_(r_k_threshold)_pct.yml`.

This file contains all the information needed to carry out the analysis,
so it does not need to be re-entered.

By default, the workflow will stop if errors are identified in data
marked for inclusion: If you identify any issues in the data, they
should be checked before proceeding: `stop_on_data_error <- TRUE`. To
proceed with analyses after excluding erroneous data, make sure
`stop_on_data_error <- FALSE` is not commented out.

The configuration file is used to read in the data, select the relevant
columns, check for errors, and write out the processed data to the
`..\BayesDLR\data\(data_subfolder)\processed_data` subfolder. The files
will be named
`(data_subfolder)-(V1-data_version_name)_processed_v0.0.Rdata` (Rdata
file) and `(data_subfolder)-(V1-data_version_name)_processed_v0.0.xlsx`
(Excel format). In the example analysis, this becomes
`example_1-V1-240110_processed_v0.0.Rdata` and
`example_1-V1-240110_processed_v0.0.xlsx`.

If any errors are encountered in the data, the workflow will either stop
or issue a warning, depending on `stop_on_data_error`. The example
dataset yields gives:

`Warning message:` `Data entry errors detected in input file.`
`File: ../BayesDLR/data/example_1/raw_data/example_1_component_v1_240110.csv`
`Rows: 1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13`

`REVIEW DATA CAREFULLY BEFORE PROCEEDING.`

This indicates that the data were successfully processed, but errors
were found. These can be seen in tab `4. Invalid data` in the Excel
output.

Analysis results are stored in `../BayesDLR/results/(data_subfolder)` in
R and Excel files named with `data_subfolder`, `data_version_name`, and
`r_k_threshold`.

------------------------------------------------------------------------

# Try it For Yourself

There is a second version of the `example_1` dataset in .csv without the
errors: it is marked v2 for version 2. Try steps 2-3 for this new file.
There is also an Excel format dataset for `example_1`: try steps 2-3 for
the Excel spreadsheet.

There is also an `example_2` in .csv and .xlsx formats: try steps 2-3 on
these files as well.

When you are ready to use your own data, rename the `new_project` folder
and follow steps 1-3 to analyze it.

------------------------------------------------------------------------

# The Analysis

The prevalence ratio of exposure given disease status is a measure of
association comparing the prevalence of an exposure in a population with
a condition to the prevalence of an exposure in a population without
that condition.

$$PR_{E \vert D} = \frac{Pr\{\text{Exposed} | \text{Disease}\}}{Pr\{\text{Exposed}|\text{No Disease}\}}$$
For example, if the exposure is a genetic variant that’s considered a
potential diagnostic marker or test $T$, the prevalence ratio of the
exposure in those with the disease to those without is the same as the
*positive diagnostic likelihood ratio*:

$$LR_{+} = \frac{Pr\{T+ \vert D+\}}{Pr\{T+ \vert D-\}} = \frac{\text{sensitivity}}{1\,-\,\text{specificity}}$$

------------------------------------------------------------------------

## 2x2 Table

This can be obtained using a 2x2 table, where $n_{0k}$ controls are
tested for variant $k$, yielding $Y_{0k}$ positives, and where $n_{1k}$
cases are tested, yielding $Y_{1k}$ positives, with
$T_{k} = Y_{0k} + Y_{1k}$ total positives.

|    Variant     |         Cases         |       Controls        |              Total              |
|:--------------:|:---------------------:|:---------------------:|:-------------------------------:|
|  Variant $k$   |     $Y_{0k} = A$      |     $Y_{1k} = B$      |         $T_{k} = A + B$         |
| Other Variants | $n_{0k} - Y_{0k} = C$ | $n_{1k} - Y_{1k} = D$ | $n_{0} + n_{1} - T_{k} = C + D$ |
|     Total      |   $n_{0k} = A + C$    |   $n_{1k} = B + D$    |                                 |

------------------------------------------------------------------------

## Statistical Model

When we have a fixed sample size of $n$ independent individuals, each
from a population with prevalence $\pi$, we can model the number of
prevalent cases in this sample using the binomial likelihood model:

$$Y \sim Bin(n, \pi):\,Pr\{Y = y|n, \pi\} = {n \choose y} \pi^{y} (1 - \pi)^{n-y}$$
This binomial likelihood can be approximated using a Poisson
distribution with rate parameter $\lambda = n\pi$ if $n$, the number of
observations, is large and $\pi_{k}$, the probability (i.e. prevalence),
is small.

$$Y \sim Poisson(\lambda = n\pi):\,Pr\{Y = y|n, \pi\} = \frac{(n\pi)^{y}e^{-n\pi}}{y!}$$
We can model the proportion of type $k$ variants that arose from cases
out of the the total number of type $k$ variants observed, denoted
$\theta_{k}$, using a binomial distribution:

$$Y_{0k} \sim Poisson(n_{0k}\pi_{0k}),\, Y_{1k} \sim Poisson(n_{1k}\pi_{1k});\quad Y_{0k} \perp Y_{1k}; \quad \left(Y_{1k} \vert Y_{0k} + Y_{1k} = T_{k}\right) \sim Bin(T_{k}, \theta_{k}),$$

where $\theta_{k} = (n_{1k}\pi_{1k})/(n_{1k}\pi_{1k} + n_{0k}\pi_{0k})$,
the rate of occurrences in cases divided by the total rate of
occurrences (the sum of the rates in cases and controls).

Note that since $\theta_{k}$ is a proportion: $0 \le \theta_{k} \le1$.
We are interested in obtaining the *prevalence ratio*
$\gamma_{k} = \pi_{1k}/\pi_{0k}$, where $0 < \gamma_{k}$. We can infer
about this quantity by using a transformation.

------------------------------------------------------------------------

## The Role of Sample Sizes

Let $r_{k} = n_{1k}/n_{0k}$ denote the ratio of the sample size of cases
relative to controls. Dividing the numerator and denominator again by
$n_{0k}$, the control sample size, gives:

$$\theta_{k} = \frac{n_{1k}\gamma_{k}/n_{0k}}{n_{1k}\gamma_{k}/n_{0k} + n_{0k}/n_{0k}} = \frac{r_{k}\gamma_{k}}{r_{k}\gamma_{k} + 1}$$

Parameterizing the model this way allows the modeling multiple variants,
allowing for variation in sample sizes across different variants, **as
long as their ratio of sample sizes for any given variant $r_{k}$ is
comparable.**

Since $\theta_{k} = r_{k}\gamma_{k}/(r_{k}\gamma_{k} + 1)$, we can
rearrange this to get back to the prevalence ratio:
$\gamma_{k} = \frac{1}{r_{k}} \frac{\theta_{k}}{(1 - \theta_{k})}$.

So we infer about $\theta_{k}$, and then use the above transformation to
obtain the prevalence ratio $\gamma_{k}.$ Note that this transformation
depends on the relative samples sizes $r_{k} = n_{1k}/n_{0k}$.

If the relative sample sizes are all equal, this transformation is
identical. When the relative sample sizes vary, this transformation is
not the same for all observations. This is why observations with very
different relative sample size are exclude from analyses.
