
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.23341773.svg)](https://doi.org/10.5281/zenodo.23341773)

<!-- The .md file is generated from an .Rmd file. Please edit the .Rmd; not the .md !! -->

## Welcome to the **R** code base repository for the DEXA-PSYCH trial

------------------------------------------------------------------------

> - [**Authors (repo)**](CITATION.cff): Rune Haubo B. Christensen and
>   Eva Nina Sparre Wandall
> - [**Primary-Publication**]() (Link will be updated upon publication)
> - [**Tables, Listings and Figures for primary
>   publication**](https://dexa-psych.netlify.app/) (password-protected
>   for reviewer access until the primary publication is published)
> - **EU Trial registration number**:
>   [2022-501428-45-00](https://euclinicaltrials.eu/ctis-public/view/2022-501428-45-00?lang=en)
> - **WHO universal Trial Identifier**: U1111-1280-7614
> - [**Trial
>   protocol**](https://euclinicaltrials.eu/ctis-public/view/2022-501428-45-00?lang=en)
>   (latest version)
> - [**Trial protocol**](https://doi.org/10.1186/s13063-025-08989-2)
>   (Journal publication of earlier version)
> - [**Statistical Analysis
>   Plan**](https://doi.org/10.5281/zenodo.23343482)
> - **DEXA-PSYCH team**:
>   - [Troels Boldt Rømer](https://orcid.org/0000-0002-0832-2667)
>     (Sub-Investigator)
>   - [Camilla Goetzsche
>     Frederiksen](https://orcid.org/0000-0002-4475-7617)
>     (Sub-Investigator)
>   - Pernille Bølling Hansen (Sub-Investigator)
>   - [Eva Nina Sparre Wandall](https://orcid.org/0009-0008-1745-6027)
>     (Statistician)
>   - Sille Holst (Research Nurse)
>   - [Rune Haubo Bojesen
>     Christensen](https://orcid.org/0000-0002-4494-3399) (Trial
>     Statistician)
>   - [Michael Eriksen Benros](https://orcid.org/0000-0003-4939-9465)
>     (Sponsor and Principal Investigator)
> - **Zenodo DOI archives**:
>   - [Pre-DBL and post-DBL
>     code](https://doi.org/10.5281/zenodo.23341773)
>   - [Tables, Listings and Figures for primary
>     publication](https://doi.org/10.5281/zenodo.23350243) (available
>     after primary publication is published)
>   - [Tables, Listings and Figures for clinical trial
>     report](https://doi.org/10.5281/zenodo.23349388) (available after
>     secondary outcomes are published)
>   - [Statistical Analysis
>     Plan](https://doi.org/10.5281/zenodo.23343482)

------------------------------------------------------------------------

The repository contains all code to produce the pre-specified
statistical analyses. This includes all analyses and results for the
primary publication and pre-specified secondary analyses.

The primary publication of the DEXA-PSYCH trial was published in
[JOURNAL]() and available [here]() (Links will be updated upon
publication.)

The DEXA-PSYCH trial has EU Trial registration number
[2022-501428-45-00](https://euclinicaltrials.eu/ctis-public/view/2022-501428-45-00?lang=en)
with WHO universal Trial Identifier U1111-1280-7614.

The Trial protocol is available
[here](https://euclinicaltrials.eu/ctis-public/view/2022-501428-45-00?lang=en)
in its latest version and an earlier version is presented in [Rømer et
al (2025)](https://doi.org/10.1186/s13063-025-08989-2)

The Statistical Analysis Plan (SAP) was archived and locked in a
persistent and immutable [Zenodo
DOI](https://doi.org/10.5281/zenodo.23343482) prior to unblinding of the
randomization list. The time of unblinding coincides with locking of the
database and is called *database lock* (DBL).

The code is released in two versions:
[pre-dbl-v1.0](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/releases/tag/pre-dbl-v1.0)
and
[post-dbl-v1.0](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/releases/tag/post-dbl-v1.0).
Each version is archived in [Zenodo
DOIs](https://doi.org/10.5281/zenodo.23341773) and any differences
between the two versions of the code base is documented.

All code was written by Senior Statistician Rune Haubo B Christensen and
Statistician and PhD Researcher Eva Nina Sparre Wandall.

## Table, Listings and Figures books

The code in this repository produces two Tables, Listings, and Figures
(TLF) books; a primary publication version and a clinical trial report
(CTR) version. All outcomes are reported from day 0 to day 28. Long term
follow-up (until 6 months after baseline) will be presented separately.

1.  Primary Publication: This version documents all summaries and
    statistical analyses for safety and efficacy endpoints presented in
    the primary publication as documented in the SAP and trial protocol.
    This version is archived in a Zenodo DOI
    [here](https://doi.org/10.5281/zenodo.23350243).
2.  Clinical Trial Protocol (CTR): This version includes all content
    reported in the primary publication version but also presents
    results for additional secondary outcomes. Contents for the
    secondary outcomes will be released upon publication of secondary
    papers presenting the additional secondary outcomes. This version of
    the TLF-book is archived in a Zenodo DOI
    [here](https://doi.org/10.5281/zenodo.23349388).

The primary publication TLF-book is available
[here](https://dexa-psych.netlify.app/) although it is
password-protected for reviewer access until the primary publication is
published.

## Description of the code base

All code is written in **R** using [Quarto](https://quarto.org/) to
produce browsable TLF-books hosted online.

The code is written such that all analyses can be run by execution of
the file: `39_run_all/00_run_all.Rmd` provided there is access to the
raw data. The raw trial data in the form of a `.csv` file and an `.r`
file are held on a secure drive and are not publicly available due to
data privacy restrictions.

The code is structured around the following key elements:

1.  Metadata `.csv`-files that provide structured information on all
    tables, figures and listings.
2.  An automatic analysis pipeline which auto-generates the majority of
    analyses, tables and figures complete with captions, footnotes and
    organized in parts, chapters, sections and subsections.
3.  Manual analyses in separate scripts supplement the automatic
    pipeline. Products of the manual analyses are also included in
    metadata.
4.  A library of custom written **R** functions on which both the
    automatic pipeline, data processing and manual analyses rely.
5.  The **R**-package [data.table](https://r-datatable.com/) is used for
    data handling. The **R**-package [gt](https://gt.rstudio.com/) is
    used to format all tables.
6.  `.qmd`-files for each chapter of each book are auto-generated from
    metadata and points to `html`-tables and `.png`-figures generated by
    the analysis pipeline.

Additionally we write all scripts in the form of **R**-markdown (`.Rmd`)
files. This has the advantage that each script can be executed in a
fresh R-process providing integrity of the results, and that the
resulting `html`-files provide humanly readable results of scripts. The
scripts can be executed using the `rmarkdown` package
`rmarkdown::("my-rmd-file.Rmd")`, by pressing the `knit` button in the
[RStudio editor](https://posit.co/products/open-source/rstudio).

### Structure of repository contents

The code is organized in the following folder structure:

- `01_data_input`: Definitions of supplementary variable names.
- `03_checksums`: SHA-256 checksums for the raw data to confirm identity
  of raw data.
- `10_metadata`: 2 files:
  - `tlf_registry.csv`: Registry of all tables, listings, and figures
    produced by the code.
  - `table_footers.csv`: Footnotes for automatic application to tables
    and figures in the resulting TLF-books.
- `20_functions`: Custom written **R** functions for analyses and
  processing.
- `30_process_data` : Scripts processing the raw data preparing them for
  statistical analysis.
- `32_manual_analyses`: Scripts with analyses that supplement the
  automatic pipeline.
- `34_tlf_pipeline`: Scripts that execute the automatic pipeline
  generating the majority of results. Additional scripts that build on
  results produced earlier in the pipeline are also placed here. The
  final script that runs `quarto` to produce the TLF-books is placed
  last.
- `39_run_all`: The `00_run_all.Rmd` script that executes all code in
  the repo.
- `40_tlf_source`: Files required to produce Quarto-books with results.
- `80_parallel_programming`: In addition to the automatic analysis in
  the pipeline, the primary analysis of the primary endpoint is also
  independently programmed by two separate statisticians (RHBC and
  ENSW). A separate script compares the results of the independently
  programmed scripts to each other and compares these results to those
  produced by the pipeline. An error is reported if any differences in
  results are found.

### Materials produced by the code

Running the code to produce TLF-books with all analyses, tables and
figures generates the following materials:

- `04_data_checks`: QC-checks of data.
- `05_cache`: `.csv`-file with simple cache of unconditional exact
  tests. Computing the confidence interval for the odds-ratio parameter
  takes a substantial amount of time so to save time on subsequent runs
  this cache is generated. The cache has no influence on the results;
  only on the computing time.
- `50_output_db`: data base with information on all statistical results
  from the automatic pipeline stored as `.rds`-files. MMRM-imputed
  datasets are also stored here.
- `60_tlf_book_all`: Internal QC-version of the TLF-book.
- `60_tlf_book_ctr`: CTR-version of TLF-book.
- `60_tlf_book_pp`: Primary publication version of TLF-book.

### Computing environment

The repository is equipped with an
[`renv.lock`](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/blob/main/renv.lock)-file
which makes the **R**-package environment reproducible by capturing all
versions and inter-dependencies. To re-create the **R** environment used
to produce this book,
[clone](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository?tool=desktop)
the
[repository](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH),
open [R](https://www.r-project.org/) and run `renv::restore()`. For
further information see [the renv package
homepage](https://rstudio.github.io/renv/).

## How to cite this work

For citation in published works, please cite the primary publication and
any relevant DOI given above—citation information is available in each
of them. The code base repository also has citation information
available in a [CITATION.cff
file](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/blob/main/CITATION.cff).

## REDCap data export

Raw data are exported from REDCap as follows:

1.  Choose “Data exports, …” under Applications on the left
2.  Choose “Export Data” for export name: “rchr_data”
3.  Choose **R** as export format; Leave all options in the middle
    column un-selected; in the right column choose
    1.  Export gray Form Status fields with blank value
    2.  “, (comma) - default”
    3.  Use period/full stop (.) as decimal for all numbers
4.  Choose “Export Data” (bottom right) and download both **R**-file
    (`*.r`) and data-file (`*.csv`)

When exporting data from Redcap and choosing **R** two files will be
generated:

1.  A data `.csv` file containing all raw data.
2.  An `.r`-code file with

<!-- -->

1)  labels for all variables; these are text strings describing the
    contents of the variable, and
2)  levels of categorical variables (called ‘factors’ in **R**).

- Store both files (which are date and time stamped) under a logged and
  audited Windows network drive.

Rather than running the **R**-code in the `.r`-file we extract the
information on labels and factor levels by computing on the text that is
the content of the file.
