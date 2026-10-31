
<!-- The .md file is generated from an .Rmd file. Please edit the .Rmd; not the .md !! -->

# Changelog

**Changes to the analysis code and results for the DEXA-PSYCH trial are
documented here.**

- Pre-specified and pre-programmed results of the statistical analyses
  for the DEXA-PSYCH trial are available in in browsable web-books
  containing the so-called Tables, Listings, and Figures (TLF-books).
- Additional post-hoc results will be made available when produced.
- Archived code or results are never edited, tags are not moved,
  archived books are not replaced, and corrections appear as new
  releases.
- Tags identify snapshots of the code base.
- Releases are made primarily to publish new results or to correct
  existing results. Note that all releases are tags but not all tags are
  releases.

**Tags are versioned by the kind of change:**

| Type        | Meaning                                             | Version  |
|-------------|-----------------------------------------------------|----------|
| Maintenance | Code or environment changes. Results are unchanged. | `v1.0.x` |
| Addition    | New results. Existing results are unchanged.        | `v1.x.0` |
| Correction  | Visible changes to presented results.               | `vx.0.0` |

<!-- ## [v1.0.2-draft](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/main) --- 2026-10-31 (no release) -->

<!-- - **Type:** Maintenance -->

<!-- - **Summary:**: Update of computing environment (only) -->

<!-- - **Corrections:** None -->

<!-- - **Additions:** None -->

<!-- - **Major changes:**  -->

<!--   1. R updated from 4.5.2 to 4.6.0 (newest available at workplace) -->

<!--   2. All R packages updated to newest available version and `renv.lock` file updated. -->

<!-- - **Minor changes:** None -->

<!-- - **Effect on results:**  -->

<!--   - Reproducibility checks showed no differences in any published results to `v1.0.1`. -->

<!--   - The attributes of some model objects (MMRM) stored alongside the datasets changed due to package updates. This had no influence on the analysis results to a tolerance of $10^{-6}$ and consequently no number in the published results changed due to this update of the computing environment. -->

<!-- - **Commit:** [HEAD](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/main) -->

<!-- - **Changes to code:** [compare-view](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/compare/v1.0.1...main) -->

<!-- - **Environment:** R version 4.6.0 (2025-10-31 ucrt), Quarto 1.9.38, Pandoc 3.8.3. -->

## [v1.0.2](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/v1.0.2) — 2026-10-31 (no release)

- **Type:** Maintenance
  - Update of computing environment (only)
- **Corrections:** None
- **Additions:** None
- **Major changes:**
  1.  R updated from 4.5.2 to 4.6.0 (newest available at workplace)
  2.  All R packages updated to newest available version and `renv.lock`
      file updated.
- **Minor changes:** None
- **Effect on results:**
  - Reproducibility checks showed no differences in any published
    results to `v1.0.1`.
  - The attributes of some model objects (MMRM) stored alongside the
    datasets changed due to package updates. This had no influence on
    the analysis results to a tolerance of $10^{-6}$ and no number in
    the published results changed due to this update of the computing
    environment.
- **Commit:**
  [main](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/main)
- **Changes to code:**
  [compare-view](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/compare/v1.0.1...v1.0.2)
- **Environment:** R version 4.6.0 (2025-10-31 ucrt), Quarto 1.9.38,
  Pandoc 3.8.3.

## [v1.0.1](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/v1.0.1) — 2026-10-30 (no release)

- **Type:** Maintenance
- **Corrections:** None
- **Additions:** None
- **Major changes:**
  1.  Save `rds` files of results of manual analyses to enable
      reproducibility checks at full precision (checks at the displayed
      precision levels is already covered by `html` tables.)
  2.  Add Code to perform reproducibility checks of all results and
      derived data.
- **Minor changes:**
  1.  Add Change-log to repo.
  2.  `renv` lock file update with packages 3 packages (`httr`, `rvest`,
      `selectr`)
- **Effect on results:** None
  - Reproducibility checks showed no differences to `post-dbl-v1.0`.
- **Commit:**
  [0c7de3369788b7c3ba7997fbba1829294d272898](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/0c7de3369788b7c3ba7997fbba1829294d272898)
- **Changes to code:**
  [compare-view](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/compare/post-dbl-v1.0...v1.0.1)
- **Environment:** R version 4.5.2 (2025-10-31 ucrt), Quarto 1.9.38,
  Pandoc 3.8.3.

## [v1.0.0](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/post-dbl-v1.0) — 2026-10-23 (release [post-dbl-v1.0](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/releases/tag/post-dbl-v1.0))

- **Type:** Baseline—first post-database-lock release.
- **Summary:** Analysis code as used for the post-DBL analyses.
- **Major changes:** None
- **Minor changes:** Details are [in the release
  notice](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/releases/tag/post-dbl-v1.0)
  1.  Change `import_randomization_list` flag from `FALSE` to `TRUE` to
      apply the true randomization variable.
  2.  Fix minor bug to display the testing hierarchy correctly.
- **Commit:**
  [3d17df8c7c692590417dff431ec1718ebbe7bcd5](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/3d17df8c7c692590417dff431ec1718ebbe7bcd5)
- **Changes to code:**
  [compare-view](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/compare/pre-dbl-v1.0...post-dbl-v1.0)
- **Zenodo code archive:** <https://doi.org/10.5281/zenodo.23587985>
- **Environment:** R version 4.5.2 (2025-10-31 ucrt), Quarto 1.9.38,
  Pandoc 3.8.3.

## [Pre-DBL](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/pre-dbl-v1.0) — 2026-10-21 (release [pre-dbl-v1.0](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/releases/tag/pre-dbl-v1.0))

- **Type:** Baseline—blinded treatment assignment.
- **Summary:** Analysis code locked before unblinding.
- **Commit:**
  [c31f6a9f5a207e5bda68b4d3a6df0887d973e3a3](https://github.com/Biological-and-Precision-Psychiatry/DEXA-PSYCH/tree/c31f6a9f5a207e5bda68b4d3a6df0887d973e3a3)
- **Changes to code:** Baseline.
- **Zenodo code archive:** <https://doi.org/10.5281/zenodo.23341773>
- **Environment:** R version 4.5.2 (2025-10-31 ucrt), Quarto 1.9.38,
  Pandoc 3.8.3.
