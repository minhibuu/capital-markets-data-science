# Capital Markets Data Science

Group coursework at the Technical University of Munich, covering market transaction analysis, retirement simulations, and empirical asset pricing.

**Authors** — Bui Ngoc Minh Quach, Yen-Fang Tseng, Benjamin Phumin Schmidhalter, and Nikolas Arent. The submitted Python notebook names David Osterrieder as instructor and gives a January 2025 submission date.

These projects were developed jointly. Individual contributions are unspecified in the supplied files.

## Projects

| Project | Contents | Required access |
| --- | --- | --- |
| [Python notebook](python/market-data-and-retirement.ipynb) | Transaction preparation, euro volume construction, trading-hour and weekday indicators, outlier filtering, OLS regression, and a Monte Carlo retirement exercise | Course transaction data for Task 1, Yahoo Finance access for Task 2 |
| [R analysis](r/asset-pricing.R) | Rolling CAPM betas, growth and operating-leverage signals, decile portfolios, weighting and filtering comparisons, Newey–West inference, and Fama–French five-factor regressions over 1980–2023 | An authorized WRDS account with the relevant CRSP and Compustat subscriptions |

## Run the Python notebook

Use Python 3.11 or 3.12. From the repository root, create a virtual environment and install the dependencies.

```bash
python3 -m venv .venv
source .venv/bin/activate
python -m pip install -r python/requirements.txt
cd python
jupyter lab
```

Place authorized local copies of `LSEDATA_2022.parquet` and `Instruments_LSEDATA_2022.csv` inside `python/data/`. Open the notebook and execute Task 1 in order.

Task 2 can run independently after the import cell. Run its cells in order and answer the interactive prompts. Its defaults are illustrative coursework inputs. The simulation has no fixed random seed and uses historical SPY data, so estimates can differ across runs.

## Run the R analysis

Open an R session with `r/` as the working directory.

```r
source("install-packages.R")
# Configure WRDS access locally using tidyfinance's credential setup.
source("asset-pricing.R")
```

Use an R version supporting the native pipe and a current dplyr version supporting `join_by()`. Configure WRDS credentials locally before running the analysis. The script downloads data and writes a local SQLite database inside `r/data/`. It uses parallel workers and can require substantial memory and runtime. Package versions are unpinned, so future API changes may require updates.

## Interpretation and limitations

The files preserve the coursework's analytical approach. The Python regression is observational. Its coefficients describe associations. The retirement exercise simplifies investment returns, withdrawals, inflation, and mortality.

The R variable called asset growth is calculated from monthly market capitalization changes. The operating-leverage merge uses calendar year without a reporting-availability lag. The long–short return calculation subtracts decile 1 from decile 10, while its original comment describes the reverse direction. The quantity labeled total return sums monthly returns. These details affect interpretation and should be reviewed before extending the results.

## Data and privacy

Course datasets, WRDS extracts, databases, credentials, student numbers, and notebook outputs are excluded from this release. Authorized users must obtain the data separately. Git ignores local data directories and common export formats. Clear notebook outputs before committing changes after executing with restricted data.

## Attribution

The R submission explicitly states that much of its code and explanations was adapted from Tidy Finance. The original source comments and references are retained. Consult the upstream terms when reusing adapted material.

- [Tidy Finance](https://www.tidy-finance.org/)
- [Tidy Finance chapter on WRDS, CRSP, and Compustat](https://www.tidy-finance.org/r/wrds-crsp-and-compustat.html)
- [Chen and Zimmermann's Open Source Asset Pricing project](https://www.openassetpricing.com/)
- [Guo et al. paper referenced in the submission](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4990134)
- [Social Security Administration life table referenced in the Python submission](https://www.ssa.gov/oact/STATS/table4c6.html)

The Python mortality arrays retain the submitted values. The linked life table may have changed since submission.

## Release preparation

The Python notebook comes from the previously supplied ZIP. Outputs and environment metadata were cleared, dependency installation was moved outside the notebook, and input files now load from `python/data/`. The Yahoo Finance call explicitly requests adjusted-close data and a flat column structure.

The R script comes from the local course project folder. Student numbers were removed, package installation was moved to a separate setup script, and parallel setup now uses explicit namespaces and at least one worker.

Python cells and R syntax were checked. Full empirical execution requires the restricted datasets and WRDS access. This release has no added blanket license.
