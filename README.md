# ICT Investment and Unemployment in Welfare States  
**Author:** Tobias A. Rau  
**Repository:** [GitHub Project Archive](https://github.com/TAR-IT/ict-investment-and-unemployment)

This is a preprint version of a manuscript intended for submission to *Information Economics and Policy*.  
It has not been peer-reviewed or accepted.  
Please do not cite without permission.  

---

## Project Overview

This research project analyzes how national **investments in information and communication technologies (ICT)** affect **unemployment across different educational groups** in OECD and selected partner countries. The empirical analysis is based on **panel data from 2005–2022** and applies **fixed-effects models** with interaction terms for welfare state regimes and time lags (1–8 years), focusing on a **3-year lag** as the preferred specification.

### Research Question

> *How does national ICT investment influence unemployment rates across educational levels in different welfare state regimes?*

### Hypotheses

- **H1:** Countries with higher ICT investment experience lower unemployment among high-skilled workers.
- **H2:** ICT investment increases unemployment among low-skilled workers due to task automation.
- **H3:** Welfare state institutions moderate these effects—Nordic and coordinated market economies reduce polarization, while liberal regimes show stronger inequalities.

---

## Data & Methodology

### Data Sources

- OECD panel data (2005–2022) covering:
  - Unemployment by education
  - ICT investment (% of GDP)
  - GDP per capita
  - Union density
  - Tertiary education share
  - Labor market regulation
- 35 countries including OECD and selected partner states (e.g., Brazil, Bulgaria, Romania, Croatia)

### Statistical Approach

- Fixed-effects panel regression (`plm`) with:
  - Interaction terms: `ICT × welfare state regime`
  - Lags: 0–8 years (preferred: 3-year lag)
  - Controls: GDP per capita, education, labor market regulation, union density
  - Year dummies removed due to multicollinearity (controlled via panel structure)

---

## Repository Structure

### `R/script.R`

- Prepares and cleans all datasets
- Creates lag variables and performs interpolation
- Executes all statistical models (with and without lag)
- Outputs LaTeX-ready tables (`assets/*.tex`)

### `R/data/`

- Contains raw and preprocessed datasets from OECD sources
- Filtering and merging handled via `script.R`

### `TeX/`

- Main LaTeX file: `main.tex` (full academic article)
- Chapters organized in `chapters/`
- Output: article-ready PDF for journal submission

---

## Technologies Used

- [**R**](https://www.r-project.org/)
  - Panel data analysis (`plm`, `dplyr`, `modelsummary`)
  - Visualization (for internal validation, not used in final article)
- [**RStudio**](https://posit.co/download/rstudio-desktop/)
  - Integrated development environment for R
- [**LaTeX**](https://www.latex-project.org/)
  - Used for academic paper typesetting

---

## Reproduction Instructions

1. Clone or download the repository.
2. Open the RStudio project: `R/Data.Rproj`.
3. Run `R/script.R` to prepare the data and generate model tables.
4. Compile the article via `TeX/main.tex` using the recipe:
