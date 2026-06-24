# Reproducibility Resource: The Forward March of Categorical Tolerance in the United States

This repository contains the data, functions, and analysis notebooks to fully reproduce all findings, figures, and tables in the paper:

> **Lizardo, O. (2026). The Forward March of Categorical Tolerance in the United States. *Sociological Science*, 13, 22-44.**  
> [Read the Paper (Sociological Science)](https://sociologicalscience.com/articles-v13-2-22/) | [Download PDF](https://sociologicalscience.com/download/volume-13/january/SocSci_v13_22to44.pdf)

---

## 📂 Repository Structure

The repository has been structured logically to keep the root tidy and the files easy to navigate:

* **📂 Root Directory**
  * `[analysis-new.qmd](analysis-new.qmd)`: The main analysis notebook reproducing the primary results, tables, and figures from the paper.
  * `[analysis-comp.qmd](analysis-comp.qmd)`: The comparative analysis notebook reproducing the appendix and overtime comparative findings (2012 vs 2025).
  * `[overclaim.qmd](overclaim.qmd)`: Supplemental analysis on overclaiming in the 2012 data.
  * `[sociological-science-categorical-tolerance-followup.Rproj](sociological-science-categorical-tolerance-followup.Rproj)`: R Project file. Open this in your editor (e.g., RStudio or Positron) to automatically configure the working directory via the `here` package.
* **`📂 data/`**: Contains the processed datasets required for the analyses:
  * `cint-prolific-data1.rda`: Primary dataset for 2023/2025 surveys.
  * `cint-prolific-data2.rda`: Cleaned dataset for 2025 comparative analysis.
  * `SSI2012.dta`: Stata dataset for the 2012 comparative survey.
* **`📂 Functions/`**: Active helper R scripts:
  * `dat.genres.R`: Cleans and formats the musical genre taste responses.
  * `dat.demog.R`: Standardizes and cleans the demographic variables.
* **`📂 Plots/`**: Output directory where all reproduced figures (PNG files) are saved.
* **`📂 Tabs/`**: Output directory where all reproduced tables (HTML and PNG versions) are saved.
* **`📂 presentations/`**: Materials for conference/seminar presentations (includes `duke-presentation.qmd`).
* **`📂 manuscript/`**: Source LaTeX files (`manuscript.tex`, `.bib`, `.bst`) for the paper's manuscript.
* **`📂 archive/`**: Legacy scripts (e.g., old Google Sheet data loader) kept for reference but not needed for replication.

---

## 🛠️ Prerequisites & Installation

### 1. Required R Packages
The analyses rely on several standard R packages. You can install all of them at once by running the following command in your R console:

```R
install.packages(c(
  "conflicted", "here", "dplyr", "tidyr", "ggplot2", 
  "kableExtra", "ggeffects", "sjPlot", "pscl", "haven", 
  "webshot", "marginaleffects"
))
```

### 2. Rendering Table PNGs (Optional)
The Quarto scripts generate HTML tables and automatically attempt to save high-resolution `.png` screenshots of them in the `Tabs/` folder using the `webshot` package. This requires **PhantomJS**. 

If you want to save table PNGs, run this in your R console:
```R
webshot::install_phantomjs()
```
*Note: If PhantomJS is not installed, the scripts will run perfectly and compile without errors, but will skip exporting the table PNGs (the HTML tables will still render correctly in your Quarto viewer).*

---

## 🚀 Step-by-Step Reproduction Guide

To reproduce the findings, follow these simple steps:

1. **Clone the Repository**:
   Clone this repository to your local machine using git or download it as a ZIP file.
2. **Open the Project**:
   Double-click `sociological-science-categorical-tolerance-followup.Rproj` to open the project in your favorite R IDE (RStudio, Positron, etc.). This ensures the `here` package can resolve all paths relative to the project root.
3. **Install Dependencies**:
   Install the R packages listed above.
4. **Render the Analyses**:
   You can run individual code blocks interactively or render the entire Quarto document to compile a beautiful, integrated HTML report:
   * **Primary Findings**: Open and render `[analysis-new.qmd](analysis-new.qmd)`.
   * **Comparative Appendix Results**: Open and render `[analysis-comp.qmd](analysis-comp.qmd)`.
   * **Supplemental Work**: Open and render `[overclaim.qmd](overclaim.qmd)`.

Upon rendering/running, all tables and figures will be automatically saved to the `Tabs/` and `Plots/` folders, matching the published article exactly.
