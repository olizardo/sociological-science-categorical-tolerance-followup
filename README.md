# Reproducibility Resource: The Forward March of Categorical Tolerance in the United States

This repository contains the complete replication materials—data, code, manuscripts, and presentation slides—for the paper **"The Forward March of Categorical Tolerance in the United States"** by Omar Lizardo.

> **Lizardo, O. (2026). The Forward March of Categorical Tolerance in the United States. *Sociological Science*, 13, 22-44.**  
> [Read the Paper (Sociological Science)](https://sociologicalscience.com/articles-v13-2-22/) | [Download PDF](https://sociologicalscience.com/download/volume-13/january/SocSci_v13_22to44.pdf)

---

## 📂 Repository Structure

The project directory is structured as follows:

```text
├── analysis-new.qmd                # Main analysis notebook reproducing primary results
├── analysis-comp.qmd               # Comparative analysis notebook (2012 vs 2025)
├── overclaim.qmd                   # Supplemental analysis on overclaiming in 2012 data
├── sociological-science-categorical-tolerance-followup.Rproj
├── data/                           # Processed datasets (.rda and .dta)
├── Functions/                      # Active helper R scripts (e.g., dat.genres.R)
├── Plots/                          # Output directory for reproduced figures (PNGs)
├── Tabs/                           # Output directory for reproduced tables (HTML and PNGs)
├── presentations/                  # Materials for conference/seminar presentations
├── manuscript/                     # Source LaTeX files for the paper manuscript
└── archive/                        # Legacy scripts kept for reference
```

---

## 🛠️ Prerequisites & Installation

To run the reproducibility workflow, you will need **R** and the **Quarto** CLI (pre-installed in Positron and RStudio).

### 1. Required R Packages
Ensure you have the required R packages installed. You can install them by running the following command in your R console:

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

## 🚀 How to Reproduce the Findings

1. **Clone the Repository**: Clone this repository to your local machine using git or download it as a ZIP file.
2. **Open the Project**: Open the `.Rproj` or `.R` file in your editor (e.g., RStudio or Positron). This ensures paths are resolved correctly relative to the project root.
3. **Install Dependencies**: Ensure the packages listed above are installed.
4. **Run the Computational Pipeline**:
   * **Using the Command Line (Quarto CLI)**:
     ```bash
     quarto render analysis.qmd
     ```
   * **Using R**:
     ```R
     quarto::quarto_render("analysis.qmd")
     ```
   * Or run interactively in your IDE. This step ensures that all tables and figures are updated directly from the code, guaranteeing that the numbers in the paper are exactly what the code computes.

---

## 📝 License & Citation

If you use the materials or code in this repository, please cite the paper:

```bibtex
@article{lizardo2026forward,
  title={The Forward March of Categorical Tolerance in the United States},
  author={Lizardo, Omar},
  journal={Sociological Science},
  volume={13},
  pages={22--44},
  year={2026}
}
```
