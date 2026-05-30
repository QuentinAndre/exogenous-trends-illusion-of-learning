This repository contains the data, code, and materials for the paper "Exogenous Trends and the Illusion of Learning". It is organized into eight studies, a simulation, and a set of online appendices.

## Repository Structure

### `Studies/`

Contains one subfolder per study: `Study1`, `Study2`, `Study3`, `Study3b`, `Study3c`, `Study4`, `Appendix_DiagCue`, and `Appendix_Exec`. Each study folder has three subfolders:

- `Code/`: R scripts for data processing (`DataProcessing.R`) and statistical analysis (`DataAnalysis_*.R`).
- `Data/`: raw and cleaned data files.
- `Materials/`: survey instruments, pre-registration documents, and game parameter files.

### `Appendices/`

Eight Quarto documents (`AppendixA.qmd`–`AppendixH.qmd`) that generate the online appendices. Rendered output (HTML and PDF) is written to `Appendices/_output/`.

### `Simulation/`

Python simulation code (`Simulation_Setup.ipynb`) and results (`Simulation_Results_*.csv`). The R script `Simulation_Analysis.R` analyzes the simulation output.

### `Results/`

Pre-computed statistical summaries in JSON format, one file per study.

### `Figures/`

Publication-ready figures in PNG format.

## File Types

| Extension        | Purpose                                                   |
| ---------------- | --------------------------------------------------------- |
| `.R`             | Data processing and statistical analysis scripts          |
| `.qmd`           | Quarto documents for the online appendices                |
| `.csv`           | Cleaned data, game parameters, and simulation results     |
| `.rds`           | R serialized objects: raw game data and pre-fitted models |
| `.pickle`        | Python-format raw game data                               |
| `.qsf` / `.docx` | Survey instruments (Qualtrics export and Word format)     |
| `.pdf`           | Pre-registration documents                                |
| `.json`          | Cached statistical results                                |

## Reproducing the Online Appendices

The online appendices are a Quarto book. To render them:

1. Install [R](https://www.r-project.org/) and [Quarto](https://quarto.org/).
2. Install the required R packages: `tidyverse`, `tidymodels`, `mlogit`, `Hmisc`, `gt`, `glue`, `here`, `lfe`.
3. From the project root, run:

```bash
quarto render
```

Appendix A (simulation code) requires Python with `numpy`, `scipy`, `pandas`, `matplotlib`, and `seaborn`. All other appendices use R only.

Pre-fitted mixed logit models were not included in the repository for full reproducibility check. Rendering without cached models will re-estimate them, which takes several minutes per study.

## Citation

This is a working paper. To cite it, please cite the manuscript (see [`CITATION.cff`](CITATION.cff) for machine-readable metadata):

> André, Q., & de Langhe, B. (2024). *Exogenous Trends and the Illusion of Learning*. Working paper.

Overview: https://quentinandre.net/posts/learning-with-trends/

## License

- Original **code** by the authors is released under the [MIT License](LICENSE).
- Original **text, figures, and author-collected data** are released under [CC-BY-4.0](LICENSE-CC-BY-4.0.txt).
- Any **third-party data or code** redistributed here remains under its original license and terms.
