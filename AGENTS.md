# Agent & LLM Guide

This repository holds the data, code, and materials for a research paper. If you
are an AI agent or LLM working in this repo, start here.

## What this is

- **Paper:** Exogenous Trends and the Illusion of Learning
- **Authors:** Quentin André, Bart de Langhe
- **Status:** Working paper (unpublished — no DOI yet)
- **Overview:** https://quentinandre.net/posts/learning-with-trends/
- **Citation:** see `CITATION.cff`.

## Layout

`README.md` is the authoritative description of the structure. In brief:
`Studies/` has one subfolder per study (`Study1`…`Study4`, plus `Study3b`,
`Study3c`, `Appendix_DiagCue`, `Appendix_Exec`), each with `Code/`, `Data/`, and
`Materials/`; `Appendices/` is a Quarto book (`AppendixA.qmd`–`AppendixH.qmd`);
`Simulation/` holds Python simulation code; `Results/` holds cached per-study
statistics as JSON; `Figures/` holds the figures.

## Reproducing

This project is primarily **R**, with **Python** for the simulation.

- The online appendices are a Quarto book: install R + Quarto and the R packages
  `tidyverse`, `tidymodels`, `mlogit`, `Hmisc`, `gt`, `glue`, `here`, `lfe`,
  then run `quarto render` from the project root.
- Appendix A (simulation) needs Python with `numpy`, `scipy`, `pandas`,
  `matplotlib`, `seaborn`. All other appendices are R only.
- **Pre-fitted mixed logit models are not included.** Rendering without them
  re-estimates the models, which takes several minutes per study — don't trigger
  a full render to spot-check something.

## Conventions & gotchas for agents

- **Office/PDF files:** `.docx` and `.pdf` are binary — do not read them
  directly. Convert first (e.g., `pandoc file.docx -t markdown`, or a
  PDF-to-markdown tool) and read the output.
- **Qualtrics `.qsf` files** are large JSON. Use targeted `grep`, not full reads.
- **Serialized data:** raw game data is stored as `.rds` (R) and `.pickle`
  (Python); cleaned data is in `.csv`. Prefer the `.csv` and the cached JSON in
  `Results/` over re-running the pipeline.

## License

- Original **code** by the authors: MIT (see `LICENSE`).
- Original **text, figures, and author-collected data**: CC-BY-4.0 (see
  `LICENSE-CC-BY-4.0.txt`).
- Any **third-party data or code** redistributed here remains under its original
  license and terms.
