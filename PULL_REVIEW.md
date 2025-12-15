
# Pull Request Scientific Review Checklist

Use this checklist when reviewing MRs that add scientific content (methods, models, datasets).

## Scientific soundness
- [ ] RTM–ML link is correct (e.g., PROSAIL/SCOPE coupling)
- [ ] Equations or algorithms are documented in `docs/METHODS.md`
- [ ] References are cited (paper or dataset DOI)

## Datasets
- [ ] Dataset registered in `docs/DATASETS.md`
- [ ] Provenance and licensing clarified
- [ ] Small reproducible sample added to `data/processed/` if possible

## Reproducibility
- [ ] New code runs with `environment.yml` or `renv`
- [ ] Example notebook included under `notebooks/`
- [ ] Unit test or validation metric provided

## Governance
- [ ] Labels and milestone assigned
- [ ] CODEOWNERS tagged
