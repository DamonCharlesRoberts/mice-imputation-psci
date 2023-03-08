<h4 align="center">

Replication Code: Connecting Leaves to the Forest

</h4>

<p align="center">

<a href="https://github.com/DamonCharlesRoberts/mice-imputation-psci/commits/main"> <img src="https://img.shields.io/github/last-commit/DamonCharlesRoberts/mice-imputation-psci.svg?style=flat-square&amp;logo=github&amp;logoColor=white" alt="GitHub last commit"/></a> <a href="https://github.com/DamonCharlesRoberts/mice-imputation-psci/issues"> <img src="https://img.shields.io/github/issues-raw/DamonCharlesRoberts/mice-imputation-psci.svg?style=flat-square&amp;logo=github&amp;logoColor=white" alt="GitHub issues"/></a> <a href="https://github.com/DamonCharlesRoberts/mice-imputation-psci/pulls"> <img src="https://img.shields.io/github/issues-pr-raw/DamonCharlesRoberts/mice-imputation-psci.svg?style=flat-square&amp;logo=github&amp;logoColor=white" alt="GitHub pull requests"/></a>

</p>

------------------------------------------------------------------------

Academic project examining the utility of using Random Forest models for multiple imputation with chained equations in political science.

# Code

-   helper.py: Extra user-defined functions to help with some of the calculations done in the paper.
-   helper.R: Extra user-defined functions to help with some of the calculations done in the paper.
-   dcr_rf_imputation_draft_spring_2023.qmd: Compile this file to replicate the paper.
    -   For full replication of the paper, change the yaml meta data option params.reproduce to "true" instead of false.
        -   *Warning*: This will be really computationally intensive.
-   ols.stan: stan model used to fit regressions in paper to measure bias.
-   ols.rds: stored stan object.
-   extensions/: useful partials for the formatting of the manuscript.

# Drafts

-   archived/: older drafts
-   dcr_rf_imputation_fall_2022.pdf: older draft
-   dcr_rf_imputation_draft_spring_2023.qmd: current draft

# renv and .venv

-   renv/: produced with `renv::snapshot()`. Documents packages used in project, their dependencies, and the versions for all of that.
-   .venv/: produced with `poetry`. Documents python packages used in the project, their dependencies, and the versions for all of that.
-   \*.lock: documents environment information for renv and .venv
-   .tomal: documents system information for .venv
-   .Rprofile: requires a loading of the renv information upon R start up in this working directory

# Other stuff

-   .gitattributes: a file that helps sort out repository code language composition
-   .gitignore: do not submit the following files and directories to github.

# Contributors and maintainers

-   [Damon C. Roberts](https://github.com/DamonCharlesRoberts)
