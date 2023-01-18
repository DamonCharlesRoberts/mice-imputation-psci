<h4 align="center">Replication Code: Connecting Leaves to the Forest</h4>
<p align="center">
    <a href="https://github.com/DamonCharlesRoberts/imputation-with-random-forests/commits/main">
    <img src="https://img.shields.io/github/last-commit/DamonCharlesRoberts/imputation-with-random-forests.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub last commit"></a>
    <a href="https://github.com/DamonCharlesRoberts/imputation-with-random-forests/issues">
    <img src="https://img.shields.io/github/issues-raw/DamonCharlesRoberts/imputation-with-random-forests.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub issues"></a>
    <a href="https://github.com/DamonCharlesRoberts/imputation-with-random-forests/pulls">
    <img src="https://img.shields.io/github/issues-pr-raw/DamonCharlesRoberts/imputation-with-random-forests.svg?style=flat-square&logo=github&logoColor=white"
         alt="GitHub pull requests"></a>
</p>

--- 
Academic project examining the utility of using Random Forest models for multiple imputation with chained equations in political science. 



# Code

* anes_cleaning.py: Python script to clean the 2020 ANES for the Application section of the paper
* anes_imputation.R: R Script that performs the MI and RF-MICE imputation procedures on the 2020 ANES data.
* anes_models.R: R Script that performs the main models and t-tests using LWD, MI, and RF-MICE procedures on 2020 ANES data.
* introduce_mar.R: R Script that performs the amputation for the simulated data.
* simulated_imputation.R: R Script that performs the imputation procedures on the amputed simulated data.
* simulated_models.R: R Script that performs the main models and data exploration on the simulated data.
* simulation_data.R: R Script that generates the simulated data.
