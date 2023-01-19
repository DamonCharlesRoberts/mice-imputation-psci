"""
Title: Functions script

Notes:
    - Description: Script that contains user-defined functions for rf-mice project
    - Updated: 2023-01-18
        - by: dcr
"""
# Load dependencies
    #* from env
import numpy as np
import pandas as pd
import miceforest as mf
    #* User defined
from produce_na import produce_na
import utils

def create_iter_table(data_frame, name_append, engine, i):
    """
    Store list of pd.DataFrame objects in duckdb database

    Parameters
    -----
    list_obj: list
                a list object containing pd.DataFrame elements
    name_append: str
                 a string to append the table name
    engine: str
            an engine connected to a duckdb database

    Returns
    -----
    Commits a list of pd.DataFrame objects to a duckdb file
    """
    name = str(name_append) + "_" + str(i)
    engine.execute("CREATE OR REPLACE TABLE " + name + " AS SELECT * FROM data_frame")

def simulate(num_obs, num_vars = 5):
    """
    Simulation data

    Parameters
    -----
    num_obs: int
                number of observations
    num_vars: int
                number of features

    Returns
    -----
    A list of pd.DataFrame objects
    """
    mean = np.random.random(num_vars)
    cov = np.random.random((num_vars, num_vars))
    dist = np.random.multivariate_normal(mean, cov, num_obs)
    data_frame = pd.DataFrame(dist, columns=['A', 'B', 'X', 'Z', 'Y'])
    return data_frame

def ampute(data_frame, p_miss, p_obs, mecha):
    """
    Ampute the data

    Parameters
    -----
    data_frame: pd.DataFrame
                dataframe input to ampute
    p_miss: float
            the percent of missingness to introduce
    p_obs: float
            the percent of variables without missingness
    Returns
    -----
    A list of pd.DataFrame objects
    """
    original = data_frame
    amputed = produce_na(original, p_miss=p_miss, p_obs=p_obs, mecha = mecha)
    data_frame = pd.DataFrame(amputed["X_incomp"].numpy(), columns = ['A', 'B', 'X', 'Z', 'Y'])
    return data_frame

def impute(data_frame, datasets, save_all_iterations=True, random_state = 902010):
    """
    Impute the data with lightGBM

    Parameters
    -----
    data_frame: pd.DataFrame
                A pandas DataFrame object with missingness to impute
    datasets: int
                An integer for the number of datasets to generate
    save_all_iterations: bool
                A boolean for whether you should store each dataframe or write over original
    random_state: int
                An integer for the random state 
    Returns
    -----
    imputed: A list of imputed pd.DataFrame objects
    total_time: A int of total time it took to execute
    """
    amputed = data_frame
    kernel = mf.ImputationKernel(
        amputed,
        datasets=datasets,
        save_all_iterations=save_all_iterations,
        random_state=random_state
    )
    kernel.mice(datasets)
    imputed = list(map(lambda x: kernel.complete_data(dataset=x), range(datasets)))
    return imputed
