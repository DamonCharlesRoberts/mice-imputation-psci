"""
Title: Functions script

Notes:
    - Description: Script that contains user-defined functions for rf-mice project
    - Updated: 2023-01-18
        - by: dcr
"""
# Import dependencies
import numpy as np
import polars as pl
import duckdb as db
import miceforest as mf
import plotly.figure_factory as ff
from itertools import repeat
# Define functions
def create_iter_table(data_frame, name_append, engine, i):
    """
    Store list of pl.DataFrame objects in duckdb database

    Parameters
    -----
    list_obj: list 
        - a list object containing pl.DataFrame elements

    name_append: str
        - a string to append the table name
    
    engine: str
        - an engine connected to a duckdb database

    Returns
    -----
    Commits a list of pl.DataFrame objects to a duckdb file
    """
    # Convert the polars dataframe to an arrow object
    data = data_frame.to_arrow()
    # add the name and the dataframe number
    name = str(name_append) + "_" + str(i)
    # add to table
    engine.execute("CREATE OR REPLACE TABLE " + name + " AS SELECT * FROM data")

def simulate(num_obs, samples = 1000):
    """
    Simulation data

    Parameters
    -----
    num_obs: int 
        - number of observations

    num_vars: int
        - number of features

    Returns
    -----
    A list of pl.DataFrame objects
    """
    # N = size of population
    N = 1000000
    # calculate data
    a = np.random.gamma(2, 2, N)
    b = np.random.binomial(1, 0.6, N)
    x = 0.2 * a + 0.5 * b + np.random.normal(0, 10, N)
    z = 0.9 * a * b + np.random.normal(0, 10, N)
    y = 0.6 * x + 0.9 * z + np.random.normal(0, 10, N)
    # add these data to a pd.DataFrame
    pop_data_frame = pl.DataFrame({
        'A':a,
        'B':b,
        'X':x,
        'Z':z,
        'Y':y,
        'id': range(1, N+1)
    })
    # Get my specified number of samples from this population data
    data_frame = list(repeat(pop_data_frame.sample(n = num_obs), samples))
    # return the sample and population dataframes
    return pop_data_frame, data_frame

def ampute(data_frame, perc, random_state):
    """
    Ampute the data

    Parameters
    -----
    data_frame: pl.DataFrame
        - dataframe input to ampute
   
    perc: float
        - the percent of missingness to introduce

    random_state: int
        - the seed

    Returns
    -----
    A list of pl.DataFrame objects
    """
    # grab the id column and turn it into a series object
    id_series = data_frame.get_column("id")
    # exclude the id column from the dataframe to be amputed
    data_frame = data_frame.select(pl.exclude("id"))
    # convert the dataframe into a pandas object
    original = data_frame.to_pandas()
    # use produce_na to introduce missingness
    amputed = mf.utils.ampute_data(original, perc = perc, random_state = random_state)
    # take missing data and put in polars.DataFrame
    data_frame = pl.DataFrame(amputed, schema = ['A', 'B', 'X', 'Z', 'Y'])
    # merge the id series back into the dataframe
    data_frame = data_frame.insert_at_idx(0, id_series)
    # return the dataframe
    return data_frame

#def distplot(input_list,names,colors):
#    """
#    Transform the data into a list of lists to then feed into a plotly distplot
#
#    Parameters
#    ----
#    - list: list object containing polars objects
#    - names: list of dataset names
#    - colors: list of hex values
#
#    Returns
#    ----
#    plt.create_dist object
#    """
#    # convert list of dataframes into a list of lists
#    list_o_list = [[c.to_list() for c in i] for i in input_list]
#
#    ## grab the list elements for each variable of interest
#    x_list = [x[2] for x in list_o_list]
#    z_list = [x[3] for x in list_o_list]
#    y_list = [x[4] for x in list_o_list]
#    ## Make plot wit it
#    fig_x = ff.create_distplot(
#        x_list,
#        names,
#        colors=colors,
#        show_hist=False,
#        show_rug=False
#    )
#    fig_z = ff.create_distplot(
#        z_list,
#        names,
#        colors=colors,
#        show_hist=False,
#        show_rug=False
#    )
#    fig_y = ff.create_distplot(
#        y_list,
#        names,
#        colors=colors,
#        show_hist=False,
#        show_rug=False
#    )
#    return fig_x,fig_z,fig_y
#def transformation(data_frames, datasets):
#    """
#    Transform the data from list of pl.DataFrames to pl.DataFrame for graphing
#
#    Parameters
#    ----
#    data_frames: list
#        - A list of pl.DataFrame objects
#    datasets: int
#        - An integer of number of samples produced
#    
#    Returns
#    ----
#    x_list, z_list, y_list: pl.DataFrame
#        - Three pl.DataFrames that combine the x, y, and z variables across samples produced
#    """
#    x_list = pl.DataFrame()
#    z_list = pl.DataFrame()
#    y_list = pl.DataFrame()
#    for i in range(datasets):
#        col_x = data_frames[i].select("X").rename({"X": "Dataset " + str(i)})
#        x_list = x_list.hstack(col_x)
#        col_z = data_frames[i].select("Z").rename({"Z": "Dataset " + str(i)})
#        z_list = z_list.hstack(col_z)
#        col_y = data_frames[i].select("Y").rename({"Y": "Dataset " + str(i)})
#        y_list = y_list.hstack(col_y)
#    return x_list, z_list, y_list

#def impute(data_frame, datasets, save_all_iterations=True, random_state = 902010):
#    """
#    Impute the data with lightGBM
#
#    Parameters
#    -----
#    data_frame: pl.DataFrame
#        - A pandas DataFrame object with missingness to impute
#
#    datasets: int
#        - An integer for the number of datasets to generate
#
#    save_all_iterations: bool
#        - A boolean for whether you should store each dataframe or write over original
#
#    random_state: int
#        - An integer for the random state
#
#    Returns
#    -----
#    imputed: A list of imputed pl.DataFrame objects
#    
#    total_time: A int of total time it took to execute
#    """
#    # rename data_frame
#    amputed = data_frame
#    # start kernel for imputation
#    kernel = mf.ImputationKernel(
#        amputed,
#        datasets=datasets,
#        save_all_iterations=save_all_iterations,
#        random_state=random_state
#    )
#    # perform mice on the kernel
#    kernel.mice(datasets)
#    # take the completed datasets from the imputation procedure
#    imputed = list(map(lambda x: kernel.complete_data(dataset=x), range(datasets)))
#    # return the list of imputed datasets
#    return imputed
#
#def mean_benchmarks(procedure):
#    """
#    calculate the mean time for a specified imputation procedure to complete
#
#    Parameters
#    ----
#    procedure: str
#        - name of imputation procedure
#
#    Returns
#    ----
#    bench_mean: int
#        - mean of the imputation procedure
#    """
#    con = db.connect("../data/benchmarks.db", read_only = True)
#    df = con.execute(
#        f'''
#        SELECT procedure, seconds/1000 AS mean_seconds FROM benchmark WHERE procedure ILIKE '%{procedure}%';
#        '''
#    ).arrow()
#    po = pl.from_arrow(df)
#    return po