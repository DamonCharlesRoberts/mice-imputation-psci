# Title: testing simulate

# Notes:
    #* Description
        #** Test of the simulate function
    #* Updated
        #** 2023-06-01
        #** dcr
    
# Setup
    #* Set working directory
setwd("../src/")
    #* load simulate function
box::use(
    testthat[...]
    , ./R/simulate[
        simulate
    ]
)
    #* Run example
N <- 1000
df_pop <- simulate(N = N)

# Tests
    #* Number of rows should match size desired
test_that(
    "row size matches"
    , {
        expect_true(nrow(df_pop) == N)
    }
)
    #* There should be 6 columns
test_that(
    "columns match"
    , {
        expect_true(ncol(df_pop) == 6)
    }
)
