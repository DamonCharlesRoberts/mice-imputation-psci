# Title: test impute

# Notes:
    #* Description
        #** Test the impute function
    #* Updated
        #** 2023-06-01
        #** dcr

# Setup
    #* Set working directory
setwd("./src/")
    #* Load functions
box::use(
    testthat[...]
    , data.table[...]
    , ./R/impute[...]
)
    #* Example
if (file.exists("../data/sim_amputed.RData")) {
    load(
        file = "../data/sim_amputed.RData"
    )
    df_amputed <- df_amputed[dataset == 1 | dataset == 2, ]
} else {
    # Simulate population data
    df_pop <- simulate(N = 1000)
    # Simulate mutlple random samples
    df_samples <- infer::rep_sample_n(
        tbl = df_pop
        , size = 100
        , replace = TRUE
        , reps = 2
    ) |>
    as.data.table()
    df_samples <- df_samples[
        , dataset  := factor(replicate)
    ][
        , replicate := NULL
    ]
    # ampute the samples
    list_ampute_prep <- split(
        df_samples
        , f = df_samples$dataset
    )
    # Perform amputation on each sample
    list_amputed <- lapply(
        list_ampute_prep
        , function(x) {
            df_amputed_temp <- amputeData(
                data = x
                , perc = 0.4
                , cols = c("X", "Z", "Y")
            )
        }
    )
    # Combine the list into one data.table
    df_amputed <- data.table::rbindlist(list_amputed)
}

# Define possible procedures
list_procedure_names <- c(
    "mean"
    , "Amelia"
    , "norm"
    , "rf"
    , "miceRanger"
)

# Do imputation
list_imputed <- lapply(
    list_procedure_names
    , function(x) {
        if (x == "Amelia" | x == "miceRanger") {
            df_temp <- impute(
                data_frame = df_amputed
                , package = x
            )
        } else {
            df_temp <- impute(
                data_frame = df_amputed
                , package = "mice"
                , meth = x
            )
        }    
    }
)

# Tests
    #* Should have 5 list elements
test_that(
    "5 list elements"
    , {
        expect_true(
            length(list_imputed) == length(list_procedure_names)
        )
    }
)
    #* Should have 2 dataframes in each list element
test_that(
    "2 list elements in each list element"
    , {
        expect_true(
            length(list_imputed[[1]]) == 2
        )
    }
)
    #* They should be data.table objects in each list element
test_that(
    "expect data.table"
    , {
        expect_s3_class(
            list_imputed[[1]][[1]], "data.frame"
        )
    }
)
    #* The data.frame should be 1000 rows, 100 per 10 datasets
test_that(
    "expect 100 rows"
    , {
        expect_true(
            nrow(list_imputed[[1]][[1]]) == 1000
        )
    }
)