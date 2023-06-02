# Title: 2022 CES cleaning

# Notes:
    #* Description
        #** Cleaning of the 2022 CES
    #* Updated
        #** 2023-07-02
        #** dcr

# Setup
    #* Modularly load relevant packages
box::use(
    data.table[...]
)
    #* Load CES data
df_ces_original <- read.csv(
    file = "../data/2020_cces/CES22_Common.csv"
) |>
as.data.table()

# Cleaning
col_timing_cols <- grep("tim", names(df_ces_original))
df_ces_clean <- df_ces_original[
    #* remove columns reporting page times
    , - ..col_timing_cols
][
    #* remove columns that are not numeric
    , .SD
    , .SDcols = is.numeric
    
][
    # remove some repeated post study columns
    , -regzip_confirm_post:-region
][
    , -caseid:-add_confirm
]

col_post_cols <- grep("_post", names(df_ces_clean))
df_ces_clean <- df_ces_clean[
    #* remove columns that are repeats for post study
    , - ..col_post_cols
]

# Store cleaned version of dataset
save(
    df_ces_clean
    , file = "../data/2020_cces/cleaned.RData"
)
