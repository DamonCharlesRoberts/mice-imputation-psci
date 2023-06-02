# Title: test discrepancy

# Notes:
    #* Description
        #** test the discrepancy function
    #* Updated
        #** 2023-06-01
        #** dcr

# Setup
  #* set the working directory
setwd("./src/")
  #* Load functions
box::use(
    testthat[...]
		, data.table[...]
		, ./R/impute[...]
    , ./R/discrepancy[
        discrepancy
    ]
)
  #* examples
if (file.exists("../data/sim_amputed.RData")) {
		#** population data
	load(
		file = "../data/sim_pop_and_samples.RData"
	)
	df_samples <- df_samples[dataset == 1 | dataset == 2, ]
		#** amputed data
  load(
    file = "../data/sim_amputed.RData"
  )
  df_amputed <- df_amputed[dataset == 1 | dataset==2, ]
} else {
  # simulate population data
  df_pop <- simulate(N = 1000)
  # Simulate multiple random samples
  df_samples <- infer::rep_sample_n(
    tbl = df_pop
    , size = 100
    , replace = TRUE
    , reps = 2
  ) |>
  as.data.table()
  df_samples <- df_samples[
    , dataset := factor(replicate)
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
    , function (x) {
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
  #* define possible procedures
list_procedure_names <- c(
	"mean"
	, "Amelia"
	, "norm"
	, "rf"
	, "miceRanger"
)
	#* do imputation
list_imputed <- lapply(
	list_procedure_names
	, function (x) {
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
		df_temp <- data.table::rbindlist(
			df_temp
			, id = "dataset"
		)
		df_temp <- df_temp[
			, dataset := as.integer(dataset)
		]
	}
)
	#* assign names to list
names(list_imputed) <- list_procedure_names
	#* collapse into data.table
df_imputed <- data.table::rbindlist(
	list_imputed
	, id = "procedure"
)
	#* calculate discrepancy
list_discrepancy <- base::lapply(
	list_procedure_names
	, function (x) {
		df_temp <- discrepancy(
			sample_data = df_samples
			, imputed_data = df_imputed
			, impute_type = x
			, model = FALSE
			, ct_type = "mean"
		)
	}
)
	#* assign names to list
names(list_discrepancy) <- list_procedure_names
	#* collapse into a data.table
df_discrepancy <- data.table::rbindlist(
	list_discrepancy
	, id = "procedure"
)

list_model_discrepancy <- base::lapply(
	list_procedure_names
	, function (x) {
		df_temp <- discrepancy(
			sample_data = df_samples
			, imputed_data = df_imputed
			, impute_type = x
			, model = TRUE
			, ct_type = "mean"
		)
	}
)
	#* assign names to list
names(list_model_discrepancy) <- list_procedure_names
	#* collapse into a data.table
df_model_discrepancy <- data.table::rbindlist(
	list_model_discrepancy
	, id = "procedure"
)