# Load covariates outcomes dataset wide format
data_wide <- readRDS(fs::path(dir_data,
                              "tl_covariates_outcomes_environmentalchemicals_w.RDS"))

# Load covariates outcomes dataset long format
data_long <- readRDS(fs::path(dir_data,
                              "tl_covariates_outcomes_environmentalchemicals_l.RDS"))
# Load processed proteomics data
load(
  fs::path(
    dir_data_proteomcis,
    "TL_proteomics.RData"
  )
)
# solar -------------
solar <- readRDS(fs::path(dir_solar, "SOL_exposure_outcome_data_v4.rds"))

# load solar proteomics
solar_ft <- read_csv(fs::path(dir_solar,"proteomics","SOL_screened_proteomics.csv"))


