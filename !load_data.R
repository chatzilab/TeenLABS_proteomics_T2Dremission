# Load processed proteomics data
load(
  fs::path(
    dir_data_proteomcis,
    "TL_proteomics.RData"
  )
)

# Load covariates outcomes dataset wide format
data_wide <- readRDS(fs::path(dir_data,
                              "tl_covariates_outcomes_environmentalchemicals_w.RDS"))

# Load covariates outcomes dataset long format
data_long <- readRDS(fs::path(dir_data,
                              "tl_covariates_outcomes_environmentalchemicals_l.RDS"))

# Load pathways infomation
gene_list <- readRDS(
  fs::path(
    dir_project,
    "0_Data",
    "assay_pathway_KEGG.rds"
  )
)
