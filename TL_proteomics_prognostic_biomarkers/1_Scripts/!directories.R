##### directories #########

# TL Home directory
dir_home <- here::here() %>% dirname() %>% dirname()  %>% dirname() %>% dirname()

# Project Directory
dir_project <- here::here()


dir_data_proteomcis <- fs::path(
  dir_home,
  "2_Cleaned Data",
  "Proteomics"
)
#  Data location of outcomes and covariates
dir_data <- fs::path(dir_home,
                     "2_Cleaned Data") 

# Temp results folder
dir_temp <- fs::path(dir_project,
                     "0_Data")

# figures folder
dir_figures <- fs::path(dir_project,
                        "3_Figures")
# reports folder
dir_reports <- fs::path(dir_project,
                        "2_Reports")

# external cohort folder
dir_external <- dir_home %>% dirname() 
dir_solar <- fs::path(dir_external, "Env Chem SOL-CHS", "Analysis", 
                    "1_analysis_ready_data")