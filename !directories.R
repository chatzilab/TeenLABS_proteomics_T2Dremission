# Directory

# home directory for project
dir_home <- here::here() |> 
  dirname() |> dirname() |> fs::path()

# project folder
dir_project <- here::here() |> fs::path()

# data folder
dir_data <- fs::path(dir_home,"2_Cleaned Data")

dir_data_proteomcis <- fs::path(
  dir_home,
  "2_Cleaned Data",
  "Proteomics"
)

# report folder
dir_report <- fs::path(dir_project, "2_Reports")

# figure folder
dir_figure <- fs::path(dir_project, "3_Figures")
