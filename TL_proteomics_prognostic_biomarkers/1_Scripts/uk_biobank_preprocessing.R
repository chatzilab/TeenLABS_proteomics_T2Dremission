# load uk biobank results

## feature annotation in UKB
ft_table <- read_excel(fs::path(dir_temp, "uk_biobank_prior_results_diabetes.xlsx"), 
                                                sheet = "annotation", skip =3) %>% janitor::clean_names()
## p-value table from table S5
results_summary <- read_excel(fs::path(dir_temp, "uk_biobank_prior_results_diabetes.xlsx"), 
                       sheet = "adjusted_cox", skip =6, col_names = T) %>% filter(Outcome == c("Type 2 diabetes")) %>%
  janitor::clean_names()

# minimal adjusted table 
# results_summary <- read_excel(fs::path(dir_temp, "uk_biobank_prior_results_diabetes.xlsx"),
#                               sheet = "minimal_adjusted", skip =6, col_names = T) %>% filter(Outcome == c("Type 2 diabetes")) %>%
#   janitor::clean_names() %>%
#   mutate(p_value = as.numeric(p_value)) %>%  
#   filter(p_value != 0)                      # Filter out rows where p_value is 0


# left join ft_table to results

uk_final <- left_join(results_summary,ft_table, by = c("predictor" ="uk_biobank_ppp_protein_id")) %>%
  filter(olink_panel %in% c("Cardiometabolic","Inflammation"))

### check overlapped proteins with teenlabs ######
# assign all other protein p-value as 0.5
tl_proteomics_property_sub <- left_join(tl_proteomics_property,uk_final, by = c("Assay" = "protein_group")) %>%
  mutate(p_value = replace_na(p_value,0.5)) 
# # subset intensity datasets
# tl_proteomics_NPX_sub <- tl_proteomics_NPX %>%
#   select(key, visit, 
#          all_of(tolower(tl_proteomics_property_sub$Assay[tolower(tl_proteomics_property_sub$Assay) %in% colnames(tl_proteomics_NPX)])))
