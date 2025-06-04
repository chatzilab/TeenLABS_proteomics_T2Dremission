# teen-labs------
# correctly configure the factor levels
# Label categories
data_wide <- data_wide %>% mutate(parents_income_new = ifelse(is.na(data_wide$parents_income_0), "unknown", parents_income_0))

data_wide$parents_income_new <- factor(data_wide$parents_income_new,
                                       levels = c("less than 25000", "25000-74999", "75000 or more", "unknown"))

data_wide$site <- factor(data_wide$site,
                         levels = c("BCM","CIN","NCH","PIT","UAB"),
                         labels = c("A", "B","C","D","E"))
dat_final$race_binary <- factor(dat_final$race_binary,
                                levels = c(0, 1),
                                labels = c("Others", "Non-hispanice whites"))

dat_final$sex <- factor(dat_final$sex, labels = c("Male", "Female"))

dat_final$parents_education_0 <- as.factor(ifelse(is.na(dat_final$parents_education_0), "Missing", as.character(dat_final$parents_education_0)))
tb1 <- dat_final  %>% 
  mutate(age = agemos_0/12)%>%
  tbl_summary(
    by = diabetes_remission,
    include = c("sex","age","bmi_0",
                "parents_education_0",
                "race_binary"),
    label = list(sex ~ "Sex", age~"Age in years at baseline",
                 bmi_0 ~"BMI in kg/m2 at baseline"
                 ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)")
  ) |>
  add_overall() 


#### solar -----

# the total number of pariticpants were 312 which are participants who completed 2h OGTT

solar_df_re$sex_1 <- factor(solar_df_re$sex_1, labels = c("Male", "Female"))

solar_df_tb1 <- solar_df_w3 %>% 
  mutate(age_1 = case_when(
    id == "r02_sol_00078"|id == "r02_sol_00178"~age_2,
    TRUE ~age_1),
    bmi_og_1 = case_when(
      id == "r02_sol_00078"|id == "r02_sol_00178"~bmi_og_2,
      TRUE ~bmi_og_1),
    bmiz_1 = case_when(
      id == "r02_sol_00078"|id == "r02_sol_00178"~bmiz_2,
      TRUE ~bmiz_1),
    tag_1 = case_when(
      id == "r02_sol_00078"|id == "r02_sol_00178"~tag_2,
      TRUE ~tag_1)) %>% 
  mutate(include = case_when(
    is.na(diabetes_remission) ~ "Excluded",
    TRUE ~ "Included"
  ))

solar_df_tb1 %>% 
  rename(sex = sex_1, age = age_1,edu = edu_house_1) %>% # have the same var names as tb1 
  tbl_summary(
    by = include,
    include = c("sex","age", "edu","bmi_og_1","bmiz_1"),
    label = list(sex ~ "Sex", 
                 age~"Age at baseline, years [mean (SD)]",
                 bmi_og_1 ~ "BMI at baseline, kg/m2 [mean (SD)]",
                 bmiz_1 ~"BMI z scores at baseline [mean (SD)]",
                 edu ~ "Household educational levels"),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~c(2,3)))|>
  add_p(list(all_continuous() ~ "t.test",
        all_categorical() ~ "fisher.test"))|>
  bold_labels()  |>
  modify_table_styling(
    columns = label,
    label == "Study site",
    footnote = "Locations were denoted as A and B to maintain confidentiality in accordance with Institutional Review Board requirements",  # specify your footnote text
  )

# table 2 glycemic biomarkers 
solar_df_re %>% 
  rename(sex = sex_1, age = age_1) %>% # have the same var names as tb1 
  tbl_summary(
    by = diabetes_remission,
    include = c("diabete_change",
                "prediabete_1","a1c_baseline","a1c_fu","og_glu_5_baseline",
                "og_glu_5_fu","og_glu120_baseline","og_glu120_fu"),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} ({p}%)"),
  digits = list(all_continuous() ~c(4,4)))|>
  bold_labels() 

tb2


tb1_combined <- tbl_merge(tbls = list(tb1,tb2),
                          tab_spanner =  c("Teen-LABS (Discovery cohort)","solar (Replication cohort)")
)

tb1_combined