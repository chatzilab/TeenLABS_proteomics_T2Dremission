## Recoding
# Create BMI change variable: bmi_c
data_wide <- data_wide %>% 
  mutate(
    bmi0_0 = 0,
    bmi0_6 = (bmi_6 - bmi_0),
    bmi6_12 =(bmi_12 - bmi_6),
    bmi12_36 =(bmi_36 - bmi_12),
    bmi36_60 =(bmi_60 - bmi_36))

temp1 <- data_wide %>% pivot_longer(
  cols = bmi0_0:bmi36_60,
  values_to = "bmi_c",
  names_to = "name"
) %>% 
  separate(name, c(NA, "visit_new"), 
           sep = "_",
           remove = TRUE) %>% 
  dplyr::select(key, visit_new, bmi_c) 

data_l2 <- data_long%>%
  tidylog::left_join(temp1, by = c("key","visit_new")) %>%
  dplyr::select(key:agemos, bmi_c,everything())

data_long <- data_l2 %>% janitor::clean_names() %>% 
  arrange(key, visit_new) %>%
  mutate(visit = factor(visit_new, levels = c("0", "6", "12", "36", "60"), ordered = TRUE))

data_wide <- data_wide %>% 
  mutate(
    bmi0_6 = (bmi_6 - bmi_0)/bmi_0,
    bmi0_12 =(bmi_12 - bmi_0)/bmi_0,
    bmi0_36 =(bmi_36 - bmi_0)/bmi_0,
    bmi0_60 =(bmi_60 - bmi_0)/bmi_0)

temp1 <- data_wide %>% pivot_longer(
  cols = c("bmi0_0", "bmi0_6", "bmi0_12", "bmi0_36", "bmi0_60"),
  values_to = "bmi_percent_c",
  names_to = "name"
) %>% 
  separate(name, c(NA, "visit_new"), 
           sep = "_",
           remove = TRUE) %>% 
  dplyr::select(key, visit_new, bmi_percent_c) 

data_l2 <- data_long%>%
  tidylog::left_join(temp1, by = c("key","visit_new")) %>%
  dplyr::select(key:agemos, bmi_percent_c,everything())

data_long <- data_l2 %>% janitor::clean_names() %>% 
  arrange(key, visit_new) %>%
  mutate(visit = factor(visit_new, levels = c("0", "6", "12", "36", "60"), ordered = TRUE))

# Create percent change for fpg, hba1c, ins, homa

data_wide <- data_wide %>% 
  mutate(
    fpg0_0 = 0,
    fpg0_6 = (fpg_6 - fpg_0)/fpg_0,
    fpg0_12 =(fpg_12 - fpg_0)/fpg_0,
    fpg0_36 =(fpg_36 - fpg_0)/fpg_0,
    fpg0_60 =(fpg_60 - fpg_0)/fpg_0)

temp1 <- data_wide %>% pivot_longer(
  cols = c("fpg0_0", "fpg0_6", "fpg0_12", "fpg0_36", "fpg0_60"),
  values_to = "fpg_percent_c",
  names_to = "name"
) %>% 
  separate(name, c(NA, "visit_new"), 
           sep = "_",
           remove = TRUE) %>% 
  dplyr::select(key, visit_new, fpg_percent_c) 

data_l2 <- data_long%>%
  tidylog::left_join(temp1, by = c("key","visit_new")) %>%
  dplyr::select(key:agemos, fpg_percent_c, everything())

data_long <- data_l2 %>% janitor::clean_names() %>% 
  arrange(key, visit_new) %>%
  mutate(visit = factor(visit_new, levels = c("0", "6", "12", "36", "60"), ordered = TRUE))

data_wide <- data_wide %>% 
  mutate(
    hba1c0_0 = 0,
    hba1c0_6 = (hba1c_6 - hba1c_0)/hba1c_0,
    hba1c0_12 =(hba1c_12 - hba1c_0)/hba1c_0,
    hba1c0_36 =(hba1c_36 - hba1c_0)/hba1c_0,
    hba1c0_60 =(hba1c_60 - hba1c_0)/hba1c_0)

temp1 <- data_wide %>% pivot_longer(
  cols = c("hba1c0_0", "hba1c0_6", "hba1c0_12", "hba1c0_36", "hba1c0_60"),
  values_to = "hba1c_percent_c",
  names_to = "name"
) %>% 
  separate(name, c(NA, "visit_new"), 
           sep = "_",
           remove = TRUE) %>% 
  dplyr::select(key, visit_new, hba1c_percent_c) 

data_l2 <- data_long%>%
  tidylog::left_join(temp1, by = c("key","visit_new")) %>%
  dplyr::select(key:agemos, hba1c_percent_c,everything())

data_long <- data_l2 %>% janitor::clean_names() %>% 
  arrange(key, visit_new) %>%
  mutate(visit = factor(visit_new, levels = c("0", "6", "12", "36", "60"), ordered = TRUE))

data_wide <- data_wide %>% 
  mutate(
    ins0_0 = 0,
    ins0_6 = (ins_6 - ins_0)/ins_0,
    ins0_12 =(ins_12 - ins_0)/ins_0,
    ins0_36 =(ins_36 - ins_0)/ins_0,
    ins0_60 =(ins_60 - ins_0)/ins_0)

temp1 <- data_wide %>% pivot_longer(
  cols = c("ins0_0", "ins0_6", "ins0_12", "ins0_36", "ins0_60"),
  values_to = "ins_percent_c",
  names_to = "name"
) %>% 
  separate(name, c(NA, "visit_new"), 
           sep = "_",
           remove = TRUE) %>% 
  dplyr::select(key, visit_new, ins_percent_c) 

data_l2 <- data_long%>%
  tidylog::left_join(temp1, by = c("key","visit_new")) %>%
  dplyr::select(key:agemos, ins_percent_c,everything())

data_long <- data_l2 %>% janitor::clean_names() %>% 
  arrange(key, visit_new) %>%
  mutate(visit = factor(visit_new, levels = c("0", "6", "12", "36", "60"), ordered = TRUE))

data_wide <- data_wide %>% 
  mutate(
    homa0_0 = 0,
    homa0_6 = (homa_6 - homa_0)/homa_0,
    homa0_12 =(homa_12 - homa_0)/homa_0,
    homa0_36 =(homa_36 - homa_0)/homa_0,
    homa0_60 =(homa_60 - homa_0)/homa_0)

temp1 <- data_wide %>% pivot_longer(
  cols = c("homa0_0", "homa0_6", "homa0_12", "homa0_36", "homa0_60"),
  values_to = "homa_percent_c",
  names_to = "name"
) %>% 
  separate(name, c(NA, "visit_new"), 
           sep = "_",
           remove = TRUE) %>% 
  dplyr::select(key, visit_new, homa_percent_c) 

data_l2 <- data_long%>%
  tidylog::left_join(temp1, by = c("key","visit_new")) %>%
  dplyr::select(key:agemos, homa_percent_c,everything())

data_long <- data_l2 %>% janitor::clean_names() %>% 
  arrange(key, visit_new) %>%
  mutate(visit = factor(visit_new, levels = c("0", "6", "12", "36", "60"), ordered = TRUE))

# Re-order visit ###############################################################
levels(data_long$visit_new) <- as.numeric(c('0','0.5','1','3','5'))

# Create parents_income variable with "unknown" as level #######################
data_long <- data_long %>%
  group_by(key) %>%
  fill(parents_income) %>% 
  ungroup()
data_long <- data_long %>% mutate(parents_income_new = ifelse(is.na(parents_income), "unknown", parents_income))

# IMPORTANT: Exclude participant with >300 for HOMA-IR, as advised by Todd Jenkins #####

data_long <- data_long %>% 
  replace_with_na_at(.vars = "homa_0",
                     condition = ~ (.x) >300)

data_wide <- data_wide %>% 
  replace_with_na_at(.vars = "homa_0",
                     condition = ~ (.x) >300)

# Rename datasets #

data <- data_long

data_w <- data_wide

data <- data %>%
  mutate(bmi_baseline = case_when(visit_new == 0 ~  bmi))

data <- data %>%
  group_by(key) %>%
  fill(bmi_baseline) %>% 
  ungroup()

data <- data %>%
  mutate(age_baseline = case_when(visit_new == 0 ~  agemos))

data <- data %>%
  group_by(key) %>%
  fill(age_baseline) %>% 
  ungroup()

data$age_baseline <- data$age_baseline/12

# Restrict to those we have key and visit #######################################
data <- data %>% drop_na(key)

data_w <- data_w %>% drop_na(key)

# Make sure variables are correctly classified correctly ######################

data$key <- as.factor(data$key)
data$sex <- as.factor(data$sex)
data$parents_income_new <- as.factor(data$parents_income_new)
data$race_binary <- as.factor(data$race_binary)
data$site <- as.factor(data$site)

data_w$homa_0 <- as.numeric(data_w$homa_0)
data_w$homa_6 <- as.numeric(data_w$homa_6)
data_w$homa_12 <- as.numeric(data_w$homa_12)
data_w$homa_36 <- as.numeric(data_w$homa_36)
data_w$homa_60 <- as.numeric(data_w$homa_60)

# Revalue visit variable #######################################################

data$visit <- case_when(
  as.character(data$visit_new) == '0' ~ '0',
  as.character(data$visit_new) == '6' ~ '0.5',
  as.character(data$visit_new) == '12' ~ '1',
  as.character(data$visit_new) == '36' ~ '3',
  as.character(data$visit_new) == '60' ~ '5',
  .default = NA
) |> as.numeric()
data$visit <- as.numeric(data$visit)


## Relevel variables ###############################################################

data$parents_income_new <- relevel(data$parents_income_new, ref = "less than 25000")
data$site <- relevel(data$site, ref = "CIN")

data$bmi_baseline_cat<-ifelse(data$bmi_baseline>=50,"Above 50","Below 50")

# Label categories
data$race_binary <- factor(data$race_binary,
                           levels = c(0, 1),
                           labels = c("Others", "White or Caucasian"))

data$parents_income_new <- factor(data$parents_income_new,
                                  levels = c("less than 25000", "25000-74999", "75000 or more", "unknown"),
                                  labels = c("Less than $25000", "$25000 to $74000", "$75000 or more", "Unknown"))

data$site <- factor(data$site,
                    levels = c("CIN", "BCM", "NCH", "PIT", "UAB"),
                    labels = c("A", "B", "C", "D", "E"))

# Label variables
label(data$race_binary) <- "Race"
label(data$sex) <-"Sex"
label(data$age_baseline) <- "Age at baseline (in years)"
label(data$parents_income_new) <- "Parents income category"
label(data$site) <- "Site"

# Create T2D-related variables

# Create new outcomes
data$ifg <- ifelse(data$fpg >= 100, "Yes", 
                   ifelse(data$fpg < 100, "No", NA))
data$ifg <- as.factor(data$ifg)
data$ifg <- relevel(data$ifg, ref = "No")

data$ins_abnormal <- ifelse(data$ins > 17, "Abnormal",
                            ifelse(data$ins <= 17 , "Normal", NA))
data$ins_abnormal <- as.factor(data$ins_abnormal)
data$ins_abnormal <- relevel(data$ins_abnormal, ref = "Normal")

data$hba1c_abnormal <- ifelse(data$hba1c >= 6.5, "Abnormal",
                              ifelse(data$hba1c < 6.5, "Normal", NA))
data$hba1c_abnormal <- as.factor(data$hba1c_abnormal)
data$hba1c_abnormal <- relevel(data$hba1c_abnormal, ref = "Normal")

data$homa_abnormal <- ifelse(data$homa >= 4, "Abnormal",
                             ifelse(data$homa < 4, "Normal", NA))
data$homa_abnormal <- as.factor(data$homa_abnormal)
data$homa_abnormal <- relevel(data$homa_abnormal, ref = "Normal")

# Create diabetes or diabetes medication
data <- data %>%
  group_by(key) %>%
  fill(diabetes) %>% 
  ungroup()

diab_med_temp <- data %>%
  dplyr::filter(visit == 0) %>%
  dplyr::select(key, visit, diab_med)
colnames(diab_med_temp)[3] <- "diab_med_baseline"

data <- left_join(data, diab_med_temp, by = c("key", "visit"))
data <- data %>%
  dplyr::group_by(key) %>%
  fill(diab_med_baseline) %>% 
  ungroup()

data$diab_baseline <- ifelse(data$diabetes == 1 | data$diab_med_baseline == 1, "Yes", "No")
data$diab_baseline <- as.factor(data$diab_baseline)
data$diab_baseline <- relevel(data$diab_baseline, ref = "No")

# Create diabetes and prediabetes

data <- data %>% 
  dplyr::mutate(
    t2d = case_when(
      diab_med == 1 ~ "Diabetes",
      fpg >= 126 | hba1c >= 6.5 ~ "Diabetes",
      fpg >= 100 | hba1c >= 5.7 ~ "Prediabetes",
      fpg < 100 & hba1c < 5.7 ~ "No",
      .default = NA_character_
    )
  )

# Create binary site variable
data$site_binary <- ifelse(
  data$site == "CIN", "CIN", ifelse(
    is.na(data$site), NA, "Non CIN"
  )
)

data_w$site_binary <- ifelse(
  data_w$site == "CIN", "CIN", ifelse(
    is.na(data_w$site), NA, "Non CIN"
  )
)

# diabetes remission at each follow-up visit
data <- data %>% 
  arrange(key, visit) %>% 
  dplyr::group_by(key) %>% 
  dplyr::mutate(
    first_t2d = case_when(
      visit == 0 ~ t2d,
      .default = NA_character_
    )) %>%
  fill(first_t2d) %>%
  dplyr::mutate(
    diabetes_remission = case_when(
      visit == 0 ~ NA_character_,
      first_t2d == "Diabetes" & t2d == "Prediabetes" ~ "Yes",
      first_t2d == "Diabetes" & t2d == "No" ~ "Yes",
      first_t2d == "Diabetes" & t2d == "Diabetes" ~ "No",
      first_t2d == "Prediabetes" & t2d == "No" ~ "Yes",
      first_t2d == "Prediabetes" & t2d == "Prediabetes" ~ "No",
      .default = NA_character_
    ),
    diabetes_remission = if_else(diabetes_remission == "Yes", 1,
                                 if_else(diabetes_remission == "No", 0, NA))
  ) %>%
  dplyr::ungroup() %>% 
  dplyr::select(-first_t2d)

rm(data_l2, data_long, data_wide, temp1, diab_med_temp)
gc()
