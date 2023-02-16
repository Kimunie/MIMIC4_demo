# Encoding: UTF-8

#################################################
#' MIMIC-IV demodata cleaning
#' 
#'  Editor: S.K
#'  Date: 2023/02/10-
#' 
#' 3. Time-to-event Prediction
#' 
#################################################

gc(); gc() # Free Unused R memory


# Settings ----------------------------------------------------------------

library(tidyverse)
library(rms)
library(zoo)
library(lubridate)

# Read all .csv files ------------------------------------------------------------

root = getwd()

# hosp
csv_list = list.files(path = paste(root, "/demodata/hosp", sep=""), 
                      pattern = "*.csv", full.names = T)
data_hosp = lapply(csv_list, read.csv, na.strings=c("", "na", "NA"))

# icu
csv_list = list.files(path = paste(root, "/demodata/icu", sep=""), 
                      pattern = "*.csv", full.names = T)
data_icu = lapply(csv_list, read.csv, na.strings=c("", "na", "NA"))


# data from hosp/icu module ---------------------------------------------------

hosp_adm = data_hosp[[1]]
hosp_pat = data_hosp[[14]]
hosp_diag = data_hosp[[6]]
hosp_lab = data_hosp[[11]]
labitems = data_hosp[[5]]

icu_icustays = data_icu[[5]]


# inclusion criteria ------------------------------------------------------

hosp_adm_pat_icustays = hosp_adm %>% 
  full_join(hosp_pat, by="subject_id") %>% 
  full_join(icu_icustays, by=c("subject_id", "hadm_id")) %>% 
  
  ## 18 < anchor_age < 90
  filter(anchor_age > 18, anchor_age < 90) %>% 
  
  ## icu admission duration >= 24hours
  filter(difftime(outtime, intime,units = "hours") >= 24) %>% 
  
  ## not died in hospital
  filter(hospital_expire_flag == 0) %>% 
  select(-deathtime) %>% 
  
  ## number of admission
  group_by(subject_id) %>% 
  arrange(admittime) %>% 
  mutate(num_adm = row_number(), .after = hadm_id) %>%
  ungroup() %>% 
  
  ## number of icu admission
  group_by(subject_id) %>% 
  arrange(intime) %>% 
  mutate(num_icuadm = row_number(), .after = stay_id) %>% 
  ungroup()

# get second admission date
df_second_adm = hosp_adm_pat_icustays %>% 
  filter(num_adm == 2) %>% 
  select(subject_id, admittime) %>% 
  rename(secondadm = admittime)

# outcome: readmission within 28days 
hosp_adm_pat_icustays_1st = hosp_adm_pat_icustays %>%
  filter(num_icuadm == 1) %>%
  left_join(df_second_adm, by = "subject_id") %>% 
  mutate(readm_28d = as.integer(!is.na(secondadm) & difftime(secondadm, outtime, units = "days") <= 28))

# subject_id of included patients
subject_id_included = (hosp_adm_pat_icustays %>% distinct(subject_id, .keep_all = TRUE))$subject_id
length(subject_id_included)

describe(hosp_adm_pat_icustays)

View(
hosp_adm_pat_icustays %>%
  filter(num_icuadm == 2) %>%
  left_join(df_second_adm, by = "subject_id") %>% 
  mutate(readm_28d = as.integer(!is.na(secondadm) & difftime(secondadm, outtime, units = "days") <= 28))
)

View(
hosp_icu1stadm_pat %>% 
  mutate(diff = ceiling(difftime(dischtime, outtime,units = "days"))) %>%
  group_by(diff) %>%
  count %>%
  ungroup() %>%
  mutate(cum = cumsum(`n`))
)

# extract variables from labevents ----------------------------------------

vars = c("Sodium", "Potassium", "Creatinine", "Hematocrit", "White Blood Cells", 
         "Urea Nitrogen", "Bicarbonate", "Bilirubin, Total", "Platelet Count")


# N先生が選んだ変数
# vars = c("Sodium", "Potassium", "Chloride", "Calcium", "Phosphate", "Magnesium", 
#          "AST", "ALT", "Alkaline Phosphatase", "Gamma Glu", "Bilirubin", "Urea", 
#          "Creatinine", "Albumin", "Protein, Total", "Glucose", "Creatine Kinase ", 
#          "Lactate Dehydrogenase ", "Amy", "Fibrin Degradation Products", "Fibrinog",  
#          "D-Dimer", "White")
# betu = c("Hemoglobin", "PT", "PTT", "Platelet Count")
# gas = c("pH", "pCO2", "pO2", "Base Excess", "Bicarbonate", "Lactate", "Glucose)


labitems_sub = data.frame()

for(i in 1:length(vars)){
  tmp = labitems %>% filter(fluid == "Blood", category != "Blood Gas") %>% 
    filter(str_detect(label, vars[i]))
  labitems_sub = rbind(labitems_sub, tmp)
}


# for(i in 1:length(betu)){
#   tmp = labitems %>% filter(fluid == "Blood", category != "Blood Gas") %>% 
#     filter(label == betu[i])
#   labitems_sub = rbind(labitems_sub, tmp)
# }
# 
# for(i in 1:length(gas)){
#   tmp = labitems %>% filter(fluid == "Blood", category == "Blood Gas") %>% 
#     filter(str_detect(label, gas[i]))
#   labitems_sub = rbind(labitems_sub, tmp)
# }

labitems_sub = labitems_sub %>% 
  select(-fluid) %>% 
  filter(!str_detect(label, "Bilirubin, Neonatal"))

hosp_lab_sub = hosp_lab %>% 
  select(-c(order_provider_id, comments)) %>% 
  filter(subject_id %in% subject_id_included) %>% 
  filter(itemid %in% labitems_sub$itemid) %>% 
  mutate(storetime_h = as.POSIXct(storetime, format="%Y-%m-%d %H"), .after=storetime) %>% 
  mutate(value = as.numeric(value)) %>% 
  left_join(labitems_sub[,1:2], by="itemid")

# convert to wide
hosp_lab_sub_wide = 
  pivot_wider(hosp_lab_sub,
              id_cols = c(subject_id, hadm_id, storetime_h),
              names_from = label,
              values_from = value,
              values_fn = ~ median(.x, na.rm = TRUE)) %>% 
  inner_join(hosp_adm_pat_icustays_1st[, "hadm_id"], by="hadm_id") %>% 
  group_by(subject_id, hadm_id) %>% 
  arrange(storetime_h) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

describe(hosp_lab_sub_wide)


# icu chanmiku ------------------------------------------------------------

icu_chart = data_icu[[2]]
icu_d_items = data_icu[[3]]

icu_chart = icu_chart[, c(1, 2, 3, 6, 7, 9)]
icu_d_items = icu_d_items[, c(1,2)]

icu_chart_sub = left_join(icu_chart, icu_d_items, by = "itemid") %>%
  filter(itemid %in% c(220210, 220277, 220045, 223901, 223900, 220739, 
                       223762, 220179, 220180, 220050, 220051, 223761)) %>% 
  mutate(storetime_h = as.POSIXct(storetime, format="%Y-%m-%d %H"), .after=storetime)

chartitems_sub = icu_chart_sub %>% 
  select(itemid, label) %>% 
  distinct(itemid, .keep_all = TRUE)

icu_chart_sub_wide = 
  pivot_wider(icu_chart_sub,
              id_cols = c(subject_id, hadm_id, stay_id, storetime_h),
              names_from = label,
              values_from = valuenum,
              values_fn = ~ median(.x, na.rm = TRUE)) %>%
  inner_join(hosp_adm_pat_icustays_1st[, "stay_id"], by="stay_id") %>% 
  group_by(subject_id, hadm_id) %>% 
  arrange(storetime_h) %>% 
  ungroup() %>% 
  mutate(across(where(is.numeric), ~na_if(., "NaN")))
  

# 摂氏体温>60の人は華氏に変換
icu_chart_sub_wide$`Temperature Celsius` = 
  ifelse(icu_chart_sub_wide$`Temperature Celsius` > 60,
         (icu_chart_sub_wide$`Temperature Celsius`-32)*5/9,
         icu_chart_sub_wide$`Temperature Celsius`)
# 華氏がNAの箇所は、摂氏を華氏に変換して置換
icu_chart_sub_wide$`Temperature Fahrenheit` = 
  ifelse(is.na(icu_chart_sub_wide$`Temperature Fahrenheit`),
         (icu_chart_sub_wide$`Temperature Celsius` * 1.8) + 32,
         icu_chart_sub_wide$`Temperature Fahrenheit`)
icu_chart_sub_wide = icu_chart_sub_wide %>% select(-`Temperature Celsius`)


# merge hosp & icu --------------------------------------------------------

lab_vital = full_join(icu_chart_sub_wide, hosp_lab_sub_wide, 
                      by = c("subject_id", "hadm_id", "storetime_h"))

hosp_icu_merged = left_join(hosp_adm_pat_icustays_1st, lab_vital,
                            by=c("subject_id", "hadm_id", "stay_id"))


describe(hosp_adm_pat_icustays_1st$subject_id)
describe(lab_vital$subject_id)

describe(hosp_icu_merged$subject_id)

# data check --------------------------------------------------------------

# last observation carried forward
for(var in names(hosp_icu_merged)){
  if(is.numeric(hosp_icu_merged[var][[1]])){
    hosp_icu_merged[var] = na.locf(hosp_icu_merged[var], na.rm = FALSE)
  }
}

hosp_icu_merged_24h = hosp_icu_merged %>% 
  mutate(outtime_bfr24h = as.POSIXct(outtime) - days(1), 
         .after=outtime) %>% 
  filter(storetime_h > outtime_bfr24h, 
         storetime_h < outtime)

describe(hosp_icu_merged_24h)

hosp_icu_merged_24h %>%
  group_by(readm_28d) %>%
  distinct(subject_id) %>% 
  count()


# test --------------------------------------------------------------------

# 確認用density plot
# hosp_icu %>%
#   filter(`White Blood Cells` >= 0) %>%
#   ggplot(aes(x = `White Blood Cells` * 1000)) + geom_density()
# 
# tmp = data_hosp[[3]]
# tmp %>% filter(str_detect(long_title, "metas"))
# # DIC = D65
# names(hosp_diag)
# hosp_diag %>% filter(str_detect(icd_code, "65"))
