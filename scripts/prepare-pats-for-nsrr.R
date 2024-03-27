ver="0.1.0"

library(haven)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

#setwd("//rfawin.partners.org/bwh-sleepepi-pats/Data/SAS/_datasets/_upload")

setwd("/Volumes/bwh-sleepepi-pats/Data/SAS/_datasets/_upload")

asthma_fup <- read.csv("asthma_fup.csv")
asthma_base <- read.csv("asthma_base.csv")

asthma <- rbind(asthma_fup, asthma_base)

asthma <- asthma %>%
  arrange(subject, monthpoint) %>%
  mutate(timepoint = case_when(
    monthpoint %in% 0 ~ 1,
    monthpoint %in% 6 ~ 2, 
    monthpoint %in% 12 ~ 3  
  )) %>%
  select(-monthpoint) %>% 
  select(subject, timepoint, everything()) %>%
  filter(!is.na(timepoint))

file_names <- c("childinfo_base.csv", "studyinfo.csv", "anthropometry_base.csv", "anthropometry_fup.csv",
                "abas_base.csv", "abas_fup.csv", 
                "actsleepsummary_base.csv", "actsleepsummary_fup.csv","brief_base.csv", "brief_fup.csv", "cbclandtrf_base.csv", 
                "cbclandtrf_fup.csv", "childhealthmedhx_base.csv", "conners_base.csv", "conners_fup.csv", 
                "cotinine_base.csv", "datasharing.csv", "eod_base.csv", "epworth_base.csv", "epworth_fup.csv", 
                "familyassessment_base.csv", "familyhealthmedhx_base.csv", "gonogo_base.csv", 
                "gonogo_fup.csv", "ige_base.csv", "isaac_base.csv", "isaac_fup.csv",
                "osa18_base.csv", "osa18_fup.csv", "parentinfo_base.csv", "parentingstress_base.csv", 
                "parentingstress_fup.csv", "parentingstyle_base.csv", "pedsql_base.csv", "pedsql_fup.csv", 
                "pegboard_base.csv", "pegboard_fup.csv", "physicalexam_base.csv", "polysomnography_base.csv", 
                "polysomnography_fup.csv", "realm_base.csv", "shq_base.csv", 
                "shq_fup.csv", "srbds_base.csv", "srbds_fup.csv", "surgery.csv")

data_list <- list()

for (file in file_names) {
  base_name <- gsub("_base.csv|_fup.csv|_base_csv.csv|_fup_csv.csv", "", file)
  data <- read.csv(file)
  if (base_name %in% names(data_list)) {
    data_list[[base_name]] <- rbind(data_list[[base_name]], data)
  } else {
    data_list[[base_name]] <- data
  }
}
data_list[["asthma"]] <- asthma

files_missing_timepoint <- character(0)

for (name in names(data_list)) {
  if (!("timepoint" %in% colnames(data_list[[name]]))) {
    files_missing_timepoint <- c(files_missing_timepoint, name)
  }
}
data_with_timepoint <- data_list[!names(data_list) %in% files_missing_timepoint]
data_without_timepoint <- data_list[names(data_list) %in% files_missing_timepoint]

joined_data_subject <- Reduce(function(x, y) merge(x, y, by = "subject", all = TRUE), data_without_timepoint)
joined_data_subject_timepoint <- Reduce(function(x, y) merge(x, y, by = c("subject", "timepoint"), all = TRUE), data_with_timepoint)

merged_data <- merge(joined_data_subject, joined_data_subject_timepoint, by = "subject", all = TRUE)
merged_data$timepoint[is.na(merged_data$timepoint)] <- 0

#expand consented and consented_to_share_data to all timepoint
subjects_with_consent <- merged_data %>%
  filter(consented == 1) %>%
  distinct(subject)
merged_data <- merged_data %>%
  mutate(consented = ifelse(subject %in% subjects_with_consent$subject, 1, consented))

subjects_with_consent_data <- merged_data %>%
  filter(consented_to_share_data == 1) %>%
  distinct(subject)
merged_data <- merged_data %>%
  mutate(consented_to_share_data = ifelse(subject %in% subjects_with_consent_data$subject, 1, consented_to_share_data))

arm1 <- merged_data %>%
  filter(studyinfo_randomized_arm == 1) %>%
  distinct(subject)
merged_data <- merged_data %>%
  mutate(studyinfo_randomized_arm = ifelse(subject %in% arm1$subject, 1, studyinfo_randomized_arm))

arm2 <- merged_data %>%
  filter(studyinfo_randomized_arm == 2) %>%
  distinct(subject)
merged_data <- merged_data %>%
  mutate(studyinfo_randomized_arm = ifelse(subject %in% arm2$subject, 2, studyinfo_randomized_arm))

uncensored <- merged_data %>% 
  filter(consented == 1)%>%
  mutate(across(c(brief_test_date_parent, brief_test_date_teacher,
                  gonogo_test_date, cbcl_form_date, trf_form_date,
                  conners_form_date_parent, conners_form_date_teacher,
                  abas_form_date, ess_form_date, peg_exam_date,
                  psg_study_date, psg_scored_date, pedsql_child_form_date,
                  pedsql_parent_form_date, osa18_form_date, srbds_form_date,
                  shq_form_date, anthro_date, pe_exam_date, chmh_form_date,
                  chmh_ears_date_performed, chmh_adenoids_date_performed,
                  chmh_tonsils_date_performed, fhmh_form_date,
                  isaac_form_date, asthma_form_date, fad_form_date,
                  psq_form_date, eod_form_date, realm_form_date,
                  psi_test_date, studyinfo_randomized_date,
                  studyinfo_surgery_date, studyinfo_stopped_date,
                  studyinfo_treat_stop_date, preop_form_date,
                  oper_form_date, oper_date_of_adentonsillectomy,
                  post_date_of_phone_call), ~ as.character(ymd(.) + days(random_date_offset))))

column_names <- character(0)

for (name in files_missing_timepoint) {
  if (name %in% names(data_list) && name != "datasharing.csv") {
    column_names <- c(column_names, colnames(data_list[[name]]))
  }
}

columns_to_replace <- unique(column_names)
for (column in columns_to_replace) {
  uncensored[uncensored$timepoint %in% c(2, 3), column] <- NA
}

variables_to_remove <- c(
  "random_date_offset","siteid",
  "psg_study_failed_reason",
  "psg_urgent_low_spo2_comments",
  "psg_urgent_other_hr_comments",
  "psg_flow_limitation_comments",
  "psg_alpha_intrusion_comments",
  "psg_periodic_breathing_comments",
  "psg_other_outlier_comments",
  "psg_study_passed",
  "oper_complications__1",
  "oper_complications__2",
  "oper_complications__3",
  "oper_complications__4",
  "oper_complications__5",
  "oper_complications__6",
  "oper_complications__7",
  "oper_complications__8",
  "oper_complications__98",
  "oper_complications_other",
  "chmh_cerebral_palsy_diagnosed",
  "chmh_cerebral_palsy_present",
  "chmh_cerebral_palsy_meds",
  "chmh_diabetes_diagnosed",
  "chmh_diabetes_still_present",
  "chmh_diabetes_medication",
  "chmh_autism_diagnosed",
  "chmh_autism_still_present",
  "chmh_autism_medication",
  "subject","screened", "studyinfo_site","oper_digital_photo_file_name", "oper_digital_photo_sent", 
  "oper_digital_photo_taken", "cotinine_specimen_id", "ige_specimen_id", "childinfo_ageinyear", "childinfo_ageinmonth", 
  "childinfo_sex","childinfo_race","childinfo_grade","childinfo_ethnicity"
)

uncensored <- uncensored[, !(names(uncensored) %in% variables_to_remove)]
variables_to_remove <- grep("^studyinfo_lfus_", names(uncensored), value = TRUE)

uncensored <- uncensored %>%
  select(public_subject_id, public_site_id, timepoint, everything()) %>%
  arrange(public_subject_id, timepoint)%>%select(-consented)
censored<-uncensored%>%filter(consented_to_share_data==1)

id <- unique(merged_data$subject)

write.csv(id, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_ids/ids.csv", row.names = FALSE, na='')
write.csv(uncensored, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_uncensored/0.1.0.pre/pats-dataset-uncensored-0.1.0.pre.csv", row.names = FALSE, na='')
write.csv(censored, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_releases/0.1.0.pre/pats-dataset-0.1.0.pre.csv", row.names = FALSE, na='')


# Harmonized data

harmonized_data<-censored[,c("public_subject_id","timepoint", "anthro_age", "anthro_bmi", "demo_ethnicity", "demo_race", 
                             "demo_sex", "anthro_bp_dia_avg123", "anthro_bp_sys_avg123")]%>%
  dplyr::mutate(nsrrid=public_subject_id,
                nsrr_age=anthro_age,
                nsrr_bmi=anthro_bmi,
                nsrr_bp_diastolic=anthro_bp_dia_avg123,
                nsrr_bp_systolic=anthro_bp_sys_avg123,
                nsrr_race=dplyr::case_when(
                  demo_race==1 ~ "american indian or alaska native",
                  demo_race==2 ~ "asian",
                  demo_race==3 ~ "native hawaiian or other pacific islander",
                  demo_race==4 ~ "black or african american",
                  demo_race==5 ~ "white",
                  demo_race==6 ~ "multiple",
                  TRUE ~ "not reported"
                ),
                nsrr_sex=dplyr::case_when(
                  demo_sex==1 ~ "male",
                  demo_sex==2 ~ "female",
                  TRUE ~ "not reported"
                ),
                nsrr_ethnicity=dplyr::case_when(
                  demo_ethnicity==1 ~ "hispanic or latino",
                  demo_ethnicity==2 ~ "not hispanic or latino",
                  TRUE ~ "not reported"
                ))%>%
  select(nsrrid,timepoint, nsrr_age,nsrr_race,nsrr_ethnicity,nsrr_sex,nsrr_bmi,nsrr_bp_diastolic,nsrr_bp_systolic)



#write.csv(harmonized_data, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_releases/0.1.0.pre/pats-harmonized-dataset-0.1.0.csv", row.names = FALSE, na='')
