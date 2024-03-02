ver="0.1.0"

library(haven)
library(readr)
library(dplyr)


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

merge_data <- function(x, y) {
  if ("timepoint" %in% names(x) && "timepoint" %in% names(y)) {
    merge(x, y, by = c("subject", "timepoint"), all = TRUE)
  } else {
    merge(x, y, by = "subject", all = TRUE)
  }
}

merged_data <- Reduce(merge_data, data_list)

id_timepoint <- merged_data[c("subject", "timepoint")]
other_columns <- merged_data[, !(colnames(merged_data) %in% c("subject", "timepoint"))]
merged_data <- cbind(id_timepoint, other_columns)

merged_data_subset <- subset(merged_data, consented == 1)
merged_data_subset <- merged_data_subset %>%
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
merged_data_subset <- merged_data_subset %>% 
  select(-random_date_offset)

id <- unique(merged_data$subject)

write.csv(id, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_ids/ids.csv", row.names = FALSE, na='')

write.csv(merged_data_subset, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_releases/0.1.0.pre/pats-dataset-0.1.0.pre.csv", row.names = FALSE, na='')

harmonized_data<-merged_data[,c("subject", "childinfo_ageinyear","childinfo_sex","childinfo_race","anthro_bmi")]%>%
  dplyr::mutate(nsrrid=subject,
                nsrr_age=childinfo_ageinyear,
                nsrr_bmi=anthro_bmi,
                nsrr_race=dplyr::case_when(
                  childinfo_race==1 ~ "American Indian/Alaska Native",
                  childinfo_race==2 ~ "Asian",
                  childinfo_race==3 ~ "Native Hawaiian or Other Pacific Islander",
                  childinfo_race==4 ~ "Black or African American",
                  childinfo_race==5 ~ "White",
                  childinfo_race==6 ~ "More than one race",
                  TRUE ~ "Unknown or not reported"
                ),
                nsrr_sex=dplyr::case_when(
                  childinfo_sex==1 ~ "male",
                  childinfo_sex==2 ~ "female",
                  TRUE ~ "not reported"
                ))%>%
  select(nsrrid,nsrr_age,nsrr_race,nsrr_sex,nsrr_bmi)

# need to add anthro_height_avg, anthro_weight_avg? childinfo_ethnicity? what else need to be added?
# anthro_bp_sys_avg123	Anthropometry: Calculated - Average resting systolic blood pressure (mmHg) over three measurements
# anthro_bp_sys_avg23	Anthropometry: Calculated - Average resting systolic blood pressure (mmHg) over the 2nd and 3rd measurements
# anthro_bp_dia_avg123	Anthropometry: Calculated - Average resting diastolic blood pressure (mmHg) over three measurements
# anthro_bp_dia_avg23	Anthropometry: Calculated - Average resting diastolic blood pressure (mmHg) over the 2nd and 3rd measurements
# which ones should be used for nsrr_bp_systolic and nsrr_bp_diastolic?

#write.table(harmonized_data, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_releases/0.1.0.pre/pats-harmonized-dataset-1.0.1.csv", col.names = FALSE, row.names = FALSE, sep = ","
#write.csv(merged_data, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_releases/0.1.0.pre/pats-harmonized-dataset-0.1.0.csv", row.names = FALSE, na='')


