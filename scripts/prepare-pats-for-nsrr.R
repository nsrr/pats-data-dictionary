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

has_timepoint <- function(df) {
  "timepoint" %in% colnames(df)
}

updated_data_frames <- list()

for (file_name in file_names) {
  data <- read.csv(file_name)
  if (!has_timepoint(data)) {
    data$timepoint <- 1 
    
    cat("Added 'timepoint' variable to", file_name, "\n")
  } 
  updated_data_frames[[file_name]] <- data
}

updated_data_frames[["asthma"]] <- asthma

file_list <- list()

for (file_name in c(file_names, "asthma")) {  
  base_name <- gsub("_base.csv|_fup.csv|_base_csv.csv|_fup_csv.csv", "", file_name)
  data <- updated_data_frames[[file_name]] 
  if (base_name %in% names(file_list)) {
    file_list[[base_name]] <- rbind(file_list[[base_name]], data)
  } else {
    file_list[[base_name]] <- data
  }
}

merge_cols <- "subject"

merged_data <- Reduce(function(x, y) {
  if (all(c(merge_cols) %in% colnames(x)) && all(c(merge_cols) %in% colnames(y))) {
    if ("timepoint" %in% colnames(x) && "timepoint" %in% colnames(y)) {
      merge(x, y, by = c("subject", "timepoint"), all = TRUE)
    } else {
      y$timepoint <- 1  
      merge(x, y, by = merge_cols, all = TRUE)
    }
  } else {
    merge(x, y, by = merge_cols, all = TRUE)
  }
}, file_list)


id_timepoint <- merged_data[c("subject", "timepoint")]
other_columns <- merged_data[, !(colnames(merged_data) %in% c("subject", "timepoint"))]
merged_data <- cbind(id_timepoint, other_columns)

id <- unique(merged_data$subject)
subjects_consented <- unique(merged_data$subject[merged_data$consented == 1])
filtered_merged_data <- merged_data[merged_data$subject %in% subjects_consented, ]

write.csv(id, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_ids/ids.csv", row.names = FALSE, na='')

write.csv(filtered_merged_data, file = "/Volumes/bwh-sleepepi-pats/nsrr-prep/_releases/0.1.0.pre/pats-dataset-0.1.0.pre.csv", row.names = FALSE, na='')

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


