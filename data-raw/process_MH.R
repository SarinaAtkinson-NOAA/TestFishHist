
library(dplyr) # mutate, left_join
library(stringr) # str_remove, str_detect
library(tidyr) # separate_rows, pivot_longer
library(lubridate) # month, day

mh_raw <- read.csv("data-raw/MH_DOWNLOAD_DEC_16_2024.csv",
                   stringsAsFactors = FALSE,
                   colClasses=c("REGULATION_ID" = "numeric",
                                "START_DAY" = "numeric",
                                "START_MONTH" = "numeric",
                                "START_YEAR" = "numeric",
                                "END_DAY" = "numeric",
                                "END_MONTH" = "numeric",
                                "END_YEAR" = "numeric"),
                   # Added on 10/26/2022 because of a specific issue in how Sarina & Adyan's Rstudio reads in the csv file
                   # For some reason Adyan had all hyphens from species groups re coded to "\x97"
                   fileEncoding = 'Windows-1252') |>
  # Reformat data frame for date formats and NAs
  # Format dates to be mm/dd/yyyy to match added data (this may not be an issue in the future when pull directly from the database)
  dplyr::mutate(EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
                INEFFECTIVE_DATE = as.Date(INEFFECTIVE_DATE, "%m/%d/%Y"),
                #Remove ="..." characters in species ITIS codes
                SPECIES_ITIS = gsub('="', '', SPECIES_ITIS),
                SPECIES_ITIS = gsub('"', '', SPECIES_ITIS)) |>
  # Issue with species ITIS appearing as "\t173138"
  dplyr::mutate(SPECIES_ITIS = gsub('\t', '', SPECIES_ITIS),
                # Remove A or B in FR Citation (example regulation ID = 11514)
                FR_CITATION = stringr::str_remove(FR_CITATION, " [AB]"),
                # Add code for getting zone name for 3 regulations that are not getting fixed in the database
                # Sarina created data bugs to update the data, but the change was not reflected
                ZONE = dplyr::case_when(REGULATION_ID %in% c(11537, 11538, 11539) ~ "SMZ - PA - 04 RON MCMANUS MEMORIAL REEF", TRUE ~ ZONE))
# Replace all "blank" fields with NA for consistency
mh_raw[mh_raw==""]<-NA

# Add "DETAILED" YES/NO field (from Google Sheets) based on MANAGEMENT_TYPE ####
# Read in file outlining whether a MANAGEMENT_TYPE is detailed (Y/N)
detailed_xref <- read.csv("data-raw/mtype_detailed_xref.csv",
                          stringsAsFactors = FALSE,
                          fileEncoding = 'Windows-1252') |>
  dplyr::select(-MANAGEMENT_CATEGORY)

# Translate from old ZONE names to new ZONE names ####
# Read in file that outlines new ZONE names for all FMPs
# These ZONEs were cleaned up for consistency
area_xref <- read.csv("data-raw/zone_name_xref.csv",
                      stringsAsFactors = FALSE,
                      fileEncoding = 'Windows-1252') |>
  # Create single variable for ZONE_USE
  dplyr::mutate(ZONE_USE = dplyr::case_when(NEW_ZONE_NAME == "" ~ ZONE, TRUE ~ NEW_ZONE_NAME)) |>
  dplyr::select(ZONE, ZONE_USE)

mh_setup <- mh_raw |>
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL'
  dplyr::mutate(SECTOR_USE = dplyr::case_when(MANAGEMENT_CATEGORY != "CATCH LIMITS" & SECTOR == 'ALL' ~ 'RECREATIONAL,COMMERCIAL', TRUE ~ SECTOR)) |>
  # Expand SECTOR_USE at the commas
  tidyr::separate_rows(SECTOR_USE) |>
  dplyr::left_join(detailed_xref, by = "MANAGEMENT_TYPE") |>
  # CREATE: the variables of O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP to retain the original species information from the raw data
  dplyr::mutate(O_COMMON_NAME = COMMON_NAME,
                O_SPECIES_AGGREGATE = SPECIES_AGGREGATE,
                O_SPECIES_GROUP = SPECIES_GROUP) |>
  # CREATE: the variables of SPP_TYPE and SPP_NAME and transpose the species information contained in the O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP fields.
  # The SPP_TYPE field indicates whether the record relates to a SPECIES_AGGREGATE, SPECIES_GROUP, or individual species (COMMON_NAME)
  # The SPP_NAME field indicates the specific aggregate, group, or species name to which the record applies
  tidyr::pivot_longer(cols = c(COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP), names_to = "SPP_TYPE", values_to = "SPP_NAME") |>
  # Remove records where SPP_NAME is null
  dplyr::filter(!is.na(SPP_NAME)) |>
  # Standardize ZONE names
  dplyr::left_join(area_xref, by = c("ZONE" = "ZONE")) |>
  # CREATE: vol and page but pulling out the volume and page number as separate fields from the FR_CITATION
  dplyr::mutate(vol = as.numeric(sub(" FR.*", "", FR_CITATION)),
                page = as.numeric(sub(".*FR ", "", FR_CITATION)),
                # CREATE: ADJUSTMENT variable to flag when the MANAGEMENT_TYPE contains the word "ADJUSTMENT" and remove "ADJUSTMENT" from the MANAGEMENT_TYPE name
                # ADJUSTMENT records are never redundant
                # Added " MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" & month(INEFFECTIVE_DATE) == 12 & day(INEFFECTIVE_DATE) == 31 ~ 0" due to error in CLUSTER 280
                # Without this addition, a closure within CLUSTER 280 that ended at the end of the calendar year was being flagged as an adjustment instead of ceasing to exist.
                ADJUSTMENT = dplyr::case_when(stringr::str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ 1,
                                       MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" & lubridate::month(INEFFECTIVE_DATE) == 12 & lubridate::day(INEFFECTIVE_DATE) == 31 ~ 0,
                                       MANAGEMENT_TYPE == "REOPENING" & !is.na(INEFFECTIVE_DATE) ~ 1,
                                       TRUE ~ 0),
                MANAGEMENT_TYPE_USE = dplyr::case_when(stringr::str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ stringr::str_replace(MANAGEMENT_TYPE, " ADJUSTMENT", ""),
                                                TRUE ~ MANAGEMENT_TYPE),
                # Rename records with the MANAGEMENT_TYPE of REOPENING to the MANAGEMENT_TYPE of CLOSURE and add OPEN or CLOSED to the VALUE field
                # Although the MANAGEMENT_TYPE will be different from the raw data (since the raw data stays true to the FR Language)
                # this will assist in processing dates to accurately capture the time series of openings/closures in the fishery
                MANAGEMENT_TYPE_USE = dplyr::case_when(MANAGEMENT_TYPE == "REOPENING" ~ "CLOSURE",
                                                TRUE ~ MANAGEMENT_TYPE_USE),
                VALUE= dplyr::case_when(MANAGEMENT_TYPE == "CLOSURE" ~ "CLOSE",
                                 MANAGEMENT_TYPE == "REOPENING" ~ "OPEN",
                                 TRUE ~ VALUE),
                # CREATE: MANAGEMENT_STATUS_USE variable by transposing the MANAGEMENT_STATUS values
                # Replace any NA values under MANAGEMENT_STATUS_USE as ONCE because  NA values can be complicated to process
                # Both MANAGEMENT_STATUS of NA and ONCE are meant to be processed the same way
                MANAGEMENT_STATUS_USE = dplyr::case_when(is.na(MANAGEMENT_STATUS) ~ 'ONCE',
                                                  TRUE ~ MANAGEMENT_STATUS),
                # CREATE: the variable of STATUS_TYPE with the values of SIMPLE or COMPLEX
                # A STATUS_TYPE of SIMPLE indicates a MANAGEMENT_STATUS_USE of ONCE
                # A STATUS_TYPE of COMPLEX indicates a MANAGEMENT_STATUS_USE that is RECURRING (SEASONAL, WEEKLY RECURRING, MONTHLY RECURRING, DAILY)
                STATUS_TYPE = dplyr::case_when(MANAGEMENT_STATUS_USE == "ONCE" ~ "SIMPLE",
                                        MANAGEMENT_STATUS_USE %in% c("SEASONAL", "WEEKLY RECURRING", "MONTHLY RECURRING", "DAILY") ~ "RECURRING",
                                        TRUE ~ "COMPLEX"),
                # CREATE: the variable of REG_REMOVED to indicate when a regulation is "turned off"
                # A regulation is "turned off when the EFFECTIVE_DATE is equal to the INEFFECTIVE_DATE
                REG_REMOVED = dplyr::case_when(EFFECTIVE_DATE == INEFFECTIVE_DATE ~ 1, TRUE ~ 0),
                # CREATE: the variables of GENERAL and COMPLEX to flag when a regulation has a STAUS_TYPE of SIMPLE or COMPLEX, respectively
                GENERAL = dplyr::case_when(STATUS_TYPE == "SIMPLE" & is.na(VALUE) ~ 1, TRUE ~ 0),
                COMPLEX = dplyr::case_when(STATUS_TYPE == "COMPLEX" ~ 1, TRUE ~ 0),
                # Format start and end day of week as ordered factors
                START_DAY_OF_WEEK = factor(START_DAY_OF_WEEK, levels = c("MONDAY", "TUESDAY", "WEDNESDAY",
                                                                         "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"),
                                           ordered = TRUE),
                END_DAY_OF_WEEK = factor(END_DAY_OF_WEEK, levels = c("MONDAY", "TUESDAY", "WEDNESDAY",
                                                                     "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"),
                                         ordered = TRUE),
                # CREATE: START_DATE from the START_DAY, START_MONTH, and START_YEAR fields
                # The START_DATE field is only created using START_DAY, START_MONTH, and START_YEAR when
                # all three start day, month, year fields are provided and the management status is ONCE
                START_DATE = dplyr::case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                         !is.na(START_DAY) &
                                         !is.na(START_MONTH) &
                                         !is.na(START_YEAR) ~ as.Date(paste(START_MONTH, START_DAY, START_YEAR, sep = "/"), "%m/%d/%Y"),
                                       TRUE ~ EFFECTIVE_DATE),
                # Adjust the start date if its after the effective date so the start date = effective date
                START_DATE = dplyr::case_when(START_DATE < EFFECTIVE_DATE ~ EFFECTIVE_DATE,
                                       TRUE ~ START_DATE),
                # When the START_TIME is equal to "11:59:00 PM", the START_DATE should be pushed ahead by one day since
                # the regulation will be in effect for the entirety of that day.
                START_DATE = dplyr::case_when(START_TIME == "11:59:00 PM" ~ START_DATE + 1,
                                       TRUE ~ START_DATE),
                # Adjust the start day, time, and day of the week accordingly so when those fields are used for recurring
                # the individual fields will be consistent
                START_DAY_USE = dplyr::case_when(START_TIME == "11:59:00 PM" & !is.na(START_DAY) ~ START_DAY + 1,
                                          TRUE ~ START_DAY),
                # For start time, we remove the start time when "11:59:00 PM" because a null assumes the full day
                START_TIME_USE = dplyr::case_when(START_TIME != "11:59:00 PM" ~ START_TIME),
                START_DAY_OF_WEEK_USE = dplyr::case_when(START_TIME == "11:59:00 PM" & !is.na(START_DAY_OF_WEEK) ~ as.numeric(START_DAY_OF_WEEK) + 1,
                                                  TRUE ~ as.numeric(START_DAY_OF_WEEK)),

                # CREATE: END_DATE from the END_DAY, END_MONTH, and END_YEAR fields
                # The END_DATE field is only created using END_DAY, END_MONTH, and END_YEAR when
                # all end year, month, day fields are provided and and the management status is ONCE
                END_DATE = dplyr::case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                       !is.na(END_DAY) &
                                       !is.na(END_MONTH) &
                                       !is.na(END_YEAR) ~ as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y"),
                                     TRUE ~ INEFFECTIVE_DATE),
                # Added condition on 2/2/24 because of REG_ID 762
                END_DATE = dplyr::case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                       !is.na(END_DAY) &
                                       !is.na(END_MONTH) &
                                       !is.na(END_YEAR) &
                                       as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y") > INEFFECTIVE_DATE ~ INEFFECTIVE_DATE,
                                     TRUE ~ END_DATE),
                # For records with an END_TIME of "12:01:00 AM", the END_DATE should be reverted to one day prior.
                # This will infer that the regulation remained in place through the end of that day and not one minute into the next day.
                END_DATE = dplyr::case_when(END_TIME == "12:01:00 AM" ~ END_DATE - 1,
                                     TRUE ~ END_DATE),
                # Adjust the end day, time, and day of the week accordingly
                # When end time is 12:01, use the day of end date because the year of February already factored in to determine if its the 28th or 29th
                END_DAY_USE = dplyr::case_when(END_TIME == "12:01:00 AM" & !is.na(END_DATE) ~ as.numeric(lubridate::day(END_DATE)),
                                        TRUE ~ END_DAY),
                END_MONTH_USE = dplyr::case_when(END_TIME == "12:01:00 AM" & !is.na(END_DATE) ~ as.numeric(lubridate::month(END_DATE)),
                                          TRUE ~ END_MONTH),
                END_YEAR_USE = dplyr::case_when(END_TIME == "12:01:00 AM" & END_MONTH == 1 & END_DAY == 1 ~ END_YEAR - 1,
                                         TRUE ~ END_YEAR),
                # Retain end time of 12:01 only for recurring regulations where the end day is the 1st
                # Otherwise remove 12:01 from end time or use the reported end time
                END_TIME_USE = dplyr::case_when(END_TIME == "12:01:00 AM" & STATUS_TYPE == "RECURRING" & END_DAY == 1 ~ END_TIME,
                                         # Single regulation (REG_ID = 80) where end time is 11:59 PM
                                         END_TIME != "12:01:00 AM" & END_TIME != "11:59:00 PM" ~ END_TIME),
                # When the END_TIME is listed as "12:01:00 AM" and the end day of the week is not missing then revert to one day prior
                # TO be consistent, still use the condition when end day does not equal 1, but as of 12/30/2022 there were no end day of the weeks with an end day of 1
                END_DAY_OF_WEEK_USE = dplyr::case_when(END_TIME == "12:01:00 AM" & END_DAY != 1 ~ as.numeric(END_DAY_OF_WEEK) - 1,
                                                TRUE ~ as.numeric(END_DAY_OF_WEEK)),
                # Format start and end day of week use to deal with 0 and 8 of 7 level factor
                START_DAY_OF_WEEK_USE = dplyr::case_when(START_DAY_OF_WEEK_USE == 8 ~ 1,
                                                  TRUE ~ START_DAY_OF_WEEK_USE),
                END_DAY_OF_WEEK_USE = dplyr::case_when(END_DAY_OF_WEEK_USE == 0 ~ 7,
                                                TRUE ~ END_DAY_OF_WEEK_USE),
                # Format start and end day of week use as ordered factors
                START_DAY_OF_WEEK_USE = dplyr::recode_factor(START_DAY_OF_WEEK_USE,
                                                      `1` = "MONDAY", `2` = "TUESDAY", `3` = "WEDNESDAY",
                                                      `4` = "THURSDAY", `5` = "FRIDAY", `6` = "SATURDAY", `7` = "SUNDAY"),
                END_DAY_OF_WEEK_USE = dplyr::recode_factor(END_DAY_OF_WEEK_USE,
                                                    `1` = "MONDAY", `2` = "TUESDAY", `3` = "WEDNESDAY",
                                                    `4` = "THURSDAY", `5` = "FRIDAY", `6` = "SATURDAY", `7` = "SUNDAY"))

fishhist <- mh_setup

usethis::use_data(fishhist, overwrite = TRUE)
