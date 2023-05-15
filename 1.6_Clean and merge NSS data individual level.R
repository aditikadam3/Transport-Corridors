# 1.6 Individual_level_data_cleaning.R
# Project: Regional Integration
# Purpose: create individual female level outcomes such as marital status, employment, education
# Author: Aditi Kadam (AdK)
# Date created: Feb 24 2023
# Modified by: AdK
# Date modified: May 15 2023
# datasets created, full name including whatever global macros would be used to find the full path:
# ########### STEPS [outline of this program -- see example below]
  # - 1: Install packages
  # - 2: Load libraries
  # - 3: Create district-sex-industry wise avg employment for all years
      # - 3.1: NSS 2004-05 data
        # 3.1.1 Get the data and create variables
        # 3.1.2 Subset data to women over 14 and under 65 only
      # - 3.2: NSS 2005-06 data
        # 3.2.2 Get the data and create variables
        # 3.2.2 create file of total districts to match to 2004
        # 3.2.3 Subset data to women over 14 and under 65 only
      # - 3.3: NSS 2007-08 data
        # 3.3.2 Get the data and create variables
        # 3.3.2 create file of total districts to match to 2004
        # 3.3.3 Subset data to women over 14 and under 65 only
      # - 3.4: NSS 2009-10 data
        # 3.4.2 Get the data and create variables
        # 3.4.2 create file of total districts to match to 2004
        # 3.4.3 Subset data to women over 14 and under 65 only
      # - 3.5: NSS 2011-12 data
        # 3.5.2 Get the data and create variables
        # 3.5.2 create file of total districts to match to 2004
              # 3.5.3 Subset data to women over 14 and under 65 only
      # - 3.6: NSS 2003-04 data
        # 3.6.2 Get the data and create variables
        # 3.6.2 create file of total districts to match to 2004
        # 3.6.3 Subset data to women over 14 and under 65 only
# - 4: Append all years data
  # - 5: 
  # - 6: 
  # - 7: 

######################################################################################
# - 1: Install packages

necessaryPackages <- c("readxl", "tidyverse", "ggplot2", "dplyr", "RColorBrewer", "ggthemes", "colorspace",
                       "Rttf2pt1", "extrafontdb", "foreign", "plm", "stargazer", "mediation", "devtools",
                       "ggdag", "rvest", "margins", "survival", "data.table", "cowplot", "ggpubr", "sp", "maptools", "mapdata",
                       "rgdal", "mapview", "ggmap", "sf", "geosphere", "stringr")
new.packages <- necessaryPackages[
  !(necessaryPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
# - 2: Load libraries
lapply(necessaryPackages, require, character.only = TRUE)

setwd("/Users/Samyam/OneDrive - University of Georgia/World Bank/indiv/")
######################################################################################
# - 3: Create dataset for all districts-year individual level

############################
  # - 3.1: NSS 2004-05 data
iaDataNss0405 <- read.dta("./Data/NSS/04-05/Block_5pt3_level_06.dta")
names(iaDataNss0405)
# get demographic data on sex
iaDataNssDem0405 <- read.dta("./Data/NSS/04-05/Block_4_level_03.dta")
names(iaDataNssDem0405)
# 3.1.1 Get the data and create variables
# merge wages data with sex, marital status, eductaion, etc
iaDataNss0405 <- merge(x=iaDataNss0405, y=iaDataNssDem0405[,c("PID", "Marital_status", "General_education")], by = "PID", all.x=T)
# create district code
iaDataNss0405$district_code<- iaDataNss0405$DISTRICT_CODE
# employment data (two digit industry codes, restrict to three for comparison)
iaDataNss0405$industry <- str_sub(iaDataNss0405$Current_weekly_activity_NIC_1998, 1,2)
# create employment status by district and sex
iaDataNss0405$emp <- as.integer(iaDataNss0405$Current_weekly_activity_status >= 11 & iaDataNss0405$Current_weekly_activity_status < 81)
table(iaDataNss0405$emp)
# assign ag, manufacturing, or services
iaDataNss0405$empSector <- ifelse(iaDataNss0405$industry < 15, 1,
                                         ifelse(iaDataNss0405$industry  >= 15 & iaDataNss0405$industry < 40, 2,
                                                ifelse(iaDataNss0405$industry >= 40, 3, 
                                                       ifelse(is.na(iaDataNss0405$industry), NA, 0)))) # add dummy for ag 
iaDataNss0405 <- iaDataNss0405 %>%
  mutate(empSector_ag = ifelse(iaDataNss0405$empSector == 1, 1, 
                               ifelse(is.na(iaDataNss0405$empSector), NA, 0)),
         empSector_manu = ifelse(iaDataNss0405$empSector == 2, 1, 
                                 ifelse(is.na(iaDataNss0405$empSector), NA, 0)),
         empSector_serv = ifelse(iaDataNss0405$empSector == 3, 1, 
                                 ifelse(is.na(iaDataNss0405$empSector), NA, 0)))
table(iaDataNss0405$empSector_ag)
table(iaDataNss0405$empSector_manu)
table(iaDataNss0405$empSector_serv)

# create file of total districts to match to 2004
iaDataDistMapping0405 <- iaDataNss0405 %>% group_by(district_code) %>% summarise(n = n())
# add year
iaDataNss0405$year <- 2
# check for duplicates
length(unique(iaDataNss0405$PID)) == nrow(iaDataNss0405)
n_occur <- data.frame(table(iaDataNss0405$PID))
names(n_occur) <- c("PID", "Freq")
iaDataNss0405 <- merge(iaDataNss0405, n_occur, by = "PID")
# duplicates are same person with many different activity statuses, for now, keep the first one
iaDataNss0405 <- iaDataNss0405[!duplicated(iaDataNss0405[ , c("PID","Current_weekly_activity_status")]),]

# 3.1.2.1 Subset data to men over 14 and below 65 only
# only men
iaDataNss0405_Men <- subset(iaDataNss0405, iaDataNss0405$Sex == 1)
# keep only above 14 and below 65
iaDataNss0405_Men <- subset(iaDataNss0405, iaDataNss0405$Age > 14 & iaDataNss0405$Age < 65)
# keep relevant vars
names(iaDataNss0405_Men)
iaDataNss0405_Men <- iaDataNss0405_Men[,c("HHID", "PID", "district_code", "Marital_status", "General_education", "Age", "Current_weekly_activity_status", "empSector_ag", "empSector_manu", "empSector_serv", "STATE_CODE", "year")]
names(iaDataNss0405_Men)
# keep consistent names
names(iaDataNss0405_Men) <- c("HHID", "PID", "district_code",  "Marital_status", "General_education", "Age", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "State", "year")
names(iaDataNss0405_Men)

# 3.1.2.2 Subset data to women over 14 and under 65 only
# keep only women
iaDataNss0405_Women <- subset(iaDataNss0405, iaDataNss0405$Sex == 2)
# keep only above 14 and below 65
iaDataNss0405_Women <- subset(iaDataNss0405, iaDataNss0405$Age > 14 & iaDataNss0405$Age < 65)
# keep relevant vars
names(iaDataNss0405_Women)
iaDataNss0405_Women <- iaDataNss0405_Women[,c("HHID", "PID", "district_code", "Marital_status", "General_education", "Age", "Current_weekly_activity_status", "empSector_ag", "empSector_manu", "empSector_serv", "STATE_CODE", "year")]
names(iaDataNss0405_Women)
# keep consistent names
names(iaDataNss0405_Women) <- c("HHID", "PID", "district_code",  "Marital_status", "General_education", "Age", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "State", "year")
names(iaDataNss0405_Women)

############################
# - 3.2: NSS 2005-06 data
iaDataNss0506 <- read.dta("./Data/NSS/05-06/Block-6-Persons-daily-activity-time-disposition-reecords.dta")
names(iaDataNss0506)
# get demographic data on sex
iaDataNssDem0506 <- read.dta("./Data/NSS/05-06/Block-4-Persons-demographic-particulars-records.dta")
names(iaDataNssDem0506)
table(iaDataNssDem0506$B4_q7)
# 3.2.1 Get the data and create variables
# merge wages data with sex, marital status, eductaion, etc
iaDataNss0506 <- merge(x=iaDataNss0506, y=iaDataNssDem0506[,c("Person_key", "B4_q4", "B4_q6", "B4_q7", "B4_q7")], by = "Person_key", all.x=T)
# create DISTRICT_CODE
iaDataNss0506$district_code = str_c(iaDataNss0506$State,iaDataNss0506$District)
# employment data (two digit industry codes, restrict to three for comparison)
iaDataNss0506$industry <- str_sub(iaDataNss0506$B6_q5, 1,2)
# create employment status by district and sex
iaDataNss0506$emp <- as.integer(iaDataNss0506$B6_q18 >= 11 & iaDataNss0506$B6_q18 < 91)
table(iaDataNss0506$emp)
# assign ag, manufacturing, or services
iaDataNss0506$empSector <- ifelse(iaDataNss0506$industry < 15, 1,
                                  ifelse(iaDataNss0506$industry  >= 15 & iaDataNss0506$industry < 40, 2,
                                         ifelse(iaDataNss0506$industry >= 40, 3, 
                                                ifelse(is.na(iaDataNss0506$industry), NA, 0)))) # add dummy for ag 
iaDataNss0506 <- iaDataNss0506 %>%
  mutate(empSector_ag = ifelse(iaDataNss0506$empSector == 1, 1, 
                               ifelse(is.na(iaDataNss0506$empSector), NA, 0)),
         empSector_manu = ifelse(iaDataNss0506$empSector == 2, 1, 
                                 ifelse(is.na(iaDataNss0506$empSector), NA, 0)),
         empSector_serv = ifelse(iaDataNss0506$empSector == 3, 1, 
                                 ifelse(is.na(iaDataNss0506$empSector), NA, 0)))
table(iaDataNss0506$empSector_ag)
table(iaDataNss0506$empSector_manu)
table(iaDataNss0506$empSector_serv)

# # Assign ag and non ag industry employment
# iaDataNss0506$emp <- ifelse(iaDataNss0506$B6_q18 >= 11 & iaDataNss0506$B6_q18 < 91, 1 , 0)
# table(iaDataNss0506$emp)
# iaDataNss0506$Ag <- ifelse(iaDataNss0506$industry < 15 & iaDataNss0506$industry > 11, 1, 0) # add dummy for ag and non ag employment
# iaDataNss0506$nonAg <-  ifelse(iaDataNss0506$industry >= 15 & iaDataNss0506$industry < 91 , 1, 0)
# 3.2.2 create file of total districts to match to 2004
iaDataDistMapping0506 <- iaDataNss0506 %>% group_by(district_code) %>% summarise(n = n())
# join 2004-05 and 2005-06 files
iaDataDistMapping0406 <- merge(iaDataDistMapping0405, iaDataDistMapping0506, by="district_code", all=T)
colnames(iaDataDistMapping0406)[c(2,3)] <- c("n_0405","n_0506")
# add year
iaDataNss0506$year <- 3
# check for duplicates
length(unique(iaDataNss0506$Person_key)) == nrow(iaDataNss0506)
n_occur <- data.frame(table(iaDataNss0506$Person_key))
names(n_occur) <- c("Person_key", "Freq")
iaDataNss0506 <- merge(iaDataNss0506, n_occur, by = "Person_key")
# duplicates are same person with many different activity statuses, for now, keep the first one
iaDataNss0506 <- iaDataNss0506[!duplicated(iaDataNss0506[ , c("Person_key","B6_q18")]),]

# 3.2.3.1 Subset data to men over 14 and under 65 only
#  only men
iaDataNss0506_Men <- subset(iaDataNss0506, iaDataNss0506$B4_q4 == 1)
# keep only above 14 and below 65
iaDataNss0506_Men <- subset(iaDataNss0506_Men, iaDataNss0506_Men$B6_q2 > 14 & iaDataNss0506_Men$B6_q2 < 65)
# keep only relevant vars
iaDataNss0506_Men <- iaDataNss0506_Men[, c("Hhold_key", "Person_key", "district_code", "B6_q18", "empSector_ag", "empSector_manu", "empSector_serv","B6_q2", "B4_q6","B4_q7", "State", "year")]
# keep consistent names
names(iaDataNss0506_Men) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status",  "Ag", "Manufac", "Serv","Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0506_Men)

# 3.2.3.2 Subset data to women over 14 and under 65 only
#  keep only women
iaDataNss0506_Women <- subset(iaDataNss0506, iaDataNss0506$B4_q4 == 2)
# keep only above 14 and below 65
iaDataNss0506_Women <- subset(iaDataNss0506_Women, iaDataNss0506_Women$B6_q2 > 14 & iaDataNss0506_Women$B6_q2 < 65)
# keep only relevant vars
iaDataNss0506_Women <- iaDataNss0506_Women[, c("Hhold_key", "Person_key", "district_code", "B6_q18",  "empSector_ag", "empSector_manu", "empSector_serv", "B6_q2", "B4_q6","B4_q7", "State", "year")]
# keep consistent names
names(iaDataNss0506_Women) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0506_Women)

############################
# - 3.3: NSS 2007-08 data
iaDataNss0708 <- read.dta("./Data/NSS/07-08/Block-5-members-time-disposition-records.dta")
names(iaDataNss0708)
iaDataNss0708$key_memb = str_c(iaDataNss0708$key_hhold,iaDataNss0708$B5_c1)
# get demographic data on sex
iaDataNssDem0708  <- read.dta("./Data/NSS/07-08/Block-4-demographic-usual-activity-members-records.dta")
names(iaDataNssDem0708)
# 3.2.1 Get the data and create variables
# merge wages data with sex, marital status, eductaion, etc
iaDataNss0708 <- merge(x=iaDataNss0708, y=iaDataNssDem0708[,c("key_memb", "B4_c4", "B4_c5", "B4_c6", "B4_c7")], by = "key_memb", all.x=T)
# 3.1.1 Get the data and create variables
# create DISTRICT_CODE
iaDataNss0708$district_code = str_c(iaDataNss0708$state,iaDataNss0708$District)
# employment data (two digit industry codes, restrict to three for comparison)
iaDataNss0708$industry <- str_sub(iaDataNss0708$B5_c19, 1,2)
# create employment status by district and sex
iaDataNss0708$emp <- as.integer(iaDataNss0708$B5_c18 >= 11 & iaDataNss0708$B5_c18 < 91)
table(iaDataNss0708$emp)
# assign ag, manufacturing, or services
iaDataNss0708$empSector <- ifelse(iaDataNss0708$industry < 15, 1,
                                  ifelse(iaDataNss0708$industry  >= 15 & iaDataNss0708$industry < 40, 2,
                                         ifelse(iaDataNss0708$industry >= 40, 3, 
                                                ifelse(is.na(iaDataNss0708$industry), NA, 0)))) # add dummy for ag 
iaDataNss0708 <- iaDataNss0708 %>%
  mutate(empSector_ag = ifelse(iaDataNss0708$empSector == 1, 1, 
                               ifelse(is.na(iaDataNss0708$empSector), NA, 0)),
         empSector_manu = ifelse(iaDataNss0708$empSector == 2, 1, 
                                 ifelse(is.na(iaDataNss0708$empSector), NA, 0)),
         empSector_serv = ifelse(iaDataNss0708$empSector == 3, 1, 
                                 ifelse(is.na(iaDataNss0708$empSector), NA, 0)))
table(iaDataNss0708$empSector_ag)
table(iaDataNss0708$empSector_manu)
table(iaDataNss0708$empSector_serv)
# # Assign ag and non ag industry employment
# iaDataNss0708$emp <- ifelse(iaDataNss0708$B5_c18 >= 11 & iaDataNss0708$B5_c18 < 91, 1 , 0)
# table(iaDataNss0708$emp)
# iaDataNss0708$Ag <- ifelse(iaDataNss0708$industry < 15 & iaDataNss0708$emp == 1, 1, 0) # add dummy for ag and non ag employment
# iaDataNss0708$nonAg <-  ifelse(iaDataNss0708$industry >= 15 & iaDataNss0708$emp == 1, 1, 0)
# 3.1.2 create file of total districts to match to 2004
iaDataDistMapping0708 <- iaDataNss0708 %>% group_by(district_code) %>% summarise(n = n())
# join 2004-06 and 2007-08 files
iaDataDistMapping0408 <- merge(iaDataDistMapping0406, iaDataDistMapping0708, by="district_code", all=T)
colnames(iaDataDistMapping0408)[4] <- 'n_0708'
# NOTE: No district changes were made between NSS surveys 2004-05 and 2007-08, proceed without making any changes to districts
# add year
iaDataNss0708$year <- 4
# check for duplicates
length(unique(iaDataNss0708$key_memb)) == nrow(iaDataNss0708)
n_occur <- data.frame(table(iaDataNss0708$key_memb))
names(n_occur) <- c("key_memb", "Freq")
iaDataNss0708 <- merge(iaDataNss0708, n_occur, by = "key_memb")
# duplicates are same person with many different activity statuses, for now, keep the first one
iaDataNss0708 <- iaDataNss0708[!duplicated(iaDataNss0708[ , c("key_memb","B5_c18")]),]

# 3.1.3.1 Subset data to men over 14 and under 65 only
# keep only women
iaDataNss0708_Men <- subset(iaDataNss0708, iaDataNss0708$B4_c4 == 1)
# keep only above 14 and below 65
iaDataNss0708_Men <- subset(iaDataNss0708_Men, iaDataNss0708_Men$B4_c5 > 14 & iaDataNss0708_Men$B4_c5 < 65)
# keep only relevant vars
names(iaDataNss0708_Men)
iaDataNss0708_Men <- iaDataNss0708_Men[, c("key_hhold", "key_memb", "district_code", "B5_c18", "empSector_ag", "empSector_manu", "empSector_serv", "B4_c5", "B4_c6","B4_c7", "state", "year")]
# keep consistent names
names(iaDataNss0708_Men) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0708_Men)

# 3.1.3.1 Subset data to women over 14 and under 65 only
# keep only women
iaDataNss0708_Women <- subset(iaDataNss0708, iaDataNss0708$B4_c4 == 2)
# keep only above 14 and below 65
iaDataNss0708_Women <- subset(iaDataNss0708_Women, iaDataNss0708_Women$B4_c5 > 14 & iaDataNss0708_Women$B4_c5 < 65)
# keep only relevant vars
names(iaDataNss0708_Women)
iaDataNss0708_Women <- iaDataNss0708_Women[, c("key_hhold", "key_memb", "district_code", "B5_c18", "empSector_ag", "empSector_manu", "empSector_serv","B4_c5", "B4_c6","B4_c7", "state", "year")]
# keep consistent names
names(iaDataNss0708_Women) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv","Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0708_Women)


############################
# - 3.4: NSS 2009-10 data
iaDataNss0910 <- read.dta("./Data/NSS/09-10/Block_5_3_Time disposition during the week ended on ...............dta")
names(iaDataNss0910)
# get demographic data on sex
iaDataNssDem0910 <- read.dta("./Data/NSS/09-10/Block_4_Demographic particulars of household members.dta")
names(iaDataNssDem0910)
# 3.1.1 Get the data and create variables
# merge sex with wages data
iaDataNss0910 <- merge(x=iaDataNss0910, y=iaDataNssDem0910[,c("PID", "Sex", "Marital_Status", "General_Education")], by = "PID", all.x=T)
# create DISTRICT_CODE
iaDataNss0910$district_code = iaDataNss0910$DISTRICT_CODE
# employment data (two digit industry codes, restrict to three for comparison)
iaDataNss0910$industry <- str_sub(iaDataNss0910$Current_Weekly_Activity_NIC_2004, 1,2)
# create employment status by district and sex
iaDataNss0910$emp <- as.integer(iaDataNss0910$Current_Weekly_Activity_Status >= 11 & iaDataNss0910$Current_Weekly_Activity_Status < 91)
table(iaDataNss0910$emp)
# assign ag, manufacturing, or services
iaDataNss0910$empSector <- ifelse(iaDataNss0910$industry < 15, 1,
                                  ifelse(iaDataNss0910$industry  >= 15 & iaDataNss0910$industry < 40, 2,
                                         ifelse(iaDataNss0910$industry >= 40, 3, 
                                                ifelse(is.na(iaDataNss0910$industry), NA, 0)))) # add dummy for ag 
iaDataNss0910 <- iaDataNss0910 %>%
  mutate(empSector_ag = ifelse(iaDataNss0910$empSector == 1, 1, 
                               ifelse(is.na(iaDataNss0910$empSector), NA, 0)),
         empSector_manu = ifelse(iaDataNss0910$empSector == 2, 1, 
                                 ifelse(is.na(iaDataNss0910$empSector), NA, 0)),
         empSector_serv = ifelse(iaDataNss0910$empSector == 3, 1, 
                                 ifelse(is.na(iaDataNss0910$empSector), NA, 0)))
table(iaDataNss0910$empSector_ag)
table(iaDataNss0910$empSector_manu)
table(iaDataNss0910$empSector_serv)
# # Assign ag and non ag industry employment
# iaDataNss0910$emp <- ifelse(iaDataNss0910$Current_Weekly_Activity_Status >= 11 & iaDataNss0910$Current_Weekly_Activity_Status < 91, 1 , 0)
# table(iaDataNss0910$emp)
# iaDataNss0910$Ag <- ifelse(iaDataNss0910$industry < 15 & iaDataNss0910$emp == 1, 1, 0) # add dummy for ag and non ag employment
# iaDataNss0910$nonAg <-  ifelse(iaDataNss0910$industry >= 15 & iaDataNss0910$emp == 1, 1, 0)
# 3.1.2 create file of total districts to match to 2004
iaDataDistMapping0910 <- iaDataNss0910 %>% group_by(district_code) %>% summarise(n = n())
# join 2004-06 and 2007-08 files
iaDataDistMapping0410 <- merge(iaDataDistMapping0408, iaDataDistMapping0910, by="district_code", all=T)
colnames(iaDataDistMapping0410)[5] <- 'n_0910'
# change district codes that have "NA" in 0405-0910 to match the original 0405, see excel
# 3.4.2.1 change district codes that have "NA" in 0405-0910 to match the original 0405, see excel
# import excel of matched districts
nss_district_mapping_2011_2004 <- read_xlsx("./Data/Districts/nss_district_mapping_2011_2004.xlsx")
nss_district_mapping_2011_2004$district_2011 <- sprintf("%04d", nss_district_mapping_2011_2004$district_2011)
nss_district_mapping_2011_2004$district_2004 <- sprintf("%04d", nss_district_mapping_2011_2004$district_2004)
# merge with 0910 NSS data to get district_code_2004 and district_name_2004
iaDataNss0910 <- merge(iaDataNss0910, nss_district_mapping_2011_2004, by.x="district_code", by.y="district_2011", all.x=T)
# keep only 2004 district code and rename name to district_code
iaDataNss0910 <- iaDataNss0910[,!names(iaDataNss0910) %in% c("district_code")]
names(iaDataNss0910)[names(iaDataNss0910) == "district_2004"] <- "district_code"
names(iaDataNss0910)
# add year
iaDataNss0910$year <- 5
# check for duplicates
length(unique(iaDataNss0910$PID)) == nrow(iaDataNss0910)
n_occur <- data.frame(table(iaDataNss0910$PID))
names(n_occur) <- c("PID", "Freq")
iaDataNss0910 <- merge(iaDataNss0910, n_occur, by = "PID")
# duplicates are same person with many different activity statuses, for now, keep the first one
iaDataNss0910 <- iaDataNss0910[!duplicated(iaDataNss0910[ , c("PID","Current_Weekly_Activity_Status")]),]

# 3.1.3 Subset data to Men over 14 and under 65 only
# only men
iaDataNss0910_Men <- subset(iaDataNss0910, iaDataNss0910$Sex == 1)
# keep only above 14 and below 65
iaDataNss0910_Men <- subset(iaDataNss0910_Men, iaDataNss0910_Men$Age > 14 & iaDataNss0910_Men$Age < 65)
# keep only relevant vars
names(iaDataNss0910_Men)
# keep only 2004 district code
names(iaDataNss0910_Men)[names(iaDataNss0910_Men) == 'district_2004'] <- 'district_code'
iaDataNss0910_Men <- iaDataNss0910_Men[, c("HHID", "PID", "district_code", "Current_Weekly_Activity_Status", "empSector_ag", "empSector_manu", "empSector_serv", "Age", "Marital_Status","General_Education", "STATE", "year")]
# keep consistent names
names(iaDataNss0910_Men) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0910_Men)

# 3.1.3 Subset data to women over 14 and under 65 only
# keep only women
iaDataNss0910_Women <- subset(iaDataNss0910, iaDataNss0910$Sex == 2)
# keep only above 14 and below 65
iaDataNss0910_Women <- subset(iaDataNss0910_Women, iaDataNss0910_Women$Age > 14 & iaDataNss0910_Women$Age < 65)
# keep only relevant vars
names(iaDataNss0910_Women)
# keep only 2004 district code
names(iaDataNss0910_Women)[names(iaDataNss0910_Women) == 'district_2004'] <- 'district_code'
iaDataNss0910_Women <- iaDataNss0910_Women[, c("HHID", "PID", "district_code", "Current_Weekly_Activity_Status","empSector_ag", "empSector_manu", "empSector_serv", "Age", "Marital_Status","General_Education", "STATE", "year")]
# keep consistent names
names(iaDataNss0910_Women) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0910_Women)

############################
# - 3.5: NSS 2011-12 data
iaDataNss1112 <- read.dta("./Data/NSS/11-12/Block_5_3_Time disposition during the week ended on ...............dta")
names(iaDataNss1112)
# get demographic data on sex
iaDataNssDem1112 <- read.dta("./Data/NSS/11-12/Block_4_Demographic particulars of household members.dta")
names(iaDataNssDem1112)
table(iaDataNssDem1112$General_Education)
# 3.1.1 Get the data and create variables
# create member identifier (key_memb)
iaDataNss1112$key_memb = str_c(iaDataNss1112$HHID,iaDataNss1112$Person_Serial_No)
iaDataNssDem1112$key_memb = str_c(iaDataNssDem1112$HHID,iaDataNssDem1112$Person_Serial_No)
# merge sex with wages data
iaDataNss1112 <- merge(x=iaDataNss1112, y=iaDataNssDem1112[,c("key_memb", "Sex", "Marital_Status","General_Education")], by = "key_memb", all.x=T)
# create DISTRICT_CODE
iaDataNss1112$district_code = str_c(iaDataNss1112$State,iaDataNss1112$District)
# employment data (two digit industry codes, restrict to three for comparison)
iaDataNss1112$industry <- str_sub(iaDataNss1112$Current_Weekly_Activity_NIC_2008, 1,2)
# create employment status by district and sex
iaDataNss1112$emp <- as.integer(iaDataNss1112$Current_Weekly_Activity_Status >= 11 & iaDataNss1112$Current_Weekly_Activity_Status < 91)
table(iaDataNss1112$emp)
# assign ag, manufacturing, or services
iaDataNss1112$empSector <- ifelse(iaDataNss1112$industry < 15, 1,
                                  ifelse(iaDataNss1112$industry  >= 15 & iaDataNss1112$industry < 40, 2,
                                         ifelse(iaDataNss1112$industry >= 40, 3, 
                                                ifelse(is.na(iaDataNss1112$industry), NA, 0)))) # add dummy for ag 
iaDataNss1112 <- iaDataNss1112 %>%
  mutate(empSector_ag = ifelse(iaDataNss1112$empSector == 1, 1, 
                               ifelse(is.na(iaDataNss1112$empSector), NA, 0)),
         empSector_manu = ifelse(iaDataNss1112$empSector == 2, 1, 
                                 ifelse(is.na(iaDataNss1112$empSector), NA, 0)),
         empSector_serv = ifelse(iaDataNss1112$empSector == 3, 1, 
                                 ifelse(is.na(iaDataNss1112$empSector), NA, 0)))
table(iaDataNss1112$empSector_ag)
table(iaDataNss1112$empSector_manu)
table(iaDataNss1112$empSector_serv)
# # Assign ag and non ag industry employment
# iaDataNss1112$emp <- ifelse(iaDataNss1112$Current_Weekly_Activity_Status >= 11 & iaDataNss1112$Current_Weekly_Activity_Status < 91, 1 , 0)
# table(iaDataNss1112$emp)
# iaDataNss1112$Ag <- ifelse(iaDataNss1112$industry < 15 & iaDataNss1112$emp == 1, 1, 0) # add dummy for ag and non ag employment
# iaDataNss1112$nonAg <-  ifelse(iaDataNss1112$industry >= 15 & iaDataNss1112$emp == 1, 1, 0)
# 3.5.2.1 change district codes that have "NA" in 0405-0910 to match the original 0405, see excel
# import excel of matched districts
nss_district_mapping_2011_2004 <- read_xlsx("./Data/Districts/nss_district_mapping_2011_2004.xlsx")
nss_district_mapping_2011_2004$district_2011 <- sprintf("%04d", nss_district_mapping_2011_2004$district_2011)
nss_district_mapping_2011_2004$district_2004 <- sprintf("%04d", nss_district_mapping_2011_2004$district_2004)
# merge with 1112 NSS data to get district_code_2004 and district_name_2004
iaDataNss1112 <- merge(iaDataNss1112, nss_district_mapping_2011_2004, by.x="district_code", by.y="district_2011", all.x=T)
# keep only 2004 district code and rename name to district_code
names(iaDataNss1112)
iaDataNss1112 <- iaDataNss1112[,!names(iaDataNss1112) %in% c("district_code")]
names(iaDataNss1112)[names(iaDataNss1112) == "district_2004"] <- "district_code"
# add year
iaDataNss1112$year <- 6
# check for duplicates
length(unique(iaDataNss1112$PID)) == nrow(iaDataNss1112)
n_occur <- data.frame(table(iaDataNss1112$key_memb))
names(n_occur) <- c("key_memb", "Freq")
iaDataNss1112 <- merge(iaDataNss1112, n_occur, by = "key_memb")
# duplicates are same person with many different activity statuses, for now, keep the first one
iaDataNss1112 <- iaDataNss1112[!duplicated(iaDataNss1112[ , c("key_memb","Current_Weekly_Activity_Status")]),]

# 3.1.3 Subset data to men over 14 and under 65 only
# only men
iaDataNss1112_Men <- subset(iaDataNss1112, iaDataNss1112$Sex == 1)
# keep only above 14 and below 65
iaDataNss1112_Men <- subset(iaDataNss1112_Men, iaDataNss1112_Men$Age > 14 & iaDataNss1112_Men$Age < 65)
# keep only relevant vars
names(iaDataNss1112_Men)
# keep only 2004 district code
names(iaDataNss1112_Men)[names(iaDataNss1112_Men) == 'district_2004'] <- 'district_code'
iaDataNss1112_Men <- iaDataNss1112_Men[, c("HHID", "key_memb", "district_code", "Current_Weekly_Activity_Status", "empSector_ag", "empSector_manu", "empSector_serv", "Age", "Marital_Status","General_Education", "State", "year")]
# keep consistent names
names(iaDataNss1112_Men) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss1112_Men)

# 3.1.3 Subset data to women over 14 and under 65 only
# keep only women
iaDataNss1112_Women <- subset(iaDataNss1112, iaDataNss1112$Sex == 2)
# keep only above 14 and below 65
iaDataNss1112_Women <- subset(iaDataNss1112_Women, iaDataNss1112_Women$Age > 14 & iaDataNss1112_Women$Age < 65)
# keep only relevant vars
names(iaDataNss1112_Women)
# keep only 2004 district code
names(iaDataNss1112_Women)[names(iaDataNss1112_Women) == 'district_2004'] <- 'district_code'
iaDataNss1112_Women <- iaDataNss1112_Women[, c("HHID", "key_memb", "district_code", "Current_Weekly_Activity_Status", "empSector_ag", "empSector_manu", "empSector_serv","Age", "Marital_Status","General_Education", "State", "year")]
# keep consistent names
names(iaDataNss1112_Women) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss1112_Women)

############################
# - 3.6: NSS 2003-04 data
iaDataNss0304 <- read.dta("./Data/NSS/2003-04/Block 4_Person records.dta")
names(iaDataNss0304)
# 3.1.1 Get the data and create variables
# create DISTRICT_CODE
iaDataNss0304$district_code = str_c(iaDataNss0304$State,iaDataNss0304$District)
# # employment data (two digit industry codes, restrict to two for comparison)
iaDataNss0304$industry <- str_sub(iaDataNss0304$B4_q12, 1,2)
# create employment status by district and sex
iaDataNss0304$emp <- as.integer(iaDataNss0304$B4_q12 >= 11 & iaDataNss0304$B4_q12 < 91)
table(iaDataNss0304$emp)
# assign ag, manufacturing, or services
iaDataNss0304$empSector <- ifelse(iaDataNss0304$industry < 15, 1,
                                  ifelse(iaDataNss0304$industry  >= 15 & iaDataNss0304$industry < 40, 2,
                                         ifelse(iaDataNss0304$industry >= 40, 3, 
                                                ifelse(is.na(iaDataNss0304$industry), NA, 0)))) # add dummy for ag 
iaDataNss0304 <- iaDataNss0304 %>%
  mutate(empSector_ag = ifelse(iaDataNss0304$empSector == 1, 1, 
                               ifelse(is.na(iaDataNss0304$empSector), NA, 0)),
         empSector_manu = ifelse(iaDataNss0304$empSector == 2, 1, 
                                 ifelse(is.na(iaDataNss0304$empSector), NA, 0)),
         empSector_serv = ifelse(iaDataNss0304$empSector == 3, 1, 
                                 ifelse(is.na(iaDataNss0304$empSector), NA, 0)))
table(iaDataNss0304$empSector_ag)
table(iaDataNss0304$empSector_manu)
table(iaDataNss0304$empSector_serv)
# # Assign ag and non ag industry employment
# iaDataNss0304$emp <- ifelse(iaDataNss0304$B4_q12 >= 11 & iaDataNss0304$B4_q12 < 91, 1 , 0)
# table(iaDataNss0304$emp)
# iaDataNss0304$Ag <- ifelse(iaDataNss0304$industry < 15 & iaDataNss0304$emp == 1, 1, 
#                            ifelse(iaDataNss0304$industry > 15 & iaDataNss0304$emp == 1 ,0, NA)) # add dummy for ag and non ag employment
# iaDataNss0304$nonAg <-  ifelse(iaDataNss0304$industry >= 15 & iaDataNss0304$emp == 1, 1,
#                                ifelse(iaDataNss0304$industry < 15 & iaDataNss0304$emp == 1 ,0, NA))
# 3.1.2 create file of total districts to match to 2004
# iaDataDistMapping0304 <- iaDataNss0304 %>% group_by(district_code) %>% summarise(n = n())
# # join 2004-06 and 2007-08 files
# iaDataDistMapping0408 <- merge(iaDataDistMapping0406, iaDataDistMapping0304, by="district_code", all=T)
# colnames(iaDataDistMapping0408)[4] <- 'n_0304'
# NOTE: No district changes were made between NSS surveys 2004-05 and 2007-08, proceed without making any changes to districts
# add year
iaDataNss0304$year <- 1
# check for duplicates
length(unique(iaDataNss0304$Person_key)) == nrow(iaDataNss0304) # no duplicates

# 3.1.3.1 Subset data to men over 14 and under 65 only
# only men
iaDataNss0304_Men <- subset(iaDataNss0304, iaDataNss0304$B4_q4 == 1)
# keep only above 14 and below 65
iaDataNss0304_Men <- subset(iaDataNss0304_Men, iaDataNss0304_Men$B4_q5 > 14 & iaDataNss0304_Men$B4_q5 < 65)
# keep only relevant vars
names(iaDataNss0304_Men)
iaDataNss0304_Men <- iaDataNss0304_Men[, c("HHID", "Person_key", "district_code", "B4_q12", "empSector_ag", "empSector_manu", "empSector_serv", "B4_q5", "B4_q6","B4_q7", "State", "year")]
# keep consistent names
names(iaDataNss0304_Men) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0304_Men)

# 3.1.3.2 Subset data to women over 14 and under 65 only
# keep only women
iaDataNss0304_Women <- subset(iaDataNss0304, iaDataNss0304$B4_q4 == 2)
# keep only above 14 and below 65
iaDataNss0304_Women <- subset(iaDataNss0304_Women, iaDataNss0304_Women$B4_q5 > 14 & iaDataNss0304_Women$B4_q5 < 65)
# keep only relevant vars
names(iaDataNss0304_Women)
iaDataNss0304_Women <- iaDataNss0304_Women[, c("HHID", "Person_key", "district_code", "B4_q12", "empSector_ag", "empSector_manu", "empSector_serv", "B4_q5", "B4_q6","B4_q7", "State", "year")]
# keep consistent names
names(iaDataNss0304_Women) <- c("HHID", "PID", "district_code", "Weekly_principal_activity_status", "Ag", "Manufac", "Serv", "Age",  "Marital_status", "General_education", "State", "year")
names(iaDataNss0304_Women)

# append 0304 through 1112
# women
iaDataNss0312_Women <- rbind(iaDataNss0304_Women, iaDataNss0405_Women, iaDataNss0506_Women, iaDataNss0708_Women, iaDataNss0910_Women, iaDataNss1112_Women)
write.csv(iaDataNss0312_Women, "./Output/datafiles/iaDataNss0312_Women.csv")
# men
iaDataNss0312_Men <- rbind(iaDataNss0304_Men, iaDataNss0405_Men, iaDataNss0506_Men, iaDataNss0708_Men, iaDataNss0910_Men, iaDataNss1112_Men)
write.csv(iaDataNss0312_Men, "./Output/datafiles/iaDataNss0312_Men.csv")
table(iaDataNss0312_Men$year)
