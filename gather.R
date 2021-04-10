library(tidyverse)
library(readr)
library(primer.data)
library(janitor)
library(ggplot2)
library(gcookbook)
library(lubridate)
library(tidycensus)

####################### MASTER DATA SECTION ############################

# master_data <- covid_data %>%
#as.character(covid_data$countyFIPS) %>%
#inner_join(baltimore_merged_data, by = "countyFIPS") %>%
#inner_join(cincinnati_merged_data, by = "countyFIPS") %>%
#inner_join(los_angeles_merged_data, by = "countyFIPS") %>%
#inner_join(orlando_merged_data, by = "countyFIPS") %>%
#inner_join(portland_merged_data, by = "countyFIPS") %>%
#inner_join(seattle_merged_data, by = "countyFIPS")


###################### COVID-19 DATA SECTION ############################

# Read covid data
covid_data <- read_csv("covid-and-domestic-dispuits/rawdata/covid_confirmed_usafacts (1) copy 2.csv", 
                       col_types = cols(.default = col_double(),
                                        `County Name` = col_character(),
                                        State = col_character(),
                                        StateFIPS = col_character())) %>%
  filter(`County Name` == "Baltimore County" |
           `County Name` == "Maricopa County" |
           `County Name` == "Hamilton County" |
           `County Name` == "Los Angeles County" |
           `County Name` == "Multnomah County" | 
           `County Name` == "Orange County" |
           `County Name` == "King County") %>%
  filter(countyFIPS == "24005" |
           countyFIPS == "4013" |
           countyFIPS == "39061" |
           countyFIPS == "6037" |
           countyFIPS == "41051" |
           countyFIPS == "12095" |
           countyFIPS == "53033") %>%
  rowwise() %>%
  mutate(total_covid_cases = sum(c_across(c(`2020-01-22`:`2021-03-11`)))) %>%
  mutate(January_2020 = sum(c_across(c(`2020-01-22`:`2020-01-31`)))) %>%
  mutate(February_2020 = sum(c_across(c(`2020-02-01`:`2020-02-29`)))) %>%
  mutate(March_2020 = sum(c_across(c(`2020-03-01`:`2020-03-31`)))) %>%
  mutate(April_2020 = sum(c_across(c(`2020-04-01`:`2020-04-30`)))) %>%
  mutate(May_2020 = sum(c_across(c(`2020-05-01`:`2020-05-31`)))) %>%
  mutate(June_2020 = sum(c_across(c(`2020-06-01`:`2020-06-30`)))) %>%
  mutate(July_2020 = sum(c_across(c(`2020-07-01`:`2020-07-31`)))) %>%
  mutate(August_2020 = sum(c_across(c(`2020-08-01`:`2020-08-31`)))) %>%
  mutate(September_2020 = sum(c_across(c(`2020-09-01`:`2020-09-30`)))) %>%
  mutate(October_2020 = sum(c_across(c(`2020-10-01`:`2020-10-31`)))) %>%
  mutate(November_2020 = sum(c_across(c(`2020-11-01`:`2020-11-30`)))) %>%
  mutate(December_2020 = sum(c_across(c(`2020-12-01`:`2020-12-31`)))) %>%
  mutate(January_2021 = sum(c_across(c(`2021-01-01`:`2021-01-31`)))) %>%
  mutate(February_2021 = sum(c_across(c(`2021-02-01`:`2021-02-28`)))) %>%
  mutate(March_2021 = sum(c_across(c(`2021-03-01`:`2021-03-11`)))) %>%
  select(countyFIPS, `County Name`, State, total_covid_cases, January_2020, 
         February_2020, March_2020, April_2020, May_2020,
         June_2020, July_2020, August_2020, September_2020,
         October_2020, November_2020, December_2020, January_2021, 
         February_2021, March_2021) %>%
  pivot_longer(names_to = "Month", 
               values_to = "COVID-19 Cases",
               cols = January_2020 : March_2021) %>%
  separate(Month, c("Month", "Year"), "_") 


saveRDS(covid_data, "covid-and-domestic-dispuits/cleandata/covid-data.rds")

# create a plot for milestone 6
covid_data %>%
  ggplot(aes(x = `County Name`,
             y = total_covid_cases)) +
  geom_point() +
  labs(title = "Total Covid Cases by County between 2020-01-22 and 2021-03-11",
       x = " ",
       y = "Total Covid Cases",
       caption = "Source: USA Today COVID-19 Data") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  coord_flip() +
  theme_classic()


###################### RACE VARS DATA SECTION ############################

# Race variables 
racevars <- c(White = "B02001_002", 
              Black = "B02001_003", 
              Asian = "B02001_005",
              Hispanic = "B03003_003")

###################### BALTIMORE DATA SECTION ############################

# Read in police data for Baltimore, Maryland 

baltimore <- read_csv("covid-and-domestic-dispuits/rawdata/Baltimore_911_Calls_For_Service_2020 copy 2.csv",
                      col_types = cols(OBJECTID = col_double(),
                                       recordId = col_double(),
                                       callKey = col_character(),
                                       callDateTime = col_character(),
                                       priority = col_character(),
                                       district = col_character(),
                                       description = col_character(),
                                       callNumber = col_character(),
                                       incidentLocation = col_character(),
                                       location = col_character(),
                                       Neighborhood = col_character(),
                                       PoliceDistrict = col_character(),
                                       PolicePost = col_double(),
                                       CouncilDistrict = col_double(),
                                       SheriffDistricts = col_character(),
                                       Community_Statistical_Areas = col_character(),
                                       Census_Tracts = col_character(),
                                       VRIZones = col_character(),
                                       ZIPCode = col_double())) %>%
  filter(description == "FAMILY DISTURB") %>%
  mutate(countyFIPS = "24005") %>%
  select(callDateTime, priority, district, description, Neighborhood, PoliceDistrict,
         Census_Tracts, ZIPCode, countyFIPS)

# Get ACS data on Baltimore County 

baltimore_acs <- get_acs(geography = "county",
                         variables = racevars, 
                         year = 2018,
                         state = "MD",
                         county = "Baltimore County",
                         geometry = TRUE,
                         summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID) 

# Join the two datasets for Baltimore data 

baltimore_merged_data <- baltimore %>%
  inner_join(baltimore_acs, by = "countyFIPS") 


###################### CINCINNATI DATA SECTION ############################

# Read police data for Cincinnati, Ohio 
cincinnati <- read_csv("covid-and-domestic-dispuits/rawdata/Cinncinati_PDI__Police_Data_Initiative copy 2.csv", 
                       col_types = cols(ADDRESS_X = col_character(),
                                        LATITUDE_X = col_double(),
                                        LONGITUDE_X = col_double(),
                                        AGENCY = col_character(),
                                        CREATE_TIME_INCIDENT = col_character(),
                                        DISPOSITION_TEXT = col_character(),
                                        EVENT_NUMBER = col_character(),
                                        INCIDENT_TYPE_ID = col_character(),
                                        INCIDENT_TYPE_DESC = col_character(),
                                        PRIORITY = col_double(),
                                        PRIORITY_COLOR = col_character(),
                                        ARRIVAL_TIME_PRIMARY_UNIT = col_character(),
                                        CLOSED_TIME_INCIDENT = col_character(),
                                        DISPATCH_TIME_PRIMARY_UNIT = col_character(),
                                        BEAT = col_character(),
                                        COMMUNITY_COUNCIL_NEIGHBORHOOD = col_character(),
                                        DISTRICT = col_double(),
                                        SNA_NEIGHBORHOOD = col_character(),
                                        CPD_NEIGHBORHOOD = col_character())) %>%
  filter(INCIDENT_TYPE_ID == "FAMTRB") %>%
  select(INCIDENT_TYPE_ID, INCIDENT_TYPE_DESC, CREATE_TIME_INCIDENT, 
         COMMUNITY_COUNCIL_NEIGHBORHOOD, ADDRESS_X) %>%
  mutate(countyFIPS = "39061") 


# Read in Hamilton County acs data

hamilton_acs <- get_acs(geography = "county",
                        variables = racevars, 
                        year = 2018,
                        state = "OH",
                        county = "Hamilton County",
                        geometry = TRUE,
                        summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)

# Join the two datasets for Cincinnati data 

cincinnati_merged_data <- cincinnati %>%
  inner_join(hamilton_acs, by = "countyFIPS") 

###################### LOS ANGELES DATA SECTION ############################

# Read police data for Los Angeles, California
los_angeles <- read_csv("covid-and-domestic-dispuits/rawdata/LAPD_Calls_for_Service_2020 copy 2.csv", 
                        col_types = cols(Incident_Number = col_character(),
                                         Area_Occ = col_character(),
                                         Rpt_Dist = col_character(),
                                         Dispatch_Date = col_character(),
                                         Dispatch_Time = col_time(format = ""),
                                         Call_Type_Code = col_character(),
                                         Call_Type_Text = col_character())) %>%
  filter(Call_Type_Text %in% c("DOM VIOL", "MAN/WMN", "DOM VIOL SUSP", 
                               "MAN ASSLTG WMN", "FAMILY", "DOM VIOL R/O",
                               "ABUSE INVEST")) %>%
  select(Call_Type_Text, Area_Occ, Dispatch_Date) %>%
  mutate(countyFIPS = "6037")

# Get Los Angeles County acs data

los_angeles_acs <- get_acs(geography = "county",
                           variables = racevars, 
                           year = 2018,
                           state = "CA",
                           county = "Los Angeles County",
                           geometry = TRUE,
                           summary_var = "B02001_001") %>%
  mutate(countyFIPS = "6037") %>%
  select(countyFIPS, NAME, variable, estimate) 

# Join the two datasets for Los Angeles data 

los_angeles_merged_data <- los_angeles %>%
  inner_join(los_angeles_acs, by = "countyFIPS")


###################### PORTLAND DATA SECTION ############################

# Read police data for Portland, Oregon

portland <- read_csv("covid-and-domestic-dispuits/rawdata/Portland_CAD-2020 copy 2.csv", 
                     col_types = cols(Address = col_logical(),
                                      CallNumber = col_double(),
                                      FinalCallCategory = col_character(),
                                      FinalCallGroup = col_character(),
                                      Neighborhood = col_character(),
                                      OpenDataLat = col_logical(),
                                      OpenDataLon = col_logical(),
                                      OpenDataX = col_logical(),
                                      OpenDataY = col_logical(),
                                      Priority = col_character(),
                                      ReportMonthYear = col_character(),
                                      ResponseTime_sec = col_number(),
                                      TimeInQueue_sec = col_number(),
                                      TravelTime_sec = col_number())) %>%
  filter(FinalCallCategory == "Domestic Violence") %>%
  select(FinalCallCategory, Neighborhood, ReportMonthYear) %>%
  mutate(countyFIPS = "41051")

# Get Multnomah County acs data

multnomah_acs <- get_acs(geography = "county",
                         variables = racevars, 
                         year = 2018,
                         state = "OR",
                         county = "Multnomah County",
                         geometry = TRUE,
                         summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)


# Join the two datasets for Portland data 

portland_merged_data <- portland %>%
  inner_join(multnomah_acs, by = "countyFIPS")


###################### ORLANDO DATA SECTION ############################

# Read police data for Orlando, Florida

orlando <- read_csv("covid-and-domestic-dispuits/rawdata/Orlando_OPD_Calls_For_Service copy 2.csv", 
                    col_types = cols(`Incident Number` = col_character(),
                                     `Incident Date Time` = col_character(),
                                     `Incident Location` = col_character(),
                                     `Incident Type` = col_character(),
                                     `Incident Disposition Class` = col_character(),
                                     `Incident Disposition` = col_character(),
                                     Status = col_character(),
                                     Location = col_character())) %>%
  filter(`Incident Type` %in% c("Domestic disturbanc", "House/business check")) %>%
  select(`Incident Date Time`, `Incident Location`, `Incident Type`) %>%
  mutate(countyFIPS = "12095")

# Get Orange County acs data

orange_acs <- get_acs(geography = "county",
                      variables = racevars, 
                      year = 2018,
                      state = "FL",
                      county = "Orange County",
                      geometry = TRUE,
                      summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)

# Join the two datasets for Orlando data 

orlando_merged_data <- orlando %>%
  inner_join(orange_acs, by = "countyFIPS")

###################### SEATTLE DATA SECTION ############################

# Read data for Seattle, Washington

seattle <- read_csv("covid-and-domestic-dispuits/rawdata/Seattle_Call_Data copy 2.csv", 
                    col_types = cols(`CAD Event Number` = col_double(),
                                     `Event Clearance Description` = col_character(),
                                     `Call Type` = col_character(),
                                     Priority = col_double(),
                                     `Initial Call Type` = col_character(),
                                     `Final Call Type` = col_character(),
                                     `Original Time Queued` = col_character(),
                                     `Arrived Time` = col_character(),
                                     Precinct = col_character(),
                                     Sector = col_character(),
                                     Beat = col_character())) %>%
  filter(`Initial Call Type` %in% c("PREMISE CHECK, OFFICER INITIATED ONVIEW ONLY",
                                    "DISTURBANCE, MISCELLANEOUS/OTHER", 
                                    "SERVICE - WELFARE CHECK", "DIST - DV - NO ASLT",
                                    "REQUEST TO WATCH", "ASLT - IP/JO - DV", 
                                    "PEACE-STANDBY TO ASSURE (NO COURT ORDR SVC)",
                                    "THREATS (INCLS IN-PERSON/BY PHONE/IN WRITING)")) %>%
  select(`Initial Call Type`, `Final Call Type`, `Original Time Queued`, Sector) %>%
  mutate(countyFIPS = "12095")


# King County acs data

king_acs <- get_acs(geography = "county",
                    variables = racevars, 
                    year = 2018,
                    state = "WA",
                    county = "King County",
                    geometry = TRUE,
                    summary_var = "B02001_001") %>%
  select(GEOID, NAME, variable, estimate) %>%
  rename(countyFIPS = GEOID)

# Join the two datasets for Orlando data 

seattle_merged_data <- seattle %>%
  inner_join(king_acs, by = "countyFIPS")



