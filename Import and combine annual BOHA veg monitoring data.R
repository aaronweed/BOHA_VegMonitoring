# Function to import and combine annual BOHA veg monitoring data 

importData<-function (x){

#load packages
library(tidyverse)
library(magrittr)
library(lubridate)
library(readxl)


# The process will be to import individual veg monitoring data per year and island and then combine
# data by subprotocol (herb, scrub, tree)

#### import data sets and save as lists to working directory----

#Thompson Island

# Thompson 2019
path<-"./data/Thompson2019.xlsx" 

Thomp19<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)
  
# Thompson 2018
path<-"./data/Thompson2018.xlsx" 

Thomp18<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Thompson 2017
path<-"./data/Thompson2017.xlsx" 

Thomp17<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Thompson 2016
path<-"./data/Thompson2016.xlsx" 

Thomp16<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Thompson 2015
path<-"./data/Thompson2015.xlsx" 

Thomp15<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)


# Grape

# Grape 2019
path<-"./data/Grape2019.xlsx" 

Grape19<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Grape 2018
path<-"./data/Grape2018.xlsx" 

Grape18<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Grape 2017
path<-"./data/Grape2017.xlsx" 

Grape17<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Grape 2016
path<-"./data/Grape2016.xlsx" 

Grape16<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)

# Grape 2015
path<-"./data/Grape2015.xlsx" 

Grape15<-path %>%
  excel_sheets() %>%
  purrr::set_names() %>%
  map(read_excel, path = path)


### Extract data from lists and create combined data per subprotocol (e.e.g, herb layer) ----


# Extract layer data per year/island and combine into one DF 

herbs<-bind_rows(Thomp15$`HERB LAYER`,Thomp16$`HERB LAYER`,Thomp17$`HERB LAYER`,Thomp18$`HERB LAYER`,Thomp19$`HERB LAYER`, 
                 Grape15$`HERB LAYER`,Grape16$`HERB LAYER`,Grape17$`HERB LAYER`,Grape18$`HERB LAYER`,Grape19$`HERB LAYER`) %>% View()


scrubs<-bind_rows(Thomp15$`SCRUB LAYER`,Thomp16$`SCRUB LAYER`,Thomp17$`SCRUB LAYER`,Thomp18$`SCRUB LAYER`,Thomp19$`SCRUB LAYER`,
                  Grape15$`SCRUB LAYER`,Grape16$`SCRUB LAYER`,Grape17$`SCRUB LAYER`,Grape18$`SCRUB LAYER`,Grape19$`SCRUB LAYER`) %>% View()


trees<-bind_rows(Thomp15$`TREE LAYER`,Thomp16$`TREE LAYER`,Thomp17$`TREE LAYER`,Thomp18$`TREE LAYER`,Thomp19$`TREE LAYER`,
                 Grape15$`TREE LAYER`,Grape16$`TREE LAYER`,Grape17$`TREE LAYER`,Grape18$`TREE LAYER`,Grape19$`TREE LAYER`) %>% View()


# Add some fields and check the datasets
# 6/24/21: right now there are "-" and 0s in the Density column when Cover > 0. I asked Sahil/Rachel to clarify. I think the "-" should be NAs.


########## HERBS ########
#########################

# for testing df until data are finalized:
herbs<-bind_rows(Thomp16$`HERB LAYER`,Thomp18$`HERB LAYER`)
head(herbs)

herbs <-herbs %>% mutate(year= year(Date), month= month(Date)) %>% # extract year and month for later filtering/modelling
  mutate(Pct_Cover = case_when( 
    Cover == 0  ~ 0,
    Cover == 1  ~ 0.25,
    Cover == 2  ~ 0.5,
    Cover == 3  ~ 1.5,
    Cover == 4  ~ 3.5,
    Cover == 5  ~ 7.5,
    Cover == 6  ~ 17.5,
    Cover == 7  ~ 37.5,
    Cover == 8  ~ 62.5,
    Cover == 9  ~ 85,
    Cover == 10  ~ 97.5,
    TRUE ~ NA_real_)) %>%   # create median cover class for each estimate of Cover: 1= <0.5%;  2 = 0-1%;  3 = 1-2%;  4 = 2-5%;  5 = 5-10%;  6 = 10-25%;  7 = 25-50%;  8 = 50-75%;  9 = 75-95%;  10 = 95-100%
  mutate(Island = case_when(Island == "THOMPSON" ~ "Thompson",
                            Island == "Thompson" ~ "Thompson",
                            Island == "GRAPE" ~ "Grape")) %>% # rename Islands for consistency
  mutate(Zone = case_when(Zone == "U" ~ "Upland",
                          Zone == "W" ~"Wetland",
                          Zone == "Wet" ~"Wetland",
                          Zone == "UP" ~"Upland",
                          Zone == "WET" ~ "Wetland",
                          Zone == "C" ~ "Control")) # rename zones for consistency

## DATA CHECKS: FORMATTING, GROUPING LEVELS, and MISSING VALUES

## check levels of each column type
# ZOne
herb.obs.zones<-herbs %>% dplyr::select(Island,  Zone) %>%
  group_by(Island,  Zone) %>% 
  distinct() 

# transect X zone
herb.obs.trans.Zone<-herbs %>% dplyr::select(Island, Transect, Zone) %>%
  group_by(Island, Transect,  Zone) %>% 
  distinct()

#Count number of Transects sampled per Zone per year

herb.trans.year<-herbs %>% dplyr::select(Island, year, Transect,  Zone) %>%
  group_by(Island, year, Zone) %>% 
  distinct() %>% 
  tally(name= "Transects_Sampled")

#Count number of quads  sampled per transect, Zone per year

herb.quads.year<-herbs %>% dplyr::select(Island, year, Transect,  Zone, Quadrat) %>%
  group_by(Island, year, Zone, Transect) %>% 
  distinct() %>% 
  tally(name= "Quads_Sampled")

#Check for missing values in the observational data

herbs.cov.NAs<-herbs[is.na(herbs$Pct_Cover),]


########## SCRUBS ##########
############################

########## TREES ##########
###########################








## Clean up data, export to CSVs and return list of the DFs

write_csv(herbs, file = "./data/herbs.csv")

write_csv(scrubs, file = "./data/scrubs.csv")

write_csv(trees, file = "./data/trees.csv")

return(lst(herbs, scrubs, trees))
}










