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


# Extract layer data per year/island and combine into one DF. Also currently removes the notes fields for easier binding (1 or more years without NOtes w/ ID field)

herbs<-bind_rows(Thomp16$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Thomp18$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Thomp19$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")), 
                 Grape16$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Grape18$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Grape19$`HERB LAYER` %>%  dplyr::select(-starts_with("Note"))) %>% View()


scrubs<-bind_rows(Thomp15$`SCRUB LAYER`,Thomp16$`SCRUB LAYER`,Thomp17$`SCRUB LAYER`,Thomp18$`SCRUB LAYER`,Thomp19$`SCRUB LAYER`,
                  Grape15$`SCRUB LAYER`,Grape16$`SCRUB LAYER`,Grape17$`SCRUB LAYER`,Grape18$`SCRUB LAYER`,Grape19$`SCRUB LAYER`) %>% View()


trees<-bind_rows(Thomp15$`TREE LAYER`,Thomp16$`TREE LAYER`,Thomp17$`TREE LAYER`,Thomp18$`TREE LAYER`,Thomp19$`TREE LAYER`,
                 Grape15$`TREE LAYER`,Grape16$`TREE LAYER`,Grape17$`TREE LAYER`,Grape18$`TREE LAYER`,Grape19$`TREE LAYER`) %>% View()


# Add some fields and check the datasets
# 6/24/21: right now there are "-" and 0s in the Density column when Cover > 0. I asked Sahil/Rachel to clarify. I think the "-" should be NAs.



########## HERBS ########

## DATA CHECKS: FORMATTING, GROUPING LEVELS, and MISSING VALUES  ########

# for testing df until above data are finalized:
herbs<-bind_rows(Thomp16$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Thomp18$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Thomp19$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")), 
                 Grape16$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Grape18$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")),Grape19$`HERB LAYER` %>%  dplyr::select(-starts_with("Note")))

#View(herbs)

# inspect herbs object:
str(herbs)

# Clean up column values (Island and Zone) and Derive new variables from the data set: year, month, Pct_Cover

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
                            Island == "GRAPE" ~ "Grape",
                            Island == "Grape" ~ "Grape")) %>% # rename Islands for consistency
  mutate(Zone = case_when(Zone == "U" ~ "Upland",
                          Zone == "W" ~"Wetland",
                          Zone == "Wet" ~"Wetland",
                          Zone == "UP" ~"Upland",
                          Zone == "WET" ~ "Wetland",
                          Zone == "C" ~ "Control")) # rename zones for consistency



# check the levels or values of a few fields

unique(herbs$Island)# unique values of island
unique(herbs$Zone)

# Are there observations at each Island X Zone?

# Zone
herb.obs.zones<-herbs %>% dplyr::select(Island,  Zone, year) %>%
  group_by(Island,  Zone, year) %>% 
  distinct() 

View(herb.obs.zones)

# Are there any missing values that need to be addressed:

# in the year column
herbs[is.na(herbs$year),] %>% View()

# in the Island column
herbs[is.na(herbs$Island),] %>% View()

# transect X zone
herb.obs.trans.Zone<-herbs %>% dplyr::select(Island, Transect, Zone) %>%
  group_by(Island, Transect,  Zone) %>% 
  distinct() 

View(herb.obs.trans.Zone)

#Count number of Transects sampled per Zone per year

herb.trans.year<-herbs %>% dplyr::select(Island, year, Transect,  Zone) %>%
  group_by(Island, year, Zone) %>% 
  distinct() %>% 
  tally(name= "Transects_Sampled") %>% pivot_wider( names_from= year, values_from = Transects_Sampled)

View(herb.trans.year)


#Count number of quads  sampled per transect, Zone per year

herb.quads.year<-herbs %>% dplyr::select(Island, year, Zone, Transect,Quadrat) %>%
  group_by(Island, year, Zone, Transect) %>% 
  distinct() %>% 
  tally(name= "Quads_Sampled")

View(herb.quads.year)



##### #Check for missing values in the observational data (c0ver, Density, Pct_Cover)

# Species with no 
## there are apparently some species/cover classes that don't have cover values assigned to them.

herbs[is.na(herbs$Pct_Cover),] %>% View()

herbspecies.no.cover<-filter(herbs, is.na(Pct_Cover)) %>% select(Species) %>% distinct()  %>% as_tibble()
  
View(herbspecies.no.cover)

write_csv(herbspecies.no.cover, "./data/checks/herbspeciesNoCover.csv")

#### SHOULD WE DROP THESE SPECIES?
# iF SO:
#herbs<-drop_na(herbs,Pct_Cover) 



### ### GENERATE LIST OF OBSERVED SPECIES codes ###### 

herb.species<-herbs %>%  distinct(Species) %>% as_tibble() # generate list

tally(distinct(herb.species,Species)) # 355 species observed!

# import species list provided by Rachel Vincent: plant_lists_GRAP_THOM_FieldGiude.xlsx

species_tlu <- read_excel("data/plant_lists_GRAP_THOM_FieldGiude.xlsx")
tally(distinct(species_tlu,`BOHA Code`)) # 220 species in field guide

#Return the list of observed species from the field guide

Obs.Herbs<-semi_join(species_tlu,herb.species, by = c("BOHA Code"= "Species")) 

# Show the list of observed species NOT in the field guide

anti_join(species_tlu,herb.species, by = c("BOHA Code"= "Species")) 



##### QUAD FREQUENCY

### What are the most abundant species?
#  DETERMINE THE FREQ EACH SPECIES HAS BEEN DETECTED EACH YEAR in each quadrat

herb.species.QuadFreq <-herbs %>%  group_by(Island, Zone, year, Transect, Species) %>% tally() %>% 
    left_join(herb.quads.year, by = c("Island", "Zone", "year", "Transect")) %>%   # ADD ON NUMBER OF QUADS SAMPLED PER YEAR (FROM ABOVE)
    mutate(Quad_Freq = n/Quads_Sampled)

#need to check potential duplicate records:

herb.check.dups<-filter(herb.species.QuadFreq , Quad_Freq >1)

write_csv(herb.check.dups, "./data/checks/herb.check.dups.csv")


#Calculate avg quad frequency;

herb.MeanSiteQuadFreq <-herbs %>%  group_by(Island, Zone, year, Transect, Species) %>% tally() %>% 
  left_join(herb.quads.year, by = c("Island", "Zone", "year", "Transect")) %>%   # ADD ON NUMBER OF QUADS SAMPLED PER YEAR (FROM ABOVE)
  mutate(Quad_Freq = n/Quads_Sampled) %>% 
  group_by(Species) %>% 
  summarise(MeanFreq = mean(Quad_Freq, na.rm= TRUE), Trans_Sampled= n(), sd= sd(Quad_Freq, na.rm= TRUE))


top50<- filter(herb.MeanSiteQuadFreq, MeanFreq >= 0.50 | Trans_Sampled > 20) %>% pull(Species) 


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










