library(tidyverse)
library(readxl)
library(ggplot2)
library(urbnmapr)

###############################################################################################################################

unemployment <- read_excel("Unemployment.xls")
population <- read_excel("PopulationEstimates.xls")
education <- read_excel("Education.xls")

###############################################################################################################################

#Unemployment

table(unemployment$State)

#Find Co value
#unemployment$State

#Change Co to CO
unemployment$State[254] <- "CO"

#Double Check
#table(unemployment$State)

#Convert State to a factor
unemployment$State <- as.factor(unemployment$State)

#Take out some columns
unemployment <- select(unemployment, FIPS:Area_name, Civilian_labor_force_2010:Median_Household_Income_2017)

#view(unemployment)


#I want to separate the states from the counties. This can be filtered by fips since fips is numeric
unemployment_state <- unemployment %>%
  filter(FIPS %% 1000 == 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_name")

#Now separate the counties from the states

unemployment_counties <- unemployment %>%
  filter(FIPS %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_name")


#Convert county_fips to character
unemployment_state$county_fips <- as.character(unemployment_state$county_fips)

unemployment_counties$county_fips <- as.character(unemployment_counties$county_fips)

###############################################################################################################################

##Population

#table(population$State)

#Check to see if FIPS is numeric
#str(population)

#Conver FIPS to numeric
population$FIPS <- as.numeric(population$FIPS)

#Convert State to a factor
population$State <- as.factor(population$State)

#Take out columns that aren't needed
population <- select(population, FIPS:Area_Name, POP_ESTIMATE_2010:POP_ESTIMATE_2018, Births_2010:Deaths_2018)

#Separate state data & county data
population_state <- population %>%
  filter(FIPS %% 1000 == 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_Name")

population_county <- population %>%
  filter(FIPS %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_Name")

#Convert county_fips back to character
population_state$county_fips <- as.character(population_state$county_fips)

population_counties$county_fips <- as.character(population_counties$county_fips)


###############################################################################################################################

#Education

#table(education$State)

#check to see if FIPS_Code is numeric
#str(education)

#Conver FIPS to numeric
education$FIPS_Code <- as.numeric(education$FIPS_Code)


#Adjust column names
names(education) <- str_replace_all(names(education), c(" " = "_", "," = "_", "-" = "_to_", "'"="", "__"  = "_"))
#view(education)

#Convert State to a factor
education$State <- as.factor(education$State)

#Take out columns that aren't needed
education <- select(education, FIPS_Code:Area_name, 
                    Less_than_a_high_school_diploma__2000:Percent_of_adults_with_a_bachelors_degree_or_higher__2013_to_17)

#Separate state data & county data
education_state <- education %>%
  filter(FIPS_Code %% 1000 == 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS_Code", state = "State", state_name = "Area_name")

education_county <- education %>%
  filter(FIPS_Code %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS_Code", state = "State", state_name = "Area_name")

#Convert county_fips back to character
education_state$county_fips <- as.character(education_state$county_fips)

education_county$county_fips <- as.character(education_county$county_fips)


##############################################################################################################################

#Prepping map data for plotting

#Create blank map of the US states
blank_states <- get_urbn_map("states", sf = TRUE)

#head(blank_states)

blank_states <- rename(blank_states, state = "state_abbv")

blank_states$state <- as.factor(blank_states$state)

#Create blank map of the US counties
blank_counties <- get_urbn_map("counties", sf = TRUE)

#head(blank_map)

blank_counties <- rename(blank_counties, state = "state_abbv")

#Convert state to a factor
blank_counties$state <- as.factor(blank_counties$state)

blank_counties <- arrange(blank_counties, county_fips)

##############################################################################################################################

left_join(blank_states,population_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = POP_ESTIMATE_2010),
          color  = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 2) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Population Estimate 2010")



