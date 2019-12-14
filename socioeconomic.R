library(tidyverse)
library(readxl)
library(ggplot2)
library(urbnmapr)


unemployment <- read_excel("Unemployment.xls")
population <- read_excel("PopulationEstimates.xls")
education <- read_excel("Education.xls")

#Data Cleaning
##########################################################################################

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

#Filter out GA, OH, and TX
unemployment <- filter(unemployment, State == "GA" | 
                         State == "OH" | State == "TX")

#Take out some columns, take out area name to since it is inputted differently
unemployment <- select(unemployment, FIPS:State, Civilian_labor_force_2010:Median_Household_Income_2017)
#view(unemployment)

#Rename column
unemployment <- rename(unemployment, county_fips = "FIPS", state = "State")

#Convert unemployment$county_fips to character
unemployment$county_fips <- as.character(unemployment$county_fips)


#Add zero in front of fips code to match the other files
#unemployment$county_fips <- sapply(0, paste0,unemployment$fips_county)

##########################################################################################

##Population

table(population$State)

#Convert State to a factor
population$State <- as.factor(population$State)

#Filter out GA, OH, and TX
population <- filter(population, State == "GA" | 
                         State == "OH" | State == "TX")

#Take out columns that aren't needed
population <- select(population, FIPS:Area_Name, POP_ESTIMATE_2010:POP_ESTIMATE_2018, Births_2010:Deaths_2018)

#Rename columns
population <- rename(population, county_fips = "FIPS", county_name = "Area_Name", state = "State")


##########################################################################################

#Education

#table(education$State)

#Adjust column names
names(education) <- str_replace_all(names(education), c(" " = "_", "," = "_", "-" = "_to_", "'"=""))
#view(education)

#Convert State to a factor
education$State <- as.factor(education$State)

#Filter out GA, OH, and TX
education <- filter(education, State == "GA" | 
                       State == "OH" | State == "TX")

#Take out columns that aren't needed
education <- select(education, FIPS_Code:Area_name, 
                    Less_than_a_high_school_diploma__2000:Percent_of_adults_with_a_bachelors_degree_or_higher__2013_to_17)

#Rename columns
education <- rename(education, county_fips = "FIPS_Code", county_name = "Area_name", state = "State")

##########################################################################################

# Joining tables

dataset <- left_join(unemployment, population)

dataset <- left_join(dataset, education)

##########################################################################################

#Pull out "Georgia", "Ohio", and "Texas" data points because they aren't in county urbnmaps

states <- filter(dataset, county_name == "Georgia" | 
                         county_name == "Ohio" | county_name == "Texas")

dataset <- filter(dataset, !(county_name == "Georgia" | 
                    county_name == "Ohio" | county_name == "Texas"))

##########################################################################################

#Prepping map data for plotting

#Create blank map of the US states
blank_states <- get_urbn_map("states", sf = TRUE)

#head(blank_states)

blank_states <- rename(blank_states, state = "state_abbv")

blank_states$state <- as.factor(blank_states$state)

blank_states <- filter(blank_states, state == "GA" | 
                      state == "OH" | state == "TX")

states_data <- left_join(blank_states, states)


#Create blank map of the US counties
blank_counties <- get_urbn_map("counties", sf = TRUE)

#head(blank_map)

blank_counties <- rename(blank_counties, state = "state_abbv")

#Convert state to a factor
blank_counties$state <- as.factor(blank_counties$state)

#Filter out GA, OH, and TX
blank_counties <- filter(blank_counties, state == "GA" | 
                      state == "OH" | state == "TX")

blank_counties <- arrange(blank_counties, county_fips) 

#Join mapping dataset
county_data <- left_join(blank_counties, dataset)

#create labeling da

##########################################################################################

#Create bar graph of the states' populations

ggplot(data = states_data, aes(x = state_name, y = POP_ESTIMATE_2018)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Population of People", labels = scales::comma)
  
##########################################################################################

#Create line graph to show the change in population

states_data %>%
  select(state_fips:state_name, POP_ESTIMATE_2010:POP_ESTIMATE_2018, geometry) %>%
  rename(
    "2010" = POP_ESTIMATE_2010,
    "2011" = POP_ESTIMATE_2011,
    "2012" = POP_ESTIMATE_2012,
    "2013" = POP_ESTIMATE_2013,
    "2014" = POP_ESTIMATE_2014, 
    "2015" = POP_ESTIMATE_2015,
    "2016" = POP_ESTIMATE_2016,
    "2017" = POP_ESTIMATE_2017,
    "2018" = POP_ESTIMATE_2018
    ) %>%
  gather(Year, Population, "2010":"2018") %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Population, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Population of People", labels = scales::comma) +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50))

##########################################################################################

#Create bar graph of the states' 2018 unemployment numbers

ggplot(data = states_data, aes(x = state_name, y = Unemployed_2018)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Number of Unemployed People", labels = scales::comma)

##########################################################################################

#Create line graph to show the change in unemployment numbers

states_data %>%
  select(state_fips:state_name, Unemployed_2010, Unemployed_2011, Unemployed_2012,
         Unemployed_2013, Unemployed_2014, Unemployed_2015, Unemployed_2016, 
         Unemployed_2017, Unemployed_2018) %>%
  rename(
    "2010" = Unemployed_2010,
    "2011" = Unemployed_2011,
    "2012" = Unemployed_2012,
    "2013" = Unemployed_2013,
    "2014" = Unemployed_2014, 
    "2015" = Unemployed_2015,
    "2016" = Unemployed_2016,
    "2017" = Unemployed_2017,
    "2018" = Unemployed_2018
  ) %>%
  gather(Year, Unemployed, "2010":"2018") %>%
  view(states_data)
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Unemployed, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number of People Unemployed", labels = scales::comma) +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50))

##########################################################################################

#Create line graph to show the change in unemployment rate

states_data %>%
  select(state_fips:state_name, Unemployment_rate_2010, Unemployment_rate_2011, Unemployment_rate_2012,
         Unemployment_rate_2013, Unemployment_rate_2014, Unemployment_rate_2015, Unemployment_rate_2016, 
         Unemployment_rate_2017, Unemployment_rate_2018) %>%
  rename(
    "2010" = Unemployment_rate_2010,
    "2011" = Unemployment_rate_2011,
    "2012" = Unemployment_rate_2012,
    "2013" = Unemployment_rate_2013,
    "2014" = Unemployment_rate_2014, 
    "2015" = Unemployment_rate_2015,
    "2016" = Unemployment_rate_2016,
    "2017" = Unemployment_rate_2017,
    "2018" = Unemployment_rate_2018
  ) %>%
  gather(Year, Unemployment_rate, "2010":"2018") %>%
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Unemployment_rate, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Percent of Unemployed") +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50))

##########################################################################################

#Create bar graph to compare percent of those that did not graduate high school 2000

ggplot(data = states_data, aes(x = state_name, 
                               y = Percent_of_adults_with_less_than_a_high_school_diploma__2000)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Percent")

##########################################################################################

#Create bar graph to compare percent of those that did not graduate high school 2017

ggplot(data = states_data, aes(x = state_name, 
                               y = Percent_of_adults_with_less_than_a_high_school_diploma__2013_to_17)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Percent")

##########################################################################################

#Create bar graph to show education levels in 2000

  states_data %>%
    select(state_fips:state_name, Less_than_a_high_school_diploma__2000: Bachelors_degree_or_higher__2013_to_17) %>%
    rename(
      "No High School Diploma 2000" = Less_than_a_high_school_diploma__2000,
      "High School Diploma 2000" = High_school_diploma_only__2000,
      "Some College 2000" = Some_college_or_associates_degree__2000,
      "Bachelors or Higher 2000" = Bachelors_degree_or_higher__2000,
      "No High School Diploma 2013-17" = Less_than_a_high_school_diploma__2013_to_17, 
      "High School Diploma 2013-17" = High_school_diploma_only__2013_to_17,
      "Some College 2013-17" = Some_college_or_associates_degree__2013_to_17,
      "Bachelors or Higher 2013-17" = Bachelors_degree_or_higher__2013_to_17
    ) %>%
    gather(Education, People, "No High School Diploma 2000":"Bachelors or Higher 2000") %>%
    ggplot(mapping = aes(x = Education, y = People, fill = state)) +
    geom_bar(stat="identity", position = "dodge", color = "black") +
    scale_x_discrete(name = "Education") +
    scale_y_continuous(name = "Number of People", labels = scales::comma) +
    theme(axis.text.x = element_text(size = 8, angle = 0)) +
    coord_flip()
  
  ##########################################################################################
  
  #Create bar graph to show education levels in 2017
  
  states_data %>%
    select(state_fips:state_name, Less_than_a_high_school_diploma__2000: Bachelors_degree_or_higher__2013_to_17) %>%
    rename(
      "No High School Diploma 2000" = Less_than_a_high_school_diploma__2000,
      "High School Diploma 2000" = High_school_diploma_only__2000,
      "Some College 2000" = Some_college_or_associates_degree__2000,
      "Bachelors or Higher 2000" = Bachelors_degree_or_higher__2000,
      "No High School Diploma 2013-17" = Less_than_a_high_school_diploma__2013_to_17, 
      "High School Diploma 2013-17" = High_school_diploma_only__2013_to_17,
      "Some College 2013-17" = Some_college_or_associates_degree__2013_to_17,
      "Bachelors or Higher 2013-17" = Bachelors_degree_or_higher__2013_to_17
    ) %>%
    gather(Education, People, "No High School Diploma 2013-17":"Bachelors or Higher 2013-17") %>%
    ggplot(mapping = aes(x = Education, y = People, fill = state)) +
    geom_bar(stat="identity", position = "dodge", color = "black") +
    scale_x_discrete(name = "Education") +
    scale_y_continuous(name = "Number of People", labels = scales::comma) +
    theme(axis.text.x = element_text(size = 8, angle = 0)) +
    coord_flip()

  ##########################################################################################
  
#Graph the population of counties of the US in 2017
  
  county_data %>%
    ggplot() +
    geom_sf(mapping = aes(fill = POP_ESTIMATE_2017),
            color = "green", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "Population Estimate 2017")  
  
##########################################################################################
  
#Graph the population of counties in 2017
  
  county_data %>%
    filter(state == "TX"|state == "GA"| state =="OH") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = POP_ESTIMATE_2017),
            color = "green", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "Population Estimate 2017")  

##########################################################################################
  
#Graph to see unemployment rates in 2017
  
  county_data %>%
    filter(state == "TX"|state == "GA"| state =="OH") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Unemployment_rate_2017),
            color = "white", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "Unemployment Rate 2017")
  
##########################################################################################
  
#Graph to see counties with the most people without high school diploma
  
  county_data %>%
    filter(state == "TX"|state == "GA"| state =="OH") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Less_than_a_high_school_diploma__2000),
            color = "white", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "No High School 2000")  
  
##########################################################################################
  
#Graph to see counties with the most people with at least a bachelors
  
  county_data %>%
    filter(state == "TX"|state == "GA"| state =="OH") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Bachelors_degree_or_higher__2000),
            color = "white", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "Bachelors or Higher 2000")

  ##########################################################################################
  
  #Graph to see counties with the most people without high school diploma
  
  county_data %>%
    filter(state == "TX"|state == "GA"| state =="OH") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Less_than_a_high_school_diploma__2013_to_17),
            color = "white", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "No High School 2017")  
  
  ##########################################################################################
  
  #Graph to see counties with the most people with at least a bachelors
  
  county_data %>%
    filter(state == "TX"|state == "GA"| state =="OH") %>%
    ggplot() +
    geom_sf(mapping = aes(fill = Bachelors_degree_or_higher__2013_to_17),
            color = "white", size = 0.05) +
    geom_sf_text(data = states_data, aes(label = state), size = 3) +
    coord_sf(datum = NA) +
    scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
    labs(fill = "Bachelors or Higher 2017")
