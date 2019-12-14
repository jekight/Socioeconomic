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

unemployment_county <- unemployment %>%
  filter(FIPS %% 1000 != 0 & State != "US" & State != "PR") %>%
  rename(county_fips = "FIPS", state = "State", state_name = "Area_name")


#Convert county_fips to character
unemployment_state$county_fips <- as.character(unemployment_state$county_fips)

unemployment_county$county_fips <- as.character(unemployment_county$county_fips)

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

population_county$county_fips <- as.character(population_county$county_fips)


###############################################################################################################################

#Education

#table(education$State)

#Adjust column names
names(education) <- str_replace_all(names(education), c(" " = "_", "," = "_", "-" = "_to_", "'"="", "__"  = "_"))
#view(education)

#check to see if FIPS_Code is numeric
#str(education)

#Conver FIPS to numeric
education$FIPS_Code <- as.numeric(education$FIPS_Code)

#Convert State to a factor
education$State <- as.factor(education$State)

#Take out columns that aren't needed
education <- select(education, FIPS_Code:Area_name, 
                    Less_than_a_high_school_diploma_2000:Percent_of_adults_with_a_bachelors_degree_or_higher_2013_to_17)

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

unemployment_state <- mutate(unemployment_state, 
                             unemployment_percent_change = (Unemployment_rate_2018 - Unemployment_rate_2010)
                             /Unemployment_rate_2010)

left_join(blank_states,unemployment_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = unemployment_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Change in Unemployment Rate from 2010 to 2018")

unemployment_state <- mutate(unemployment_state,
                             laborforce_percent_change = (Civilian_labor_force_2018 - Civilian_labor_force_2010)
                             /Civilian_labor_force_2010)

left_join(blank_states,unemployment_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = laborforce_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Civilian Labor Force")


##############################################################################################################################

left_join(blank_states,population_state) %>%
  ggplot() +
    geom_sf(mapping = aes(fill = POP_ESTIMATE_2018),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Amount of People",
       x = "",
       y = "",
       title = "Population Estimate 2018")


population_state <- mutate(population_state,
                           population_percent_change = (POP_ESTIMATE_2018 - POP_ESTIMATE_2010)
                           / POP_ESTIMATE_2010)

left_join(blank_states,population_state) %>%
  ggplot() +
  geom_sf(mapping = aes(fill = population_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Population")

##############################################################################################################################

education_state <- mutate(education_state,
                           High_school_or_less = Less_than_a_high_school_diploma_2013_to_17 + 
                             High_school_diploma_only_2013_to_17)

left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = High_school_or_less),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Amount of People",
       x = "",
       y = "",
       title = "Number of People with a High School Education or Less 2017")



left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_degree_or_higher_2013_to_17),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Bachelors or Higher",
       x = "",
       y = "",
       title = "Number of People with a Bachelors Degree or Higher 2017")


education_state <- mutate(education_state,
                          highschool_or_less_percent_change = (High_school_or_less-(Less_than_a_high_school_diploma_2000 + 
                                                                 High_school_diploma_only_2000)) 
                            /(Less_than_a_high_school_diploma_2000 + 
                                High_school_diploma_only_2000))

left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = highschool_or_less_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with High School Diploma or Less")



education_state <- mutate(education_state,
                          Bachelors_or_higher_percent_change = (Bachelors_degree_or_higher_2013_to_17 
                                                               - Bachelors_degree_or_higher_2000) 
                          /Bachelors_degree_or_higher_2000)

left_join(blank_states,education_state, by = "state") %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_or_higher_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with a Bachelors Degree or Higher")

##############################################################################################################################

#Filter out GA, OH, and TX state data
unemployment_state <- filter(unemployment_state, state == "GA" | 
                               state == "OH" | state == "TX")

population_state <- filter(population_state, state == "GA" | 
                              state == "OH" | state == "TX")

education_state <- filter(education_state, state == "GA" | 
                             state == "OH" | state == "TX")

blank_states <- filter(blank_states, state == "GA" | 
                         state == "OH" | state == "TX")

# Joining them together

dataset_state <- left_join(unemployment_state, population_state)

dataset_state <- left_join(dataset_state, education_state)

#Mapping dataset for states
state_data <- left_join(blank_states, dataset_state)

##############################################################################################################################

#Before we filter out the data, remove Area_name from unemployment because it is inputed differently.
unemployment_county <- select(unemployment_county, county_fips:state, Civilian_labor_force_2010:Median_Household_Income_2017)
#view(unemployment)

unemployment_county <- filter(unemployment_county, state == "GA" | 
                         state == "OH" | state == "TX")

population_county <- filter(population_county, state == "GA" | 
                       state == "OH" | state == "TX")

education_county <- filter(education_county, state == "GA" | 
                      state == "OH" | state == "TX")

blank_counties <- filter(blank_counties, state == "GA" | 
                           state == "OH" | state == "TX")
# Joining them together

dataset_county <- left_join(unemployment_county, population_county)

dataset_county <- left_join(dataset_county, education_county)

dataset_county <- filter(dataset_county, !(state_name == "Georgia" | 
                               state_name == "Ohio" | state_name == "Texas"))


#Mapping dataset for counties
county_data <- left_join(blank_counties, dataset_county, by = "county_fips")

##############################################################################################################################

#Create bar graph of the states' populations

ggplot(data = state_data, aes(x = state_name, y = POP_ESTIMATE_2018)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  ggtitle("Population of People 2018")

##########################################################################################

#Create line graph to show the change in population

state_data %>%
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
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50)) +
  ggtitle("Change in Population 2010-2018")

##########################################################################################

#Create bar graph of the states' 2018 unemployment numbers

ggplot(data = state_data, aes(x = state_name, y = Unemployed_2018)) +
  geom_bar(stat = "identity", fill = "green4") +
  scale_x_discrete(name = "State") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  ggtitle("Number of Unemployed People 2018")

##########################################################################################

#Create line graph to show the change in unemployment numbers

state_data %>%
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
  ggplot() +
  geom_point(mapping = aes(x = Year, y = Unemployed, color = state)) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50)) +
  ggtitle("Change in Unemployment 2010 - 2018")


##########################################################################################

#Create line graph to show the change in unemployment rate

state_data %>%
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
  scale_y_continuous(name = "Percent") +
  facet_wrap(~state, nrow = 1) +
  theme(axis.text.x = element_text(size = 8, angle = 50)) +
  ggtitle("Change in Unemployment Rate 2010 - 2018")

##########################################################################################

#Create bar graph to show education levels in 2000

state_data %>%
  select(state_fips:state_name, Less_than_a_high_school_diploma_2000: Bachelors_degree_or_higher_2000) %>%
  rename(
    "No High School Diploma 2000" = Less_than_a_high_school_diploma_2000,
    "High School Diploma 2000" = High_school_diploma_only_2000,
    "Some College 2000" = Some_college_or_associates_degree_2000,
    "Bachelors or Higher 2000" = Bachelors_degree_or_higher_2000
  ) %>%
  gather(Education, People, "No High School Diploma 2000":"Bachelors or Higher 2000") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Amount of People with Different Education Levels, 2000")

##########################################################################################

#Create bar graph to show percent of education levels in 2000

state_data %>%
  select(state_fips:state_name, Percent_of_adults_with_less_than_a_high_school_diploma_2000: 
           Percent_of_adults_with_a_bachelors_degree_or_higher_2000) %>%
  rename(
    "No High School Diploma 2000" = Percent_of_adults_with_less_than_a_high_school_diploma_2000,
    "High School Diploma 2000" = Percent_of_adults_with_a_high_school_diploma_only_2000,
    "Some College 2000" = Percent_of_adults_completing_some_college_or_associates_degree_2000,
    "Bachelors or Higher 2000" = Percent_of_adults_with_a_bachelors_degree_or_higher_2000
  ) %>%
  gather(Education, People, "No High School Diploma 2000":"Bachelors or Higher 2000") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Percent") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Percent of Different Education Levels, 2000")

##########################################################################################

#Create bar graph to show education levels in 2017

state_data %>%
  select(state_fips:state_name, Less_than_a_high_school_diploma_2013_to_17: Bachelors_degree_or_higher_2013_to_17) %>%
  rename(
    "No High School Diploma 2017" = Less_than_a_high_school_diploma_2013_to_17, 
    "High School Diploma 2017" = High_school_diploma_only_2013_to_17,
    "Some College 2017" = Some_college_or_associates_degree_2013_to_17,
    "Bachelors or Higher 2017" = Bachelors_degree_or_higher_2013_to_17
  ) %>%
  gather(Education, People, "No High School Diploma 2017":"Bachelors or Higher 2017") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Number of People", labels = scales::comma) +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Amount of People with Different Education Levels, 2017")

##########################################################################################

#Create bar graph to show percent of education levels in 2017

state_data %>%
  select(state_fips:state_name, Percent_of_adults_with_less_than_a_high_school_diploma_2013_to_17: 
           Percent_of_adults_with_a_bachelors_degree_or_higher_2013_to_17) %>%
  rename(
    "No High School Diploma 2017" = Percent_of_adults_with_less_than_a_high_school_diploma_2013_to_17, 
    "High School Diploma 2017" = Percent_of_adults_with_a_high_school_diploma_only_2013_to_17,
    "Some College 2017" = Percent_of_adults_completing_some_college_or_associates_degree_2013_to_17,
    "Bachelors or Higher 2017" = Percent_of_adults_with_a_bachelors_degree_or_higher_2013_to_17
  ) %>%
  gather(Education, People, "No High School Diploma 2017":"Bachelors or Higher 2017") %>%
  ggplot(mapping = aes(x = Education, y = People, fill = state)) +
  geom_bar(stat="identity", position = "dodge", color = "black") +
  scale_x_discrete(name = "Education") +
  scale_y_continuous(name = "Percent") +
  theme(axis.text.x = element_text(size = 8, angle = 0)) +
  coord_flip() +
  ggtitle("Percent of Different Education Levels, 2017")

##########################################################################################

#Graph the population of counties in 2018

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = POP_ESTIMATE_2018),
          color = "green", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Population Estimate 2018")


##########################################################################################

#Graph to see unemployment rates in 2018

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Unemployment_rate_2018),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Unemployment Rate 2018")


##########################################################################################

#Graph to see counties with the most people without high school diploma

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Less_than_a_high_school_diploma_2000),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People Without a High School Diploma 2000")


##########################################################################################

#Graph to see counties with the most people with at least a bachelors

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_degree_or_higher_2000),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People with At Least a Bachelors Degree 2000")


##########################################################################################

#Graph to see counties with the most people without high school diploma

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Less_than_a_high_school_diploma_2013_to_17),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People Without a High School Diploma 2017")


##########################################################################################

#Graph to see counties with the most people with at least a bachelors

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_degree_or_higher_2013_to_17),
          color = "white", size = 0.05) +
  geom_sf_text(data = state_data, aes(label = state), size = 3) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::comma, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Number of People",
       x = "",
       y = "",
       title = "Amount of People with At Least a Bachelors Degree 2017")

##########################################################################################

county_data <- mutate(county_data, 
                             unemployment_percent_change = (Unemployment_rate_2018 - Unemployment_rate_2010)
                             /Unemployment_rate_2010)

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = unemployment_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Change in Unemployment Rate from 2010 to 2018")

##########################################################################################

county_data <- mutate(county_data,
                             laborforce_percent_change = (Civilian_labor_force_2018 - Civilian_labor_force_2010)
                             /Civilian_labor_force_2010)

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = laborforce_percent_change),
          color  = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) + 
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Civilian Labor Force")

##########################################################################################

county_data <- mutate(county_data,
                           population_percent_change = (POP_ESTIMATE_2018 - POP_ESTIMATE_2010)
                           / POP_ESTIMATE_2010)

county_data %>%
  ggplot() +
  geom_sf(mapping = aes(fill = population_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent",
       x = "",
       y = "",
       title = "Percent of Change in Population")

##########################################################################################

county_data <- mutate(county_data,
                          highschool_or_less_percent_change = ((Less_than_a_high_school_diploma_2013_to_17
                                                               + High_school_diploma_only_2013_to_17)
                      - (Less_than_a_high_school_diploma_2000 + High_school_diploma_only_2000)) 
                          / (Less_than_a_high_school_diploma_2000 + 
                              High_school_diploma_only_2000))

county_data %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = highschool_or_less_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with High School Diploma or Less")

##########################################################################################

county_data <- mutate(county_data,
                          Bachelors_or_higher_percent_change = (Bachelors_degree_or_higher_2013_to_17 
                                                                - Bachelors_degree_or_higher_2000) 
                          /Bachelors_degree_or_higher_2000)

county_data %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = Bachelors_or_higher_percent_change),
          color = "white", size = 0.2) +
  geom_sf_text(data = state_data, aes(label = state), size = 1.5) +
  coord_sf(datum = NA) +
  scale_fill_gradient(labels = scales::percent, low = "#98CF90", high = "#1a2e19" ) +
  labs(fill = "Percent of Change",
       x = "",
       y = "",
       title = "Percent of Change for People with a Bachelors Degree or Higher")
