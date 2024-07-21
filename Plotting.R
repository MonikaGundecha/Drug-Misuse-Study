#Cleaning the Environment ####
cat("\014") # clears console
rm(list = ls()) # clears global environment
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE) # clears plots
try(p_unload(p_loaded(), character.only = TRUE), silent = TRUE) 
#clears packages
options(scipen = 100) # disables scientific notation for entire R session


#Installing and Loading Packages ####
library(tidyverse)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(ggrepel)
library(countrycode)
library(plotly)
library(animation)
library(countrycode)
library(rnaturalearthdata)

#Reading file:
drugs_df<-read_csv("Drugs_Data_Visualisations.csv")

#Questions for Visualisations:
#1. What is the average rate of usage of various substances among different age groups over the years?
#2. Among different age groups, which state has the highest number of new users for various substance usage over the years?
#3. What are the most and least common drug types used for various substances?
#4. What is the percentage difference in substance usage over the years among different age groups?
#5. What is the trend of usage for different user types for each substance over the years?


#Basic manipulation:
# Creating Year Groups
drugs_df <- drugs_df %>%
  mutate(
    Year_Group = case_when(
      Year %in% 2002:2005 ~ "2002 to 2005",
      Year %in% 2006:2009 ~ "2006 to 2009",
      Year %in% 2010:2013 ~ "2010 to 2013",
      Year %in% 2014:2018 ~ "2014 to 2018"
    )
  )

#Adjusting substance names:
drugs_df <- drugs_df %>%
  mutate(
    FK_Substance_Group = recode(
      FK_Substance_Group,
      "Alcohol" = "Alcohol",
      "Marijuana" = "Marijuana",
      "Illicit_Drugs_Cocaine" = "Cocaine",
      "Tobacco" = "Tobacco",
      "Tobacco_Cigarette" = "Cigarette"
    )
  )

#Visualistion one####
#Questions: What is the average rate of usage of various substances among different age groups over the years?
#Create 4 year wise groups and then for every age 
#Columns needed: 
#Summarising data for question 1
summary_data_q1<- drugs_df %>%
  group_by(Year_Group, FK_Substance_Group, FK_Age_Group) %>%
  summarise(Average_Population_Rates = mean(Population_Rates))

#Plotting
ggplot(summary_data_q1, aes(x = Year_Group, y = Average_Population_Rates, fill = FK_Substance_Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~FK_Age_Group, labeller = labeller(FK_Age_Group = c('12_17' = '12 to 17', '18_25' = '18 to 25', '26+' = '26+'))) +
  labs(title = "Average Population Rates for Every Age Group Over the Years",
       x = "Year_Group",
       y = "Average Population Rates(Per 1000)",
       fill = "Substance group") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5,margin = margin(b = 15)))




#Q2: Among different age groups, which state has the highest number of new users for various substance usage over the years?####
library(plotly)
states = read_csv("states.csv",show_col_types = FALSE) %>%
  rename(Code = Abbreviation)


Marijuana_df <- data %>%
  inner_join(states) %>%
  select(year, state, Code, y1 = `totals_marijuana_new_users_12_17`)



Marijuana_graph <- plot_geo(Marijuana_df,
                            locationmode = 'USA-states',
                            frame = ~year) %>%
  add_trace(locations = ~Code,
            z = ~y1,
            zmin = 0,
            zmax = max(Marijuana_df$y1),
            color = ~y1,
            colorscale = "Earth",
            colorbar = list(title = "Totals_Age=12-17")) %>%
  layout(geo = list(scope = 'usa'),
         title='\n New Users of Marijuana in the US\n 2002-2018')


Marijuana_graph <- plot_geo(Marijuana_df,
                            locationmode = 'USA-states',
                            frame = ~year) %>%
  add_trace(locations = ~Code,
            z = ~y1,
            zmin = 0,
            zmax = max(Marijuana_df$y1),
            color = ~y1,
            colorscale = "Earth",
            colorbar = list(title = "Totals_Age=12-17")) %>%
  layout(geo = list(scope = 'usa'),
         title = '\n New Users of Marijuana in the US\n 2002-2018',
         annotations = list(
           list(
             x = 0.5,
             y = -0.1,
             showarrow = FALSE,
             text = "Note: This chart displays the total number of new marijuana users aged 12-17 in totals i.e (in thousands of people).",
             xref = "paper",
             yref = "paper"
           )
         )
  )

Marijuana_graph


#Q3: #3. What are the most and least common drug types used for various substances? ####
### This is to be taken from iti



#4: ####
total_usage <- drugs_df %>%
  group_by(Year, FK_Age_Group, FK_Substance_Group) %>%
  summarise(Total_Usage = sum(Population))

# Calculate the percentage change in total usage from one year to the next
total_usage <- total_usage %>%
  group_by(FK_Age_Group, FK_Substance_Group) %>%
  mutate(Percentage_Change = (Total_Usage / lag(Total_Usage, default = 1) - 1) * 100)

# Create a line chart or bar chart to visualize the percentage changes
ggplot(total_usage, aes(x = Year, y = Percentage_Change, color = FK_Age_Group)) +
  geom_line() +
  labs(
    title = "Percentage Change in Total Substance Usage Over the Years by Age Group",
    x = "Year",
    y = "Percentage Change",
    color = "Age Group"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#Q5: #5. What is the trend of usage for different user types for each substance over the years?####
# Grouping by Year, FK_Substance_Group, and FK_User_Type and calculate the average usage
usage_trends_q5 <- drugs_df %>%
  group_by(Year, FK_Substance_Group, FK_User_Type) %>%
  summarise(Average_Usage_Rates = mean(Population_Rates))

# Create a line chart
ggplot(usage_trends_q5, aes(x = Year, y = Average_Usage_Rates, color = FK_Substance_Group)) +
  geom_line() +
  facet_wrap(~FK_User_Type) +
  theme_minimal() +
  labs(
    title = "Usage Trends for Different User Types by Substance",
    x = "Year",
    y = "Average Usage Rates (Per 1000)",
    color = "Substance Group"  # Use 'color' for the legend title
  ) +
  scale_color_brewer(palette = "Set1") +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

####New Attempt ####
calculation_table<-read_csv("D:\MPS\DBMS\Group Project\Drug_Data_Visualisations\drugs_new_1")

calculation_table <- read.csv("D:\\MPS\\DBMS\\Group Project\\Drug_Data_Visualisations\\drugs_new_1.csv")


#New Attempt ####
calculation_table<-calculation_table %>%
  select(-User_Difference,-Percentage_Difference)
summary(calculation_table)
calculation_table <- calculation_table %>%
  group_by(Year, Age_Group_Name, Substance_Group_Name) %>%
  summarise(new_population_total=sum(Population_Totals))


calculation_table <- calculation_table %>%
  group_by(Substance_Group_Name,Age_Group_Name,Year )

view(calculation_table)



calculation_table <- calculation_table %>%
  arrange(Year) %>%
  group_by(Substance_Group_Name, Age_Group_Name) %>%
  mutate(Percentage_Increase = (new_population_total - lag(new_population_total)) / lag(new_population_total) * 100) %>%
  ungroup()

ggplot(calculation_table, aes(x = Year, y = Percentage_Increase, color = Substance_Group_Name)) +
  geom_line() +
  facet_wrap(~ Age_Group_Name, labeller = labeller(Age_Group_Name = c('12_17' = '12 to 17', '18_25' = '18 to 25', '26+' = '26+'))) +
  labs(title = "Trend of Percentage Difference Over the Years",
       x = "Year",
       y = "Percentage Difference",
       color = "Substance Group") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, margin = margin(b = 15)))+
  theme_minimal()
  
