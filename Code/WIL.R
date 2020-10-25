library(readr)
library(dplyr)
library(devtools)
library(coronavirus)
library(ggplot2)
library(tidyr)
library(plotly) # To make graphs interactive
library(leaflet) # to plot dynamic map
library(purrr)

# Use this commands to load coronavirus package from github
# install.packages("devtools")
# devtools::install_github("RamiKrispin/coronavirus")

# Run this statement individually to update dataset and to refresh data, restart the session
update_dataset()

# Checking the output
covid <- coronavirus
head(covid)
tail(covid)

# checking for total null values
sapply(covid, function(x) sum(is.na(x)))

# summary details
summary(covid)

# total days with reported cases numbers (+ve and -ve)
covid %>% summarise(n = n())

# total days with negative numbers reported
count <- covid[covid$cases < 0, ]
count %>% summarise(total_negative_cases = n())

# some countries report tests performed which are positive as (confirmed) and if negative than confirmed are 0.
# while some countries report both positive and negative test results in all type of cases. So those negative values should be reported as 0.
# Moreover, the percentage of negative values is 0.18% (346/191540)(checked as of 24th September). 
# Therefore, we decide to set those negative reported results as 0. 
covid$cases[covid$cases < 0] <- 0

# dropping province column
covid <- select(covid, -matches("province"))
head(covid)

# changing column data types to factor, numeric and integer
covid[, 2] <- sapply(covid[, 2], as.factor)
covid$lat <- as.numeric(covid$lat)
covid[, 4] <- sapply(covid[, 4], as.numeric)
covid$type <- as.factor(covid$type)

# checking the data types
sapply(covid, class)

# Descriptive statistics grouped by type. Global
covid %>% group_by(type) %>% 
  summarise(Min = min(cases, na.rm = TRUE),
                                       Q1 = quantile(cases, probs = .25, na.rm = TRUE),
                                       Median = median(cases ,na.rim = TRUE),
                                       Q3 = quantile(cases, probs = .75, na.rm = TRUE),
                                       Max = max(cases, na.rm = TRUE),
                                       Mean = mean(cases, na.rm = TRUE),
                                       SD = sd(cases , na.rm = TRUE),
                                       InterquartileRange = IQR(cases, na.rm = TRUE),
                                       N = n(),
                                       Cases = sum(cases, na.rm = TRUE))

# Descriptive statistics. 
covidAUS <- covid %>% filter(country == "Australia")

# sub setting data quarterly
covidAUS$quarter <- quarters(covidAUS$date)
subset(covidAUS, quarter=='Q1')

# Cases type with total count in Australia
covidAUS %>% group_by(type) %>% 
  summarise(Mean = mean(cases, na.rm = TRUE),
                        Cases = sum(cases, na.rm = TRUE))

# top cases in each quarter by type in Australia. Lowest is 0
covidAUS %>% select(date, quarter, type, cases) %>% 
  group_by(type, quarter) %>% 
  top_n(1, cases)

# Top countries with cases 
#---- Wider format
covidW <- covid %>% 
  select(country, lat, long, type, cases) %>% 
  group_by(country, lat, long, type) %>% 
  summarise(cases = sum(cases)) %>% 
  pivot_wider(names_from = type,
              values_from = cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  arrange(-active) 
  
# Using pivot wider brings NANs. Removing those rows
sapply(covidW, function(x) sum(is.na(x)))
covidW <- covidW[complete.cases(covidW), ]

#---long format
covidL <- 
  covidW %>% 
  pivot_longer(cols = confirmed:active, 
               names_to = "type", 
               values_to = "cases")

# Total cases past 24 hours (Global)
covid %>% 
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(country, type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) %>% 
  arrange(-confirmed)

# total cases past 24 hours (Australia)
covidAUS %>% 
  filter(date == max(date)) %>%
  select(country, type, cases) %>%
  group_by(type) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type,
              values_from = total_cases) 


# New cases type for last 14 days in Australia. Also cumulative cases (sum of cases for each day)
covidVis <- covidAUS %>% 
  filter(date %in% tail(covidAUS$date, 14) ) %>% 
  select(date, type, cases) %>% 
  group_by(date, type) %>% 
  summarise(cases = sum(cases)) %>% 
  group_by(type) %>% 
  mutate(cumulative_cases = cumsum(cases)) 

# Growth rate
covidVis <- covidVis %>% 
  group_by(type) %>% 
  mutate(growth = ((cases - lag(cases))/lag(cases)) * 100)

# Total cases in Australia for past 14 days 
ggplot(covidVis, aes(fill=type, y=cases, x=date)) + 
  geom_bar(position="dodge", 
           stat="identity") +
  scale_color_manual(values = c("orange", "red", "green")) + 
  scale_fill_manual(values = c("orange", "red", "green")) + 
  scale_x_date(date_labels = "%b %d",
               breaks = covidVis$date) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  geom_text(
    aes(label = cases, group = type), 
    position = position_dodge(0.8),
    vjust = -0.3, size = 3.5
  ) + 
  ggtitle("New cases in Australia for past 14 days")

ggplot(covidVis, aes(x=date, y=cases, colour=type, group=type)) +
  geom_point() +
  geom_line() + 
  ggtitle("New cases in Australia for past 14 days")

# Daily cases shows how fast its growing but doesn't show how big it has grown overall. 
ggplot(covidVis, aes(x=date, y=cumulative_cases, colour=type, group=type)) +
  geom_line() + 
  ggtitle("Cumulative cases in Australia for past 14 days")

# Growth rate. The daily number of cases as a percentage of the total.
# This method is perhaps the most useful for demonstrating the effectiveness of social distancing and other public health measures for "flattening the curve".
# Negative growth rate indicates pandemic is under control
ggplot(covidVis, aes(x=date, y=growth, colour=type, group=type)) +
  geom_line() + 
  ggtitle("Growth rate in Australia for past 14 days")

#####
# Recovery rate. 14.6 is max value and those are percentages
covidVIS <- covidAUS %>% 
  filter(date %in% tail(covidAUS$date, 14) ) %>% 
  select(date, type, cases) %>% 
  group_by(date, type) %>% 
  summarise(cases = sum(cases)) %>% 
  group_by(type) %>% 
  pivot_wider(names_from = type,
              values_from = cases) %>% 
  mutate(recover_rate = ((recovered / confirmed) /  14.6) * 100) %>% 
  mutate(recover_rate = if_else(is.na(recover_rate), 0, recover_rate))

ggplot(covidVIS, aes(x=date, y=recover_rate)) +
  geom_line() + 
  ggtitle("Recovery rate in Australia for past 14 days") +
  scale_x_date(date_labels = "%b %d",breaks = covidVis$date) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

#----------------------------------------------------------------------

# changing the data frame as wider for interactive visualization
# Daily new cases
coviddaily <- covidVis %>% 
  select(date, type, cases) %>% 
  pivot_wider(names_from = type,
              values_from = cases) 

# Cumulative cases
# Don't use active now since we need to use whole dataset to show active cases.
covidcum <- covidVis %>% 
  select(date, type, cumulative_cases) %>% 
  pivot_wider(names_from = type,
              values_from = cumulative_cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  mutate(active = cumsum(active))

# Growth rate
covidgrow <- covidVis %>% 
  select(date, type, growth) %>% 
  pivot_wider(names_from = type,
              values_from = growth)

###########################################
# Daily new cases interactive

confirmed_color <- "orange"
active_color <- "#1f77b4"
recovered_color <- "green"
death_color <- "red"

plot_ly(data = coviddaily, 
        x = ~ date,
        y = ~ confirmed,
        name = 'Confirmed', 
        fillcolor = confirmed_color,
        type = 'scatter',
        mode = 'none', 
        stackgroup = 'one') %>% 
  add_trace(y = ~ recovered,
                    name = "Recovered",
                    fillcolor = recovered_color) %>%
  add_trace(y = ~ death,
                    name = "Death",
                    fillcolor = death_color) %>% 
  layout(title = "Daily New Cases in Australia for past 14 days",
         yaxis = list(title = "Daily New Cases"),
         xaxis = list(title = "Date",
                      type = "date"),
         legend = list(x = 0.1, y = 0.9),
         hovermode = "compare")

# similarly goes for covidcum and covidgrow data frame

#-----------------------------------------------------------------------
# Map
# leaflet() %>% addTiles(). this shows static blank world map
pal <- colorFactor(c("orange", "blue", "red", "green"), domain = c("confirmed", "active", "death", "recovered"))

# adding log cases for radius purpose and removing NaNs
covidL <- covidL %>% 
  mutate(log_cases = 2 * log(cases))
covidL <- covidL[complete.cases(covidL), ]

# Split act as 3 data frames for 3 types
covidL.split <- covidL %>% 
  split(covidL$type)

leaflet() %>% addTiles() %>% 
  addProviderTiles(providers$CartoDB.Positron) 

# ##
# Iterate in foor loop code doesnt work
# names(covidL.split) %>%
#   purrr::walk( function(covidL) {
#     world <<- world %>%
#       addCircleMarkers(data = covidL.split[[covidL]], lat = ~lat, lng = ~long,
#                        color = ~pal(type),
#                        stroke = FALSE,
#                        fillOpacity = 0.5,
#                        radius = case_when(
#                          covidL$type == "active" ~ 6,
#                          covidL$type == "death" ~ 2,
#                          covidL$type == "recovered" ~ 4,
#                          covidL$type == "confirmed" ~ 8
#                        ),
#                        group = covidL,
#                        clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = F),
#                        labelOptions = labelOptions(noHide = F,
#                                                    direction = 'auto'))
#   })
# #####


groupI <- names(covidL.split)
leaflet() %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(data = covidL.split[[1]], lat = ~lat, lng = ~long,
                   color = active_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[1]]$country,
                                  "</br>Type: ", covidL.split[[1]]$type,
                                  "</br>Cases: ", covidL.split[[1]]$cases),
                   group = groupI[1],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addCircleMarkers(data = covidL.split[[2]], lat = ~lat, lng = ~long,
                   color = confirmed_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[2]]$country,
                                  "</br>Type: ", covidL.split[[2]]$type,
                                  "</br>Cases: ", covidL.split[[2]]$cases),
                   group = groupI[2],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addCircleMarkers(data = covidL.split[[3]], lat = ~lat, lng = ~long,
                   color = death_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[3]]$country,
                                  "</br>Type: ", covidL.split[[3]]$type,
                                  "</br>Cases: ", covidL.split[[3]]$cases),
                   group = groupI[3],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addCircleMarkers(data = covidL.split[[4]], lat = ~lat, lng = ~long,
                   color = recovered_color,
                   stroke = FALSE,
                   fillOpacity = 0.5,
                   radius = ~log_cases,
                   popup = paste0("Country: ", covidL.split[[4]]$country,
                                  "</br>Type: ", covidL.split[[4]]$type,
                                  "</br>Cases: ", covidL.split[[4]]$cases),
                   group = groupI[4],
                   labelOptions = labelOptions(noHide = F,
                                               direction = 'auto')) %>%
  addLayersControl(overlayGroups = names(covidL.split),
                   options = layersControlOptions(collapsed = FALSE))
# ####
# ## 

#################################################################

# top 5 countries with most active cases
covCountry <- covidW %>% 
  ungroup() %>% 
  top_n(5, active)

covCountry
  
# Extract those top 5 country names 
countnames <- covCountry$country

# Contains top 5 countries with their daily cases
coworld <- covid %>% 
  select(date, country, type, cases) %>% 
  group_by(date, country, type) %>% 
  summarise(cases = sum(cases)) %>% 
  pivot_wider(names_from = type,
              values_from = cases) %>% 
  mutate(active = confirmed - death - recovered) %>% 
  filter(country %in% countnames)  %>% 
  arrange(date) 

# Now considering top 5 countries with their daily cases for past 14 days
tailWorld <- coworld %>% 
  tail(70, date) %>% 
  select(date, country, confirmed) %>% 
  pivot_wider(names_from = country, 
              values_from = confirmed)

## top 5 countries with recovery rates. 459 highest no
TOP5 <- coworld %>% 
  filter(date %in% tail(coworld$date, 70) ) %>% 
  mutate(recover_rate = ((recovered / confirmed) / 459) * 100) %>% 
  mutate(recover_rate = if_else(is.na(recover_rate), 0, recover_rate))

ggplot(TOP5, aes(x=date, y=recover_rate, colour=country, group=country)) +
  geom_line() + 
  ggtitle("Recovery rate of Top 5 countries for past 14 days") +
  scale_x_date(date_labels = "%b %d",breaks = TOP5$date) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Daily Confirmed Cases in Top 5 Countries for past 14 days (Log scale)
plot_ly(data = tailWorld, 
        x = ~ date,
        y = ~ US,
        name = 'US', 
        mode = 'lines+markers', 
        type = 'scatter',
        stackgroup = 'one') %>% 
  add_trace(y = ~ India,
            name = "India",
            mode = 'lines+markers') %>%
  add_trace(y = ~ Brazil,
            name = "Brazil",
            mode = 'lines+markers') %>% 
  add_trace(y = ~ France,
            name = "France",
            mode = 'lines+markers') %>%
  add_trace(y = ~ `United Kingdom`,
            name = "United Kingdom",
            mode = 'lines+markers') %>%
  layout(title = "Daily Confirmed Cases in Top 5 Countries for past 14 days (Log scale)",
         yaxis = list(title = "Daily Confirmed Cases",
                      type = "log"),
         xaxis = list(title = "Date",
                      type = "date"),
         legend = list(x = 0.1, y = 0.9),
         hovermode = "compare")
