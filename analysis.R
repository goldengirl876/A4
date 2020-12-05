library(ggplot2)
library(dplyr)
library(reshape2)
library(usmap)

# Load incarceration data sets
county_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")
jurisdiction_data <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")

# Select information (rows) with max black prison population rates
max_black_jail_pop_rate <- max(county_data$black_jail_pop_rate, na.rm=TRUE)
max_bjpr_row <- county_data[which(county_data$black_jail_pop_rate == max_black_jail_pop_rate), ]

# Select information (rows) with min black prison population rates
min_black_jail_pop_rate <- min(county_data$black_jail_pop_rate, na.rm=TRUE)
min_bjpr_row <- county_data[which(county_data$black_jail_pop_rate == min_black_jail_pop_rate), ]

# Select information (rows) with max white prison population rates
max_white_jail_pop_rate <- max(county_data$white_jail_pop_rate, na.rm=TRUE)
max_wjpr_row <- county_data[which(county_data$white_jail_pop_rate == max_white_jail_pop_rate), ]

# Select information (rows) with min white prison population rates
min_white_jail_pop_rate <- min(county_data$white_jail_pop_rate, na.rm=TRUE)
min_wjpr_row <- county_data[which(county_data$white_jail_pop_rate == min_white_jail_pop_rate), ]

# Select information (rows) with max native prison population rates
max_native_jail_pop_rate <- max(county_data$native_jail_pop_rate, na.rm=TRUE)
max_njpr_row <- county_data[which(county_data$native_jail_pop_rate == max_native_jail_pop_rate), ]

# Select information (rows) with min native prison population rates
min_native_jail_pop_rate <- min(county_data$native_jail_pop_rate, na.rm=TRUE)
min_njpr_row <- county_data[which(county_data$native_jail_pop_rate == min_native_jail_pop_rate), ]

# Select information (rows) with max latinx prison population rates
max_latinx_jail_pop_rate <- max(county_data$latinx_jail_pop_rate, na.rm=TRUE)
max_ljpr_row <- county_data[which(county_data$latinx_jail_pop_rate == max_latinx_jail_pop_rate), ]

# Select information (rows) with min latinx prison population rates
min_latinx_jail_pop_rate <- min(county_data$latinx_jail_pop_rate, na.rm=TRUE)
min_ljpr_row <- county_data[which(county_data$latinx_jail_pop_rate == min_latinx_jail_pop_rate), ]

# Select information (rows) with max total prison population rates
max_total_jail_pop_rate <- max(county_data$total_jail_pop_rate, na.rm=TRUE)
max_tjpr_row <- county_data[which(county_data$total_jail_pop_rate == max_total_jail_pop_rate), ]

# Select information (rows) with min total prison population rates
min_total_jail_pop_rate <- min(county_data$total_jail_pop_rate, na.rm=TRUE)
min_tjpr_row <- county_data[which(county_data$total_jail_pop_rate == min_total_jail_pop_rate), ]

# Function to calculate average of black prison population rates 
# across all the counties in the given state (in the given year)
avg_bjpr_year_state <- function(target_year, target_state) {
  black_prison_county_data <- select(county_data, "year", "state", "black_jail_pop_rate")
  black_prison_county_data <- filter(black_prison_county_data, !is.na(black_prison_county_data$black_jail_pop_rate))
  black_prison_county_data <- filter(black_prison_county_data, black_prison_county_data$year == target_year, black_prison_county_data$state == target_state)
  avg_bjpr <- mean(black_prison_county_data$black_jail_pop_rate)
  # return(sprintf("The average black jail population rate in %s in %s was %f.", target_state, target_year, avg_bjpr))
  return(avg_bjpr)
}

# Function to calculate average of white prison population rates 
# across all the counties in the given state (in the given year)
avg_wjpr_year_state <- function(target_year, target_state) {
  white_prison_county_data <- select(county_data, "year", "state", "white_jail_pop_rate")
  white_prison_county_data <- filter(white_prison_county_data, !is.na(white_prison_county_data$white_jail_pop_rate))
  white_prison_county_data <- filter(white_prison_county_data, white_prison_county_data$year == target_year, white_prison_county_data$state == target_state)
  avg_wjpr <- mean(white_prison_county_data$white_jail_pop_rate)
  # return(sprintf("The average white jail population rate in %s in %s was %f.", target_state, target_year, avg_wjpr))
  return(avg_wjpr)
}

# Function to calculate average of native prison population rates 
# across all the counties in the given state (in the given year)
avg_njpr_year_state <- function(target_year, target_state) {
  native_prison_county_data <- select(county_data, "year", "state", "native_jail_pop_rate")
  native_prison_county_data <- filter(native_prison_county_data, !is.na(native_prison_county_data$native_jail_pop_rate))
  native_prison_county_data <- filter(native_prison_county_data, native_prison_county_data$year == target_year, native_prison_county_data$state == target_state)
  avg_njpr <- mean(native_prison_county_data$native_jail_pop_rate)
  # return(sprintf("The average white jail population rate in %s in %s was %f.", target_state, target_year, avg_wjpr))
  return(avg_njpr)
}

# Function to calculate average of latinx prison population rates 
# across all the counties in the given state (in the given year)
avg_ljpr_year_state <- function(target_year, target_state) {
  latinx_prison_county_data <- select(county_data, "year", "state", "latinx_jail_pop_rate")
  latinx_prison_county_data <- filter(latinx_prison_county_data, !is.na(latinx_prison_county_data$latinx_jail_pop_rate))
  latinx_prison_county_data <- filter(latinx_prison_county_data, latinx_prison_county_data$year == target_year, latinx_prison_county_data$state == target_state)
  avg_ljpr <- mean(latinx_prison_county_data$latinx_jail_pop_rate)
  # return(sprintf("The average white jail population rate in %s in %s was %f.", target_state, target_year, avg_wjpr))
  return(avg_ljpr)
}

# Function to calculate average of total prison population rates
# across all the counties in the given state (in the given year)
avg_tjpr_year_state <- function(target_year, target_state) {
  total_prison_county_data <- select(county_data, "year", "state", "total_jail_pop_rate")
  total_prison_county_data <- filter(total_prison_county_data, !is.na(total_prison_county_data$total_jail_pop_rate))
  total_prison_county_data <- filter(total_prison_county_data, total_prison_county_data$year == target_year, total_prison_county_data$state == target_state)
  avg_tjpr <- mean(total_prison_county_data$total_jail_pop_rate)
  # return(sprintf("The average white jail population rate in %s in %s was %f.", target_state, target_year, avg_wjpr))
  return(avg_tjpr)
}

# Test avg_?jpr_year_state functions
avg_bjpr_year_state("2018", "CA")
avg_wjpr_year_state("2018", "CA")
avg_njpr_year_state("2018", "CA")
avg_ljpr_year_state("2018", "CA")
avg_tjpr_year_state("2018", "CA")

# Function to calculate average change of black prison population rates 
# across all the counties in the given state (between the given years)
avg_bjpr_change_years_state <- function(target_start_year, target_end_year, target_state) {
  black_prison_county_data <- select(county_data, "year", "state", "black_jail_pop_rate")
  black_prison_county_data <- filter(black_prison_county_data, !is.na(black_prison_county_data$black_jail_pop_rate))
  black_prison_county_data <- filter(black_prison_county_data, black_prison_county_data$year == target_start_year, black_prison_county_data$state == target_state)
  avg_start_bjpr <- mean(black_prison_county_data$black_jail_pop_rate)
  
  black_prison_county_data <- select(county_data, "year", "state", "black_jail_pop_rate")
  black_prison_county_data <- filter(black_prison_county_data, !is.na(black_prison_county_data$black_jail_pop_rate))
  black_prison_county_data <- filter(black_prison_county_data, black_prison_county_data$year == target_end_year, black_prison_county_data$state == target_state)
  avg_end_bjpr <- mean(black_prison_county_data$black_jail_pop_rate)
  
  avg_change_bjpr <- avg_start_bjpr - avg_end_bjpr
  # return(sprintf("The average black jail population rate in %s changed %f between %s and %s.", target_state, avg_change_bjpr, target_start_year, target_end_year))
  return(avg_change_bjpr)
}

# Function to calculate average change of white prison population rates 
# across all the counties in the given state (between the given years)
avg_wjpr_change_years_state <- function(target_start_year, target_end_year, target_state) {
  white_prison_county_data <- select(county_data, "year", "state", "white_jail_pop_rate")
  white_prison_county_data <- filter(white_prison_county_data, !is.na(white_prison_county_data$white_jail_pop_rate))
  white_prison_county_data <- filter(white_prison_county_data, white_prison_county_data$year == target_start_year, white_prison_county_data$state == target_state)
  avg_start_wjpr <- mean(white_prison_county_data$white_jail_pop_rate)
  
  white_prison_county_data <- select(county_data, "year", "state", "white_jail_pop_rate")
  white_prison_county_data <- filter(white_prison_county_data, !is.na(white_prison_county_data$white_jail_pop_rate))
  white_prison_county_data <- filter(white_prison_county_data, white_prison_county_data$year == target_end_year, white_prison_county_data$state == target_state)
  avg_end_wjpr <- mean(white_prison_county_data$white_jail_pop_rate)
  
  avg_change_wjpr <- avg_start_wjpr - avg_end_wjpr
  # return(sprintf("The average white jail population rate in %s changed %f between %s and %s.", target_state, avg_change_wjpr, target_start_year, target_end_year))
  return(avg_change_wjpr)
}

# Function to calculate average change of native prison population rates 
# across all the counties in the given state (between the given years)
avg_njpr_change_years_state <- function(target_start_year, target_end_year, target_state) {
  native_prison_county_data <- select(county_data, "year", "state", "native_jail_pop_rate")
  native_prison_county_data <- filter(native_prison_county_data, !is.na(native_prison_county_data$native_jail_pop_rate))
  native_prison_county_data <- filter(native_prison_county_data, native_prison_county_data$year == target_start_year, native_prison_county_data$state == target_state)
  avg_start_njpr <- mean(native_prison_county_data$native_jail_pop_rate)
  
  native_prison_county_data <- select(county_data, "year", "state", "native_jail_pop_rate")
  native_prison_county_data <- filter(native_prison_county_data, !is.na(native_prison_county_data$native_jail_pop_rate))
  native_prison_county_data <- filter(native_prison_county_data, native_prison_county_data$year == target_end_year, native_prison_county_data$state == target_state)
  avg_end_njpr <- mean(native_prison_county_data$native_jail_pop_rate)
  
  avg_change_njpr <- avg_start_njpr - avg_end_njpr
  # return(sprintf("The average native jail population rate in %s changed %f between %s and %s.", target_state, avg_change_njpr, target_start_year, target_end_year))
  return(avg_change_njpr)
}

# Function to calculate average change of latinx prison population rates 
# across all the counties in the given state (between the given years)
avg_ljpr_change_years_state <- function(target_start_year, target_end_year, target_state) {
  latinx_prison_county_data <- select(county_data, "year", "state", "latinx_jail_pop_rate")
  latinx_prison_county_data <- filter(latinx_prison_county_data, !is.na(latinx_prison_county_data$latinx_jail_pop_rate))
  latinx_prison_county_data <- filter(latinx_prison_county_data, latinx_prison_county_data$year == target_start_year, latinx_prison_county_data$state == target_state)
  avg_start_ljpr <- mean(latinx_prison_county_data$latinx_jail_pop_rate)
  
  latinx_prison_county_data <- select(county_data, "year", "state", "latinx_jail_pop_rate")
  latinx_prison_county_data <- filter(latinx_prison_county_data, !is.na(latinx_prison_county_data$latinx_jail_pop_rate))
  latinx_prison_county_data <- filter(latinx_prison_county_data, latinx_prison_county_data$year == target_end_year, latinx_prison_county_data$state == target_state)
  avg_end_ljpr <- mean(latinx_prison_county_data$latinx_jail_pop_rate)
  
  avg_change_ljpr <- avg_start_ljpr - avg_end_ljpr
  # return(sprintf("The average latinx jail population rate in %s changed %f between %s and %s.", target_state, avg_change_ljpr, target_start_year, target_end_year))
  return(avg_change_ljpr)
}

# Function to calculate average change of total prison population rates 
# across all the counties in the given state (between the given years)
avg_tjpr_change_years_state <- function(target_start_year, target_end_year, target_state) {
  total_prison_county_data <- select(county_data, "year", "state", "total_jail_pop_rate")
  total_prison_county_data <- filter(total_prison_county_data, !is.na(total_prison_county_data$total_jail_pop_rate))
  total_prison_county_data <- filter(total_prison_county_data, total_prison_county_data$year == target_start_year, total_prison_county_data$state == target_state)
  avg_start_tjpr <- mean(total_prison_county_data$total_jail_pop_rate)
  
  total_prison_county_data <- select(county_data, "year", "state", "total_jail_pop_rate")
  total_prison_county_data <- filter(total_prison_county_data, !is.na(total_prison_county_data$total_jail_pop_rate))
  total_prison_county_data <- filter(total_prison_county_data, total_prison_county_data$year == target_end_year, total_prison_county_data$state == target_state)
  avg_end_tjpr <- mean(total_prison_county_data$total_jail_pop_rate)
  
  avg_change_tjpr <- avg_start_tjpr - avg_end_tjpr
  # return(sprintf("The average total jail population rate in %s changed %f between %s and %s.", target_state, avg_change_tjpr, target_start_year, target_end_year))
  return(avg_change_tjpr)
}

# Test avg_bjpr_change_years_state() and avg_wjpr_change_years_state()
avg_bjpr_change_years_state("1999", "2018", "CA")
avg_wjpr_change_years_state("1999", "2018", "CA")
avg_njpr_change_years_state("1999", "2018", "CA")
avg_ljpr_change_years_state("1999", "2018", "CA")
avg_tjpr_change_years_state("1999", "2018", "CA")

###
# Time-based trend analysis
###
# Create a matrix of all state abbreviations for trend analysis
all_states <- c('AK', 'AL', 'AR', 'AZ', 'CA', 'CO', 'CT', 'DC', 'DE', 'FL', 
                'GA', 'HI', 'IA', 'ID', 'IL', 'IN', 'KS', 'KY', 'LA', 'MA', 
                'MD', 'ME', 'MI', 'MN', 'MO', 'MS', 'MT', 'NC', 'ND', 'NE', 
                'NH', 'NJ', 'NM', 'NV', 'NY', 'OH', 'OK', 'OR', 'PA', 'RI',
                'SC', 'SD', 'TN', 'TX', 'UT', 'VA', 'VT', 'WA', 'WI', 'WV', 'WY')

# Trend analysis for 2018
year_2018 <- c(2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 
               2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 
               2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 
               2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 
               2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018, 2018)

avg_bjpr_col <- c(mapply(avg_bjpr_year_state, year_2018, all_states))
avg_bjpr_2018_all_states <- data.frame(year_2018, all_states, avg_bjpr_col)
avg_bjpr_2018_all_states[order(avg_bjpr_2018_all_states$avg_bjpr_col, decreasing = TRUE), ]

avg_wjpr_col <- c(mapply(avg_wjpr_year_state, year_2018, all_states))
avg_wjpr_2018_all_states <- data.frame(year_2018, all_states, avg_wjpr_col)
avg_wjpr_2018_all_states[order(avg_wjpr_2018_all_states$avg_wjpr_col, decreasing = TRUE), ]

avg_njpr_col <- c(mapply(avg_njpr_year_state, year_2018, all_states))
avg_njpr_2018_all_states <- data.frame(year_2018, all_states, avg_njpr_col)
avg_njpr_2018_all_states[order(avg_njpr_2018_all_states$avg_njpr_col, decreasing = TRUE), ]

avg_ljpr_col <- c(mapply(avg_ljpr_year_state, year_2018, all_states))
avg_ljpr_2018_all_states <- data.frame(year_2018, all_states, avg_ljpr_col)
avg_ljpr_2018_all_states[order(avg_ljpr_2018_all_states$avg_ljpr_col, decreasing = TRUE), ]

avg_tjpr_col <- c(mapply(avg_tjpr_year_state, year_2018, all_states))
avg_tjpr_2018_all_states <- data.frame(year_2018, all_states, avg_tjpr_col)
avg_tjpr_2018_all_states[order(avg_tjpr_2018_all_states$avg_tjpr_col, decreasing = TRUE), ]

# Create tables of average black and white prison population rates for every year between 2008 
# and 2018 across 5 states with top total prison population rates in 2018 (LA, UT, NM, MS, KY)
top_5_tpjr_states <- c('LA', 'LA', 'LA', 'LA', 'LA', 
                       'LA', 'LA', 'LA', 'LA', 'LA', 'LA',
                       'UT', 'UT', 'UT', 'UT', 'UT', 
                       'UT', 'UT', 'UT', 'UT', 'UT', 'UT',
                       'NM', 'NM', 'NM', 'NM', 'NM', 
                       'NM', 'NM', 'NM', 'NM', 'NM', 'NM',
                       'MS', 'MS', 'MS', 'MS', 'MS', 
                       'MS', 'MS', 'MS', 'MS', 'MS', 'MS',
                       'KY', 'KY', 'KY', 'KY', 'KY', 
                       'KY', 'KY', 'KY', 'KY', 'KY', 'KY')
years_2008_to_2018 <- c(2008, 2009, 2010, 2011, 2012, 
                        2013, 2014, 2015, 2016, 2017, 2018,
                        2008, 2009, 2010, 2011, 2012, 
                        2013, 2014, 2015, 2016, 2017, 2018,
                        2008, 2009, 2010, 2011, 2012, 
                        2013, 2014, 2015, 2016, 2017, 2018,
                        2008, 2009, 2010, 2011, 2012, 
                        2013, 2014, 2015, 2016, 2017, 2018,
                        2008, 2009, 2010, 2011, 2012, 
                        2013, 2014, 2015, 2016, 2017, 2018)
avg_bjpr_2008_2018_col <- c(mapply(avg_bjpr_year_state, years_2008_to_2018, top_5_tpjr_states))
avg_bjpr_2008_2018_top_states <- data.frame(years_2008_to_2018, top_5_tpjr_states, avg_bjpr_2008_2018_col)

avg_wjpr_2008_2018_col <- c(mapply(avg_wjpr_year_state, years_2008_to_2018, top_5_tpjr_states))
avg_wjpr_2008_2018_top_states <- data.frame(years_2008_to_2018, top_5_tpjr_states, avg_wjpr_2008_2018_col)

# Use tables to make stacked area plots
# Trends over time chart
avg_bjpr_2008_2018_top_states_2 <- avg_bjpr_2008_2018_top_states
avg_bjpr_2008_2018_top_states_2$avg_bjpr_2008_2018_col <- avg_bjpr_2008_2018_top_states_2$avg_bjpr_2008_2018_col*.01
ggplot(avg_bjpr_2008_2018_top_states_2, aes(x=years_2008_to_2018, y=avg_bjpr_2008_2018_col, fill=top_5_tpjr_states)) + 
  geom_area() + labs(x = "Year", y = "Population Rate", fill = "State", title = "Average Black Prison Population Rates", subtitle = "In Top 5 States For Average Total Prison Population Rates (2018)") + 
  scale_x_continuous(breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

# Trends over time chart
avg_wjpr_2008_2018_top_states_2 <- avg_wjpr_2008_2018_top_states
avg_wjpr_2008_2018_top_states_2$avg_wjpr_2008_2018_col <- avg_wjpr_2008_2018_top_states_2$avg_wjpr_2008_2018_col*.01
ggplot(avg_wjpr_2008_2018_top_states_2, aes(x=years_2008_to_2018, y=avg_wjpr_2008_2018_col, fill=top_5_tpjr_states)) + 
  geom_area() + labs(x = "Year", y = "Population Rate", fill = "State", title = "Average White Prison Population Rates", subtitle = "In Top 5 States For Average Total Prison Population Rates (2018)") + 
  scale_x_continuous(breaks = c(2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018))

# Create table of change in average black, white, latinx, native, and total prison population rates between 2008 and 2018
# across 5 states with top total prison population rates in 2018 (LA, UT, NM, MS, KY)
top_5_tpjr_states_2 <- c('LA','UT', 'NM', 'MS', 'KY')
start_years_2008 <- c(2008, 2008, 2008, 2008, 2008)
end_years_2018 <- c(2018, 2018, 2018, 2018, 2018)
avg_change_bjpr_2008_2018_col <- c(mapply(avg_bjpr_change_years_state, start_years_2008, end_years_2018, top_5_tpjr_states_2))
avg_change_bjpr_2008_2018_top_states <- data.frame(top_5_tpjr_states_2, avg_change_bjpr_2008_2018_col)
avg_change_njpr_2008_2018_col <- c(mapply(avg_njpr_change_years_state, start_years_2008, end_years_2018, top_5_tpjr_states_2))
avg_change_njpr_2008_2018_top_states <- data.frame(top_5_tpjr_states_2, avg_change_njpr_2008_2018_col)
avg_change_wjpr_2008_2018_col <- c(mapply(avg_wjpr_change_years_state, start_years_2008, end_years_2018, top_5_tpjr_states_2))
avg_change_wjpr_2008_2018_top_states <- data.frame(top_5_tpjr_states_2, avg_change_wjpr_2008_2018_col)
avg_change_ljpr_2008_2018_col <- c(mapply(avg_ljpr_change_years_state, start_years_2008, end_years_2018, top_5_tpjr_states_2))
avg_change_ljpr_2008_2018_top_states <- data.frame(top_5_tpjr_states_2, avg_change_ljpr_2008_2018_col)
avg_change_tjpr_2008_2018_col <- c(mapply(avg_tjpr_change_years_state, start_years_2008, end_years_2018, top_5_tpjr_states_2))
avg_change_tjpr_2008_2018_top_states <- data.frame(top_5_tpjr_states_2, avg_change_tjpr_2008_2018_col)
avg_change_all_2008_2018_top_states <- data.frame(top_5_tpjr_states_2, 
                                                  avg_change_bjpr_2008_2018_col,
                                                  avg_change_wjpr_2008_2018_col)

# Variable comparison chart
graph_data <- melt(avg_change_all_2008_2018_top_states,id.vars="top_5_tpjr_states_2")
graph_data$value <- graph_data$value*.01
ggplot(graph_data,aes(x=variable,y=value,fill=factor(top_5_tpjr_states_2))) +
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(breaks = c("avg_change_bjpr_2008_2018_col", "avg_change_wjpr_2008_2018_col"),
                   labels = c("Black", "White")) +
  labs(x = "Race", y = "Change in Population Rate", fill = "State", title = "Change in Average Prison Population Rates (2008-2018)", subtitle = "In Top 5 States For Average Total Prison Population Rates (2018)")

# Create US map of average total prison population rates in all states in 2018
plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

# Map
map_data_tjpr <- avg_tjpr_2018_all_states
map_data_tjpr$avg_tjpr_col <- avg_tjpr_2018_all_states$avg_tjpr_col*.01
fips_all_states <- fips(all_states)
map_data_tjpr <- cbind(map_data_tjpr, fips_all_states)
map_data_tjpr <- map_data_tjpr %>% rename(fips = fips_all_states)
plot_usmap(data = map_data_tjpr, values = "avg_tjpr_col", include = all_states) +
  scale_fill_continuous(name = "Average Total Prison Population Rate", label = scales::comma,
                        low = "white", high = "red") + 
  theme(legend.position = "right") + 
  labs(title = "Average Total Prison Population Rates (2018)")

