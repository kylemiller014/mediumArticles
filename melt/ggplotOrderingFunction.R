 # Better ordering for ggplot
####  File setup ####
require(plotly)
require(ggplot2)
require(dplyr)
require(tidyverse)
require(tidyselect)
require(stringr)
require(reshape2)

# Working directory
dir = "/Users/kylemiller/Medium Articles/ggplot ordering function/"

# Read in dataset
df <- read.csv(paste0(dir, "vgsales.csv"))

####  Quick Checks / First Look at data set ####
# Print first 10 rows of the data
head(df)

# Get summary stats on numeric columns
summary(df)

# Generic check for nulls
colSums(is.na(df))

# Row count of raw data
rawDfRowCount <- nrow(df)

# N/A values need removal/cleaning
dfCleaned <- df %>%
  filter(Year != "N/A")

# Row count after filter applied
removedNAyearsRowCount <- nrow(dfCleaned)

# Get summary stats on numeric columns
summary(dfCleaned)

# Print results of filtering to console
print(paste0("Removed ", (rawDfRowCount - removedNAyearsRowCount), " rows with values set to N/A for column Year!"))

####  Group by categorical variables  ####
# Group by Year and by Platform
platformGroups <- dfCleaned %>%
  group_by(Year, Platform) %>%
  summarise(NorthAmerica = sum(NA_Sales),
            Europe = sum(EU_Sales),
            Japan = sum(JP_Sales),
            Other = sum(Other_Sales),
            Global = sum(Global_Sales),
            .groups = "drop")

# Group by Year and by Genre
genreGroups <- dfCleaned %>%
  group_by(Year, Genre) %>%
  summarise(NorthAmerica = sum(NA_Sales),
            Europe = sum(EU_Sales),
            Japan = sum(JP_Sales),
            Other = sum(Other_Sales),
            Global = sum(Global_Sales),
            .groups = "drop")

# Group by Year and by Publisher
publisherGroups <- dfCleaned %>%
  group_by(Year, Publisher) %>%
  summarise(NorthAmerica = sum(NA_Sales),
            Europe = sum(EU_Sales),
            Japan = sum(JP_Sales),
            Other = sum(Other_Sales),
            Global = sum(Global_Sales),
            .groups = "drop")

# Filter publisher list based on global sales
plotablePublisherGroups <- publisherGroups %>%
  arrange(desc(Global)) %>%
  filter(Global > 25)

####  Quick ggplot - emphasize how bad this looks ####
# Need to filter down to the data we care about
# Grouped by Platform over time
ggplot(platformGroups, aes(x = Year, y = NorthAmerica, group = Platform, color = Platform)) +
  geom_line() +
  ylab("Sales (in millions)") +
  labs(title = "North America Video Game Sales - Per Platform",
       subtitle = "1980 - 2020",
       caption = "Data source: vgsales.csv")

# Grouped by genre over time
ggplot(genreGroups, aes(x = Year, y = NorthAmerica, group = Genre, color = Genre)) +
  geom_line() +
  ylab("Sales (in millions)") +
  labs(title = "North America Video Game Sales - Per Genre",
       subtitle = "1980 - 2020",
       caption = "Data source: vgsales.csv")

# Grouped by publisher over time
ggplot(plotablePublisherGroups, aes(x = Year, y = NorthAmerica, group = Publisher, color = Publisher)) +
  geom_line() +
  ylab("Sales (in millions)") +
  labs(title = "North America Video Game Sales - Per Publisher",
       subtitle = "1980 - 2020",
       caption = "Data source: vgsales.csv")

##### melt()####
# Order the cleaned dataframe by most global sales to the least
# Never hurts to check even when a ranking system is predefined by the dataset
# Take a peak at the games that peaked over 3 million sales
globallyRelevantGames <- dfCleaned[order(dfCleaned$Global_Sales, decreasing = TRUE), ] %>%
  filter(Global_Sales >= 3.0)

# Melt the cleaned data frame into "long" df
grilledCheese <- melt(data = globallyRelevantGames,
                      id.vars = "Publisher",
                      measure.vars = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"),
                      variable.name = "metric",
                      value.name = "values")

# Create grouped Publisher barchart for each sales cateogry
ggplot(grilledCheese, aes(x = Publisher, y = values, group = metric, fill = metric)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Global Video Game Sales  - Per Publisher",
       subtitle = "1980 - 2020",
       caption = "Data source: vgsales.csv")

# Wait... we just sorted it and Nintedno was clearly first what happened?
# Sort it again after melting maybe?
grilledCheeseWithButter <- grilledCheese[order(grilledCheese$values, decreasing = TRUE), ]

# Yep that has to fix it
ggplot(grilledCheeseWithButter, aes(x = Publisher, y = values, group = metric, fill = metric)) +
  geom_bar(stat = "identity", position = "stack") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Global Video Game Sales  - Per Publisher (Sorted)",
       subtitle = "1980 - 2020",
       caption = "Data source: vgsales.csv")

# Well well well
# Looks like we need to create a sorted list of Publishers as factors
# Get a distinct list of publishers from the sorted cleaned df earlier
globallySortedForFactors <- globallyRelevantGames %>%
  group_by(Publisher) %>%
  summarise(Total = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(desc(Total))

# Set column "Publisher" as a factor using the sorted list as "levels"
globallyRelevantGames$Publisher <- factor(globallyRelevantGames$Publisher, levels = globallySortedForFactors$Publisher)

# While we are at it - lets clean up some other stuff
# Rename variable.name and value.name columns for graphic
garlicButterToast <- melt(data = globallyRelevantGames,
                          id.vars = "Publisher",
                          measure.vars = c("NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"),
                          variable.name = "Region",
                          value.name = "Game Sales (in millions)")

# Case when for "Sales" columns
smokedGoudaGarlicButterToast <- garlicButterToast %>% mutate(Region = case_when(
      Region == "NA_Sales" ~ "North America",
      Region == "EU_Sales" ~ "Europe",
      Region == "JP_Sales" ~ "Japan",
      Region == "Other_Sales" ~ "Other"
    )
  )

# Define some better colors
colorWheel <- c("North America" = "#1f77b4",
                "Europe" = "#2ca02c",
                "Japan"  = "#d62728",
                "Other" = "#7f7f7f")

# Alright alright lets create the plot
ohNicePlot <- ggplot(smokedGoudaGarlicButterToast, aes(x = Publisher, y = `Game Sales (in millions)`, group = Region, fill = Region)) +
  geom_bar(stat = "identity", position = "stack") +
  # Make the x-axis a bit more presentable
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_fill_manual(values  = colorWheel) +
  labs(title = "Video Game Publisher Global Sales",
       subtitle = "Timespan: 1980 - 2020",
       caption = "Data source: vgsales.csv")

# Save the plot to a file
ggsave(paste0(dir, "oneDeliciousSandwich.png"), plot = ohNicePlot, width = 8, height = 6, dpi = 300)