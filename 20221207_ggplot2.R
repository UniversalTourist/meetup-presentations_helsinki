# Load libraries
library(tidyverse)

# Read data set
# read_csv from the link
game_data <- read_csv("https://tinyurl.com/3a5hxnra")

# 1st plot - Line Chart
ggplot(data = game_data) + #the plot
  geom_line(
    aes(x = date, 
        y = daily_active_users,
        color = game_name)
  ) + # geom and aesthetic
  facet_wrap(vars(platform)) + # a matrix of panels
  theme_bw() + # theme
  labs(title = "Daily Active Users",
       subtitle = "Data from January 2021", 
       x = "Date",
       y = "DAU") # labels 


# 2nd plot - Puzzle Games' Revenue - Line Chart
# Prepare data
game_data_arpdau <- game_data %>% 
  filter(genre == 'puzzle') %>% 
  mutate(arpdau = in_app_revenue / daily_active_users) 

glimpse(game_data_arpdau)

# Create plot with new data
ggplot(data = game_data_arpdau) +
  geom_line(aes(x = date,
                y = arpdau, 
                color = platform)
            ) + # geom and aesthetic
  facet_grid( rows = vars( game_name)) + # a matrix of panels
  theme_bw() + # theme
  labs(title = "Average Revenue per Daily Active Users",
       subtitle = "Data from January 2021", 
       x = "Date",
       y = "ARPDAU") # labels


# 3rd plot - Average DAU per game - Bar Chart
# Prepare data
game_data_averages <- game_data %>% 
  group_by(game_name, platform) %>% 
  summarise(avg_dau = mean(daily_active_users)) %>% 
  ungroup()

glimpse(game_data_averages)

#  Create plot with new data and assign it
plot1 <- 
  ggplot(data = game_data_averages) +
  geom_col(aes( x = platform, 
                y = avg_dau,
                fill = game_name),
           position = "dodge") + # dodge doesn't stack the bars 
  theme_minimal() + # theme
  labs(title = "Average DAU per Game",
       subtitle = "Data from January 2021", 
       x = "Platform",
       y = "Average DAU") # labels 

# How to save your plot? 
ggsave(filename = "plot1.png",
      plot = plot1, 
      width = 6, 
      height = 4)

