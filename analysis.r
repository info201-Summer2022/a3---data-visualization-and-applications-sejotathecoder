library(readr)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(usmap)
library(tidyverse)
library(scales)
library(plotly)
library(tidyr)


# csv read connection
data <- read.csv("incarceration_trends.csv")


# Nuts n' Bolts
southdata <- filter(data, region == "South")
texdata <- filter(data, state == "TX")
south_inc_rates <- southdata %>% mutate(black_inc_perc = ((black_jail_pop) / black_pop_15to64) * 100)
relevant_south_inc_rates <- south_inc_rates[,c(1,2,3,4,5,6,7,11,16,21,29,32,122)]
relevant_south_inc_rates <- relevant_south_inc_rates %>% filter(black_inc_perc != "Inf" & black_inc_perc >= 0)
numstates <- unique(relevant_south_inc_rates$state)


# Summary information
mean_value_blk_incperc <- mean(relevant_south_inc_rates$black_inc_perc)
max_value_blk_jailrate <- max(blkvtotal_data$black_jail_pop_rate)
sum_totaljail_pop <- sum(blkvtotal_data$total_jail_pop)
sum_blk15to64_pop <- sum(blkvtotal_data$black_pop_15to64)
dif_totaljail_to_blkpop <- sum_blk15to64_pop - sum_totaljail_pop
c <- ncol(data)
r <- nrow(data)


# A line chart that shows trends over time for 1 variable. This chart contains 3 lines,
# each representing different counties and their African American incarceration rates.
graph_data <- south_inc_rates[,c(2,5,96,100,103)] %>% filter(county_name == "Harris County" |
                                                               county_name == "Tarrant County" |
                                                               county_name == "Dallas County")
graph_data <- na.omit(graph_data)
tex_county_map <- graph_data %>% ggplot(aes(x = year, y = black_jail_pop_rate, group = county_name, color = county_name)) +
  ggtitle("TX black incarceration rates") + labs(y= "Rate of African Americans in Jail", x = "Time") + geom_line()


# This double variable comparison chart juxtaposes the total jail population throughout Texas and the
# black population from the ages of 15 to 64.
two_var_chart <- ggplot(blkvtotal_data, aes(x = total_jail_pop)) +  
  geom_line(aes(y = black_pop_15to64)) + ggtitle("black population increasing causes icrease in total jail population") +
  labs(y= "African American Population (ages 15-64)", x = "Total Jail Population")
blkvtotal_data <- na.omit(texdata[,c(2,11,21,96,100)])


# This map shows an overview of the African American incarceration population across counties throughout Texas.
# There is nothing to prove with this map; it just shows, on a larger scale, where African Americans are incarcerated in Texas.
tex2018_aajail_map <- filter(texdata, year == 2018)
minimalist <- theme_bw() + theme(
  axis.line = element_blank(),        
  axis.text = element_blank(),        
  axis.ticks = element_blank(),       
  axis.title = element_blank(),       
  plot.background = element_blank(),  
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(), 
  panel.border = element_blank()      
)
texmap <- plot_usmap(data = tex2018_aajail_map, values = "black_jail_pop",
                     regions = "counties", include = c("TX"), color = "gray") + 
  labs(title = "Texas 2018 African American incarceration population",
       subtitle = "This is a map showing African American incarceration populations accross Texas counties in 2018 (the black counties filled in are null values)") + 
  scale_fill_continuous(low = "white", high = "red", name = "number incarcerated", label = scales::comma) + minimalist
