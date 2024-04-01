#### Packages ####
library(tidyverse)

#### Defining Functions ####
# Month Selection
month <- function(input){
  months <- c("January", "February", "March", "April", "May", "June", 
              "July", "August", "September", "October", "November", "December")
  sprintf("%02d", match(input, months))
}

# Time Zone Selection
time_zone <- function(time, location) {
  time <- as.POSIXct(time, tz = "UTC")  
  attr(time, "tzone") <- location       
  return(time)
}

#### Importing Data ####
animal_data <- read_csv("Wombat data.csv") %>% # INPUT DATA PATH HERE
  select(1,4,23,24,35:39) %>%
  na.omit() %>%
  mutate(time_observed_at = time_zone(time_observed_at, "Australia/Adelaide")) %>% # INPUT TIME ZONE HERE
  mutate(hour = substr(time_observed_at, 12, 13)) %>%
  filter(substr(time_observed_at, 6, 7) == month("June")) # INPUT MONTH HERE


#### Plotting ####
ggplot(animal_data, aes(x=hour)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Animal Sightings", x = "Hour", y = "Counts") +
  geom_text(data = count(animal_data, hour), aes(label = n, y = n, group = 1), vjust = -0.5, size = 3) +
  theme_bw()

