#### Packages ####
library(tidyverse)

#### Defining Functions ####


#### Importing Data ####
animal_data <- read_csv("PATH") %>% # Change path to location of data file
  select(1,4) %>% 
  na.omit() %>%
  separate(time_observed_at, into = c("date", "time", "time_zone"), sep = " ") %>%
  select(-time_zone) %>%
  mutate(hour = substr(time, 1, 2)) %>%
  filter(substr(date, 6, 7) == "06") %>% #06 for June
  mutate(hour = as.numeric(hour) + 9) %>% # 9 for adelaide time
  mutate(hour = ifelse(hour >= 24, hour - 24, hour))

#### Plotting ####
ggplot(animal_data, aes(x=hour)) +
  geom_histogram(stat = "count", fill = "skyblue", color = "black") +
  labs(title = "Animal Sightings", x = "Hour", y = "Counts") +
  geom_text(data = count(animal_data, hour), aes(label = n, y = n, group = 1), vjust = -0.5, size = 3) +
  theme_bw()

