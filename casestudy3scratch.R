library(tidyverse)
library(nycflights13)

valid_dest <- flights %>%
  filter(!is.na(dest), !is.na(arr_delay)) %>%
  count(dest) %>%
  filter(n > 50) %>%
  pull(dest)
  

flights %>%
  filter(!is.na(dest), !is.na(arr_delay), dest %in% valid_dest) %>%
  group_by(dest) %>%
  summarise(q25 = quantile(arr_delay, .25),
            median = median(arr_delay), 
            q75 = quantile(arr_delay, .75)) %>%
  filter(q25 > mean(q25), 
         median > mean(median),
         q75 > mean(q75)) %>%
  ggplot() +
  geom_point(aes(x = dest, y = q25), color = "blue") +
  geom_point(aes(x = dest, y = median), color = "turquoise") +
  geom_point(aes(x = dest, y = q75), color = "green")

filtered_dest <- flights %>%
  filter(!is.na(dest), !is.na(arr_delay), dest %in% valid_dest) %>%
  group_by(dest) %>%
  summarise(q25 = quantile(arr_delay, .25),
            median = median(arr_delay), 
            q75 = quantile(arr_delay, .75)) %>%
  filter(q25 > mean(q25), 
         median > mean(median),
         q75 > mean(q75)) %>%
  pull(dest)

filtered_dest2 <- flights %>%
  filter(!is.na(dest), !is.na(arr_delay), dest %in% filtered_dest) %>%
  group_by(dest) %>%
  summarise(median = median(arr_delay)) %>%
  filter(median > median(median)) %>%
  pull(dest)

cae <- flights %>%
  filter(!is.na(dest), !is.na(arr_delay), dest %in% "CAE") %>%
  select(dest, arr_delay)

flights %>%
  filter(!is.na(dest), !is.na(arr_delay), dest %in% filtered_dest2) %>%
  select(dest, arr_delay) %>%
  ggplot() +
  geom_boxplot(aes(x = dest, y = arr_delay)) +
  geom_boxplot(data = cae, aes(x = dest, y = arr_delay), fill = "tomato1") +
  scale_y_continuous(limits = c(-20, 40)) +
  coord_flip() +
  theme_dark()
  

flights %>%
  filter(!is.na(arr_delay), dest %in% "CAE") %>%
  summarise(mean = mean(arr_delay), median = median(arr_delay))




# densities instead of histograms?

flights %>%
  filter(carrier == "DL", !is.na(arr_delay)) %>%
  ggplot() +
  geom_density(aes(x = arr_delay, color = origin), size = 1) +
  scale_x_continuous(limits = c(NA, 50)) +
  labs(title = "Comparing airports: Delta Airlines' delayed arrivals", 
       x = "Delay in arrival (minutes)", 
       y = "Density",
       color = "Origin Airport")

