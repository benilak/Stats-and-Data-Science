# Case Study 5

library(tidyverse)
library(haven)
library(readr)
library(readxl)
library(downloader)
library(foreign)

# read worldwide data
ww.height <- read_xlsx("Case_Study_05/analysis/Height.xlsx", skip = 1, col_names = TRUE)

# tidy worldwide data
ww.height.tidy <- ww.height %>%
  gather(key = "year", value = "height", '1800':'2011') %>%
  mutate(decade = gsub(".$", "0", year),
         year = parse_integer(year)) %>%
  rename(region = `Continent, Region, Country`)

# read other data
german.con <- read_dta("Case_Study_05/analysis/germanconscr.dta")
german.pris <- read_dta("Case_Study_05/analysis/germanprison.dta")
german.south <- read.dbf("Case_Study_05/analysis/Heights_south-east/B6090.DBF")
bls.heights <- read_csv("Case_Study_05/analysis/heights.csv")
'national <- read.spss("Case_Study_05/analysis/nationalsurvey.sav", to.data.frame = TRUE)'

# tidy other data
german.con.tidy <- german.con %>%
  select(bdec, height) %>%
  rename(birth_year = bdec, height.cm = height) %>%
  mutate(height.in = height.cm*0.393701, study_id = "german.con")

german.pris.tidy <- german.pris %>%
  select(bdec, height) %>%
  rename(birth_year = bdec, height.cm = height) %>%
  mutate(height.in = height.cm*0.393701, study_id = "german.pris")

german.south.tidy <- german.south %>%
  select(SJ, CMETER) %>%
  rename(birth_year = SJ, height.cm = CMETER) %>%
  mutate(height.in = height.cm*0.393701, study_id = "german.south")

bls.heights.tidy <- bls.heights %>%
  filter(sex %in% "male") %>%
  rename(height.in = height) %>%
  mutate(birth_year = 1950, height.cm = height.in*2.54, study_id = "bls.heights") %>%
  select(birth_year, height.cm, height.in, study_id)

# this data set is so big it crashes R - forget it
'national.tidy <- national %>%
  select(DOBY, RE35, RT216F, RT216I) %>%
  filter(RE35 %in% 1) %>%
  mutate(height.in = RT216F*12 + RT216I, height.cm = height.in*2.54,
         birth_year = DOBY, study_id = "national") %>%
  select(height.in, height.cm, birth_year, study_id)'

# combine tidy data
tidy.heights <- bind_rows(german.con.tidy, german.pris.tidy, german.south.tidy, bls.heights.tidy)

# save tidy data (worldwide and combined)
write_csv(ww.height.tidy, "Case_Study_05/analysis/ww.height.tidy")
write_csv(tidy.heights, "Case_Study_05/analysis/tidy.heights")

# scatterplot w/ germany highlighted
ww.height.tidy %>%
  filter(!is.na(height)) %>%
  mutate(height.in = height*0.393701) %>%
  ggplot(aes(x = decade, y = height.in)) +
  geom_jitter(aes(color = region=='Germany')) +
  scale_color_manual(values = c('gray', 'red')) +
  labs(title = "Worldwide heights of men, germany highlighted",
       x = "Decade", y = "Height (inches)", color = "Germany") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# frequency plot
tidy.heights %>% ggplot(aes(x=height.in)) + 
  geom_freqpoly(bins = 50) + 
  facet_wrap(~study_id, scales = "free_y", ncol = 1) + 
  theme_bw() + 
  labs(title = "Adult Male Heights", x = "Height (inches)", y = "Count")

tidy.heights %>%
  mutate(decade = gsub(".$", "0", birth_year)) %>%
  ggplot(aes(x = decade, y = height.cm)) +
  geom_boxplot()
  
tidy.heights %>%
  mutate(decade = gsub(".$", "0", birth_year)) %>%
  ggplot(aes(x = decade, y = height.cm)) +
  geom_jitter()

