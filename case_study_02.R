# Case Study 2 notes

# Chapter 3: R for Data Science - Data visualization

library(tidyverse)
View(mpg)

# To plot mpg, run this code to put displ on the x-axis and hwy on the y-axis:
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(data = mpg)
mpg
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = hwy, y = cyl))
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = class, y = drv))

# use levels to change the appearance of variables across different groups
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
# ggplot2 will automatically assign a unique level of the aesthetic 
# (here a unique color) to each unique value of the variable, 
# a process known as scaling.

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#> Warning: Using size for a discrete variable is not advised.

# Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
# Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
# The shape palette can deal with a maximum of 6 discrete values

# To set an aesthetic manually, set the aesthetic by name as an argument 
# of your geom function; i.e. it goes outside of aes()
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

# facet wrap - formula (~) is one variable
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 2)
# facet grid - two variables
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)
# or omit one of the variables with (.)
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(. ~ cyl)
# compared to facet wrap
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ cyl)

ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv, color = drv)) +
  geom_point(mapping = aes(x = displ, y = hwy, color = drv))

# the 'group' aesthetic sepearates the data by some discrete variable
# without assigning it any visual aspect (such as color or linetype)
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
# it also does not create a legend (which makes sense)

# these two are identical: 
# 1: local mappings
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
# 2: global mappings
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()

# local mappings will overwrite global ones for that specific layer
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()

# you may also specify different data for a particular layer
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)

# coordinate stuff

ggplot(data = mpg, mapping = aes(x = class, y = hwy)) + 
  geom_boxplot() +
  coord_flip()

bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()


##############
# Case Study plots

gapminder1 <- gapminder %>%
  mutate(pop100k = pop/100000)

plot1 <- ggplot(data = gapminder1, 
       mapping = aes(x = lifeExp, y = gdpPercap, 
                     color = continent, size = pop100k)) +
  geom_jitter() +
  facet_grid(. ~ year) +
  coord_trans(y = "sqrt", limy = c(0, 55000)) +
  scale_size(range = c(.5, 5)) +
  labs(x = "Life Expectancy", y = "GDP per capita", size = "Population (100k)") +
  theme_bw()
ggsave("case_study_2_plot1.png", plot = plot1, width = 15, units = "in")

gapminder2 <- gapminder %>%
  filter(country != "Kuwait") %>%
  mutate(pop100k = pop/100000) %>%
  group_by(continent, year) %>%
  summarise(weightedGDP = weighted.mean(gdpPercap),
            weightedpop = sum(as.numeric(pop100k))) 
  

plot2 <- ggplot(data = gapminder2) +
  geom_point(mapping = aes(x = year, y = gdpPercap, 
                           color = continent, size = pop100k)) +
  geom_line(mapping = aes(x = year, y = gdpPercap, 
                          color = continent, group = country), size = .2) +
  geom_point(mapping = aes(x = year, y = weightedGDP, size = weightedpop)) +
  geom_line(mapping = aes(x = year, y = weightedGDP), size = .2) +
  facet_grid(. ~ continent) +
  coord_trans(limy = c(-2000, 52000)) +
  # scale_size(breaks = c(1, 2, 3), labels = c("10000", "20000", "30000"), 
            # range = c(.5, 3)) +
  labs(x = "Year", y = "GDP per capita", 
       size = "Population (100k)", color = "Continent") +
  theme_bw()
ggsave("case_study_2_plot2.png", plot = plot2, width = 15, units = "in")


  






