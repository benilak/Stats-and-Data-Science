library(tidyverse)
library(car)

pathfinders <- tibble(price = c(1500, 3995, 3200, 3960, 2995, 3995, 4799, 3795, 
                 3500, 3999, 2995, 2980, 4900, 3990, 3995, 4980,
                 3388, 2990, 3750, 2995, 2495, 2995, 4995, 3995,
                 2492, 2900, 3990, 2900, 1995, 3995, 1995, 8950,
                 1795, 3500, 4500, 1500, 6888, 2002, 3700, 1995,
                 4980, 2995, 1900, 3491, 2900, 3950, 2990, 3990,
                 6888, 3999, 4495, 2492, 4980, 3500, 4799, 2850,
                 4995, 3995, 3500, 3330, 2899, 2900,
                 2895, 2099, 3222, 7289, 4999),
       mileage = c(190000, 151228, 154000, 198670, 204965, 166981, 124717, 126953,
                   214098, 144000, 157900, 216846, 99416, 224195, 224362, 129636,
                   230327, 145377, 179402, 185413, 198970, 199000, 144031, 151374,
                   185888, 159721, 72675, 146989, 177550, 228699, 226127, 37800, 
                   138730, 188000, 53221, 180200, 80167, 168671, 154751, 137001,
                   181669, 204965, 239000, 161818, 187000, 150000, 145377, 224195,
                   80167, 119000, 133718, 185888, 129636, 63221, 124717, 175565,
                   144031, 143652, 154321, 170347, 158897, 159721,
                   121190, 159343, 91789, 59880, 48123))
View(pathfinders)

path.lm <- lm(price ~ mileage, data = pathfinders)
summary(path.lm)
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  geom_abline(intercept = 6255, slope = -.01666)

plot(path.lm, which=1)
qqPlot(path.lm$residuals)
plot(path.lm$residuals)


##  Y transformations  ##
boxCox(path.lm)

# log(Y)
path.log.lm <- lm(log(price) ~ mileage, data = pathfinders)
summary(path.log.lm)
path.log <- function(x) {exp(8.705 - .000003593*x)}
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  geom_abline(intercept = 6255, slope = -.01666) +
  stat_function(fun = path.log, color = "skyblue")

# sqrt(Y)
path.sqrt.lm <- lm(sqrt(price) ~ mileage, data = pathfinders)
summary(path.sqrt.lm)
path.sqrt <- function(x) {(77.4504882 - .0001147*x)^2}
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  geom_abline(intercept = 6255, slope = -.01666) +
  stat_function(fun = path.sqrt, color = "skyblue")

# Y^.25
path.25.lm <- lm(price^.25 ~ mileage, data = pathfinders)
summary(path.25.lm)
path.25 <- function(x) {(8.911 -.000007847*x)^4}
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  ggeom_abline(intercept = 6255, slope = -.01666) +
  stat_function(fun = path.25, color = "skyblue")


##  X transformations  ##

# 1/X
pathfinders.xrecip <- pathfinders %>%
  mutate(mileage = 1/mileage)
path.xrecip.lm <- lm(price ~ mileage, data = pathfinders.xrecip)
summary(path.xrecip.lm)
plot(path.xrecip.lm, which=1)
qqPlot(path.xrecip.lm$residuals)
plot(path.xrecip.lm$residuals)
path.xrecip <- function(x) {2029 + 217500000/x}
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  geom_abline(intercept = 6255, slope = -.01666) +
  stat_function(fun = path.xrecip, color = "skyblue")

# 1/X^2
pathfinders.xrecip2 <- pathfinders %>%
  mutate(mileage = 1/(mileage)^2)
path.xrecip2.lm <- lm(price ~ mileage, data = pathfinders.xrecip2)
summary(path.xrecip2.lm)
plot(path.xrecip2.lm, which=1)
qqPlot(path.xrecip2.lm$residuals)
plot(path.xrecip2.lm$residuals)
path.xrecip2 <- function(x) {3083 + 7952000000000/(x^2)}
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  geom_abline(intercept = 6255, slope = -.01666) +
  stat_function(fun = path.xrecip2, color = "skyblue")

# log(X)
path.xlog.lm <- lm(price ~ log(mileage), data = pathfinders)
summary(path.xlog.lm)
path.xlog <- function(x) {30000.9 - 2214.8*log(x)}
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price)) +
  geom_abline(intercept = 6255, slope = -.01666) +
  stat_function(fun = path.xlog, color = "skyblue")

# sqrt(X)
summary(lm(price ~ sqrt(mileage), data = pathfinders))

# X^2
summary(lm(price ~ mileage^2, data = pathfinders))





# my mileage:  172776
# current selling price: 3288
2029 + 217500000/172776
# predicted selling price: 3117
2029 + 217500000/200000
(3117 - 3288)/(200000 - 172776)
2029 + 217500000/2000000
(0 - 3288)/(2000000 - 172776)

# linear selling price: 3376
6255 - .01666*172776
# predicted selling price: 2923
6255 - .01666*200000
(2923 - 3376)/(200000 - 172776)
6255 - .01666*375000
(7.5 - 3376)/(375000 - 172776)
((6255 - .01666*375000) - (6255 - .01666*172776))/(375000 - 172776)


# final graphic
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price), 
             color = "darkred", 
             size = 2) +
  geom_abline(aes(intercept = 6255, slope = -.01666, 
                  color = "Original Regression"), 
                  linetype = "dashed") +
  stat_function(fun = path.xrecip, xlim = c(30000, 250000),
                aes(color = "Transformation (X = 1/X)"),
                linetype = "solid",
                size = 1) +
  geom_segment(data = tibble(x = 1:2, y = 1:2),
               aes(x = c(172776, 200000), xend = c(172776, 200000), 
                   y = c(900, 900), yend = c(3288, 3117)),
               color = "darkblue") +
  geom_point(data = tibble(x = c(172776, 200000),
                           y = c(3288, 3117)),
             aes(x = x, y = y),
             color = "darkblue",
             size = 3) +
  annotate("text", x = 172776, y = 1000, 
           label = "172,776", 
           color = "darkblue", size = 3.2, angle = 30,
           vjust = 1.5, hjust = 1) +
  ggrepel::geom_label_repel(data = tibble(x = 1:2, y = 1:2), 
                            aes(x = c(172776, 200000),
                                y = c(3288, 3117)), 
                            label = c("$3288", "$3117"), 
                            color = "darkblue") +
  scale_y_continuous(breaks = seq(2000, 8000, 2000), 
                     labels = scales::dollar,
                     expand = c(0, TRUE)) +
  scale_x_continuous(labels = scales::comma,
                     expand = c(0,0)) +
  coord_cartesian(ylim = c(1000, 9300), clip = "off") +
  labs(title = "Nissan Pathfinder prices and mileage",
       subtitle = "    with added regression lines",
       x = "Mileage",
       y = "Selling Price ($)") +
  scale_color_manual(name = "", values = c("Transformation (X = 1/X)" = "skyblue",
                                           "Original Regression" = "slategray3")) +
  theme_light() +
  theme(panel.grid.minor = element_line(color = "gray95"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        axis.title.y.left = element_text(vjust = 3),
        legend.position = c(.8, .8))

  
scale_linetype_manual(name = "", values = c("Original Regression" = "dashed", 
                                            "1/X Transformation" = "solid")) + 
  
  
  
pathfinders %>%
  ggplot() +
  geom_point(aes(x = mileage, y = price), 
             color = "darkred", 
             size = 2) +
  geom_abline(aes(color = "line1", 
              intercept = 6255, slope = -.01666),
              linetype = "dashed") +
  labs(color = "yes")


  