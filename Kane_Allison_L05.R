# L05-EDA ----
# Stat 301-1

## load packages ----
library(tidyverse)
library(lvplot)
library(skimr)
library(naniar)

## load data
data(diamonds)

## Exercises ----

### Ex 1 ----
# Explore the distribution of each of the `x`, `y`, and `z` variables in `diamonds`. What do you learn? Think about a diamond; think about how we might determine which dimension of a diamond is its length, width, and depth.

# explore missingness

# exploring x 
ggplot(diamonds, aes(x = x)) +
  geom_histogram(color = "white")

# exploring y
ggplot(diamonds, aes(x = y)) +
  geom_histogram(binwidth = 1)

# exploring z
ggplot(diamonds, aes(x = z)) +
  geom_histogram(binwidth = 1)

# Generally there is more variation in diamond width (x) than diamond length (y) and diamond height (z). This has been determined since most diamonds are probably having a much smaller depth rather than length and width. Looking at distribution of z compared to distributions of x and y, its distribution is centered much lower. 
# This is makes sense, as diamonds vary in their size in width based on the size, but often these diamonds are used in rings which restrict the length and height needed. 
# 

### Ex 2 ----

# Explore the distribution of `price`. Do you discover anything unusual or surprising? 
#   *Hint:* Carefully think about the `binwidth` and make sure you try a wide range of values.

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 300, color = "white", boundary = 0)

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 150, color = "white")

ggplot(diamonds, aes(x = price)) +
  geom_histogram(binwidth = 500, color = "white", boundary = 0)

# Shape, Center, Spread (variability)
# Skewed right, asymmetrical 
# Peak at approximately $1000, unimodal
# Typical price of a diamond is around $1200
# Most diamonds range from about $1000 to $10,000

# Something strange going on around $1000. Might be easier to sell diamonds that are at $999 or $1000+ rather than $1000

### Ex 3 ----

# What is the major difference between using `coord_cartesian()` vs `xlim()` or `ylim()` to zoom in on a histogram/graphic? What happens if you leave `binwidth` unset? What happens if you try to zoom so only half a bar shows?

ggplot(diamonds, aes(x = price)) +
  geom_histogram(color = "white", boundary = 0)

ggplot(diamonds, aes(x = price)) +
  geom_histogram(color = "white", boundary = 0) +
  coord_cartesian(xlim = c(0, 15000))

ggplot(diamonds, aes(x = price)) +
  geom_histogram(color = "white", boundary = 0) +
  xlim(0, 15000)

### Ex 4 ----

# see qmd for answer

### Ex 5 ----
# Based on EDA, what variables in the diamonds dataset appears to be most important for predicting the price of a diamond? 

# see qmd for analysis

ggplot(data = diamonds, aes(x = carat, y = cut)) +
  geom_boxplot()

### Ex 6 ----

# boxplot to compare to
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_boxplot() +
  labs(
    x = "Cut",
    y = "Price",
    title = "Cut vs. Price"
  )

# letter value plot
ggplot(diamonds, aes(x = cut, y = price)) +
  geom_lv() +
  labs(
    x = "Cut",
    y = "Price",
    title = "Cut vs. Price"
  )
  

### Ex 7 ----

# Create a visualization of diamond prices vs. a categorical variable from the diamonds dataset 
# using `geom_violin()`, 
# then a faceted `geom_histogram()`,
# then a colored `geom_freqpoly()`, 
# and then a colored `geom_density()`. 
# Compare and contrast the four plots. 
# What are the pros and cons of each method of visualizing the
# distribution of a numerical variable based on the levels of a categorical variable?

# geom violin
violin_plot <- ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin() +
  labs(
    x = "Color",
    y = "Price",
    title = "Price distribution of different cut diamonds"
  )

ggsave(
  filename = "plots/violin_plot.png",
  plot = violin_plot,
  width = 6,
  units = "in"
)

# geom histogram faceted
histogram_plot <- ggplot(diamonds, aes(x = price)) +
  geom_histogram() +
  facet_wrap(~ cut) +
  labs(
    x = "Price",
    title = "Price distribution of different cut diamonds"
  )
  
ggsave(
  filename = "plots/histogram_plot.png",
  plot = histogram_plot,
  width = 6,
  units = "in"
)

# geom freqpoly 
freqpoly_plot <- ggplot(diamonds, aes(x = price, color = cut)) +
  geom_freqpoly() +
  labs(
    x = "Price",
    title = "Price distribution of different cut diamonds"
  )

ggsave(
  filename = "plots/freqpoly_plot.png",
  plot = freqpoly_plot,
  width = 6,
  units = "in"
)

# geom density
density_plot <- ggplot(diamonds, aes(x = price, color = cut, fill = cut)) +
  geom_density(alpha = .15) +
  labs(
    x = "Price",
    title = "Price distribution of different cut diamonds"
  )

ggsave(
  filename = "plots/density_plot.png",
  plot = density_plot,
  width = 6,
  units = "in"
)


### Ex 8 ----
diamonds_rescaled_color <- 
  diamonds |> 
    count(color, cut) |> 
    group_by(color) |> 
    mutate(rescaled = scale(n))

cut_within_color <- ggplot(diamonds_rescaled_color, mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = rescaled)) 

ggsave(
  filename = "plots/cut_within_color.png",
  plot = cut_within_color,
  width = 6,
  units = "in"
)

diamonds_rescaled_cut <- 
  diamonds |> 
  count(color, cut) |> 
  group_by(cut) |> 
  mutate(rescaled = scale(n))

color_within_cut <- ggplot(diamonds_rescaled_cut, mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = rescaled))

ggsave(
  filename = "plots/color_within_cut.png",
  plot = color_within_cut,
  width = 6,
  units = "in"
)

### Ex 9----

#see qmd

### Ex 10----

smaller <- diamonds |> 
  filter(carat < 3)
  
cut_width_plot <- ggplot(smaller, aes(x = cut_width(price, width = 3), y = carat)) +
  geom_point() +
  labs(
    x = "Price",
    y = "Carat",
    title = "Price by Carat for Diamonds less than 3 carats"
  )

ggsave(
  filename = "plots/cut_width_plot.png",
  plot = cut_width_plot,
  width = 6,
  units = "in"
)

cut_number_plot <- ggplot(smaller, aes(x = cut_number(price, n = 20), y = carat)) +
  geom_point() +
  labs(
    x = "Price",
    y = "Carat",
    title = "Price by Carat for Diamonds less than 3 carats"
  ) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggsave(
  filename = "plots/cut_number_plot.png",
  plot = cut_number_plot,
  width = 6,
  units = "in"
)

### Ex 11----

# additional exploration
larger <- diamonds |> 
  filter(carat > 3)

# what does the boxplot for larger diamonds look like
ggplot(larger, aes(x = price)) +
  geom_boxplot()
# compared to smaller?
ggplot(smaller, aes(x = price)) +
  geom_boxplot()

# smaller diamonds have a much larger range of prices and greater variation

# skim both
skim_without_charts(larger$price)

skim_without_charts(smaller$price)
# see qmd for full answer

### Ex 12 ----

gg_miss_var(flights)

miss_var_summary(flights) |> 
  filter(n_miss != 0)

### Ex 13 ----

missing_flights_plot_1 <- ggplot(flights, aes(x = dep_delay, y = arr_delay)) +
  geom_miss_point()

ggsave(
  filename = "plots/missing_flights_plot_1.png",
  plot = missing_flights_plot_1,
  width = 6,
  units = "in"
)

missing_flights_plot_2 <- gg_miss_var_cumsum(flights)

ggsave(
  filename = "plots/missing_flights_plot_2.png",
  plot = missing_flights_plot_2,
  width = 6,
  units = "in"
)

### Ex 14 ----

# see qmd for question

# answer
flights |> 
  summarize(
    late = sum(dep_delay > 0, na.rm = TRUE),
    on_time = sum(dep_delay == 0, na.rm = TRUE),
    early = sum(dep_delay < 0, na.rm = TRUE)
  )

### Case Study----

tinder_data <- read.csv("data/tinder_data.csv")

# Question: do female users interested in either F both M and F date differently on tinder than female users interested in only men?

# wrangling
tinder_data_women <- tinder_data |> 
  filter(user_gender == "F") |> 
  mutate(
    dates_women = ifelse(user_interested_in == "M", "Straight", "Queer")
  )

tinder_data_women_straight <- tinder_data_women |> 
  filter(dates_women == "Straight") 

tinder_data_women_queer <- tinder_data_women |> 
  filter(dates_women == "Queer") 

# initial investigation

skim_without_charts(tinder_data_women_straight)

skim_without_charts(tinder_data_women_queer)


# univariate analysis-- 

women_matches <- ggplot(data = tinder_data_women, aes(x = matches)) +
  geom_histogram(binwidth = 200) +
  facet_wrap(~dates_women) +
  labs(
    x = "Matches",
    y = "Count",
    title = "Distribution of Matches Received for Straight and Queer* Women (150) on Tinder",
    subtitle = "*The genders these women were interested in include other women and both women and men",
    caption = "Source: swipestats.io"
  ) +
  theme_classic()
  

ggsave(
  filename = "plots/women_matches.png",
  plot = women_matches,
  width = 10,
  units = "in"
)

# generally the distribution of the number of matches received is skewed right. The median number of matches for queer women is approximately 
# 300, while the median number of matches for straight women was approximately 400. The spread of the number of matches differed quite a bit between 
# queer and straight women, with queer women ranging from 0 to approximately 900 matches and straight women ranging from 0 to approximately 2100 matches. 
# This provides some insight into how queer and straight women differ in their dating experience, but further analysis is needed. 

women_ghostings <- ggplot(data = tinder_data_women, aes(x = number_ghostings)) +
  geom_histogram(binwidth = 60) +
  facet_wrap(~dates_women) +
  labs(
    x = "Matches",
    y = "Count",
    title = "Distribution of Ghostings Received for Straight and Queer* Women (150) on Tinder",
    subtitle = "*The genders these women were interested in include other women and both women and men",
    caption = "Source: swipestats.io"
  ) +
  theme_classic()

ggsave(
  filename = "plots/women_ghostings.png",
  plot = women_matches,
  width = 10,
  units = "in"
)

# Multivariate -- 

# messages sent messages received? 

women_sent_received <- ggplot(data = tinder_data_women, aes(x = messages_sent, y = messages_received)) +
  geom_point() +
  facet_wrap(~dates_women) +
  labs(
    x = "Messages Sent",
    y = "Messages Received",
    title = "Relationship between Messages Sent and Messages Received Received for Straight and Queer* Women (150) on Tinder",
    subtitle = "*The genders these women were interested in include other women and both women and men",
    caption = "Source: swipestats.io"
  ) +
  theme_classic()

ggsave(
  filename = "plots/women_sent_received.png",
  plot = women_sent_received,
  width = 10,
  units = "in"
)

# matches and likes sent
matches_and_likes_plot <- ggplot(data = tinder_data_women, aes(x = swipes_like, y = matches, color = user_interested_in)) +
  facet_wrap(~dates_women) +
  geom_point() +
  labs(
    x = "Likes Sent",
    y = "Matches",
    title = "Relationship between Matches and Likes Sent for Straight and Queer* Women (150) on Tinder",
    subtitle = "*The genders these women were interested in include other women and both women and men",
    caption = "Source: swipestats.io",
    color = "Interested In"
  ) +
  geom_abline() +
  theme_classic()

ggsave(
  filename = "plots/matches_and_likes_plot.png",
  plot = matches_and_likes_plot,
  width = 10,
  units = "in"
)