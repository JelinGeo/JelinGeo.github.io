if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse)

library(tidyverse)
library(tibble)
library(dplyr)

# Measures of Central Tendency

# BaseR package

data <- c(2, 4, 4, 4, 5, 7, 9)
mean(data)   # Arithmetic mean
median(data) # Median
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Mode(data)   # Mode

# Tidyverse package
data_tbl <- tibble(value = c(2, 4, 4, 4, 5, 7, 9))

data_tbl %>%
  summarise(
    mean = mean(value),
    median = median(value),
    mode = value %>% 
      table() %>% 
      which.max() %>% 
      names() %>% 
      as.numeric()
  )

# Measures of Dispersion

data_tbl <- tibble(value = c(2, 4, 4, 4, 5, 7, 9))

data_tbl %>%
  summarise(
    min = min(value),
    max = max(value),
    range = max(value) - min(value),
    variance = var(value),
    sd = sd(value),
    IQR = IQR(value),
    coefficient_of_variation = sd(value) / mean(value)
  )


# Measures of Shape and Distribution

library(dplyr)

data_tbl <- tibble(value = c(2, 4, 4, 4, 5, 7, 9))

data_tbl %>%
  summarise(
    n = n(),
    mean = mean(value),
    sd = sd(value),
    skewness = sum((value - mean) ^ 3) / n / (sd ^ 3),
    kurtosis = sum((value - mean) ^ 4) / n / (sd ^ 4)
  ) %>%
  select(-mean, -sd, -n) # remove intermediate columns if you only want skewness and kurtosis




# Measures of Shape and Distribution


library(ggplot2)
library(tibble)

#| label: fig-histogram
#| fig-cap: "Histogram of the data"

data_tbl <- tibble(value = c(2, 4, 4, 4, 5, 7, 9))

ggplot(data_tbl, aes(x = value)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(
    title = "Figure 2. Histogram of Data",
    x = "Value",
    y = "Frequency"
  )


library(ggplot2)
library(tibble)

data_tbl <- tibble(value = c(2, 4, 4, 4, 5, 7, 9))

ggplot(data_tbl, aes(x = factor(1), y = value)) +
  geom_boxplot(fill = "blue") +
  labs(
    title = "Figure 3. Boxplot of Data",
    x = "",
    y = "Value"
  ) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# missing data

data_tbl <- tibble(value = c(2, 4, NA, 4, 5, NA, 9))

data_tbl %>%
  summarise(mean = mean(value, na.rm = TRUE)) # Ignore NAs

# frequency

category1 <- c('A', 'B', 'A', 'C', 'B', 'A')
category2 <- c('X', 'Y', 'X', 'Y', 'X', 'Y')
data_tbl <- tibble(category1 = category1, category2 = category2)


data_tbl %>%
  count(category1, category2, name = "frequency")


data_tbl %>%
  count(category1, category2, name = "frequency") %>%
  mutate(proportion = frequency / sum(frequency))

# Another example 

# Example data
age <- c('Young', 'Old', 'Young', 'Old', 'Young', 'Old')
gender <- c('Male', 'Female', 'Female', 'Male', 'Male', 'Female')
data_tbl <- tibble(age = age, gender = gender)

# Frequency table for combinations of age and gender
data_tbl %>%
  count(age, gender, name = "frequency")

# Proportion table for combinations of age and gender
data_tbl %>%
  count(age, gender, name = "frequency") %>%
  mutate(proportion = frequency / sum(frequency))

# To calculate the total of frequencies
age <- c('Young', 'Old', 'Young', 'Old', 'Young', 'Old')
gender <- c('Male', 'Female', 'Female', 'Male', 'Male', 'Female')
data_tbl <- tibble(age = age, gender = gender)

data_tbl %>%
  count(age, gender, name = "frequency") %>%
  arrange(age, gender) %>%
  mutate(cumulative_frequency = cumsum(frequency))


age <- c('Young', 'Old', 'Young', 'Old', 'Young', 'Old')
gender <- c('M', 'F', 'F', 'M', 'F', 'M')
data_tbl <- tibble(age = age, gender = gender)

# Frequency table for age and gender
data_tbl %>%
  count(age, gender, name = "frequency")

data_tbl %>%
  count(age, gender, name = "frequency") %>%
  pivot_wider(names_from = gender, values_from = frequency, values_fill = 0)


# Summarise dataframes

library(skimr)

df <- tibble(
  Age = c(21, 22, 22, 23, 24, 25, 25),
  Gender = c('F', 'M', 'M','F', 'F', 'M', 'M'),
  Score = c(85, 90, 88, 95, 85, 88, 80)
)

# Use glimpse for a tidyverse-style structure overview
glimpse(df)

# Or use skim for a detailed summary
skim(df)


library(dplyr)
data(starwars)
starwars_tbl <- starwars %>%
  slice_head(n = 10)

kable(starwars_tbl, format = "latex", booktabs = TRUE, caption = "Table 1. Star Wars Data") %>%
  kable_styling(latex_options = "striped", full_width = FALSE)

# Mean height
starwars %>% 
  summarise(mean_height = mean(height, na.rm = TRUE))


# Median height
starwars %>% 
  summarise(median_height = median(height, na.rm = TRUE))

# Mode height

starwars %>%
  filter(!is.na(height)) %>%
  count(height, sort = TRUE) %>%
  slice_max(n = 1, order_by = n) %>%
  select(mode_height = height)

