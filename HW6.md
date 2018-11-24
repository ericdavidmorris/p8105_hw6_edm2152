HW6
================
Eric Morris
11/21/2018

Problem 1
---------

``` r
#Importing the data from GH URL like HW5 and mutating/tidying as indicated in the problem 

wp_homicide_data = 
  read_csv("https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(city_state = str_c(city, state, sep = ", "),
         solved = ifelse(disposition == "Closed by arrest", 1, 0)) %>%
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL")) %>% 
  mutate(victim_race = ifelse(victim_race != "White", "non-White", "White"), 
         victim_race = fct_relevel(victim_race, "White", "non-White"),
         victim_age = as.numeric(victim_age))
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

``` r
# Baltimore-specific glm model saved as an r object

baltimore_model =
  wp_homicide_data %>% 
  filter(city_state == "Baltimore, MD") %>% 
  glm(solved ~ victim_age + victim_sex + victim_race, family = binomial, data = .)

# Applying broom::tidy to the Baltimore model object, obtaining CIs and exponentiating as it's just a coefficient in original output 

baltimore_model %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate),
         OR_Lower_Bound = exp(estimate - (1.96 * std.error)),
         OR_Upper_Bound = exp(estimate + (1.96 * std.error))) %>% 
  filter(term == "victim_racenon-White") %>% 
  select(OR, OR_Lower_Bound, OR_Upper_Bound, p.value) %>% 
  knitr::kable(digits = 4)
```

|      OR|  OR\_Lower\_Bound|  OR\_Upper\_Bound|  p.value|
|-------:|-----------------:|-----------------:|--------:|
|  0.4406|            0.3129|            0.6204|        0|

From the results of our logisitc model above, we obtain statistically significant results regarding the homicides in Baltimore.

We see that in Baltimore, the odds of a non-white victim's homicide case being solved is 0.441 times that of a white victim's case being solved. We are 95% confident that the true odds lie between 0.31 and 0.62.

``` r
# Running the logistic model for all cities in the dataset by grouping by the citiy and nesting their data, then plotting the OR and CI in descending order

wp_homicide_data %>% 
  group_by(city_state) %>% 
  nest() %>%
  mutate(logit_all = map(data, ~glm(solved ~ victim_age + victim_sex + victim_race, family = binomial, data = .)),
         logit_all = map(logit_all, broom::tidy)) %>%
  select(-data) %>% 
  unnest() %>% 
  filter(term == "victim_racenon-White") %>%
  mutate(OR = exp(estimate),
         OR_Lower_Bound = exp(estimate - (1.96 * std.error)),
         OR_Upper_Bound = exp(estimate + (1.96 * std.error)),
         city_state = fct_reorder(city_state, estimate)) %>%
  select(city_state, OR, OR_Lower_Bound, OR_Upper_Bound) %>% 
  ggplot(aes(x = city_state, y = OR))+
  geom_point() + 
  geom_errorbar(aes(ymin = OR_Lower_Bound, ymax = OR_Upper_Bound)) +
  coord_flip() + 
  labs(title = "Adjusted Odds Ratios for Solving Homicides Comparing \n Non-White Victims to White Victims by City", 
       y = "Adjusted Odds Ratio", 
       x = "City, State", 
       caption = "Error Bars represent 95% Confidence Interval of Adjusted Odds Ratio") + 
  theme(axis.text.x = element_text(size = 3),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/All Cities GLM and plot-1.png" style="display: block; margin: auto;" />

Problem 2
---------

Load and clean the data for regression analysis (i.e. convert numeric to factor where appropriate, check for missing data, etc.).

``` r
bw = read_csv("data/birthweight.csv") %>% 
  mutate(babysex = as.factor(babysex),
         frace = as.factor(frace), 
         malform = as.factor(malform), 
         mrace = as.factor(mrace))
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   gaweeks = col_double(),
    ##   ppbmi = col_double(),
    ##   smoken = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
# After importing the data, I checked which columns seemed appropriate to be converted to factors from numeric values. Those which where dichotomous or 
# categorical variables and not appropriately continous, I converted using as.factor. 

# Checking for missing data,
# There is no missing data in the data frame when checking each of the columns with the code below: 

# sum(is.na(bw$bwt))
```

Propose a regression model for birthweight. This model may be based on a hypothesized structure for the factors that underly birthweight, on a data-driven model-building process, or a combination of the two. Describe your modeling process and show a plot of model residuals against fitted values – use add\_predictions and add\_residuals in making this plot.
