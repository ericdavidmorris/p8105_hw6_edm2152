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
  geom_point(color = "firebrick") + 
  geom_errorbar(aes(ymin = OR_Lower_Bound, ymax = OR_Upper_Bound)) +
  coord_flip() + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Adjusted Odds Ratios for Solving Homicides Comparing \n Non-White Victims to White Victims by City", 
       y = "Adjusted Odds Ratio", 
       x = "City, State", 
       caption = "Error Bars represent 95% Confidence Interval of Adjusted Odds Ratio") + 
  theme(axis.text.x = element_text(size = 3),
        plot.title = element_text(hjust = 0.5)) + 
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/All Cities GLM and plot-1.png" style="display: block; margin: auto;" />

In the above plot, we see that Tampa, Florida, followed by Birmingham, AL and Durham, NC have the 'highest' adjusted odds ratio, all of which are greater than the null value of 1. This indicates that the odds of a non-white homicide victim's case being solved are greater than a white homicide victim's case being solved. However, they all have wide confidence intervals which include the null value of 1, indicating the relationship isn't significant.

Addtionally, we see that the 'bottom' five adjusted odds ratios on the plot are Boston, Omaha, Oakland, Pittsburgh, and Cincinnati (in ascending order). This indicates that these cities, with ORs &lt;1, are the least likely to solve a non-white victim's homicide case compared to a white victim's case. The confidence intervals all do not include the null value one, indicating a significant relationship.

Overall, we see that cities with a large amount of homicides (Baltimore, Chicago) have smaller confidence intervals, while smaller cities with fewwer homicides tend to have large confidence intervals. This is expected, as with more data we can have less variability, our standard errors tend to be smaller.

Problem 2
---------

``` r
bw = read_csv("data/birthweight.csv") %>% 
  mutate(frace = ifelse(frace == "9", NA, frace),
         babysex = as.factor(babysex),
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
# categorical variables and not appropriately continous, I converted using as.factor. I also made those with value = 9 for frace = NA, which is what was indicated in the data dictionary

# Checking for missing data,
# There is no missing data in the data frame when checking each of the columns with the code below: 

# sum(is.na(bw$bwt))
```

*Below are some exploratory plots/analysis I will use to help select my model*

``` r
#Exploratory plots to see distribution of birthweight -

# No x to plot against, tried this scatter but not much info: bw %>% ggplot(aes(x = 1:nrow(bw), y = bwt)) + geom_point() + theme_bw()

bwhisto =
  bw %>% 
  ggplot(aes(x = bwt)) +
  geom_histogram(bins = 50) + 
  theme_bw()

bwbox =
  bw %>% 
  ggplot(aes(y = bwt)) + 
  geom_boxplot() +
  theme_bw()

wrap_elements(bwhisto + bwbox)
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-1.png" style="display: block; margin: auto;" />

``` r
# Birthweight is probably fairly normally distributed, with a few outliers, and should be treated as a continous variable in an analysis 

bwsex = 
  bw %>% 
  ggplot(aes(x = babysex, y = bwt)) + 
  geom_boxplot() +
  theme_bw()

fracebw = 
  bw %>% 
  ggplot(aes(x = frace, y = bwt)) + 
  geom_boxplot() +
  theme_bw()

mracebw = 
  bw %>% 
  ggplot(aes(x = mrace, y = bwt)) + 
  geom_boxplot() +
  theme_bw()

wrap_elements(bwsex + fracebw + mracebw)
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-2.png" style="display: block; margin: auto;" />

``` r
# The boxplots for categorical variables tells us a little bit of info, mostly that birthweight varies across all the categories 

# For checking the relationships with continuous variables, I'll run a loop to determine the correlation coefficients 

cor_cts_table =
  bw %>% 
  select_if(is.numeric) %>% 
  select(-bwt,
         -pnumlbw,
         -pnumsga)

corr_values = list()

for (i in 1:ncol(cor_cts_table)) {
  corr_values[[i]] = cor(bw$bwt, cor_cts_table[i])
  }

as.data.frame(corr_values) %>% 
  gather(key = var, value = correlation) %>% 
  knitr::kable(digits = 3)
```

| var      |  correlation|
|:---------|------------:|
| bhead    |        0.747|
| blength  |        0.743|
| delwt    |        0.288|
| fincome  |        0.155|
| gaweeks  |        0.412|
| menarche |       -0.024|
| mheight  |        0.192|
| momage   |        0.136|
| parity   |       -0.008|
| ppbmi    |        0.094|
| ppwt     |        0.183|
| smoken   |       -0.076|
| wtgain   |        0.247|

``` r
# From above we see that bhead, blength, delwt, gaweeks and wtgain all have correlation values >0.2, which may indicate a positive linear relationship with birthweight. While this might not mean much, it'll help me include it in a model

# Scatter plots for cts variables, viewing correlation 
# bw %>% 
#  ggplot(aes(x= smoken, y = bwt)) + 
#  geom_point() +
#  geom_smooth() + 
#  theme_bw()
```

``` r
# Using my above 'analysis' and some hypothesized factors that I think influence birthweight, my proposed model is below. 

# I'm afraid of adding too many predictors. I was wary of adding the bhead and blength variables, as these are measurements taken post-birth and cannot be changed/influenced. However, I chose to include blength because it's clear that length (height) and weight have a relationship. 

# I chose not to include both mother and father's race as it had many levels and may complicate the interpretation. Additionally I didn't add any interaction terms to keep it a simple linear model with multiple predictors. I included the baby's sex, mother's weight at delivery (high correlation), gestational age (in weeks, high correlation and hypothesized fact), and the smoking variable (possible confounder). 

eric_model = 
  bw %>% 
  lm(bwt ~ babysex + blength + delwt + gaweeks + smoken, data = .)

bw %>% 
  add_predictions(eric_model) %>% 
  add_residuals(eric_model) %>% 
  ggplot(aes(x = pred, y = resid)) + 
  geom_point() + 
  geom_smooth(se = FALSE) +
  theme_bw() + 
  labs(title = "Plotting predicted values vs. residuals",
       x = "Predicted Value",
       y = "Residual")
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

<img src="HW6_files/figure-markdown_github/My Proposed Model-1.png" style="display: block; margin: auto;" />

When I plot my model's residuals against predicted value, there seems to be some clustering, but also a good amount of outliers. I'm not sure my model predicts birthweight well, but I'm sticking to it.

``` r
cv_bwt = crossv_mc(bw, 100)

cv_bwt = cv_bwt %>% 
  mutate(my_model = map(train, ~lm(bwt ~ babysex + blength + delwt + gaweeks + smoken, data = .)),
         jeff_model_main = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         jeff_model_int = map(train, ~lm(bwt ~ bhead + blength + babysex + bhead*blength + blength*babysex + 
                                           bhead*babysex + bhead*blength*babysex, data = .x))) %>% 
  mutate(rmse_my = map2_dbl(my_model, test, ~rmse(model = .x, data = .y)),
         rmse_jeff_main = map2_dbl(jeff_model_main, test, ~rmse(model = .x, data = .y)),
         rmse_jeff_int = map2_dbl(jeff_model_int, test, ~rmse(model = .x, data = .y)))

cv_bwt %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + 
  geom_violin() +
  theme_bw() + 
  labs(title = "Violin plots of selected model's RMSE",
       x = "Model Title",
       y = "RMSE")
```

<img src="HW6_files/figure-markdown_github/Model Comparison and CV-1.png" style="display: block; margin: auto;" />

``` r
# Mean RMSE's in a clean table

cv_bwt %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  group_by(model) %>% 
  summarize(mean_rmse = mean(rmse)) %>% 
  knitr::kable()
```

| model      |  mean\_rmse|
|:-----------|-----------:|
| my         |    327.1560|
| jeff\_main |    333.4554|
| jeff\_int  |    290.1558|

The above plot visualizes violin plots for the RMSE of my proposed model, Jeff's main effects model, and Jeff's interaction model. Based on these plots, I would choose Jeff's interaction model, though my give a slight edge to my model over the main effects model. Adding interaction terms would probably help my model, as well as eliminating predictors which to add much (probably messing up the adjusted r^2).
