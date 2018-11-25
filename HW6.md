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

``` r
#Exploratory plots to see distribution of birthweight

# No x to plot against, tried this scatter but not much info: bw %>% ggplot(aes(x = 1:nrow(bw), y = bwt)) + geom_point() + theme_bw()

bw %>% 
  ggplot(aes(x = bwt)) +
  geom_histogram(bins = 50) + 
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-1.png" style="display: block; margin: auto;" />

``` r
bw %>% 
  ggplot(aes(y = bwt)) + 
  geom_boxplot() +
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-2.png" style="display: block; margin: auto;" />

``` r
# Birthweight is probably fairly normally distributed, with a few outliers, and should be treated as a continous variable in an analysis 

bw %>% 
  ggplot(aes(x = babysex, y = bwt)) + 
  geom_boxplot() +
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-3.png" style="display: block; margin: auto;" />

``` r
bw %>% 
  ggplot(aes(x = frace, y = bwt)) + 
  geom_boxplot() +
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-4.png" style="display: block; margin: auto;" />

``` r
bw %>% 
  ggplot(aes(x = mrace, y = bwt)) + 
  geom_boxplot() +
  theme_bw()
```

<img src="HW6_files/figure-markdown_github/Exploratory Plots/Analysis for Model-5.png" style="display: block; margin: auto;" />

``` r
# The boxplots for categorical variables tells us a little bit of info, mostly that birthweight varies across all the categories 

# For checking the relationships with continuous variables, I'll run a function to determine the correlation coefficients 

# Function for this??

cor(bw$bwt, bw$bhead)
```

    ## [1] 0.7471068

``` r
cor(bw$bwt, bw$blength)
```

    ## [1] 0.7434508

``` r
cor(bw$bwt, bw$delwt)
```

    ## [1] 0.2878893

``` r
cor(bw$bwt, bw$fincome)
```

    ## [1] 0.1545715

``` r
cor(bw$bwt, bw$gaweeks)
```

    ## [1] 0.4121833

``` r
cor(bw$bwt, bw$menarche)
```

    ## [1] -0.02442466

``` r
cor(bw$bwt, bw$mheight)
```

    ## [1] 0.1921632

``` r
cor(bw$bwt, bw$momage)
```

    ## [1] 0.1357734

``` r
cor(bw$bwt, bw$ppbmi)
```

    ## [1] 0.09394573

``` r
cor(bw$bwt, bw$ppwt)
```

    ## [1] 0.182892

``` r
cor(bw$bwt, bw$wtgain)
```

    ## [1] 0.2472526

``` r
# From above we see that bhead, blength, delwet, gaweeks and wtgain all have correlation values >0.2, which may indicate a positive linear relationship with birthweight. While this might not mean much, it'll help me include it in a model 
```

Propose a regression model for birthweight. This model may be based on a hypothesized structure for the factors that underly birthweight, on a data-driven model-building process, or a combination of the two. Describe your modeling process and show a plot of model residuals against fitted values – use add\_predictions and add\_residuals in making this plot.

``` r
# Using my above 'analysis' and some hypothesized factors that I think influence birthweight, my proposed model is below. I'm afraid of adding too many predictors, and wary of add bhead and blength variables, as these are measurements taken post-birth and cannot be changed/influence. Additionally I didn't add any interaction terms to keep it a simple linear model with multiple predictors. I chose to include both mother and father's race, mother's weight at delivery, gestational age, weight gained during pregnancy and the smoking variable. 

eric_model = 
  bw %>% 
  lm(bwt ~ babysex + frace + mrace + delwt + gaweeks + wtgain + smoken, data = .)

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

I'm not sure what my scatterplot of the predicted values vs. residuals tells me and I'm pretty sure my model stinks.

``` r
cv_bwt = crossv_mc(bw, 100)

cv_bwt = cv_bwt %>% 
  mutate(my_model = map(train, ~lm(bwt ~ babysex + frace + mrace + delwt + gaweeks + wtgain + smoken, data = .x)),
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
  labs(title = "Violin plots of select model's RMSE",
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
| my         |    420.8317|
| jeff\_main |    333.4554|
| jeff\_int  |    290.1558|
