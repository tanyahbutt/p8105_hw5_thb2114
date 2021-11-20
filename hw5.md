P8105 Homework 5 - Tanya Butt (thb2114)
================

## Problem 1

``` r
homicide_df = 
  read_csv("homicide-data.csv", na = c("","Unknown")) %>% 
  mutate(
    city_state = str_c(city, state),
    resolution = case_when(
      disposition == "Closed without arrest" ~ "unsolved",
      disposition == "Open/No arrest" ~ "unsolved",
      disposition == "Closed by arrest" ~ "solved"
    )) %>% 
    relocate(city_state) %>% 
  filter(city_state != "TulsaAL")
```

    ## Rows: 52179 Columns: 12

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
    ## dbl (4): reported_date, victim_age, lat, lon

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Let’s focus on Baltimore, MD.

``` r
baltimore_df =
  homicide_df %>% 
  filter(city_state == "BaltimoreMD")

baltimore_summary =
  baltimore_df %>% 
  summarize(
    unsolved = sum(resolution == "unsolved"),
    n = n()
  )

baltimore_test = 
  prop.test(
    x = baltimore_summary %>%  pull(unsolved), 
    n = baltimore_summary %>%  pull(n))

baltimore_test %>% 
  broom::tidy()
## # A tibble: 1 × 8
##   estimate statistic  p.value parameter conf.low conf.high method    alternative
##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample… two.sided
```

Let’s try to iterate across cities!

First off, write a function.

``` r
prop_test_function = function(city_df) {
 
   city_summary =
     city_df %>% 
  summarize(
    unsolved = sum(resolution == "unsolved"),
    n = n()
  )

  city_test = 
    prop.test(
      x = city_summary %>%  pull(unsolved), 
      n = city_summary %>%  pull(n))

return(city_test)

}

prop_test_function(baltimore_df)
## 
##  1-sample proportions test with continuity correction
## 
## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
## X-squared = 239.01, df = 1, p-value < 2.2e-16
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.6275625 0.6631599
## sample estimates:
##         p 
## 0.6455607

homicide_df %>% 
  filter(city_state == "AlbuquerqueNM") %>% 
  prop_test_function()
## 
##  1-sample proportions test with continuity correction
## 
## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
## X-squared = 19.114, df = 1, p-value = 1.232e-05
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.3372604 0.4375766
## sample estimates:
##         p 
## 0.3862434
```

Now, let’s iterate across all cities.

``` r
results_df = 
  homicide_df %>% 
  nest(data = uid:resolution) %>% 
  mutate(
    test_results = map(data, prop_test_function),
    tidy_results = map(test_results, broom::tidy)
  ) %>% 
  select(city_state, tidy_results) %>% 
  unnest(tidy_results) %>% 
  select(city_state, estimate, starts_with("conf"))
```

Try to make a plot showing estimates and confidence intervals.

``` r
results_df %>% 
  mutate(city_state = fct_reorder(city_state, estimate)) %>% 
  ggplot(aes(x = city_state, y = estimate )) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="hw5_files/figure-gfm/unnamed-chunk-6-1.png" width="90%" />

## Problem 2
