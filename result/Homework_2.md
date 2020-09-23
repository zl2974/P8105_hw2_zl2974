Homework 2
================
Jeffrey Liang
9/23/2020

# Problem 1

## Load Wheel data

``` r
data_collection = c()
for (i in c(1:3)){
  data_collection[[i]] = 
    readxl::read_excel(
      paste(here::here(),"/data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sep=''),
      skip = 1,
      sheet = i
    ) %>% 
    janitor::clean_names() %>% 
    select( -starts_with('X1'))
}

wheel_df = bind_rows(data_collection)

preci_collection = c()
for (i in c(4:9)){
  preci_collection[[i]] = 
    readxl::read_excel(
      paste(here::here(),"/data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sep=''),
      skip = 1,
      sheet = i
    ) %>% 
    janitor::clean_names() %>% 
    mutate(year = 2020 + 3 -i)
}

preci_df = bind_rows(preci_collection)

rm(data_collection,preci_collection)
```

## Clean Wheel data

``` r
wheel_df = #clean wheel
  wheel_df %>% 
  filter(
    #!grep(" Total",get('month')), #this command return vector with null row, so the data can't subset
    !str_detect(get("month")," Total"),
    !is.na(get("dumpster"))
    ) %>% 
  mutate(sports_balls = 
           as.integer(round(get("sports_balls")))
         )
tail(wheel_df)
```

    ## # A tibble: 6 x 15
    ##   dumpster month  year date                weight_tons volume_cubic_ya…
    ##      <dbl> <chr> <dbl> <dttm>                    <dbl>            <dbl>
    ## 1        4 Augu…  2018 2018-08-08 00:00:00        1.11               10
    ## 2        5 Octo…  2018 2018-10-15 00:00:00        1.44               10
    ## 3        6 Dece…  2018 2018-12-14 00:00:00        0.66                8
    ## 4        7 March  2019 2019-03-07 00:00:00        0.98                8
    ## 5        8 April  2019 2019-04-24 00:00:00        1.8                10
    ## 6        9 May    2019 2019-05-31 00:00:00        2.17               10
    ## # … with 9 more variables: plastic_bottles <dbl>, polystyrene <dbl>,
    ## #   cigarette_butts <dbl>, glass_bottles <dbl>, grocery_bags <dbl>,
    ## #   chip_bags <dbl>, sports_balls <int>, homes_powered <dbl>,
    ## #   plastic_bags <dbl>

## Clean precipitation data

``` r
preci_df = 
  preci_df %>%
  filter(!is.na(get("month")),
         get('year') %in% c(2018,2017)
         ) %>% 
  mutate(month =
           apply( # this is the apply() function in R, need to specific dim before giving function, weird
             as.matrix(           # as.matrix is very important here, cuz pull() or get() have NULL dim()
             get("month")
             ), 1,
             function(x) month.name[[x]]) # this is how to write lambda in R, clean
  )
tail(preci_df)
```

    ## # A tibble: 6 x 3
    ##   month     total  year
    ##   <chr>     <dbl> <dbl>
    ## 1 July       7.09  2017
    ## 2 August     4.44  2017
    ## 3 September  1.95  2017
    ## 4 October    0     2017
    ## 5 November   0.11  2017
    ## 6 December   0.94  2017

## Write something about this data

# Problem 2
