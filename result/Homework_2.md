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
             ), 1, # specific dim()
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

## load data

``` r
read_nyc = function(){
return_data = read_csv(
  paste(here::here(),"/data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",sep = '')
) %>% 
  janitor::clean_names()
return(return_data)
}
#skimr::skim_without_charts(nyc_df) # TODO: route* need to be in the same columns
```

## Clean data

``` r
nyc_df = 
  read_nyc() %>% 
  pivot_longer( #make unique names
    cols = c(line,station_name),
    names_to ='station_id_type',
    values_to = "station_id",
    names_prefix = "station_"
  ) %>% 
  relocate(station_id_type,station_id) %>% 
  separate( #clean staff hour, just experiment
    col = staff_hours,
    into = c("staff_hours_start",'staff_hours_end','staff_hours_shift'),
    sep = "[- ]"
  ) %>% 
  mutate(across(matches('route'),as.character)) %>% #OR use across(matches("something",fun))
  relocate(route1:route11,.after = last_col()) %>% 
  pivot_longer( # clean route* variables to meaningful route variable
    cols = route1:route11, #route 8 is numeric
    names_to = "route_served",
    values_to = 'route_value',
    names_prefix = 'route'
  ) %>% 
  mutate(entry = if_else(apply(as.matrix(entry),1,str_to_lower)=="yes",TRUE,FALSE,NA)) %>% 
  select(station_id_type:station_longitude,route_served,entry,vending,entrance_type,ada)
skimr::skim_without_charts(nyc_df)
```

|                                                  |         |
| :----------------------------------------------- | :------ |
| Name                                             | nyc\_df |
| Number of rows                                   | 41096   |
| Number of columns                                | 10      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 6       |
| logical                                          | 2       |
| numeric                                          | 2       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim\_variable    | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :---------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| station\_id\_type |          0 |              1 |   4 |   4 |     0 |         2 |          0 |
| station\_id       |          0 |              1 |   4 |  39 |     0 |       391 |          0 |
| division          |          0 |              1 |   3 |   3 |     0 |         3 |          0 |
| route\_served     |          0 |              1 |   1 |   2 |     0 |        11 |          0 |
| vending           |          0 |              1 |   2 |   3 |     0 |         2 |          0 |
| entrance\_type    |          0 |              1 |   4 |   9 |     0 |         7 |          0 |

**Variable type: logical**

| skim\_variable | n\_missing | complete\_rate | mean | count                  |
| :------------- | ---------: | -------------: | ---: | :--------------------- |
| entry          |          0 |              1 | 0.94 | TRU: 38566, FAL: 2530  |
| ada            |          0 |              1 | 0.25 | FAL: 30800, TRU: 10296 |

**Variable type: numeric**

| skim\_variable     | n\_missing | complete\_rate |    mean |   sd |      p0 |     p25 |     p50 |     p75 |    p100 |
| :----------------- | ---------: | -------------: | ------: | ---: | ------: | ------: | ------: | ------: | ------: |
| station\_latitude  |          0 |              1 |   40.73 | 0.07 |   40.58 |   40.69 |   40.73 |   40.77 |   40.90 |
| station\_longitude |          0 |              1 | \-73.94 | 0.06 | \-74.03 | \-73.99 | \-73.96 | \-73.91 | \-73.76 |

This NYC’s metro system data is a 1868 \(\times\) 32 size data,
describing NYC metro system’s division, line, station\_name,
station\_latitude, station\_longitude, route1, route2, route3, route4,
route5, route6, route7, route8, route9, route10, route11,
entrance\_type, entry, exit\_only, vending, staffing, staff\_hours, ada,
ada\_notes, free\_crossover, north\_south\_street, east\_west\_street,
corner, entrance\_latitude, entrance\_longitude, station\_location,
entrance\_location

Write a short paragraph about this dataset – explain briefly what
variables the dataset contains, describe your data cleaning steps so
far, and give the dimension (rows x columns) of the resulting dataset.
Are these data tidy?

Answer the following questions using these data:

How many distinct stations are there? Note that stations are identified
both by name and by line (e.g. 125th St A/B/C/D; 125st 1; 125st 4/5);
the distinct function may be useful here. How many stations are ADA
compliant? What proportion of station entrances / exits without vending
allow entrance?
