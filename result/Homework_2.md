Homework 2
================
Jeffrey Liang
9/23/2020

# Problem 1

## Load Wheel data

``` r
data_collection = c() #Load the wheel data
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

preci_collection = c() # load the precipitation data in wheel
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
    !is.na(get("dumpster")) #Or drop_na(dumpster)
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
  ) %>%  # OR use month = month.names[month], or create a tabel with order and then left join
  relocate(year)
tail(preci_df)
```

    ## # A tibble: 6 x 3
    ##    year month     total
    ##   <dbl> <chr>     <dbl>
    ## 1  2017 July       7.09
    ## 2  2017 August     4.44
    ## 3  2017 September  1.95
    ## 4  2017 October    0   
    ## 5  2017 November   0.11
    ## 6  2017 December   0.94

## Write something about this data

# Problem 2

## load data

``` r
rm(list=ls())
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
  select(station_id_type:station_longitude,route_served,vending,entry,entrance_type,ada) %>% 
  mutate(route_served = as.factor(route_served),vending = if_else(vending=="YES",T,F,NA))
  skimr::skim_without_charts(nyc_df)
```

|                                                  |         |
| :----------------------------------------------- | :------ |
| Name                                             | nyc\_df |
| Number of rows                                   | 41096   |
| Number of columns                                | 10      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 4       |
| factor                                           | 1       |
| logical                                          | 3       |
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
| entrance\_type    |          0 |              1 |   4 |   9 |     0 |         7 |          0 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts                          |
| :------------- | ---------: | -------------: | :------ | --------: | :----------------------------------- |
| route\_served  |          0 |              1 | FALSE   |        11 | 1: 3736, 10: 3736, 11: 3736, 2: 3736 |

**Variable type: logical**

| skim\_variable | n\_missing | complete\_rate | mean | count                  |
| :------------- | ---------: | -------------: | ---: | :--------------------- |
| vending        |          0 |              1 | 0.90 | TRU: 37070, FAL: 4026  |
| entry          |          0 |              1 | 0.94 | TRU: 38566, FAL: 2530  |
| ada            |          0 |              1 | 0.25 | FAL: 30800, TRU: 10296 |

**Variable type: numeric**

| skim\_variable     | n\_missing | complete\_rate |    mean |   sd |      p0 |     p25 |     p50 |     p75 |    p100 |
| :----------------- | ---------: | -------------: | ------: | ---: | ------: | ------: | ------: | ------: | ------: |
| station\_latitude  |          0 |              1 |   40.73 | 0.07 |   40.58 |   40.69 |   40.73 |   40.77 |   40.90 |
| station\_longitude |          0 |              1 | \-73.94 | 0.06 | \-74.03 | \-73.99 | \-73.96 | \-73.91 | \-73.76 |

This NYC’s metro system data is a 1868 x 32 size data, describing NYC
metro system’s ***division, line, station\_name, station\_latitude,
station\_longitude, route1, route2, route3, route4, route5, route6,
route7, route8, route9, route10, route11, entrance\_type, entry,
exit\_only, vending, staffing, staff\_hours, ada, ada\_notes,
free\_crossover, north\_south\_street, east\_west\_street, corner,
entrance\_latitude, entrance\_longitude, station\_location,
entrance\_location***.

The data is not beautiful in all sense:

1.  It has route all over the place and name and line separated instead
    of treating as unique indentity;
2.  staff hours including 3 types of information and etc.

So the first step I did to the data is to make the name unique. Then I
took the information in the route\*: ffirst dealed with route 8 to 11
which not consistant with the other line format of data and put them
into route\_served variable using pivot\_longer(). Followed with
changing values in *entrance* variable to boolean factors with
if\_else() function. And finally selecting the required variables using
select().

The outcome data after piping is a dataset of 41096 x 10 size dataset,
with variables of ***station\_id\_type, station\_id, division,
station\_latitude, station\_longitude, route\_served, vending, entry,
entrance\_type, ada***, and the data with following properties:

  - there’s 391 unique station (including names and line) in this data;
  - Of 391, 63 are ADA compliant stations;
  - Of 391, 0.03 station entrance/exits without vending allow entrance.

# Problem 3

## Load data

``` r
rm(list=ls())
read_five = function(data_name_index = 1){
  data_name = str_c(c("pols-month",'unemployment',"snp"),".csv")
  read_data = 
    read_csv(paste(here::here(),"/data/fivethirtyeight_datasets/",data_name[[data_name_index]],sep=''),
             ) %>% 
    janitor::clean_names()
}

#load month data
pols_month = read_five(1) %>% 
  separate(mon,c("year","month",'day'),"-") %>% 
  mutate(across(.cols = year:day,as.integer)) %>% 
  left_join(tibble(month = 1:12,month_name = month.name)) %>% 
  mutate(month = month_name) %>% 
  select(-month_name) %>% 
  pivot_longer( # OR mutate president = 
    c("prez_gop","prez_dem"),
    names_to = "president",
    values_to = "president_boolean",
    names_prefix="prez_"
  ) %>% 
  filter(president_boolean == 1) %>% 
  select(-c(president_boolean,day)) %>% 
  mutate(president = factor(president,levels = c('dem',"gop")))
str(pols_month)
```

    ## tibble [817 × 9] (S3: tbl_df/tbl/data.frame)
    ##  $ year     : int [1:817] 1947 1947 1947 1947 1947 1947 1947 1947 1947 1947 ...
    ##  $ month    : chr [1:817] "January" "February" "March" "April" ...
    ##  $ gov_gop  : num [1:817] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_gop  : num [1:817] 51 51 51 51 51 51 51 51 51 51 ...
    ##  $ rep_gop  : num [1:817] 253 253 253 253 253 253 253 253 253 253 ...
    ##  $ gov_dem  : num [1:817] 23 23 23 23 23 23 23 23 23 23 ...
    ##  $ sen_dem  : num [1:817] 45 45 45 45 45 45 45 45 45 45 ...
    ##  $ rep_dem  : num [1:817] 198 198 198 198 198 198 198 198 198 198 ...
    ##  $ president: Factor w/ 2 levels "dem","gop": 1 1 1 1 1 1 1 1 1 1 ...

``` r
#load and clean snp data
snp_data = read_five(3) %>% 
  separate(col = date,into = c("month","day","year"),"/") %>% 
  select(-day) %>% 
  mutate(across(.cols = year:month,as.numeric),
         month = month.name[month]
         ) %>% 
  relocate(year,month)
str(snp_data)
```

    ## tibble [787 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ year : num [1:787] 2015 2015 2015 2015 2015 ...
    ##  $ month: chr [1:787] "July" "June" "May" "April" ...
    ##  $ close: num [1:787] 2080 2063 2107 2086 2068 ...

``` r
#load unemployment data
unemploy_data = read_five(2) %>% 
  pivot_longer(
    jan:dec,
    names_to = "month",
    values_to="unemploy_rate"
  ) %>% 
  mutate(month = apply(as.matrix(month),1,
                       function(x) month.name[[
                         match(x,str_to_lower(month.abb))
                         ]]))
str(unemploy_data)
```

    ## tibble [816 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ year         : num [1:816] 1948 1948 1948 1948 1948 ...
    ##  $ month        : chr [1:816] "January" "February" "March" "April" ...
    ##  $ unemploy_rate: num [1:816] 3.4 3.8 4 3.9 3.5 3.6 3.6 3.9 3.8 3.7 ...

## Join data

``` r
five30eight = pols_month %>% 
  left_join(snp_data, by = c("year","month")) %>% 
  left_join(unemploy_data, by = c("year","month"))
skimr::skim_without_charts(five30eight)
```

|                                                  |             |
| :----------------------------------------------- | :---------- |
| Name                                             | five30eight |
| Number of rows                                   | 817         |
| Number of columns                                | 11          |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |             |
| Column type frequency:                           |             |
| character                                        | 1           |
| factor                                           | 1           |
| numeric                                          | 9           |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |             |
| Group variables                                  | None        |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| month          |          0 |              1 |   3 |   9 |     0 |        12 |          0 |

**Variable type: factor**

| skim\_variable | n\_missing | complete\_rate | ordered | n\_unique | top\_counts        |
| :------------- | ---------: | -------------: | :------ | --------: | :----------------- |
| president      |          0 |              1 | FALSE   |         2 | gop: 427, dem: 390 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |      p0 |    p25 |     p50 |     p75 |    p100 |
| :------------- | ---------: | -------------: | ------: | -----: | ------: | -----: | ------: | ------: | ------: |
| year           |          0 |           1.00 | 1980.79 |  19.84 | 1947.00 | 1964.0 | 1981.00 | 1998.00 | 2015.00 |
| gov\_gop       |          0 |           1.00 |   22.50 |   5.69 |   12.00 |   18.0 |   22.00 |   28.00 |   34.00 |
| sen\_gop       |          0 |           1.00 |   46.10 |   6.40 |   32.00 |   42.0 |   46.00 |   51.00 |   56.00 |
| rep\_gop       |          0 |           1.00 |  194.92 |  29.33 |  141.00 |  176.0 |  195.00 |  222.00 |  253.00 |
| gov\_dem       |          0 |           1.00 |   27.16 |   5.93 |   17.00 |   22.0 |   28.00 |   32.00 |   41.00 |
| sen\_dem       |          0 |           1.00 |   54.38 |   7.38 |   44.00 |   48.0 |   53.00 |   58.00 |   71.00 |
| rep\_dem       |          0 |           1.00 |  244.94 |  31.46 |  188.00 |  211.0 |  250.00 |  268.00 |  301.00 |
| close          |         36 |           0.96 |  475.43 | 544.07 |   17.05 |   84.3 |  140.64 |  947.28 | 2107.39 |
| unemploy\_rate |         12 |           0.99 |    5.83 |   1.65 |    2.50 |    4.6 |    5.60 |    6.90 |   10.80 |
