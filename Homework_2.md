Homework 2
================
Jeffrey Liang
9/23/2020

# Problem 1

## Load Wheel data

``` r
data_collection = c() #Load the wheel data
for (i in c(1:3)) {
  data_collection[[i]] = 
    readxl::read_excel(
      paste(here::here(),"/data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sep = ''),
      skip = 1, 
      sheet = i
    ) %>% 
    janitor::clean_names() %>% 
    select( -starts_with('X1'))
}

wheel_df = bind_rows(data_collection)

preci_collection = c() # load the precipitation data in wheel
for (i in c(4:9)) {
  preci_collection[[i]] = 
    readxl::read_excel(
      paste(here::here(),"/data/Trash-Wheel-Collection-Totals-8-6-19.xlsx",sep = ''),
      skip = 1,
      sheet = i
    ) %>% 
    janitor::clean_names() %>% 
    mutate(year = 2020 + 3 - i)
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
           apply( # this is the apply() function in R, need to specific dim before
                  # giving function, weird
             as.matrix(           # as.matrix is very important here, cuz 
                                  # pull() or get() have NULL dim() because they are vector
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

## Load data

``` r
rm(list=ls())
read_nyc = function(){
return_data = read_csv(
  paste(here::here(),"/data/NYC_Transit_Subway_Entrance_And_Exit_Data.csv",sep = '')
) %>% 
  janitor::clean_names()
return(return_data)
}
```

## Clean data

``` r
nyc_df = 
  read_nyc() %>% 
  select(line:station_longitude,route1:route11,vending,entry,entrance_type,ada) %>% 
  mutate(entry = if_else(
    apply(as.matrix(entry),1,str_to_lower)=="yes",
    TRUE,FALSE,NA),
    vending = if_else(vending=="YES",T,F,NA),
    )

nyc_df_tidy = nyc_df %>% 
  mutate(across(matches('route'),as.character)
         ) %>% #OR use across(matches("something",fun))
  relocate(route1:route11,.after = last_col()) %>% 
  pivot_longer( # clean route* variables to meaningful route variable
    cols = route1:route11, #route 8 is numeric
    names_to = "route_name",
    values_to = 'route_number',
    names_prefix = 'route'
  ) %>% 
  drop_na(route_number)
tail(nyc_df)
```

    ## # A tibble: 6 x 19
    ##   line  station_name station_latitude station_longitu… route1 route2 route3
    ##   <chr> <chr>                   <dbl>            <dbl> <chr>  <chr>  <chr> 
    ## 1 Whit… Simpson St               40.8            -73.9 2      5      <NA>  
    ## 2 Whit… Wakefield-2…             40.9            -73.9 2      5      <NA>  
    ## 3 Whit… Wakefield-2…             40.9            -73.9 2      5      <NA>  
    ## 4 Whit… Wakefield-2…             40.9            -73.9 2      5      <NA>  
    ## 5 Flus… 34 St Hudso…             40.8            -74.0 7      <NA>   <NA>  
    ## 6 Flus… 34 St Hudso…             40.8            -74.0 7      <NA>   <NA>  
    ## # … with 12 more variables: route4 <chr>, route5 <chr>, route6 <chr>,
    ## #   route7 <chr>, route8 <dbl>, route9 <dbl>, route10 <dbl>, route11 <dbl>,
    ## #   vending <lgl>, entry <lgl>, entrance_type <chr>, ada <lgl>

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
2.  value types in route\* were not consistent;
3.  Boolean variable in *vending* and *entry* were in form of “YES”/“NO”
    instead of boolean.

 So the first step I did to the data was to select required variables.
Followed with changing values in *entrance* variable to boolean factors
with if\_else() function. Then I took the information in the route\*:
first dealed with route 8 to 11 which not consistent with the other line
format of data and put them into *route\_name* and *route\_number*
variables using pivot\_longer(). To seperate

 The outcome data after piping is a dataset of 1868 x 19 size dataset,
with variables of ***line, station\_name, station\_latitude,
station\_longitude, route1, route2, route3, route4, route5, route6,
route7, route8, route9, route10, route11, vending, entry,
entrance\_type, ada***, and the data with following properties:

  - there’s 465 unique station (including names and line) in this data;
  - Of 465 total stations, 84 are ADA compliant stations;
  - Of 183 stations without vending, 37.7% stations’ entrance/exits
    without vending allow entrance.

 After transforming the NYC transit data, of 465 stations, 60 serve
route A. Among these stations, 17 are ADA compliant.

# Problem 3

## Load data

### Pols Month Data

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
    names_prefix = "prez_"
  ) %>% 
  filter(president_boolean != 0) %>% 
  select(-c(president_boolean,day)) %>% 
  mutate(president = factor(president,levels = c('dem',"gop")))
tail(pols_month)
```

    ## # A tibble: 6 x 9
    ##    year month    gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##   <int> <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <fct>    
    ## 1  2015 January       31      54     245      18      44     188 dem      
    ## 2  2015 February      31      54     245      18      44     188 dem      
    ## 3  2015 March         31      54     245      18      44     188 dem      
    ## 4  2015 April         31      54     244      18      44     188 dem      
    ## 5  2015 May           31      54     245      18      44     188 dem      
    ## 6  2015 June          31      54     246      18      44     188 dem

First I loaded the Pols Month Data, the original data contains *mon,
prez\_gop, gov\_gop, sen\_gop, rep\_gop, prez\_dem, gov\_dem, sen\_dem,
rep\_dem* columns, in which *prez\_dem, prez\_gov* represent an
indicator columns of which parties’ candidates were in charge of White
House. The columns *gov\_dem,sen\_dem,rep\_dem* represent the number of
governors, senators and representatives of Democrat Party and the others
represent those of GOP. The data was record in the date of count on
*date*. The data is in 822 x 9 dimension.

 To make this data looks/works more tidy, I did the following: first I
separated *date* into *year,month,day* ,changing the *month* value into
full name instead of number and keep the first two columns only. Second,
*prez\_dem, prez\_gop* columns were melted together, keeping the *dem,
gop* information in column names, their values to an indicator columns.
And use filter to the indicator columns to selected the informative
rows( that is president’s party of the record date). And the final data
is in 822 x 9 dimension.

### SNP dataset

``` r
snp_data = read_five(3) %>% 
  mutate(year = year(mdy(date)),
         month = as.character(
           month(mdy(date),label = TRUE, abbr=FALSE)
           )
         ) %>% #try to use some lubridate here
  select(-date) %>% 
  relocate(year,month)
tail(snp_data)
```

    ## # A tibble: 6 x 3
    ##    year month    close
    ##   <dbl> <chr>    <dbl>
    ## 1  1950 June      17.7
    ## 2  1950 May       18.8
    ## 3  1950 April     18.0
    ## 4  1950 March     17.3
    ## 5  1950 February  17.2
    ## 6  1950 January   17.0

Then the SNP dataset is loaded, same workflow was process: separating
*date* in to *month,day,year*, translate the numeric *month* variable to
character full name and drop *day* as required. After workflow, a 787 x
3 dimension data is produced. The *close* variable record the closing
values of the S\&P stock index on the associated date.

### Unemployment dataset

``` r
unemploy_data = read_five(2) %>% 
  pivot_longer(
    jan:dec,
    names_to = "month",
    values_to="unemploy_rate"
  ) %>% 
  mutate(month = apply(as.matrix(month),1,
                       function(x) month.name[[
                         match(x,str_to_lower(month.abb))
                         ]])) #i can build a tibble of jan:dec mapping month.name to solve
head(unemploy_data,10)
```

    ## # A tibble: 10 x 3
    ##     year month     unemploy_rate
    ##    <dbl> <chr>             <dbl>
    ##  1  1948 January             3.4
    ##  2  1948 February            3.8
    ##  3  1948 March               4  
    ##  4  1948 April               3.9
    ##  5  1948 May                 3.5
    ##  6  1948 June                3.6
    ##  7  1948 July                3.6
    ##  8  1948 August              3.9
    ##  9  1948 September           3.8
    ## 10  1948 October             3.7

Finally the Unemployment dataset was taken and similar workflow
implemented. First columns of *jan, feb, mar, apr, may, jun, jul, aug,
sep, oct, nov, dec*, containing information of unemployment rate in the
month, were melted into variable *month* and *unemploy\_rate*. Then the
abbreviated month name was translated into full name for consistence. An
816 \* 3 is produced.

## Join data

``` r
five30eight = pols_month %>% 
  left_join(snp_data, by = c("year","month")) %>% 
  left_join(unemploy_data, by = c("year","month"))
```

Joining all 3 datasets, a final 822 x 11 dataset is produced. Data is
collected through 1947, 2015. The data collect number of senator,
representative, governor of 2 parties and the party of presidency on
date of data record as well as the closing values of the S\&P stock
index on the associated date and the unemployment rate. These
information is record in columns of *year, month, gov\_gop, sen\_gop,
rep\_gop, gov\_dem, sen\_dem, rep\_dem, president, close,
unemploy\_rate*. Detailed data descriptive is provided as followed.

|                                                  |             |
| :----------------------------------------------- | :---------- |
| Name                                             | five30eight |
| Number of rows                                   | 822         |
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
| president      |          0 |              1 | FALSE   |         2 | gop: 432, dem: 390 |

**Variable type: numeric**

| skim\_variable | n\_missing | complete\_rate |    mean |     sd |      p0 |     p25 |     p50 |     p75 |    p100 |
| :------------- | ---------: | -------------: | ------: | -----: | ------: | ------: | ------: | ------: | ------: |
| year           |          0 |           1.00 | 1980.75 |  19.79 | 1947.00 | 1964.00 | 1981.00 | 1998.00 | 2015.00 |
| gov\_gop       |          0 |           1.00 |   22.48 |   5.68 |   12.00 |   18.00 |   22.00 |   28.00 |   34.00 |
| sen\_gop       |          0 |           1.00 |   46.10 |   6.38 |   32.00 |   42.00 |   46.00 |   51.00 |   56.00 |
| rep\_gop       |          0 |           1.00 |  194.92 |  29.24 |  141.00 |  176.00 |  195.00 |  222.00 |  253.00 |
| gov\_dem       |          0 |           1.00 |   27.20 |   5.94 |   17.00 |   22.00 |   28.00 |   32.00 |   41.00 |
| sen\_dem       |          0 |           1.00 |   54.41 |   7.37 |   44.00 |   48.00 |   53.00 |   58.00 |   71.00 |
| rep\_dem       |          0 |           1.00 |  244.97 |  31.37 |  188.00 |  211.00 |  250.00 |  268.00 |  301.00 |
| close          |         36 |           0.96 |  472.85 | 543.29 |   17.05 |   83.67 |  137.26 |  932.06 | 2107.39 |
| unemploy\_rate |         12 |           0.99 |    5.83 |   1.65 |    2.50 |    4.70 |    5.60 |    6.90 |   10.80 |
