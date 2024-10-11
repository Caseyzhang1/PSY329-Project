My project
================
Casey Zhang
2024-10-10

# Load packages and dataset

``` r
library(haven) #to load datasets (.csv, .hav)
library(dplyr) #to recode variables
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2) #density plots, etc.
#when you make changes, press knit, commit to main - changes will be reflected in github desktop & online


load("/Users/caseyzhang/Desktop/ICPSR_36850/DS0001/36850-0001-Data.rda")
```

# Narrow Down Data

``` r
Project_data <- da36850.0001 %>%
  select(DEMGEN, GRIT01, GRIT02, GRIT03, GRIT04, GRIT05, GRIT06, GRIT07, GRIT08, TSCS01, TSCS02, TSCS03, TSCS04, TSCS05, TSCS06, TSCS07, TSCS08, TSCS09)
```
