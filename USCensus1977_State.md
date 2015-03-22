# US Census 1977: Life.Exp <regression/classification>
bdanalytics  

**  **    
**Date: (Sun) Mar 22, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/statedata.csv  
    New:        <prdct_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(plyr))
suppressPackageStartupMessages(require(reshape2))

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_is_separate_predict_dataset <- FALSE    # or TRUE
glb_predct_var <- "Life.Exp"           # or NULL
glb_predct_var_name <- paste0(glb_predct_var, ".predict")
glb_id_vars <- c("state.abb")                # or NULL

glb_exclude_vars_as_features <- glb_id_vars     # or NULL                      
# List chrs converted into factors; num/int transformed  
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                c("state.region", "state.name", "state.division", "state.area",
                  "x", "y")     # or NULL
                                      )
# List feats that shd be excluded due to known causation by prediction variable
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("<col_name>")     # or NULL
#                                       )

glb_is_regression <- TRUE; glb_is_classification <- FALSE

glb_mdl <- glb_sel_mdl <- NULL
glb_models_df <- data.frame()

script_df <- data.frame(chunk_label="import_data", chunk_step_major=1, chunk_step_minor=0)
print(script_df)
```

```
##   chunk_label chunk_step_major chunk_step_minor
## 1 import_data                1                0
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(
    url="https://courses.edx.org/c4x/MITx/15.071x_2/asset/statedata.csv", 
    comment="glb_entity_df", force_header=TRUE, print_diagn=TRUE)
```

```
## [1] "Reading file ./data/statedata.csv..."
## [1] "dimensions of data in ./data/statedata.csv: 50 rows x 15 cols"
##   Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 1       3615   3624        2.1    69.05   15.1    41.3    20  50708
## 2        365   6315        1.5    69.31   11.3    66.7   152 566432
## 3       2212   4530        1.8    70.55    7.8    58.1    15 113417
## 4       2110   3378        1.9    70.66   10.1    39.9    65  51945
## 5      21198   5114        1.1    71.71   10.3    62.6    20 156361
## 6       2541   4884        0.7    72.06    6.8    63.9   166 103766
##   state.abb state.area         x       y     state.division state.name
## 1        AL      51609  -86.7509 32.5901 East South Central    Alabama
## 2        AK     589757 -127.2500 49.2500            Pacific     Alaska
## 3        AZ     113909 -111.6250 34.2192           Mountain    Arizona
## 4        AR      53104  -92.2992 34.7336 West South Central   Arkansas
## 5        CA     158693 -119.7730 36.5341            Pacific California
## 6        CO     104247 -105.5130 38.6777           Mountain   Colorado
##   state.region
## 1        South
## 2         West
## 3         West
## 4        South
## 5         West
## 6         West
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 21       5814   4755        1.1    71.83    3.3    58.5   103   7826
## 37       2284   4660        0.6    72.13    4.2    60.0    44  96184
## 42       4173   3821        1.7    70.11   11.0    41.8    70  41328
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 50        376   4566        0.6    70.29    6.9    62.9   173  97203
##    state.abb state.area         x       y     state.division    state.name
## 8         DE       2057  -74.9841 38.6777     South Atlantic      Delaware
## 21        MA       8257  -71.5800 42.3645        New England Massachusetts
## 37        OR      96981 -120.0680 43.9078            Pacific        Oregon
## 42        TN      42244  -86.4560 35.6767 East South Central     Tennessee
## 43        TX     267339  -98.7857 31.3897 West South Central         Texas
## 50        WY      97914 -107.2560 43.0504           Mountain       Wyoming
##    state.region
## 8         South
## 21    Northeast
## 37         West
## 42        South
## 43        South
## 50         West
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 45        472   3907        0.6    71.64    5.5    57.1   168  9267
## 46       4981   4701        1.4    70.08    9.5    47.8    85 39780
## 47       3559   4864        0.6    71.72    4.3    63.5    32 66570
## 48       1799   3617        1.4    69.48    6.7    41.6   100 24070
## 49       4589   4468        0.7    72.48    3.0    54.5   149 54464
## 50        376   4566        0.6    70.29    6.9    62.9   173 97203
##    state.abb state.area         x       y     state.division    state.name
## 45        VT       9609  -72.5450 44.2508        New England       Vermont
## 46        VA      40815  -78.2005 37.5630     South Atlantic      Virginia
## 47        WA      68192 -119.7460 47.4231            Pacific    Washington
## 48        WV      24181  -80.6665 38.4204     South Atlantic West Virginia
## 49        WI      56154  -89.9941 44.5937 East North Central     Wisconsin
## 50        WY      97914 -107.2560 43.0504           Mountain       Wyoming
##     state.region
## 45     Northeast
## 46         South
## 47          West
## 48         South
## 49 North Central
## 50          West
## 'data.frame':	50 obs. of  15 variables:
##  $ Population    : int  3615 365 2212 2110 21198 2541 3100 579 8277 4931 ...
##  $ Income        : int  3624 6315 4530 3378 5114 4884 5348 4809 4815 4091 ...
##  $ Illiteracy    : num  2.1 1.5 1.8 1.9 1.1 0.7 1.1 0.9 1.3 2 ...
##  $ Life.Exp      : num  69 69.3 70.5 70.7 71.7 ...
##  $ Murder        : num  15.1 11.3 7.8 10.1 10.3 6.8 3.1 6.2 10.7 13.9 ...
##  $ HS.Grad       : num  41.3 66.7 58.1 39.9 62.6 63.9 56 54.6 52.6 40.6 ...
##  $ Frost         : int  20 152 15 65 20 166 139 103 11 60 ...
##  $ Area          : int  50708 566432 113417 51945 156361 103766 4862 1982 54090 58073 ...
##  $ state.abb     : chr  "AL" "AK" "AZ" "AR" ...
##  $ state.area    : int  51609 589757 113909 53104 158693 104247 5009 2057 58560 58876 ...
##  $ x             : num  -86.8 -127.2 -111.6 -92.3 -119.8 ...
##  $ y             : num  32.6 49.2 34.2 34.7 36.5 ...
##  $ state.division: chr  "East South Central" "Pacific" "Mountain" "West South Central" ...
##  $ state.name    : chr  "Alabama" "Alaska" "Arizona" "Arkansas" ...
##  $ state.region  : chr  "South" "West" "West" "South" ...
##  - attr(*, "comment")= chr "glb_entity_df"
## NULL
```

```r
if (glb_is_separate_predict_dataset) {
    glb_predct_df <- myimport_data(
        url="<prdct_url>", 
        comment="glb_predct_df", force_header=TRUE, print_diagn=TRUE)
} else {
#     glb_predct_df <- subset(glb_entity_df, <condition>)
    glb_predct_df <- glb_entity_df[sample(1:nrow(glb_entity_df),
                                          max(2, nrow(glb_entity_df) / 1000)),]    
    comment(glb_predct_df) <- "glb_predct_df"
    myprint_df(glb_predct_df)
    str(glb_predct_df)
}         
```

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 17       3387   3712        1.6    70.10   10.6    38.5    95 39650
## 25       4767   4254        0.8    70.69    9.3    48.8   108 68995
##    state.abb state.area        x       y     state.division state.name
## 17        KY      40395 -84.7674 37.3915 East South Central   Kentucky
## 25        MO      69686 -92.5137 38.3347 West North Central   Missouri
##     state.region
## 17         South
## 25 North Central
## 'data.frame':	2 obs. of  15 variables:
##  $ Population    : int  3387 4767
##  $ Income        : int  3712 4254
##  $ Illiteracy    : num  1.6 0.8
##  $ Life.Exp      : num  70.1 70.7
##  $ Murder        : num  10.6 9.3
##  $ HS.Grad       : num  38.5 48.8
##  $ Frost         : int  95 108
##  $ Area          : int  39650 68995
##  $ state.abb     : chr  "KY" "MO"
##  $ state.area    : int  40395 69686
##  $ x             : num  -84.8 -92.5
##  $ y             : num  37.4 38.3
##  $ state.division: chr  "East South Central" "West North Central"
##  $ state.name    : chr  "Kentucky" "Missouri"
##  $ state.region  : chr  "South" "North Central"
##  - attr(*, "comment")= chr "glb_predct_df"
```

```r
# glb_entity_df <- subset(glb_entity_df, !<condition>)
# print(dim(glb_entity_df))

script_df <- rbind(script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##    chunk_label chunk_step_major chunk_step_minor
## 1  import_data                1                0
## 2 cleanse_data                2                0
```

## Step `2`: cleanse data

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="inspect_explore_data", 
                              chunk_step_major=max(script_df$chunk_step_major), 
                              chunk_step_minor=1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_entity_df))
#View(glb_entity_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>
# Population - the population estimate of the state in 1975
# Income - per capita income in 1974
# Illiteracy - illiteracy rates in 1970, as a percent of the population
# Life.Exp - the life expectancy in years of residents of the state in 1970
# Murder - the murder and non-negligent manslaughter rate per 100,000 population in 1976 
# HS.Grad - percent of high-school graduates in 1970
# Frost - the mean number of days with minimum temperature below freezing from 1931–1960 in the capital or a large city of the state
# Area - the land area (in square miles) of the state
# state.abb - a 2-letter abreviation for each state
# state.area - the area of each state, in square miles
# x - the longitude of the center of the state
# y - the latitude of the center of the state
# state.division - the division each state belongs to (New England, Middle Atlantic, South Atlantic, East South Central, West South Central, East North Central, West North Central, Mountain, or Pacific)
# state.name - the full names of each state
# state.region - the region each state belong to (Northeast, South, North Central, or West)

# Create new features that help diagnostics
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, obs_twin_df) {
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>)

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))) 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<max_n_val>") 

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>)        
                        )

    # If levels of a factor are different across obs_df & glb_predct_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

# glb_entity_df <- add_new_diag_feats(glb_entity_df, glb_predct_df)
# glb_predct_df <- add_new_diag_feats(glb_predct_df, glb_entity_df)

#pairs(subset(glb_entity_df, select=-c(col_symbol)))

#   Histogram of predictor in glb_entity_df & glb_predct_df
# Check for glb_predct_df & glb_entity_df features range mismatches

# Other diagnostics:
# print(subset(glb_entity_df, <col1_name> == max(glb_entity_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_entity_df$<col1_name>, na.rm=TRUE)))

# print(glb_entity_df[which.max(glb_entity_df$<col_name>),])

# print(<col_name>_freq_glb_entity_df <- mycreate_tbl_df(glb_entity_df, "<col_name>"))
# print(which.min(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col_name>)))
# print(which.max(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>)[, 2]))
# print(table(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>))
# print(table(is.na(glb_entity_df$<col1_name>), glb_entity_df$<col2_name>))
# print(xtabs(~ <col1_name>, glb_entity_df))
# print(xtabs(~ <col1_name> + <col2_name>, glb_entity_df))
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mycreate_xtab(glb_entity_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_entity_df[is.na(<col1_name>_<col2_name>_xtab_glb_entity_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_entity_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_entity_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_entity_df$<col1_name>, glb_entity_df$<col2_name>, min, na.rm=TRUE)))
print(HS.Grad_by_state.region_arr <- 
   sort(tapply(glb_entity_df$HS.Grad, glb_entity_df$state.region, mean, na.rm=TRUE)))
```

```
##         South     Northeast North Central          West 
##      44.34375      53.96667      54.51667      62.00000
```

```r
# Other plots:
print(myplot_histogram(glb_entity_df, glb_predct_var))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](USCensus1977_State_files/figure-html/inspect_explore_data_1-1.png) 

```r
# print(myplot_box(df=glb_entity_df, ycol_names="<col1_name>"))
print(myplot_box(df=glb_entity_df, ycol_names="Murder", xcol_name="state.region"))
```

```
## Warning in myplot_box(df = glb_entity_df, ycol_names = "Murder", xcol_name
## = "state.region"): xcol_name:state.region is not a factor; creating
## state.region_fctr
```

![](USCensus1977_State_files/figure-html/inspect_explore_data_1-2.png) 

```r
# print(myplot_line(subset(glb_entity_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))
print(myplot_scatter(glb_entity_df, "x", "y"))
```

![](USCensus1977_State_files/figure-html/inspect_explore_data_1-3.png) 

```r
print(myplot_scatter(glb_entity_df, "Income", "Life.Exp", smooth=TRUE))
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](USCensus1977_State_files/figure-html/inspect_explore_data_1-4.png) 

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))
# glb_entity_df <- na.omit(glb_entity_df)
# glb_predct_df <- na.omit(glb_predct_df)

script_df <- rbind(script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(script_df$chunk_step_major), 
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_entity_df <- mymap_codes(glb_entity_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_predct_df <- mymap_codes(glb_predct_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_entity_df$<col_name>.fctr <- factor(glb_entity_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))
# glb_predct_df$<col_name>.fctr <- factor(glb_predct_df$<col_name>, 
#                     as.factor(union(glb_entity_df$<col_name>, glb_predct_df$<col_name>)))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_entity_df$<col_name>), -2, na.pad=TRUE)
# glb_entity_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_predct_df$<col_name>), -2, na.pad=TRUE)
# glb_predct_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_predct_df[1, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df) - 1, 
#                                                    "<col_name>"]
# glb_predct_df[2, "<col_name>.lag.2"] <- glb_entity_df[nrow(glb_entity_df), 
#                                                    "<col_name>"]
                                                   
# glb_entity_df <- mutate(glb_entity_df,
#     <new_col_name>=
#                     )

# glb_predct_df <- mutate(glb_predct_df,
#     <new_col_name>=
#                     )

# print(summary(glb_entity_df))
# print(summary(glb_predct_df))

# print(sapply(names(glb_entity_df), function(col) sum(is.na(glb_entity_df[, col]))))
# print(sapply(names(glb_predct_df), function(col) sum(is.na(glb_predct_df[, col]))))

# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", smooth=TRUE))

script_df <- rbind(script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##            chunk_label chunk_step_major chunk_step_minor
## 1          import_data                1                0
## 2         cleanse_data                2                0
## 3 inspect_explore_data                2                1
## 4  manage_missing_data                2                2
## 5   encode_retype_data                2                3
## 6     extract_features                3                0
## 7      select_features                4                0
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features())
```

```
##                    id       cor.y  cor.y.abs
## Murder         Murder -0.78084575 0.78084575
## Illiteracy Illiteracy -0.58847793 0.58847793
## HS.Grad       HS.Grad  0.58221620 0.58221620
## Income         Income  0.34025534 0.34025534
## Frost           Frost  0.26206801 0.26206801
## Area             Area -0.10733194 0.10733194
## Population Population -0.06805195 0.06805195
```

```r
script_df <- rbind(script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(script_df$chunk_step_major),
        chunk_step_minor=script_df[nrow(script_df), "chunk_step_minor"]+1))        
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
                    merge(glb_feats_df, mydelete_cor_features(), all.x=TRUE)))
```

```
##                Murder  Illiteracy     HS.Grad     Income      Frost
## Murder      1.0000000  0.70297520 -0.48797102 -0.2300776 -0.5388834
## Illiteracy  0.7029752  1.00000000 -0.65718861 -0.4370752 -0.6719470
## HS.Grad    -0.4879710 -0.65718861  1.00000000  0.6199323  0.3667797
## Income     -0.2300776 -0.43707519  0.61993232  1.0000000  0.2262822
## Frost      -0.5388834 -0.67194697  0.36677970  0.2262822  1.0000000
## Area        0.2283902  0.07726113  0.33354187  0.3633154  0.0592291
## Population  0.3436428  0.10762237 -0.09848975  0.2082276 -0.3321525
##                  Area  Population
## Murder     0.22839021  0.34364275
## Illiteracy 0.07726113  0.10762237
## HS.Grad    0.33354187 -0.09848975
## Income     0.36331544  0.20822756
## Frost      0.05922910 -0.33215245
## Area       1.00000000  0.02254384
## Population 0.02254384  1.00000000
##               Murder Illiteracy    HS.Grad    Income     Frost       Area
## Murder     0.0000000 0.70297520 0.48797102 0.2300776 0.5388834 0.22839021
## Illiteracy 0.7029752 0.00000000 0.65718861 0.4370752 0.6719470 0.07726113
## HS.Grad    0.4879710 0.65718861 0.00000000 0.6199323 0.3667797 0.33354187
## Income     0.2300776 0.43707519 0.61993232 0.0000000 0.2262822 0.36331544
## Frost      0.5388834 0.67194697 0.36677970 0.2262822 0.0000000 0.05922910
## Area       0.2283902 0.07726113 0.33354187 0.3633154 0.0592291 0.00000000
## Population 0.3436428 0.10762237 0.09848975 0.2082276 0.3321525 0.02254384
##            Population
## Murder     0.34364275
## Illiteracy 0.10762237
## HS.Grad    0.09848975
## Income     0.20822756
## Frost      0.33215245
## Area       0.02254384
## Population 0.00000000
## [1] "cor(Murder, Illiteracy)=0.7030"
```

![](USCensus1977_State_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(Life.Exp, Murder)=-0.7808"
## [1] "cor(Life.Exp, Illiteracy)=-0.5885"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in mydelete_cor_features(): Dropping Illiteracy as a feature
```

![](USCensus1977_State_files/figure-html/remove_correlated_features-2.png) 

```
##                    id       cor.y  cor.y.abs
## Murder         Murder -0.78084575 0.78084575
## HS.Grad       HS.Grad  0.58221620 0.58221620
## Income         Income  0.34025534 0.34025534
## Frost           Frost  0.26206801 0.26206801
## Area             Area -0.10733194 0.10733194
## Population Population -0.06805195 0.06805195
##                Murder     HS.Grad     Income      Frost       Area
## Murder      1.0000000 -0.48797102 -0.2300776 -0.5388834 0.22839021
## HS.Grad    -0.4879710  1.00000000  0.6199323  0.3667797 0.33354187
## Income     -0.2300776  0.61993232  1.0000000  0.2262822 0.36331544
## Frost      -0.5388834  0.36677970  0.2262822  1.0000000 0.05922910
## Area        0.2283902  0.33354187  0.3633154  0.0592291 1.00000000
## Population  0.3436428 -0.09848975  0.2082276 -0.3321525 0.02254384
##             Population
## Murder      0.34364275
## HS.Grad    -0.09848975
## Income      0.20822756
## Frost      -0.33215245
## Area        0.02254384
## Population  1.00000000
##               Murder    HS.Grad    Income     Frost       Area Population
## Murder     0.0000000 0.48797102 0.2300776 0.5388834 0.22839021 0.34364275
## HS.Grad    0.4879710 0.00000000 0.6199323 0.3667797 0.33354187 0.09848975
## Income     0.2300776 0.61993232 0.0000000 0.2262822 0.36331544 0.20822756
## Frost      0.5388834 0.36677970 0.2262822 0.0000000 0.05922910 0.33215245
## Area       0.2283902 0.33354187 0.3633154 0.0592291 0.00000000 0.02254384
## Population 0.3436428 0.09848975 0.2082276 0.3321525 0.02254384 0.00000000
##           id       cor.y  cor.y.abs cor.low
## 3    HS.Grad  0.58221620 0.58221620       1
## 5     Income  0.34025534 0.34025534       1
## 2      Frost  0.26206801 0.26206801       1
## 7 Population -0.06805195 0.06805195       1
## 1       Area -0.10733194 0.10733194       1
## 4 Illiteracy -0.58847793 0.58847793      NA
## 6     Murder -0.78084575 0.78084575       1
```

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="run_models", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                  chunk_label chunk_step_major chunk_step_minor
## 1                import_data                1                0
## 2               cleanse_data                2                0
## 3       inspect_explore_data                2                1
## 4        manage_missing_data                2                2
## 5         encode_retype_data                2                3
## 6           extract_features                3                0
## 7            select_features                4                0
## 8 remove_correlated_features                4                1
## 9                 run_models                5                0
```

## Step `5`: run models

```r
max_cor_y_x_var <- subset(glb_feats_df, cor.low == 1)[1, "id"]

#   Regression:
if (glb_is_regression) {
    #   Linear:
    myrun_mdl_fn <- myrun_mdl_lm
}    

#   Classification:
if (glb_is_classification) {
    #   Logit Regression:
    myrun_mdl_fn <- myrun_mdl_glm
}    
    
# Highest cor.y
ret_lst <- myrun_mdl_fn(indep_vars_vctr=max_cor_y_x_var,
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 0.4552382
## [1] 0.2906719
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -3.01867 -0.67517 -0.07538  0.64483  2.17311 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 65.73965    1.04748  62.760  < 2e-16 ***
## HS.Grad      0.09676    0.01950   4.961  9.2e-06 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.103 on 48 degrees of freedom
## Multiple R-squared:  0.339,	Adjusted R-squared:  0.3252 
## F-statistic: 24.61 on 1 and 48 DF,  p-value: 9.196e-06
## 
##     feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit  SSE.fit   SSE.OOB
## 1 HS.Grad    50 0.3389757 0.2906719    0.3389757 58.36779 0.4552382
##   f.score.OOB
## 1          NA
```

```r
# Enhance Highest cor.y model with additions of interaction terms that were 
#   dropped due to high correlations
if (nrow(subset(glb_feats_df, is.na(cor.low))) > 0)
    ret_lst <- myrun_mdl_fn(indep_vars_vctr=c(max_cor_y_x_var, 
        paste(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
                            fit_df=glb_entity_df, OOB_df=glb_predct_df)    
```

```
## [1] 0.2112575
## [1] 0.6708298
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.1760 -0.4669 -0.0740  0.6329  2.7540 
## 
## Coefficients:
##                     Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        67.289351   1.274092  52.814  < 2e-16 ***
## HS.Grad             0.081820   0.020314   4.028 0.000204 ***
## HS.Grad:Illiteracy -0.012822   0.006365  -2.014 0.049705 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.069 on 47 degrees of freedom
## Multiple R-squared:  0.3915,	Adjusted R-squared:  0.3656 
## F-statistic: 15.12 on 2 and 47 DF,  p-value: 8.51e-06
## 
##                         feats n.fit  R.sq.fit  R.sq.OOB Adj.R.sq.fit
## 2 HS.Grad, HS.Grad:Illiteracy    50 0.3915126 0.6708298    0.3915126
## 1                     HS.Grad    50 0.3389757 0.2906719    0.3389757
##    SSE.fit   SSE.OOB f.score.OOB
## 2 53.72883 0.2112575          NA
## 1 58.36779 0.4552382          NA
```

```r
# Low correlated X
ret_lst <- myrun_mdl_fn(indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 1.073301
## [1] -0.6723614
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.48867 -0.50744 -0.03396  0.56356  1.51450 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.106e+01  1.237e+00  57.454  < 2e-16 ***
## HS.Grad      4.786e-02  2.000e-02   2.393   0.0211 *  
## Income      -2.397e-05  2.405e-04  -0.100   0.9211    
## Frost       -5.905e-03  2.518e-03  -2.346   0.0237 *  
## Population   5.108e-05  2.780e-05   1.837   0.0731 .  
## Area        -2.289e-08  1.556e-06  -0.015   0.9883    
## Murder      -2.997e-01  4.343e-02  -6.900 1.79e-08 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7361 on 43 degrees of freedom
## Multiple R-squared:  0.7361,	Adjusted R-squared:  0.6993 
## F-statistic: 19.99 on 6 and 43 DF,  p-value: 5.38e-11
## 
##                                              feats n.fit  R.sq.fit
## 2                      HS.Grad, HS.Grad:Illiteracy    50 0.3915126
## 1                                          HS.Grad    50 0.3389757
## 3 HS.Grad, Income, Frost, Population, Area, Murder    50 0.7361027
##     R.sq.OOB Adj.R.sq.fit  SSE.fit   SSE.OOB f.score.OOB
## 2  0.6708298    0.3915126 53.72883 0.2112575          NA
## 1  0.2906719    0.3389757 58.36779 0.4552382          NA
## 3 -0.6723614    0.7361027 23.30187 1.0733014          NA
```

```r
# All X that is not user excluded
ret_lst <- myrun_mdl_fn(indep_vars_vctr=setdiff(setdiff(names(glb_entity_df),
                                                        glb_predct_var),
                                                glb_exclude_vars_as_features),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 1.109793
## [1] -0.7292202
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.48895 -0.51232 -0.02747  0.57002  1.49447 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.094e+01  1.748e+00  40.586  < 2e-16 ***
## Population   5.180e-05  2.919e-05   1.775   0.0832 .  
## Income      -2.180e-05  2.444e-04  -0.089   0.9293    
## Illiteracy   3.382e-02  3.663e-01   0.092   0.9269    
## Murder      -3.011e-01  4.662e-02  -6.459 8.68e-08 ***
## HS.Grad      4.893e-02  2.332e-02   2.098   0.0420 *  
## Frost       -5.735e-03  3.143e-03  -1.825   0.0752 .  
## Area        -7.383e-08  1.668e-06  -0.044   0.9649    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7448 on 42 degrees of freedom
## Multiple R-squared:  0.7362,	Adjusted R-squared:  0.6922 
## F-statistic: 16.74 on 7 and 42 DF,  p-value: 2.534e-10
## 
##                                                          feats n.fit
## 2                                  HS.Grad, HS.Grad:Illiteracy    50
## 1                                                      HS.Grad    50
## 3             HS.Grad, Income, Frost, Population, Area, Murder    50
## 4 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area    50
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit  SSE.fit   SSE.OOB f.score.OOB
## 2 0.3915126  0.6708298    0.3915126 53.72883 0.2112575          NA
## 1 0.3389757  0.2906719    0.3389757 58.36779 0.4552382          NA
## 3 0.7361027 -0.6723614    0.7361027 23.30187 1.0733014          NA
## 4 0.7361563 -0.7292202    0.7361563 23.29714 1.1097927          NA
```

```r
print(summary(Prb2.1.mdl <- glb_mdl))
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.48895 -0.51232 -0.02747  0.57002  1.49447 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.094e+01  1.748e+00  40.586  < 2e-16 ***
## Population   5.180e-05  2.919e-05   1.775   0.0832 .  
## Income      -2.180e-05  2.444e-04  -0.089   0.9293    
## Illiteracy   3.382e-02  3.663e-01   0.092   0.9269    
## Murder      -3.011e-01  4.662e-02  -6.459 8.68e-08 ***
## HS.Grad      4.893e-02  2.332e-02   2.098   0.0420 *  
## Frost       -5.735e-03  3.143e-03  -1.825   0.0752 .  
## Area        -7.383e-08  1.668e-06  -0.044   0.9649    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7448 on 42 degrees of freedom
## Multiple R-squared:  0.7362,	Adjusted R-squared:  0.6922 
## F-statistic: 16.74 on 7 and 42 DF,  p-value: 2.534e-10
```

```r
# Prb 3.1
ret_lst <- myrun_mdl_fn(indep_vars_vctr=
    c("Murder", "HS.Grad", "Frost", "Population"),
                        fit_df=glb_entity_df, OOB_df=glb_predct_df)
```

```
## [1] 1.073368
## [1] -0.6724651
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.47095 -0.53464 -0.03701  0.57621  1.50683 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.103e+01  9.529e-01  74.542  < 2e-16 ***
## Murder      -3.001e-01  3.661e-02  -8.199 1.77e-10 ***
## HS.Grad      4.658e-02  1.483e-02   3.142  0.00297 ** 
## Frost       -5.943e-03  2.421e-03  -2.455  0.01802 *  
## Population   5.014e-05  2.512e-05   1.996  0.05201 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7197 on 45 degrees of freedom
## Multiple R-squared:  0.736,	Adjusted R-squared:  0.7126 
## F-statistic: 31.37 on 4 and 45 DF,  p-value: 1.696e-12
## 
##                                                          feats n.fit
## 2                                  HS.Grad, HS.Grad:Illiteracy    50
## 1                                                      HS.Grad    50
## 3             HS.Grad, Income, Frost, Population, Area, Murder    50
## 5                           Murder, HS.Grad, Frost, Population    50
## 4 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area    50
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit  SSE.fit   SSE.OOB f.score.OOB
## 2 0.3915126  0.6708298    0.3915126 53.72883 0.2112575          NA
## 1 0.3389757  0.2906719    0.3389757 58.36779 0.4552382          NA
## 3 0.7361027 -0.6723614    0.7361027 23.30187 1.0733014          NA
## 5 0.7360328 -0.6724651    0.7360328 23.30804 1.0733679          NA
## 4 0.7361563 -0.7292202    0.7361563 23.29714 1.1097927          NA
```

```r
print(summary(Prb3.1.mdl <- glb_mdl))
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.47095 -0.53464 -0.03701  0.57621  1.50683 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.103e+01  9.529e-01  74.542  < 2e-16 ***
## Murder      -3.001e-01  3.661e-02  -8.199 1.77e-10 ***
## HS.Grad      4.658e-02  1.483e-02   3.142  0.00297 ** 
## Frost       -5.943e-03  2.421e-03  -2.455  0.01802 *  
## Population   5.014e-05  2.512e-05   1.996  0.05201 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7197 on 45 degrees of freedom
## Multiple R-squared:  0.736,	Adjusted R-squared:  0.7126 
## F-statistic: 31.37 on 4 and 45 DF,  p-value: 1.696e-12
```

```r
glb_sel_mdl <- glb_mdl                        

# User specified
# ret_lst <- myrun_mdl_fn(indep_vars_vctr=c("<feat1_name>", "<feat2_name>"),
#                         fit_df=glb_entity_df, OOB_df=glb_predct_df)

# Simplify a model
# fit_df <- glb_entity_df; glb_mdl <- step(<complex>_mdl)

if (glb_is_regression)
    print(myplot_scatter(glb_models_df, "Adj.R.sq.fit", "R.sq.OOB") + 
          geom_text(aes(label=feats), data=glb_models_df, color="NavyBlue", 
                    size=3.5))
```

![](USCensus1977_State_files/figure-html/run_models-1.png) 

```r
if (glb_is_classification) {
    plot_models_df <- mutate(glb_models_df, feats.label=substr(feats, 1, 20))
    print(myplot_hbar(df=plot_models_df, xcol_name="feats.label", 
                      ycol_names="f.score.OOB"))
}

script_df <- rbind(script_df, 
                   data.frame(chunk_label="fit_training.all", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
```

## Step `6`: fit training.all

```r
print(mdl_feats_df <- myextract_mdl_feats())
```

```
##                 Estimate   Std. Error   t value         Pr.z         id
## Murder     -3.001488e-01 3.660946e-02 -8.198669 1.774520e-10     Murder
## HS.Grad     4.658225e-02 1.482706e-02  3.141704 2.968091e-03    HS.Grad
## Frost      -5.943290e-03 2.420875e-03 -2.455017 1.801778e-02      Frost
## Population  5.013998e-05 2.512002e-05  1.996017 5.200514e-02 Population
##            fit.feat
## Murder         TRUE
## HS.Grad        TRUE
## Frost          TRUE
## Population     TRUE
```

```r
if (glb_is_regression) {
    ret_lst <- myrun_mdl_lm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl    
    glb_entity_df[, glb_predct_var_name] <- predict(glb_sel_mdl, newdata=glb_entity_df)
    print(myplot_scatter(glb_entity_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
    glb_entity_df$Life.Exp.predict.err <- abs(glb_entity_df$Life.Exp.predict - 
                                            glb_entity_df$Life.Exp)
    print(orderBy(~Life.Exp.predict.err, glb_entity_df))
}    
```

```
## 
## Call:
## lm(formula = reformulate(indep_vars_vctr, response = glb_predct_var), 
##     data = fit_df)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.47095 -0.53464 -0.03701  0.57621  1.50683 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.103e+01  9.529e-01  74.542  < 2e-16 ***
## Murder      -3.001e-01  3.661e-02  -8.199 1.77e-10 ***
## HS.Grad      4.658e-02  1.483e-02   3.142  0.00297 ** 
## Frost       -5.943e-03  2.421e-03  -2.455  0.01802 *  
## Population   5.014e-05  2.512e-05   1.996  0.05201 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7197 on 45 degrees of freedom
## Multiple R-squared:  0.736,	Adjusted R-squared:  0.7126 
## F-statistic: 31.37 on 4 and 45 DF,  p-value: 1.696e-12
## 
##                                                          feats n.fit
## 2                                  HS.Grad, HS.Grad:Illiteracy    50
## 1                                                      HS.Grad    50
## 3             HS.Grad, Income, Frost, Population, Area, Murder    50
## 5                           Murder, HS.Grad, Frost, Population    50
## 4 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area    50
## 6                           Murder, HS.Grad, Frost, Population    50
##    R.sq.fit   R.sq.OOB Adj.R.sq.fit  SSE.fit   SSE.OOB f.score.OOB
## 2 0.3915126  0.6708298    0.3915126 53.72883 0.2112575          NA
## 1 0.3389757  0.2906719    0.3389757 58.36779 0.4552382          NA
## 3 0.7361027 -0.6723614    0.7361027 23.30187 1.0733014          NA
## 5 0.7360328 -0.6724651    0.7360328 23.30804 1.0733679          NA
## 4 0.7361563 -0.7292202    0.7361563 23.29714 1.1097927          NA
## 6 0.7360328         NA    0.7360328 23.30804        NA          NA
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](USCensus1977_State_files/figure-html/fit_training.all-1.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 14       5313   4458        0.7    70.88    7.1    52.9   122  36097
## 9        8277   4815        1.3    70.66   10.7    52.6    11  54090
## 13      11197   5107        0.9    70.14   10.3    52.6   127  55748
## 46       4981   4701        1.4    70.08    9.5    47.8    85  39780
## 41        681   4167        0.5    72.08    1.7    53.3   172  75955
## 33       5441   3875        1.8    69.21   11.1    38.5    80  48798
## 32      18076   4903        1.4    70.55   10.9    52.7    82  47831
## 5       21198   5114        1.1    71.71   10.3    62.6    20 156361
## 10       4931   4091        2.0    68.54   13.9    40.6    60  58073
## 39        931   4558        1.3    71.90    2.4    46.4   127   1049
## 15       2861   4628        0.5    72.56    2.3    59.0   140  55941
## 36       2715   3983        1.1    71.42    6.4    51.6    82  68782
## 35      10735   4561        0.8    70.82    7.4    53.2   124  40975
## 37       2284   4660        0.6    72.13    4.2    60.0    44  96184
## 31       1144   3601        2.2    70.32    9.7    55.2   120 121412
## 20       4122   5299        0.9    70.22    8.5    52.3   101   9891
## 12        813   4119        0.6    71.87    5.3    59.5   126  82677
## 18       3806   3545        2.8    68.76   13.2    42.2    12  44930
## 27       1544   4508        0.6    72.60    2.9    59.3   139  76483
## 7        3100   5348        1.1    72.48    3.1    56.0   139   4862
## 49       4589   4468        0.7    72.48    3.0    54.5   149  54464
## 28        590   5149        0.5    69.03   11.5    65.2   188 109889
## 29        812   4281        0.7    71.23    3.3    57.6   174   9027
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 1        3615   3624        2.1    69.05   15.1    41.3    20  50708
## 45        472   3907        0.6    71.64    5.5    57.1   168   9267
## 25       4767   4254        0.8    70.69    9.3    48.8   108  68995
## 50        376   4566        0.6    70.29    6.9    62.9   173  97203
## 21       5814   4755        1.1    71.83    3.3    58.5   103   7826
## 42       4173   3821        1.7    70.11   11.0    41.8    70  41328
## 30       7333   5237        1.1    70.93    5.2    52.5   115   7521
## 16       2280   4669        0.6    72.58    4.5    59.9   114  81787
## 23       3921   4675        0.6    72.96    2.3    57.6   160  79289
## 22       9111   4751        0.9    70.63   11.1    52.8   125  56817
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
## 44       1203   4022        0.6    72.90    4.5    67.3   137  82096
## 17       3387   3712        1.6    70.10   10.6    38.5    95  39650
## 3        2212   4530        1.8    70.55    7.8    58.1    15 113417
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
## 24       2341   3098        2.4    68.09   12.5    41.0    50  47296
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 38      11860   4449        1.0    70.43    6.1    50.2   126  44966
## 6        2541   4884        0.7    72.06    6.8    63.9   166 103766
## 47       3559   4864        0.6    71.72    4.3    63.5    32  66570
## 48       1799   3617        1.4    69.48    6.7    41.6   100  24070
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 4        2110   3378        1.9    70.66   10.1    39.9    65  51945
## 40       2816   3635        2.3    67.96   11.6    37.8    65  30225
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
##    state.abb state.area         x       y     state.division
## 14        IN      36291  -86.0808 40.0495 East North Central
## 9         FL      58560  -81.6850 27.8744     South Atlantic
## 13        IL      56400  -89.3776 40.0495 East North Central
## 46        VA      40815  -78.2005 37.5630     South Atlantic
## 41        SD      77047  -99.7238 44.3365 West North Central
## 33        NC      52586  -78.4686 35.4195     South Atlantic
## 32        NY      49576  -75.1449 43.1361    Middle Atlantic
## 5         CA     158693 -119.7730 36.5341            Pacific
## 10        GA      58876  -83.3736 32.3329     South Atlantic
## 39        RI       1214  -71.1244 41.5928        New England
## 15        IA      56290  -93.3714 41.9358 West North Central
## 36        OK      69919  -97.1239 35.5053 West South Central
## 35        OH      41222  -82.5963 40.2210 East North Central
## 37        OR      96981 -120.0680 43.9078            Pacific
## 31        NM     121666 -105.9420 34.4764           Mountain
## 20        MD      10577  -76.6459 39.2778     South Atlantic
## 12        ID      83557 -113.9300 43.5648           Mountain
## 18        LA      48523  -92.2724 30.6181 West South Central
## 27        NE      77227  -99.5898 41.3356 West North Central
## 7         CT       5009  -72.3573 41.5928        New England
## 49        WI      56154  -89.9941 44.5937 East North Central
## 28        NV     110540 -116.8510 39.1063           Mountain
## 29        NH       9304  -71.3924 43.3934        New England
## 2         AK     589757 -127.2500 49.2500            Pacific
## 1         AL      51609  -86.7509 32.5901 East South Central
## 45        VT       9609  -72.5450 44.2508        New England
## 25        MO      69686  -92.5137 38.3347 West North Central
## 50        WY      97914 -107.2560 43.0504           Mountain
## 21        MA       8257  -71.5800 42.3645        New England
## 42        TN      42244  -86.4560 35.6767 East South Central
## 30        NJ       7836  -74.2336 39.9637    Middle Atlantic
## 16        KS      82264  -98.1156 38.4204 West North Central
## 23        MN      84068  -94.6043 46.3943 West North Central
## 22        MI      58216  -84.6870 43.1361 East North Central
## 26        MT     147138 -109.3200 46.8230           Mountain
## 44        UT      84916 -111.3300 39.1063           Mountain
## 17        KY      40395  -84.7674 37.3915 East South Central
## 3         AZ     113909 -111.6250 34.2192           Mountain
## 34        ND      70665 -100.0990 47.2517 West North Central
## 24        MS      47716  -89.8065 32.6758 East South Central
## 43        TX     267339  -98.7857 31.3897 West South Central
## 38        PA      45333  -77.4500 40.9069    Middle Atlantic
## 6         CO     104247 -105.5130 38.6777           Mountain
## 47        WA      68192 -119.7460 47.4231            Pacific
## 48        WV      24181  -80.6665 38.4204     South Atlantic
## 8         DE       2057  -74.9841 38.6777     South Atlantic
## 4         AR      53104  -92.2992 34.7336 West South Central
## 40        SC      31055  -80.5056 33.6190     South Atlantic
## 19        ME      33215  -68.9801 45.6226        New England
## 11        HI       6450 -126.2500 31.7500            Pacific
##        state.name  state.region Life.Exp.predict Life.Exp.predict.err
## 14        Indiana North Central         70.90159           0.02158526
## 9         Florida         South         70.61539           0.04460505
## 13       Illinois North Central         70.19244           0.05244160
## 46       Virginia         South         70.14691           0.06691392
## 41   South Dakota North Central         72.01161           0.06839119
## 33 North Carolina         South         69.28624           0.07624179
## 32       New York     Northeast         70.62937           0.07937149
## 5      California          West         71.79565           0.08564599
## 10        Georgia         South         68.63694           0.09694227
## 39   Rhode Island     Northeast         71.76007           0.13992982
## 15           Iowa North Central         72.39653           0.16347124
## 36       Oklahoma         South         71.15860           0.26139958
## 35           Ohio North Central         71.08549           0.26548767
## 37         Oregon          West         72.41445           0.28445333
## 31     New Mexico          West         70.03119           0.28880945
## 20       Maryland         South         70.51852           0.29851996
## 12          Idaho          West         71.49989           0.37010714
## 18      Louisiana         South         69.15045           0.39044846
## 27       Nebraska North Central         72.17032           0.42967691
## 7     Connecticut     Northeast         72.03459           0.44541028
## 49      Wisconsin North Central         72.00996           0.47004324
## 28         Nevada          West         69.52482           0.49482393
## 29  New Hampshire     Northeast         71.72636           0.49635615
## 2          Alaska          West         69.85740           0.54740399
## 1         Alabama         South         68.48112           0.56888134
## 45        Vermont     Northeast         71.06135           0.57865019
## 25       Missouri North Central         70.10610           0.58389969
## 50        Wyoming          West         70.87679           0.58678863
## 21  Massachusetts     Northeast         72.44105           0.61105391
## 42      Tennessee         South         69.46583           0.64416651
## 30     New Jersey     Northeast         71.59612           0.66612086
## 16         Kansas North Central         71.90352           0.67648037
## 23      Minnesota North Central         72.26560           0.69440380
## 22       Michigan North Central         69.86893           0.76106640
## 26        Montana          West         71.40025           0.84024805
## 44           Utah          West         72.05753           0.84246817
## 17       Kentucky         South         69.24418           0.85582067
## 3         Arizona          West         71.41416           0.86415671
## 34   North Dakota North Central         71.87649           0.90350550
## 24    Mississippi         South         69.00535           0.91535384
## 43          Texas         South         69.97886           0.92114057
## 38   Pennsylvania     Northeast         71.38046           0.95045527
## 6        Colorado          West         71.10354           0.95645816
## 47     Washington          West         72.68272           0.96272426
## 48  West Virginia         South         70.44983           0.96982588
## 8        Delaware         South         71.12647           1.06646884
## 4        Arkansas         South         69.57374           1.08626119
## 40 South Carolina         South         69.06109           1.10109172
## 19          Maine     Northeast         71.86095           1.47095411
## 11         Hawaii          West         72.09317           1.50683146
```

```r
if (glb_is_classification) {
    ret_lst <- myrun_mdl_glm(indep_vars_vctr=mdl_feats_df$id, fit_df=glb_entity_df)
    glb_sel_mdl <- glb_mdl        
    glb_entity_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_entity_df, type="response") >= 0.5) * 1.0
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_entity_df))                        
}    

print(glb_feats_df <- mymerge_feats_Pr.z())
```

```
##           id       cor.y  cor.y.abs cor.low         Pr.z
## 6     Murder -0.78084575 0.78084575       1 1.774520e-10
## 3    HS.Grad  0.58221620 0.58221620       1 2.968091e-03
## 2      Frost  0.26206801 0.26206801       1 1.801778e-02
## 7 Population -0.06805195 0.06805195       1 5.200514e-02
## 1       Area -0.10733194 0.10733194       1           NA
## 4 Illiteracy -0.58847793 0.58847793      NA           NA
## 5     Income  0.34025534 0.34025534       1           NA
```

```r
# Most of this code is used again in predict_newdata chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, Pr.z < 0.1)$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_predct_var, glb_predct_var_name))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", facet_colcol_name="variable"))
    }
    
    if (glb_is_regression) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_regression(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"), 
                                           plot_vars_df$id[1])
#               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1,2] is a factor                                                         
#               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
              )
    }    
    
    if (glb_is_classification) {
        plot_vars_df <- subset(glb_feats_df, Pr.z < 0.1)
        print(myplot_prediction_classification(obs_df, 
                    ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], ".rownames"),
                                               plot_vars_df$id[1])
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(glb_entity_df)
```

![](USCensus1977_State_files/figure-html/fit_training.all-2.png) ![](USCensus1977_State_files/figure-html/fit_training.all-3.png) ![](USCensus1977_State_files/figure-html/fit_training.all-4.png) ![](USCensus1977_State_files/figure-html/fit_training.all-5.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 11        868   4963        1.9    73.60    6.2    61.9     0  6425
## 19       1058   3694        0.7    70.39    2.7    54.7   161 30920
## 40       2816   3635        2.3    67.96   11.6    37.8    65 30225
## 4        2110   3378        1.9    70.66   10.1    39.9    65 51945
## 8         579   4809        0.9    70.06    6.2    54.6   103  1982
##    state.abb state.area         x       y     state.division
## 11        HI       6450 -126.2500 31.7500            Pacific
## 19        ME      33215  -68.9801 45.6226        New England
## 40        SC      31055  -80.5056 33.6190     South Atlantic
## 4         AR      53104  -92.2992 34.7336 West South Central
## 8         DE       2057  -74.9841 38.6777     South Atlantic
##        state.name state.region Life.Exp.predict Life.Exp.predict.err
## 11         Hawaii         West         72.09317             1.506831
## 19          Maine    Northeast         71.86095             1.470954
## 40 South Carolina        South         69.06109             1.101092
## 4        Arkansas        South         69.57374             1.086261
## 8        Delaware        South         71.12647             1.066469
##    .label
## 11     HI
## 19     ME
## 40     SC
## 4      AR
## 8      DE
```

![](USCensus1977_State_files/figure-html/fit_training.all-6.png) 

```r
script_df <- rbind(script_df, 
                   data.frame(chunk_label="predict_newdata", 
                              chunk_step_major=max(script_df$chunk_step_major)+1, 
                              chunk_step_minor=0))
print(script_df)
```

```
##                   chunk_label chunk_step_major chunk_step_minor
## 1                 import_data                1                0
## 2                cleanse_data                2                0
## 3        inspect_explore_data                2                1
## 4         manage_missing_data                2                2
## 5          encode_retype_data                2                3
## 6            extract_features                3                0
## 7             select_features                4                0
## 8  remove_correlated_features                4                1
## 9                  run_models                5                0
## 10           fit_training.all                6                0
## 11            predict_newdata                7                0
```

## Step `7`: predict newdata

```r
if (glb_is_regression)
    glb_predct_df[, glb_predct_var_name] <- predict(glb_sel_mdl, 
                                        newdata=glb_predct_df, type="response")

if (glb_is_classification)
    glb_predct_df[, glb_predct_var_name] <- (predict(glb_sel_mdl, 
                        newdata=glb_predct_df, type="response") >= 0.5) * 1.0
    
myprint_df(glb_predct_df[, c(glb_id_vars, glb_predct_var, glb_predct_var_name)])
```

```
##    state.abb Life.Exp Life.Exp.predict
## 17        KY    70.10         69.24418
## 25        MO    70.69         70.10610
```

```r
if (glb_is_regression) {
    print(sprintf("Total SSE: %0.4f", 
                  sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2)))
    print(sprintf("RMSE: %0.4f", 
                  (sum((glb_predct_df[, glb_predct_var_name] - 
                        glb_predct_df[, glb_predct_var]) ^ 2) / nrow(glb_predct_df)) ^ 0.5))                        
    print(myplot_scatter(glb_predct_df, glb_predct_var, glb_predct_var_name, 
                         smooth=TRUE))
                         
#     glb_predct_df[, "<Output Pred variable>"] <- func(glb_predct_df[, glb_pred_var_name])                         
}                         
```

```
## [1] "Total SSE: 1.0734"
## [1] "RMSE: 0.7326"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : span too small.  fewer data values than degrees of freedom.
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at 70.097
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 8.7025e-06
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : pseudoinverse used at 70.097
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : neighborhood radius 0.00295
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : reciprocal condition number 1
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : at 70.693
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : radius 8.7025e-06
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : all data on boundary of neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : There are other near singularities as well. 8.7025e-06
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in simpleLoess(y, x, w, span, degree, parametric, drop.square,
## normalize, : zero-width neighborhood. make span bigger
```

```
## Warning in qt((1 - level)/2, df): NaNs produced
```

![](USCensus1977_State_files/figure-html/predict_newdata-1.png) 

```r
if (glb_is_classification)
    print(xtabs(reformulate(paste(glb_predct_var, glb_predct_var_name, sep=" + ")),
                glb_predct_df))
    
glb_analytics_diag_plots(glb_predct_df)
```

![](USCensus1977_State_files/figure-html/predict_newdata-2.png) ![](USCensus1977_State_files/figure-html/predict_newdata-3.png) ![](USCensus1977_State_files/figure-html/predict_newdata-4.png) ![](USCensus1977_State_files/figure-html/predict_newdata-5.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 17       3387   3712        1.6    70.10   10.6    38.5    95 39650
## 25       4767   4254        0.8    70.69    9.3    48.8   108 68995
##    state.abb state.area        x       y     state.division state.name
## 17        KY      40395 -84.7674 37.3915 East South Central   Kentucky
## 25        MO      69686 -92.5137 38.3347 West North Central   Missouri
##     state.region Life.Exp.predict Life.Exp.predict.err .label
## 17         South         69.24418            0.8558207     KY
## 25 North Central         70.10610            0.5838997     MO
```

![](USCensus1977_State_files/figure-html/predict_newdata-6.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 

```r
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
## R version 3.1.2 (2014-10-31)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] reshape2_1.4.1  plyr_1.8.1      doBy_4.5-13     survival_2.38-1
## [5] ggplot2_1.0.0  
## 
## loaded via a namespace (and not attached):
##  [1] colorspace_1.2-5 digest_0.6.8     evaluate_0.5.5   formatR_1.0     
##  [5] grid_3.1.2       gtable_0.1.2     htmltools_0.2.6  knitr_1.9       
##  [9] labeling_0.3     lattice_0.20-30  MASS_7.3-39      Matrix_1.1-5    
## [13] munsell_0.4.2    proto_0.3-10     Rcpp_0.11.4      rmarkdown_0.5.1 
## [17] scales_0.2.4     splines_3.1.2    stringr_0.6.2    tcltk_3.1.2     
## [21] tools_3.1.2      yaml_2.1.13
```
