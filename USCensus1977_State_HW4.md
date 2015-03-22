# USCensus1977:HW4: Life.Exp regression
bdanalytics  

**  **    
**Date: (Sat) Apr 25, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/c4x/MITx/15.071x_2/asset/statedataSimple.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- fit.models chunk:
    - Prediction accuracy scatter graph:
    -   Add tiles (raw vs. PCA)
    -   Use shiny for drop-down of "important" features
    -   Use plot.ly for interactive plots ?
    
    - Change .fit suffix of model metrics to .mdl if it's data independent (e.g. AIC, Adj.R.Squared - is it truly data independent ?, etc.)
    - move model_type parameter to myfit_mdl before indep_vars_vctr (keep all model_* together)
    - create a custom model for rpart that has minbucket as a tuning parameter
    - varImp for randomForest crashes in caret version:6.0.41 -> submit bug report

- Probability handling for multinomials vs. desired binomial outcome
-   ROCR currently supports only evaluation of binary classification tasks (version 1.0.7)
-   extensions toward multiclass classification are scheduled for the next release

- Skip trControl.method="cv" for dummy classifier ?
- Add custom model to caret for a dummy (baseline) classifier (binomial & multinomial) that generates proba/outcomes which mimics the freq distribution of glb_rsp_var values; Right now glb_dmy_glm_mdl always generates most frequent outcome in training data
- glm_dmy_mdl should use the same method as glm_sel_mdl until custom dummy classifer is implemented

- Compare glb_sel_mdl vs. glb_fin_mdl:
    - varImp
    - Prediction differences (shd be minimal ?)

- Move glb_analytics_diag_plots to mydsutils.R: (+) Easier to debug (-) Too many glb vars used
- Add print(ggplot.petrinet(glb_analytics_pn) + coord_flip()) at the end of every major chunk
- Parameterize glb_analytics_pn
- Move glb_impute_missing_data to mydsutils.R: (-) Too many glb vars used; glb_<>_df reassigned
- Replicate myfit_mdl_classification features in myfit_mdl_regression
- Do non-glm methods handle interaction terms ?
- f-score computation for classifiers should be summation across outcomes (not just the desired one ?)
- Add accuracy computation to glb_dmy_mdl in predict.data.new chunk
- Why does splitting fit.data.training.all chunk into separate chunks add an overhead of ~30 secs ? It's not rbind b/c other chunks have lower elapsed time. Is it the number of plots ?
- Incorporate code chunks in print_sessionInfo
- Test against 
    - projects in github.com/bdanalytics
    - lectures in jhu-datascience track

# Analysis: 

```r
rm(list=ls())
set.seed(12345)
options(stringsAsFactors=FALSE)
source("~/Dropbox/datascience/R/mydsutils.R")
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
# Gather all package requirements here
#suppressPackageStartupMessages(require())
#packageVersion("snow")

#require(sos); findFn("pinv", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/c4x/MITx/15.071x_2/asset/statedata.csv"
glb_newdt_url <- "<newdt_url>"
glb_is_separate_newent_dataset <- FALSE    # or TRUE
glb_split_entity_newent_datasets <- TRUE   # or FALSE
glb_split_newdata_method <- "copy"          # "condition" or "sample" or "copy"
glb_split_newdata_condition <- "<col_name> <condition_operator> <value>"    # or NULL
glb_split_newdata_size_ratio <- 0.1               # > 0 & < 1
glb_split_sample.seed <- 88               # or any integer
glb_max_obs <- NULL # or any integer

glb_is_regression <- TRUE; glb_is_classification <- FALSE; glb_is_binomial <- FALSE

glb_rsp_var_raw <- "Life.Exp"

# for classification, the response variable has to be a factor
glb_rsp_var <- glb_rsp_var_raw # or "Life.Exp.fctr"

# if the response factor is based on numbers e.g (0/1 vs. "A"/"B"), 
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- NULL # or function(raw) {
    #relevel(factor(ifelse(raw == 1, "R", "A")), as.factor(c("R", "A")), ref="A")
    #as.factor(paste0("B", raw))
    #as.factor(raw)    
#}
#glb_map_rsp_raw_to_var(c(1, 2, 3, 4, 5))

glb_map_rsp_var_to_raw <- NULL # or function(var) {
    #as.numeric(var) - 1
    #as.numeric(var)
    #levels(var)[as.numeric(var)]
    #c(" <=50K", " >50K")[as.numeric(var)]
#}
#glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 2, 3, 4, 5)))

if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")

glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later
glb_id_vars <- c("state.abb")

# List transformed vars  
glb_exclude_vars_as_features <- c("state.abb") # or c("<var_name>")    
# List feats that shd be excluded due to known causation by prediction variable
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c("state.name")) # or c("<col_name>")
# List output vars (useful during testing in console)          
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                         grep(glb_rsp_var_out, names(glb_trnent_df), value=TRUE)) 

glb_impute_na_data <- FALSE            # or TRUE
glb_mice_complete.seed <- 144               # or any integer

# rpart:  .rnorm messes with the models badly
#         caret creates dummy vars for factor feats which messes up the tuning
#             - better to feed as.numeric(<feat>.fctr) to caret 
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

glb_models_lst <- list(); glb_models_df <- data.frame()
# Baseline prediction model feature(s)
glb_Baseline_mdl_var <- NULL # or c("<col_name>")

glb_model_metric_terms <- NULL # or matrix(c(
#                               0,1,2,3,4,
#                               2,0,1,2,3,
#                               4,2,0,1,2,
#                               6,4,2,0,1,
#                               8,6,4,2,0
#                           ), byrow=TRUE, nrow=5)
glb_model_metric <- NULL # or "<metric_name>"
glb_model_metric_maximize <- NULL # or FALSE (TRUE is not the default for both classification & regression) 
glb_model_metric_smmry <- NULL # or function(data, lev=NULL, model=NULL) {
#     confusion_mtrx <- t(as.matrix(confusionMatrix(data$pred, data$obs)))
#     #print(confusion_mtrx)
#     #print(confusion_mtrx * glb_model_metric_terms)
#     metric <- sum(confusion_mtrx * glb_model_metric_terms) / nrow(data)
#     names(metric) <- glb_model_metric
#     return(metric)
# }

glb_tune_models_df <- 
   rbind(
    #data.frame(parameter="cp", min=0.00005, max=0.00005, by=0.000005),
                            #seq(from=0.01,  to=0.01, by=0.01)
    #data.frame(parameter="mtry", min=2, max=4, by=1),
    data.frame(parameter="dummy", min=2, max=4, by=1)
        ) 
# or NULL
glb_n_cv_folds <- 3 # or NULL

glb_clf_proba_threshold <- NULL # 0.5

# Model selection criteria
if (glb_is_regression)
    glb_model_evl_criteria <- c("min.RMSE.OOB", "max.R.sq.OOB", "max.Adj.R.sq.fit")
if (glb_is_classification) {
    if (glb_is_binomial)
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB", "min.aic.fit") else
        glb_model_evl_criteria <- c("max.Accuracy.OOB", "max.Kappa.OOB")
}

glb_sel_mdl_id <- NULL # or "<model_id_prefix>.<model_method>"
glb_fin_mdl_id <- glb_sel_mdl_id # or "Final"

# Depict process
glb_analytics_pn <- petrinet(name="glb_analytics_pn",
                        trans_df=data.frame(id=1:6,
    name=c("data.training.all","data.new",
           "model.selected","model.final",
           "data.training.all.prediction","data.new.prediction"),
    x=c(   -5,-5,-15,-25,-25,-35),
    y=c(   -5, 5,  0,  0, -5,  5)
                        ),
                        places_df=data.frame(id=1:4,
    name=c("bgn","fit.data.training.all","predict.data.new","end"),
    x=c(   -0,   -20,                    -30,               -40),
    y=c(    0,     0,                      0,                 0),
    M0=c(   3,     0,                      0,                 0)
                        ),
                        arcs_df=data.frame(
    begin=c("bgn","bgn","bgn",        
            "data.training.all","model.selected","fit.data.training.all",
            "fit.data.training.all","model.final",    
            "data.new","predict.data.new",
            "data.training.all.prediction","data.new.prediction"),
    end  =c("data.training.all","data.new","model.selected",
            "fit.data.training.all","fit.data.training.all","model.final",
            "data.training.all.prediction","predict.data.new",
            "predict.data.new","data.new.prediction",
            "end","end")
                        ))
#print(ggplot.petrinet(glb_analytics_pn))
print(ggplot.petrinet(glb_analytics_pn) + coord_flip())
```

```
## Loading required package: grid
```

![](USCensus1977_State_HW4_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_script_tm <- proc.time()
glb_script_df <- data.frame(chunk_label="import_data", 
                            chunk_step_major=1, chunk_step_minor=0,
                            elapsed=(proc.time() - glb_script_tm)["elapsed"])
print(tail(glb_script_df, 2))
```

```
##         chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed import_data                1                0   0.002
```

## Step `1`: import data

```r
glb_entity_df <- myimport_data(url=glb_trnng_url, 
    comment=ifelse(!glb_is_separate_newent_dataset, "glb_entity_df", "glb_trnent_df"), 
                                force_header=TRUE)
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
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 7        3100   5348        1.1    72.48    3.1    56.0   139   4862
## 17       3387   3712        1.6    70.10   10.6    38.5    95  39650
## 25       4767   4254        0.8    70.69    9.3    48.8   108  68995
## 35      10735   4561        0.8    70.82    7.4    53.2   124  40975
## 47       3559   4864        0.6    71.72    4.3    63.5    32  66570
##    state.abb state.area         x       y     state.division  state.name
## 2         AK     589757 -127.2500 49.2500            Pacific      Alaska
## 7         CT       5009  -72.3573 41.5928        New England Connecticut
## 17        KY      40395  -84.7674 37.3915 East South Central    Kentucky
## 25        MO      69686  -92.5137 38.3347 West North Central    Missouri
## 35        OH      41222  -82.5963 40.2210 East North Central        Ohio
## 47        WA      68192 -119.7460 47.4231            Pacific  Washington
##     state.region
## 2           West
## 7      Northeast
## 17         South
## 25 North Central
## 35 North Central
## 47          West
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
if (!glb_is_separate_newent_dataset) {
    glb_trnent_df <- glb_entity_df; comment(glb_trnent_df) <- "glb_trnent_df"
} # else glb_entity_df is maintained as is for chunk:inspectORexplore.data
    
if (glb_is_separate_newent_dataset) {
    glb_newent_df <- myimport_data(
        url=glb_newdt_url, 
        comment="glb_newent_df", force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_entity_df <- rbind(glb_trnent_df, glb_newent_df); comment(glb_entity_df) <- "glb_entity_df"
} else {
    if (!glb_split_entity_newent_datasets) {
        stop("Not implemented yet") 
        glb_newent_df <- glb_trnent_df[sample(1:nrow(glb_trnent_df),
                                          max(2, nrow(glb_trnent_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newent_df <- do.call("subset", 
                list(glb_trnent_df, parse(text=glb_split_newdata_condition)))
            glb_trnent_df <- do.call("subset", 
                list(glb_trnent_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnent_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newent_df <- glb_trnent_df[!split, ] 
                glb_trnent_df <- glb_trnent_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnent_df <- glb_entity_df
            comment(glb_trnent_df) <- "glb_trnent_df"
            glb_newent_df <- glb_entity_df
            comment(glb_newent_df) <- "glb_newent_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newent_df) <- "glb_newent_df"
    myprint_df(glb_newent_df)
    str(glb_newent_df)

    if (glb_split_entity_newent_datasets) {
        myprint_df(glb_trnent_df)
        str(glb_trnent_df)        
    }    
}         
```

```
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
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 1        3615   3624        2.1    69.05   15.1    41.3    20 50708
## 18       3806   3545        2.8    68.76   13.2    42.2    12 44930
## 19       1058   3694        0.7    70.39    2.7    54.7   161 30920
## 22       9111   4751        0.9    70.63   11.1    52.8   125 56817
## 37       2284   4660        0.6    72.13    4.2    60.0    44 96184
## 48       1799   3617        1.4    69.48    6.7    41.6   100 24070
##    state.abb state.area         x       y     state.division    state.name
## 1         AL      51609  -86.7509 32.5901 East South Central       Alabama
## 18        LA      48523  -92.2724 30.6181 West South Central     Louisiana
## 19        ME      33215  -68.9801 45.6226        New England         Maine
## 22        MI      58216  -84.6870 43.1361 East North Central      Michigan
## 37        OR      96981 -120.0680 43.9078            Pacific        Oregon
## 48        WV      24181  -80.6665 38.4204     South Atlantic West Virginia
##     state.region
## 1          South
## 18         South
## 19     Northeast
## 22 North Central
## 37          West
## 48         South
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
##  - attr(*, "comment")= chr "glb_newent_df"
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
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 9        8277   4815        1.3    70.66   10.7    52.6    11 54090
## 16       2280   4669        0.6    72.58    4.5    59.9   114 81787
## 22       9111   4751        0.9    70.63   11.1    52.8   125 56817
## 32      18076   4903        1.4    70.55   10.9    52.7    82 47831
## 45        472   3907        0.6    71.64    5.5    57.1   168  9267
## 47       3559   4864        0.6    71.72    4.3    63.5    32 66570
##    state.abb state.area         x       y     state.division state.name
## 9         FL      58560  -81.6850 27.8744     South Atlantic    Florida
## 16        KS      82264  -98.1156 38.4204 West North Central     Kansas
## 22        MI      58216  -84.6870 43.1361 East North Central   Michigan
## 32        NY      49576  -75.1449 43.1361    Middle Atlantic   New York
## 45        VT       9609  -72.5450 44.2508        New England    Vermont
## 47        WA      68192 -119.7460 47.4231            Pacific Washington
##     state.region
## 9          South
## 16 North Central
## 22 North Central
## 32     Northeast
## 45     Northeast
## 47          West
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
##  - attr(*, "comment")= chr "glb_trnent_df"
```

```r
if (!is.null(glb_max_obs)) {
    if (nrow(glb_trnent_df) > glb_max_obs) {
        warning("glb_trnent_df restricted to glb_max_obs: ", format(glb_max_obs, big.mark=","))
        org_entity_df <- glb_trnent_df
        glb_trnent_df <- org_entity_df[split <- 
            sample.split(org_entity_df[, glb_rsp_var_raw], SplitRatio=glb_max_obs), ]
        org_entity_df <- NULL
    }
    if (nrow(glb_newent_df) > glb_max_obs) {
        warning("glb_newent_df restricted to glb_max_obs: ", format(glb_max_obs, big.mark=","))        
        org_newent_df <- glb_newent_df
        glb_newent_df <- org_newent_df[split <- 
            sample.split(org_newent_df[, glb_rsp_var_raw], SplitRatio=glb_max_obs), ]
        org_newent_df <- NULL
    }    
}

if (nrow(glb_trnent_df) == nrow(glb_entity_df))
    warning("glb_trnent_df same as glb_entity_df")
```

```
## Warning: glb_trnent_df same as glb_entity_df
```

```r
if (nrow(glb_newent_df) == nrow(glb_entity_df))
    warning("glb_newent_df same as glb_entity_df")
```

```
## Warning: glb_newent_df same as glb_entity_df
```

```r
glb_script_df <- rbind(glb_script_df,
                   data.frame(chunk_label="cleanse_data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##           chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed   import_data                1                0   0.002
## elapsed1 cleanse_data                2                0   0.368
```

## Step `2`: cleanse data

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="inspectORexplore.data", 
                              chunk_step_major=max(glb_script_df$chunk_step_major), 
                              chunk_step_minor=1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed1          cleanse_data                2                0   0.368
## elapsed2 inspectORexplore.data                2                1   0.405
```

### Step `2`.`1`: inspect/explore data

```r
#print(str(glb_trnent_df))
#View(glb_trnent_df)

# List info gathered for various columns
# <col_name>:   <description>; <notes>

# Create new features that help diagnostics
#   Create factors of string variables
str_vars <- sapply(1:length(names(glb_trnent_df)), 
    function(col) ifelse(class(glb_trnent_df[, names(glb_trnent_df)[col]]) == "character",
                         names(glb_trnent_df)[col], ""))
if (length(str_vars <- setdiff(str_vars[str_vars != ""], 
                               glb_exclude_vars_as_features)) > 0) {
    warning("Creating factors of string variables:", paste0(str_vars, collapse=", "))
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
    for (var in str_vars) {
        glb_entity_df[, paste0(var, ".fctr")] <- factor(glb_entity_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
        glb_trnent_df[, paste0(var, ".fctr")] <- factor(glb_trnent_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
        glb_newent_df[, paste0(var, ".fctr")] <- factor(glb_newent_df[, var], 
                        as.factor(unique(glb_entity_df[, var])))
    }
}
```

```
## Warning: Creating factors of string variables:state.division, state.region
```

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

add_new_diag_feats <- function(obs_df, ref_df=glb_entity_df) {
    require(plyr)
    
    obs_df <- mutate(obs_df,
#         <col_name>.NA=is.na(<col_name>),

#         <col_name>.fctr=factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))), 
#         <col_name>.fctr=relevel(factor(<col_name>, 
#                     as.factor(union(obs_df$<col_name>, obs_twin_df$<col_name>))),
#                                   "<ref_val>"), 
#         <col2_name>.fctr=relevel(factor(ifelse(<col1_name> == <val>, "<oth_val>", "<ref_val>")), 
#                               as.factor(c("R", "<ref_val>")),
#                               ref="<ref_val>"),

          # This doesn't work - use sapply instead
#         <col_name>.fctr_num=grep(<col_name>, levels(<col_name>.fctr)), 
#         
#         Date.my=as.Date(strptime(Date, "%m/%d/%y %H:%M")),
#         Year=year(Date.my),
#         Month=months(Date.my),
#         Weekday=weekdays(Date.my)

#         <col_name>.log=log(<col.name>),        
#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

        .rnorm=rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newent_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    print(summary(obs_df))
    print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}

glb_entity_df <- add_new_diag_feats(glb_entity_df)
```

```
## Loading required package: plyr
```

```
##    Population        Income       Illiteracy       Life.Exp    
##  Min.   :  365   Min.   :3098   Min.   :0.500   Min.   :67.96  
##  1st Qu.: 1080   1st Qu.:3993   1st Qu.:0.625   1st Qu.:70.12  
##  Median : 2838   Median :4519   Median :0.950   Median :70.67  
##  Mean   : 4246   Mean   :4436   Mean   :1.170   Mean   :70.88  
##  3rd Qu.: 4968   3rd Qu.:4814   3rd Qu.:1.575   3rd Qu.:71.89  
##  Max.   :21198   Max.   :6315   Max.   :2.800   Max.   :73.60  
##                                                                
##      Murder          HS.Grad          Frost             Area       
##  Min.   : 1.400   Min.   :37.80   Min.   :  0.00   Min.   :  1049  
##  1st Qu.: 4.350   1st Qu.:48.05   1st Qu.: 66.25   1st Qu.: 36985  
##  Median : 6.850   Median :53.25   Median :114.50   Median : 54277  
##  Mean   : 7.378   Mean   :53.11   Mean   :104.46   Mean   : 70736  
##  3rd Qu.:10.675   3rd Qu.:59.15   3rd Qu.:139.75   3rd Qu.: 81162  
##  Max.   :15.100   Max.   :67.30   Max.   :188.00   Max.   :566432  
##                                                                    
##   state.abb           state.area           x                 y        
##  Length:50          Min.   :  1214   Min.   :-127.25   Min.   :27.87  
##  Class :character   1st Qu.: 37317   1st Qu.:-104.16   1st Qu.:35.55  
##  Mode  :character   Median : 56222   Median : -89.90   Median :39.62  
##                     Mean   : 72368   Mean   : -92.46   Mean   :39.41  
##                     3rd Qu.: 83234   3rd Qu.: -78.98   3rd Qu.:43.14  
##                     Max.   :589757   Max.   : -68.98   Max.   :49.25  
##                                                                       
##  state.division      state.name        state.region      
##  Length:50          Length:50          Length:50         
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##          state.division.fctr     state.region.fctr     .rnorm       
##  Mountain          : 8       South        :16      Min.   :-2.3804  
##  South Atlantic    : 8       West         :13      1st Qu.:-0.6183  
##  West North Central: 7       Northeast    : 9      Median : 0.3611  
##  New England       : 6       North Central:12      Mean   : 0.2029  
##  Pacific           : 5                             3rd Qu.: 0.8468  
##  East North Central: 5                             Max.   : 2.1968  
##  (Other)           :11                                              
##          Population              Income          Illiteracy 
##                   0                   0                   0 
##            Life.Exp              Murder             HS.Grad 
##                   0                   0                   0 
##               Frost                Area           state.abb 
##                   0                   0                   0 
##          state.area                   x                   y 
##                   0                   0                   0 
##      state.division          state.name        state.region 
##                   0                   0                   0 
## state.division.fctr   state.region.fctr              .rnorm 
##                   0                   0                   0
```

```r
glb_trnent_df <- add_new_diag_feats(glb_trnent_df)
```

```
##    Population        Income       Illiteracy       Life.Exp    
##  Min.   :  365   Min.   :3098   Min.   :0.500   Min.   :67.96  
##  1st Qu.: 1080   1st Qu.:3993   1st Qu.:0.625   1st Qu.:70.12  
##  Median : 2838   Median :4519   Median :0.950   Median :70.67  
##  Mean   : 4246   Mean   :4436   Mean   :1.170   Mean   :70.88  
##  3rd Qu.: 4968   3rd Qu.:4814   3rd Qu.:1.575   3rd Qu.:71.89  
##  Max.   :21198   Max.   :6315   Max.   :2.800   Max.   :73.60  
##                                                                
##      Murder          HS.Grad          Frost             Area       
##  Min.   : 1.400   Min.   :37.80   Min.   :  0.00   Min.   :  1049  
##  1st Qu.: 4.350   1st Qu.:48.05   1st Qu.: 66.25   1st Qu.: 36985  
##  Median : 6.850   Median :53.25   Median :114.50   Median : 54277  
##  Mean   : 7.378   Mean   :53.11   Mean   :104.46   Mean   : 70736  
##  3rd Qu.:10.675   3rd Qu.:59.15   3rd Qu.:139.75   3rd Qu.: 81162  
##  Max.   :15.100   Max.   :67.30   Max.   :188.00   Max.   :566432  
##                                                                    
##   state.abb           state.area           x                 y        
##  Length:50          Min.   :  1214   Min.   :-127.25   Min.   :27.87  
##  Class :character   1st Qu.: 37317   1st Qu.:-104.16   1st Qu.:35.55  
##  Mode  :character   Median : 56222   Median : -89.90   Median :39.62  
##                     Mean   : 72368   Mean   : -92.46   Mean   :39.41  
##                     3rd Qu.: 83234   3rd Qu.: -78.98   3rd Qu.:43.14  
##                     Max.   :589757   Max.   : -68.98   Max.   :49.25  
##                                                                       
##  state.division      state.name        state.region      
##  Length:50          Length:50          Length:50         
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##          state.division.fctr     state.region.fctr     .rnorm       
##  Mountain          : 8       South        :16      Min.   :-1.8324  
##  South Atlantic    : 8       West         :13      1st Qu.:-0.6709  
##  West North Central: 7       Northeast    : 9      Median : 0.2725  
##  New England       : 6       North Central:12      Mean   : 0.2067  
##  Pacific           : 5                             3rd Qu.: 0.9290  
##  East North Central: 5                             Max.   : 2.4771  
##  (Other)           :11                                              
##          Population              Income          Illiteracy 
##                   0                   0                   0 
##            Life.Exp              Murder             HS.Grad 
##                   0                   0                   0 
##               Frost                Area           state.abb 
##                   0                   0                   0 
##          state.area                   x                   y 
##                   0                   0                   0 
##      state.division          state.name        state.region 
##                   0                   0                   0 
## state.division.fctr   state.region.fctr              .rnorm 
##                   0                   0                   0
```

```r
glb_newent_df <- add_new_diag_feats(glb_newent_df)
```

```
##    Population        Income       Illiteracy       Life.Exp    
##  Min.   :  365   Min.   :3098   Min.   :0.500   Min.   :67.96  
##  1st Qu.: 1080   1st Qu.:3993   1st Qu.:0.625   1st Qu.:70.12  
##  Median : 2838   Median :4519   Median :0.950   Median :70.67  
##  Mean   : 4246   Mean   :4436   Mean   :1.170   Mean   :70.88  
##  3rd Qu.: 4968   3rd Qu.:4814   3rd Qu.:1.575   3rd Qu.:71.89  
##  Max.   :21198   Max.   :6315   Max.   :2.800   Max.   :73.60  
##                                                                
##      Murder          HS.Grad          Frost             Area       
##  Min.   : 1.400   Min.   :37.80   Min.   :  0.00   Min.   :  1049  
##  1st Qu.: 4.350   1st Qu.:48.05   1st Qu.: 66.25   1st Qu.: 36985  
##  Median : 6.850   Median :53.25   Median :114.50   Median : 54277  
##  Mean   : 7.378   Mean   :53.11   Mean   :104.46   Mean   : 70736  
##  3rd Qu.:10.675   3rd Qu.:59.15   3rd Qu.:139.75   3rd Qu.: 81162  
##  Max.   :15.100   Max.   :67.30   Max.   :188.00   Max.   :566432  
##                                                                    
##   state.abb           state.area           x                 y        
##  Length:50          Min.   :  1214   Min.   :-127.25   Min.   :27.87  
##  Class :character   1st Qu.: 37317   1st Qu.:-104.16   1st Qu.:35.55  
##  Mode  :character   Median : 56222   Median : -89.90   Median :39.62  
##                     Mean   : 72368   Mean   : -92.46   Mean   :39.41  
##                     3rd Qu.: 83234   3rd Qu.: -78.98   3rd Qu.:43.14  
##                     Max.   :589757   Max.   : -68.98   Max.   :49.25  
##                                                                       
##  state.division      state.name        state.region      
##  Length:50          Length:50          Length:50         
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##                                                          
##          state.division.fctr     state.region.fctr     .rnorm        
##  Mountain          : 8       South        :16      Min.   :-2.12355  
##  South Atlantic    : 8       West         :13      1st Qu.:-0.76752  
##  West North Central: 7       Northeast    : 9      Median :-0.01815  
##  New England       : 6       North Central:12      Mean   : 0.03124  
##  Pacific           : 5                             3rd Qu.: 0.80447  
##  East North Central: 5                             Max.   : 2.65579  
##  (Other)           :11                                               
##          Population              Income          Illiteracy 
##                   0                   0                   0 
##            Life.Exp              Murder             HS.Grad 
##                   0                   0                   0 
##               Frost                Area           state.abb 
##                   0                   0                   0 
##          state.area                   x                   y 
##                   0                   0                   0 
##      state.division          state.name        state.region 
##                   0                   0                   0 
## state.division.fctr   state.region.fctr              .rnorm 
##                   0                   0                   0
```

```r
# Histogram of predictor in glb_trnent_df & glb_newent_df
plot_df <- rbind(cbind(glb_trnent_df[, glb_rsp_var_raw, FALSE], data.frame(.data="Training")),
                 cbind(glb_trnent_df[, glb_rsp_var_raw, FALSE], data.frame(.data="New")))
print(myplot_histogram(plot_df, glb_rsp_var_raw) + facet_wrap(~ .data))
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![](USCensus1977_State_HW4_files/figure-html/inspectORexplore_data-1.png) 

```r
if (glb_is_classification) {
    xtab_df <- mycreate_xtab(plot_df, c(".data", glb_rsp_var_raw))
    rownames(xtab_df) <- xtab_df$.data
    xtab_df <- subset(xtab_df, select=-.data)
    print(xtab_df / rowSums(xtab_df))    
}    

# Check for duplicates in glb_id_vars
if (length(glb_id_vars) > 0) {
    id_vars_dups_df <- subset(id_vars_df <- 
            mycreate_tbl_df(glb_entity_df[, glb_id_vars, FALSE], glb_id_vars),
                                .freq > 1)
    if (nrow(id_vars_dups_df) > 0) {
        warning("Duplicates found in glb_id_vars data:", nrow(id_vars_dups_df))
        myprint_df(id_vars_dups_df)
    } else {
        # glb_id_vars are unique across obs in both glb_<>_df
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_vars)
    }
}

#pairs(subset(glb_trnent_df, select=-c(col_symbol)))
# Check for glb_newent_df & glb_trnent_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnent_df, <col1_name> == max(glb_trnent_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnent_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnent_df[which.max(glb_trnent_df$<col_name>),])

# print(<col_name>_freq_glb_trnent_df <- mycreate_tbl_df(glb_trnent_df, "<col_name>"))
# print(which.min(table(glb_trnent_df$<col_name>)))
# print(which.max(table(glb_trnent_df$<col_name>)))
# print(which.max(table(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>)[, 2]))
# print(table(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>))
# print(table(is.na(glb_trnent_df$<col1_name>), glb_trnent_df$<col2_name>))
# print(table(sign(glb_trnent_df$<col1_name>), glb_trnent_df$<col2_name>))
# print(mycreate_xtab(glb_trnent_df, <col1_name>))
# print(mycreate_xtab(glb_trnent_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mycreate_xtab(glb_trnent_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnent_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnent_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnent_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnent_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnent_df$<col1_name>, glb_trnent_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnent_df$<col1_name>.NA, glb_trnent_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnent_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnent_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnent_df, Symbol %in% c("KO", "PG")), 
#                   "Date.my", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.Date("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.Date("1983-01-01")))        
#         )
# print(myplot_scatter(glb_trnent_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_entity_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_entity_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5))

glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="manage_missing_data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                    chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed2 inspectORexplore.data                2                1   0.405
## elapsed3   manage_missing_data                2                2   1.479
```

### Step `2`.`2`: manage missing data

```r
# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))
# glb_trnent_df <- na.omit(glb_trnent_df)
# glb_newent_df <- na.omit(glb_newent_df)
# df[is.na(df)] <- 0

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function(entity_df, newent_df) {
    if (!glb_is_separate_newent_dataset) {
        # Combine entity & newent
        union_df <- rbind(mutate(entity_df, .src = "entity"),
                          mutate(newent_df, .src = "newent"))
        union_imputed_df <- union_df[, setdiff(setdiff(names(entity_df), 
                                                       glb_rsp_var), 
                                               glb_exclude_vars_as_features)]
        print(summary(union_imputed_df))
    
        require(mice)
        set.seed(glb_mice_complete.seed)
        union_imputed_df <- complete(mice(union_imputed_df))
        print(summary(union_imputed_df))
    
        union_df[, names(union_imputed_df)] <- union_imputed_df[, names(union_imputed_df)]
        print(summary(union_df))
#         union_df$.rownames <- rownames(union_df)
#         union_df <- orderBy(~.rownames, union_df)
#         
#         imp_entity_df <- myimport_data(
#             url="<imputed_trnng_url>", 
#             comment="imp_entity_df", force_header=TRUE, print_diagn=TRUE)
#         print(all.equal(subset(union_df, select=-c(.src, .rownames, .rnorm)), 
#                         imp_entity_df))
        
        # Partition again
        glb_trnent_df <<- subset(union_df, .src == "entity", select=-c(.src, .rownames))
        comment(glb_trnent_df) <- "entity_df"
        glb_newent_df <<- subset(union_df, .src == "newent", select=-c(.src, .rownames))
        comment(glb_newent_df) <- "newent_df"
        
        # Generate summaries
        print(summary(entity_df))
        print(sapply(names(entity_df), function(col) sum(is.na(entity_df[, col]))))
        print(summary(newent_df))
        print(sapply(names(newent_df), function(col) sum(is.na(newent_df[, col]))))
    
    } else stop("Not implemented yet")
}

if (glb_impute_na_data) {
    if ((sum(sapply(names(glb_trnent_df), 
                    function(col) sum(is.na(glb_trnent_df[, col])))) > 0) | 
        (sum(sapply(names(glb_newent_df), 
                    function(col) sum(is.na(glb_newent_df[, col])))) > 0))
        glb_impute_missing_data(glb_trnent_df, glb_newent_df)
}    

glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="encode_retype_data", 
        chunk_step_major=max(glb_script_df$chunk_step_major), 
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                  chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed3 manage_missing_data                2                2   1.479
## elapsed4  encode_retype_data                2                3   1.847
```

### Step `2`.`3`: encode/retype data

```r
# map_<col_name>_df <- myimport_data(
#     url="<map_url>", 
#     comment="map_<col_name>_df", print_diagn=TRUE)
# map_<col_name>_df <- read.csv(paste0(getwd(), "/data/<file_name>.csv"), strip.white=TRUE)

# glb_trnent_df <- mymap_codes(glb_trnent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
# glb_newent_df <- mymap_codes(glb_newent_df, "<from_col_name>", "<to_col_name>", 
#     map_<to_col_name>_df, map_join_col_name="<map_join_col_name>", 
#                           map_tgt_col_name="<to_col_name>")
    					
# glb_trnent_df$<col_name>.fctr <- factor(glb_trnent_df$<col_name>, 
#                     as.factor(union(glb_trnent_df$<col_name>, glb_newent_df$<col_name>)))
# glb_newent_df$<col_name>.fctr <- factor(glb_newent_df$<col_name>, 
#                     as.factor(union(glb_trnent_df$<col_name>, glb_newent_df$<col_name>)))

if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_entity_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_entity_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_entity_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    glb_trnent_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_trnent_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_trnent_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    glb_newent_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_newent_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_newent_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)    
}

glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="extract_features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                 chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed4 encode_retype_data                2                3   1.847
## elapsed5   extract_features                3                0   1.901
```

## Step `3`: extract features

```r
# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnent_df$<col_name>), -2, na.pad=TRUE)
# glb_trnent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newent_df$<col_name>), -2, na.pad=TRUE)
# glb_newent_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newent_df[1, "<col_name>.lag.2"] <- glb_trnent_df[nrow(glb_trnent_df) - 1, 
#                                                    "<col_name>"]
# glb_newent_df[2, "<col_name>.lag.2"] <- glb_trnent_df[nrow(glb_trnent_df), 
#                                                    "<col_name>"]
                                                   
# glb_trnent_df <- mutate(glb_trnent_df,
#     <new_col_name>=
#                     )

# glb_newent_df <- mutate(glb_newent_df,
#     <new_col_name>=
#                     )

# print(summary(glb_trnent_df))
# print(summary(glb_newent_df))

# print(sapply(names(glb_trnent_df), function(col) sum(is.na(glb_trnent_df[, col]))))
# print(sapply(names(glb_newent_df), function(col) sum(is.na(glb_newent_df[, col]))))

# print(myplot_scatter(glb_trnent_df, "<col1_name>", "<col2_name>", smooth=TRUE))

replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all","data.new")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0
```

![](USCensus1977_State_HW4_files/figure-html/extract_features-1.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="select_features", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##               chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed5 extract_features                3                0   1.901
## elapsed6  select_features                4                0   2.759
```

## Step `4`: select features

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnent_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                      id       cor.y exclude.as.feat
## Murder                           Murder -0.78084575               0
## Illiteracy                   Illiteracy -0.58847793               0
## HS.Grad                         HS.Grad  0.58221620               0
## state.region.fctr     state.region.fctr  0.56519612               0
## y                                     y  0.40665458               0
## Income                           Income  0.34025534               0
## Frost                             Frost  0.26206801               0
## x                                     x -0.24798347               0
## state.division.fctr state.division.fctr  0.19443418               0
## state.area                   state.area -0.10963169               0
## Area                               Area -0.10733194               0
## Population                   Population -0.06805195               0
## .rnorm                           .rnorm -0.04828783               0
##                      cor.y.abs
## Murder              0.78084575
## Illiteracy          0.58847793
## HS.Grad             0.58221620
## state.region.fctr   0.56519612
## y                   0.40665458
## Income              0.34025534
## Frost               0.26206801
## x                   0.24798347
## state.division.fctr 0.19443418
## state.area          0.10963169
## Area                0.10733194
## Population          0.06805195
## .rnorm              0.04828783
```

```r
glb_script_df <- rbind(glb_script_df, 
    data.frame(chunk_label="remove_correlated_features", 
        chunk_step_major=max(glb_script_df$chunk_step_major),
        chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))        
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed6            select_features                4                0
## elapsed7 remove_correlated_features                4                1
##          elapsed
## elapsed6   2.759
## elapsed7   2.946
```

### Step `4`.`1`: remove correlated features

```r
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, entity_df=glb_trnent_df, 
                                rsp_var=glb_rsp_var)))
```

```
## Loading required package: reshape2
```

```
##                           Murder  Illiteracy     HS.Grad state.region.fctr
## Murder               1.000000000  0.70297520 -0.48797102       -0.59295904
## Illiteracy           0.702975199  1.00000000 -0.65718861       -0.62575044
## HS.Grad             -0.487971022 -0.65718861  1.00000000        0.39211243
## state.region.fctr   -0.592959041 -0.62575044  0.39211243        1.00000000
## y                   -0.653627584 -0.73282587  0.50898691        0.59941585
## Income              -0.230077610 -0.43707519  0.61993232        0.34590348
## Frost               -0.538883437 -0.67194697  0.36677970        0.57560581
## x                   -0.009053187  0.09527077 -0.55617620        0.06285289
## state.division.fctr -0.332050043 -0.33794946 -0.07787476        0.62242930
## state.area           0.228794116  0.07986197  0.32988034       -0.08475413
## Area                 0.228390211  0.07726113  0.33354187       -0.08357567
## Population           0.343642751  0.10762237 -0.09848975        0.09838614
## .rnorm               0.128017677 -0.03048443  0.09964994        0.08496140
##                               y      Income      Frost            x
## Murder              -0.65362758 -0.23007761 -0.5388834 -0.009053187
## Illiteracy          -0.73282587 -0.43707519 -0.6719470  0.095270772
## HS.Grad              0.50898691  0.61993232  0.3667797 -0.556176202
## state.region.fctr    0.59941585  0.34590348  0.5756058  0.062852890
## y                    1.00000000  0.42654579  0.7128420 -0.079005795
## Income               0.42654579  1.00000000  0.2262822 -0.237573717
## Frost                0.71284199  0.22628218  1.0000000  0.119781966
## x                   -0.07900579 -0.23757372  0.1197820  1.000000000
## state.division.fctr  0.23697142  0.21457753  0.3604057  0.488809144
## state.area           0.18566965  0.36561813  0.0585676 -0.575345670
## Area                 0.18406742  0.36331544  0.0592291 -0.582648406
## Population          -0.17187501  0.20822756 -0.3321525  0.154625784
## .rnorm               0.06966797  0.09280812  0.1460988 -0.151795425
##                     state.division.fctr  state.area        Area
## Murder                      -0.33205004  0.22879412  0.22839021
## Illiteracy                  -0.33794946  0.07986197  0.07726113
## HS.Grad                     -0.07787476  0.32988034  0.33354187
## state.region.fctr            0.62242930 -0.08475413 -0.08357567
## y                            0.23697142  0.18566965  0.18406742
## Income                       0.21457753  0.36561813  0.36331544
## Frost                        0.36040574  0.05856760  0.05922910
## x                            0.48880914 -0.57534567 -0.58264841
## state.division.fctr          1.00000000 -0.28008380 -0.28335525
## state.area                  -0.28008380  1.00000000  0.99982464
## Area                        -0.28335525  0.99982464  1.00000000
## Population                   0.25629695  0.02156692  0.02254384
## .rnorm                      -0.07464503  0.24405698  0.24523598
##                      Population      .rnorm
## Murder               0.34364275  0.12801768
## Illiteracy           0.10762237 -0.03048443
## HS.Grad             -0.09848975  0.09964994
## state.region.fctr    0.09838614  0.08496140
## y                   -0.17187501  0.06966797
## Income               0.20822756  0.09280812
## Frost               -0.33215245  0.14609882
## x                    0.15462578 -0.15179543
## state.division.fctr  0.25629695 -0.07464503
## state.area           0.02156692  0.24405698
## Area                 0.02254384  0.24523598
## Population           1.00000000  0.02931449
## .rnorm               0.02931449  1.00000000
##                          Murder Illiteracy    HS.Grad state.region.fctr
## Murder              0.000000000 0.70297520 0.48797102        0.59295904
## Illiteracy          0.702975199 0.00000000 0.65718861        0.62575044
## HS.Grad             0.487971022 0.65718861 0.00000000        0.39211243
## state.region.fctr   0.592959041 0.62575044 0.39211243        0.00000000
## y                   0.653627584 0.73282587 0.50898691        0.59941585
## Income              0.230077610 0.43707519 0.61993232        0.34590348
## Frost               0.538883437 0.67194697 0.36677970        0.57560581
## x                   0.009053187 0.09527077 0.55617620        0.06285289
## state.division.fctr 0.332050043 0.33794946 0.07787476        0.62242930
## state.area          0.228794116 0.07986197 0.32988034        0.08475413
## Area                0.228390211 0.07726113 0.33354187        0.08357567
## Population          0.343642751 0.10762237 0.09848975        0.09838614
## .rnorm              0.128017677 0.03048443 0.09964994        0.08496140
##                              y     Income     Frost           x
## Murder              0.65362758 0.23007761 0.5388834 0.009053187
## Illiteracy          0.73282587 0.43707519 0.6719470 0.095270772
## HS.Grad             0.50898691 0.61993232 0.3667797 0.556176202
## state.region.fctr   0.59941585 0.34590348 0.5756058 0.062852890
## y                   0.00000000 0.42654579 0.7128420 0.079005795
## Income              0.42654579 0.00000000 0.2262822 0.237573717
## Frost               0.71284199 0.22628218 0.0000000 0.119781966
## x                   0.07900579 0.23757372 0.1197820 0.000000000
## state.division.fctr 0.23697142 0.21457753 0.3604057 0.488809144
## state.area          0.18566965 0.36561813 0.0585676 0.575345670
## Area                0.18406742 0.36331544 0.0592291 0.582648406
## Population          0.17187501 0.20822756 0.3321525 0.154625784
## .rnorm              0.06966797 0.09280812 0.1460988 0.151795425
##                     state.division.fctr state.area       Area Population
## Murder                       0.33205004 0.22879412 0.22839021 0.34364275
## Illiteracy                   0.33794946 0.07986197 0.07726113 0.10762237
## HS.Grad                      0.07787476 0.32988034 0.33354187 0.09848975
## state.region.fctr            0.62242930 0.08475413 0.08357567 0.09838614
## y                            0.23697142 0.18566965 0.18406742 0.17187501
## Income                       0.21457753 0.36561813 0.36331544 0.20822756
## Frost                        0.36040574 0.05856760 0.05922910 0.33215245
## x                            0.48880914 0.57534567 0.58264841 0.15462578
## state.division.fctr          0.00000000 0.28008380 0.28335525 0.25629695
## state.area                   0.28008380 0.00000000 0.99982464 0.02156692
## Area                         0.28335525 0.99982464 0.00000000 0.02254384
## Population                   0.25629695 0.02156692 0.02254384 0.00000000
## .rnorm                       0.07464503 0.24405698 0.24523598 0.02931449
##                         .rnorm
## Murder              0.12801768
## Illiteracy          0.03048443
## HS.Grad             0.09964994
## state.region.fctr   0.08496140
## y                   0.06966797
## Income              0.09280812
## Frost               0.14609882
## x                   0.15179543
## state.division.fctr 0.07464503
## state.area          0.24405698
## Area                0.24523598
## Population          0.02931449
## .rnorm              0.00000000
## [1] "cor(state.area, Area)=0.9998"
```

![](USCensus1977_State_HW4_files/figure-html/remove_correlated_features-1.png) 

```
## [1] "cor(Life.Exp, state.area)=-0.1096"
## [1] "cor(Life.Exp, Area)=-0.1073"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified Area as highly correlated with other features
```

![](USCensus1977_State_HW4_files/figure-html/remove_correlated_features-2.png) 

```
## [1] "checking correlations for features:"
##  [1] "Murder"              "Illiteracy"          "HS.Grad"            
##  [4] "state.region.fctr"   "y"                   "Income"             
##  [7] "Frost"               "x"                   "state.division.fctr"
## [10] "state.area"          "Population"          ".rnorm"             
##                           Murder  Illiteracy     HS.Grad state.region.fctr
## Murder               1.000000000  0.70297520 -0.48797102       -0.59295904
## Illiteracy           0.702975199  1.00000000 -0.65718861       -0.62575044
## HS.Grad             -0.487971022 -0.65718861  1.00000000        0.39211243
## state.region.fctr   -0.592959041 -0.62575044  0.39211243        1.00000000
## y                   -0.653627584 -0.73282587  0.50898691        0.59941585
## Income              -0.230077610 -0.43707519  0.61993232        0.34590348
## Frost               -0.538883437 -0.67194697  0.36677970        0.57560581
## x                   -0.009053187  0.09527077 -0.55617620        0.06285289
## state.division.fctr -0.332050043 -0.33794946 -0.07787476        0.62242930
## state.area           0.228794116  0.07986197  0.32988034       -0.08475413
## Population           0.343642751  0.10762237 -0.09848975        0.09838614
## .rnorm               0.128017677 -0.03048443  0.09964994        0.08496140
##                               y      Income      Frost            x
## Murder              -0.65362758 -0.23007761 -0.5388834 -0.009053187
## Illiteracy          -0.73282587 -0.43707519 -0.6719470  0.095270772
## HS.Grad              0.50898691  0.61993232  0.3667797 -0.556176202
## state.region.fctr    0.59941585  0.34590348  0.5756058  0.062852890
## y                    1.00000000  0.42654579  0.7128420 -0.079005795
## Income               0.42654579  1.00000000  0.2262822 -0.237573717
## Frost                0.71284199  0.22628218  1.0000000  0.119781966
## x                   -0.07900579 -0.23757372  0.1197820  1.000000000
## state.division.fctr  0.23697142  0.21457753  0.3604057  0.488809144
## state.area           0.18566965  0.36561813  0.0585676 -0.575345670
## Population          -0.17187501  0.20822756 -0.3321525  0.154625784
## .rnorm               0.06966797  0.09280812  0.1460988 -0.151795425
##                     state.division.fctr  state.area  Population
## Murder                      -0.33205004  0.22879412  0.34364275
## Illiteracy                  -0.33794946  0.07986197  0.10762237
## HS.Grad                     -0.07787476  0.32988034 -0.09848975
## state.region.fctr            0.62242930 -0.08475413  0.09838614
## y                            0.23697142  0.18566965 -0.17187501
## Income                       0.21457753  0.36561813  0.20822756
## Frost                        0.36040574  0.05856760 -0.33215245
## x                            0.48880914 -0.57534567  0.15462578
## state.division.fctr          1.00000000 -0.28008380  0.25629695
## state.area                  -0.28008380  1.00000000  0.02156692
## Population                   0.25629695  0.02156692  1.00000000
## .rnorm                      -0.07464503  0.24405698  0.02931449
##                          .rnorm
## Murder               0.12801768
## Illiteracy          -0.03048443
## HS.Grad              0.09964994
## state.region.fctr    0.08496140
## y                    0.06966797
## Income               0.09280812
## Frost                0.14609882
## x                   -0.15179543
## state.division.fctr -0.07464503
## state.area           0.24405698
## Population           0.02931449
## .rnorm               1.00000000
##                          Murder Illiteracy    HS.Grad state.region.fctr
## Murder              0.000000000 0.70297520 0.48797102        0.59295904
## Illiteracy          0.702975199 0.00000000 0.65718861        0.62575044
## HS.Grad             0.487971022 0.65718861 0.00000000        0.39211243
## state.region.fctr   0.592959041 0.62575044 0.39211243        0.00000000
## y                   0.653627584 0.73282587 0.50898691        0.59941585
## Income              0.230077610 0.43707519 0.61993232        0.34590348
## Frost               0.538883437 0.67194697 0.36677970        0.57560581
## x                   0.009053187 0.09527077 0.55617620        0.06285289
## state.division.fctr 0.332050043 0.33794946 0.07787476        0.62242930
## state.area          0.228794116 0.07986197 0.32988034        0.08475413
## Population          0.343642751 0.10762237 0.09848975        0.09838614
## .rnorm              0.128017677 0.03048443 0.09964994        0.08496140
##                              y     Income     Frost           x
## Murder              0.65362758 0.23007761 0.5388834 0.009053187
## Illiteracy          0.73282587 0.43707519 0.6719470 0.095270772
## HS.Grad             0.50898691 0.61993232 0.3667797 0.556176202
## state.region.fctr   0.59941585 0.34590348 0.5756058 0.062852890
## y                   0.00000000 0.42654579 0.7128420 0.079005795
## Income              0.42654579 0.00000000 0.2262822 0.237573717
## Frost               0.71284199 0.22628218 0.0000000 0.119781966
## x                   0.07900579 0.23757372 0.1197820 0.000000000
## state.division.fctr 0.23697142 0.21457753 0.3604057 0.488809144
## state.area          0.18566965 0.36561813 0.0585676 0.575345670
## Population          0.17187501 0.20822756 0.3321525 0.154625784
## .rnorm              0.06966797 0.09280812 0.1460988 0.151795425
##                     state.division.fctr state.area Population     .rnorm
## Murder                       0.33205004 0.22879412 0.34364275 0.12801768
## Illiteracy                   0.33794946 0.07986197 0.10762237 0.03048443
## HS.Grad                      0.07787476 0.32988034 0.09848975 0.09964994
## state.region.fctr            0.62242930 0.08475413 0.09838614 0.08496140
## y                            0.23697142 0.18566965 0.17187501 0.06966797
## Income                       0.21457753 0.36561813 0.20822756 0.09280812
## Frost                        0.36040574 0.05856760 0.33215245 0.14609882
## x                            0.48880914 0.57534567 0.15462578 0.15179543
## state.division.fctr          0.00000000 0.28008380 0.25629695 0.07464503
## state.area                   0.28008380 0.00000000 0.02156692 0.24405698
## Population                   0.25629695 0.02156692 0.00000000 0.02931449
## .rnorm                       0.07464503 0.24405698 0.02931449 0.00000000
## [1] "cor(Illiteracy, y)=-0.7328"
```

![](USCensus1977_State_HW4_files/figure-html/remove_correlated_features-3.png) 

```
## [1] "cor(Life.Exp, Illiteracy)=-0.5885"
## [1] "cor(Life.Exp, y)=0.4067"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified y as highly correlated with other features
```

![](USCensus1977_State_HW4_files/figure-html/remove_correlated_features-4.png) 

```
## [1] "checking correlations for features:"
##  [1] "Murder"              "Illiteracy"          "HS.Grad"            
##  [4] "state.region.fctr"   "Income"              "Frost"              
##  [7] "x"                   "state.division.fctr" "state.area"         
## [10] "Population"          ".rnorm"             
##                           Murder  Illiteracy     HS.Grad state.region.fctr
## Murder               1.000000000  0.70297520 -0.48797102       -0.59295904
## Illiteracy           0.702975199  1.00000000 -0.65718861       -0.62575044
## HS.Grad             -0.487971022 -0.65718861  1.00000000        0.39211243
## state.region.fctr   -0.592959041 -0.62575044  0.39211243        1.00000000
## Income              -0.230077610 -0.43707519  0.61993232        0.34590348
## Frost               -0.538883437 -0.67194697  0.36677970        0.57560581
## x                   -0.009053187  0.09527077 -0.55617620        0.06285289
## state.division.fctr -0.332050043 -0.33794946 -0.07787476        0.62242930
## state.area           0.228794116  0.07986197  0.32988034       -0.08475413
## Population           0.343642751  0.10762237 -0.09848975        0.09838614
## .rnorm               0.128017677 -0.03048443  0.09964994        0.08496140
##                          Income      Frost            x
## Murder              -0.23007761 -0.5388834 -0.009053187
## Illiteracy          -0.43707519 -0.6719470  0.095270772
## HS.Grad              0.61993232  0.3667797 -0.556176202
## state.region.fctr    0.34590348  0.5756058  0.062852890
## Income               1.00000000  0.2262822 -0.237573717
## Frost                0.22628218  1.0000000  0.119781966
## x                   -0.23757372  0.1197820  1.000000000
## state.division.fctr  0.21457753  0.3604057  0.488809144
## state.area           0.36561813  0.0585676 -0.575345670
## Population           0.20822756 -0.3321525  0.154625784
## .rnorm               0.09280812  0.1460988 -0.151795425
##                     state.division.fctr  state.area  Population
## Murder                      -0.33205004  0.22879412  0.34364275
## Illiteracy                  -0.33794946  0.07986197  0.10762237
## HS.Grad                     -0.07787476  0.32988034 -0.09848975
## state.region.fctr            0.62242930 -0.08475413  0.09838614
## Income                       0.21457753  0.36561813  0.20822756
## Frost                        0.36040574  0.05856760 -0.33215245
## x                            0.48880914 -0.57534567  0.15462578
## state.division.fctr          1.00000000 -0.28008380  0.25629695
## state.area                  -0.28008380  1.00000000  0.02156692
## Population                   0.25629695  0.02156692  1.00000000
## .rnorm                      -0.07464503  0.24405698  0.02931449
##                          .rnorm
## Murder               0.12801768
## Illiteracy          -0.03048443
## HS.Grad              0.09964994
## state.region.fctr    0.08496140
## Income               0.09280812
## Frost                0.14609882
## x                   -0.15179543
## state.division.fctr -0.07464503
## state.area           0.24405698
## Population           0.02931449
## .rnorm               1.00000000
##                          Murder Illiteracy    HS.Grad state.region.fctr
## Murder              0.000000000 0.70297520 0.48797102        0.59295904
## Illiteracy          0.702975199 0.00000000 0.65718861        0.62575044
## HS.Grad             0.487971022 0.65718861 0.00000000        0.39211243
## state.region.fctr   0.592959041 0.62575044 0.39211243        0.00000000
## Income              0.230077610 0.43707519 0.61993232        0.34590348
## Frost               0.538883437 0.67194697 0.36677970        0.57560581
## x                   0.009053187 0.09527077 0.55617620        0.06285289
## state.division.fctr 0.332050043 0.33794946 0.07787476        0.62242930
## state.area          0.228794116 0.07986197 0.32988034        0.08475413
## Population          0.343642751 0.10762237 0.09848975        0.09838614
## .rnorm              0.128017677 0.03048443 0.09964994        0.08496140
##                         Income     Frost           x state.division.fctr
## Murder              0.23007761 0.5388834 0.009053187          0.33205004
## Illiteracy          0.43707519 0.6719470 0.095270772          0.33794946
## HS.Grad             0.61993232 0.3667797 0.556176202          0.07787476
## state.region.fctr   0.34590348 0.5756058 0.062852890          0.62242930
## Income              0.00000000 0.2262822 0.237573717          0.21457753
## Frost               0.22628218 0.0000000 0.119781966          0.36040574
## x                   0.23757372 0.1197820 0.000000000          0.48880914
## state.division.fctr 0.21457753 0.3604057 0.488809144          0.00000000
## state.area          0.36561813 0.0585676 0.575345670          0.28008380
## Population          0.20822756 0.3321525 0.154625784          0.25629695
## .rnorm              0.09280812 0.1460988 0.151795425          0.07464503
##                     state.area Population     .rnorm
## Murder              0.22879412 0.34364275 0.12801768
## Illiteracy          0.07986197 0.10762237 0.03048443
## HS.Grad             0.32988034 0.09848975 0.09964994
## state.region.fctr   0.08475413 0.09838614 0.08496140
## Income              0.36561813 0.20822756 0.09280812
## Frost               0.05856760 0.33215245 0.14609882
## x                   0.57534567 0.15462578 0.15179543
## state.division.fctr 0.28008380 0.25629695 0.07464503
## state.area          0.00000000 0.02156692 0.24405698
## Population          0.02156692 0.00000000 0.02931449
## .rnorm              0.24405698 0.02931449 0.00000000
## [1] "cor(Murder, Illiteracy)=0.7030"
```

![](USCensus1977_State_HW4_files/figure-html/remove_correlated_features-5.png) 

```
## [1] "cor(Life.Exp, Murder)=-0.7808"
## [1] "cor(Life.Exp, Illiteracy)=-0.5885"
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, entity_df =
## glb_trnent_df, : Identified Illiteracy as highly correlated with other
## features
```

![](USCensus1977_State_HW4_files/figure-html/remove_correlated_features-6.png) 

```
## [1] "checking correlations for features:"
##  [1] "Murder"              "HS.Grad"             "state.region.fctr"  
##  [4] "Income"              "Frost"               "x"                  
##  [7] "state.division.fctr" "state.area"          "Population"         
## [10] ".rnorm"             
##                           Murder     HS.Grad state.region.fctr      Income
## Murder               1.000000000 -0.48797102       -0.59295904 -0.23007761
## HS.Grad             -0.487971022  1.00000000        0.39211243  0.61993232
## state.region.fctr   -0.592959041  0.39211243        1.00000000  0.34590348
## Income              -0.230077610  0.61993232        0.34590348  1.00000000
## Frost               -0.538883437  0.36677970        0.57560581  0.22628218
## x                   -0.009053187 -0.55617620        0.06285289 -0.23757372
## state.division.fctr -0.332050043 -0.07787476        0.62242930  0.21457753
## state.area           0.228794116  0.32988034       -0.08475413  0.36561813
## Population           0.343642751 -0.09848975        0.09838614  0.20822756
## .rnorm               0.128017677  0.09964994        0.08496140  0.09280812
##                          Frost            x state.division.fctr
## Murder              -0.5388834 -0.009053187         -0.33205004
## HS.Grad              0.3667797 -0.556176202         -0.07787476
## state.region.fctr    0.5756058  0.062852890          0.62242930
## Income               0.2262822 -0.237573717          0.21457753
## Frost                1.0000000  0.119781966          0.36040574
## x                    0.1197820  1.000000000          0.48880914
## state.division.fctr  0.3604057  0.488809144          1.00000000
## state.area           0.0585676 -0.575345670         -0.28008380
## Population          -0.3321525  0.154625784          0.25629695
## .rnorm               0.1460988 -0.151795425         -0.07464503
##                      state.area  Population      .rnorm
## Murder               0.22879412  0.34364275  0.12801768
## HS.Grad              0.32988034 -0.09848975  0.09964994
## state.region.fctr   -0.08475413  0.09838614  0.08496140
## Income               0.36561813  0.20822756  0.09280812
## Frost                0.05856760 -0.33215245  0.14609882
## x                   -0.57534567  0.15462578 -0.15179543
## state.division.fctr -0.28008380  0.25629695 -0.07464503
## state.area           1.00000000  0.02156692  0.24405698
## Population           0.02156692  1.00000000  0.02931449
## .rnorm               0.24405698  0.02931449  1.00000000
##                          Murder    HS.Grad state.region.fctr     Income
## Murder              0.000000000 0.48797102        0.59295904 0.23007761
## HS.Grad             0.487971022 0.00000000        0.39211243 0.61993232
## state.region.fctr   0.592959041 0.39211243        0.00000000 0.34590348
## Income              0.230077610 0.61993232        0.34590348 0.00000000
## Frost               0.538883437 0.36677970        0.57560581 0.22628218
## x                   0.009053187 0.55617620        0.06285289 0.23757372
## state.division.fctr 0.332050043 0.07787476        0.62242930 0.21457753
## state.area          0.228794116 0.32988034        0.08475413 0.36561813
## Population          0.343642751 0.09848975        0.09838614 0.20822756
## .rnorm              0.128017677 0.09964994        0.08496140 0.09280812
##                         Frost           x state.division.fctr state.area
## Murder              0.5388834 0.009053187          0.33205004 0.22879412
## HS.Grad             0.3667797 0.556176202          0.07787476 0.32988034
## state.region.fctr   0.5756058 0.062852890          0.62242930 0.08475413
## Income              0.2262822 0.237573717          0.21457753 0.36561813
## Frost               0.0000000 0.119781966          0.36040574 0.05856760
## x                   0.1197820 0.000000000          0.48880914 0.57534567
## state.division.fctr 0.3604057 0.488809144          0.00000000 0.28008380
## state.area          0.0585676 0.575345670          0.28008380 0.00000000
## Population          0.3321525 0.154625784          0.25629695 0.02156692
## .rnorm              0.1460988 0.151795425          0.07464503 0.24405698
##                     Population     .rnorm
## Murder              0.34364275 0.12801768
## HS.Grad             0.09848975 0.09964994
## state.region.fctr   0.09838614 0.08496140
## Income              0.20822756 0.09280812
## Frost               0.33215245 0.14609882
## x                   0.15462578 0.15179543
## state.division.fctr 0.25629695 0.07464503
## state.area          0.02156692 0.24405698
## Population          0.00000000 0.02931449
## .rnorm              0.02931449 0.00000000
##                                      id       cor.y exclude.as.feat
## HS.Grad                         HS.Grad  0.58221620               0
## state.region.fctr     state.region.fctr  0.56519612               0
## y                                     y  0.40665458               0
## Income                           Income  0.34025534               0
## Frost                             Frost  0.26206801               0
## state.division.fctr state.division.fctr  0.19443418               0
## .rnorm                           .rnorm -0.04828783               0
## Population                   Population -0.06805195               0
## Area                               Area -0.10733194               0
## state.area                   state.area -0.10963169               0
## x                                     x -0.24798347               0
## Illiteracy                   Illiteracy -0.58847793               0
## Murder                           Murder -0.78084575               0
##                      cor.y.abs cor.low
## HS.Grad             0.58221620       1
## state.region.fctr   0.56519612       1
## y                   0.40665458       0
## Income              0.34025534       1
## Frost               0.26206801       1
## state.division.fctr 0.19443418       1
## .rnorm              0.04828783       1
## Population          0.06805195       1
## Area                0.10733194       0
## state.area          0.10963169       1
## x                   0.24798347       1
## Illiteracy          0.58847793       0
## Murder              0.78084575       1
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.models", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                         chunk_label chunk_step_major chunk_step_minor
## elapsed7 remove_correlated_features                4                1
## elapsed8                 fit.models                5                0
##          elapsed
## elapsed7   2.946
## elapsed8   5.233
```

## Step `5`: fit models

```r
if (glb_is_classification && glb_is_binomial && (length(unique(glb_trnent_df[, glb_rsp_var])) < 2))
    stop("glb_trnent_df$", glb_rsp_var, ": contains less than 2 unique values: ", paste0(unique(glb_trnent_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_var <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & (cor.low == 1)))[1, "id"]
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_var != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_var, "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_var, " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Any models that have tuning parameters has "better" results with cross-validation (except rf)
#   & "different" results for different outcome metrics

# Baseline
if (!is.null(glb_Baseline_mdl_var)) {
#     lm_mdl <- lm(reformulate(glb_Baseline_mdl_var, 
#                             response="bucket2009"), data=glb_trnent_df)
#     print(summary(lm_mdl))
#     plot(lm_mdl, ask=FALSE)
#     ret_lst <- myfit_mdl_fn(model_id="Baseline", 
#                             model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                             indep_vars_vctr=glb_Baseline_mdl_var,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_trnent_df, OOB_df=glb_newent_df,
#                             n_cv_folds=0, tune_models_df=NULL,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_trnent_df, OOB_df=glb_newent_df)
}

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_trnent_df, OOB_df=glb_newent_df)
```

```
## Loading required package: caret
## Loading required package: lattice
## 
## Attaching package: 'caret'
## 
## The following object is masked from 'package:survival':
## 
##     cluster
```

```
## [1] "fitting model: MFO.lm"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-1.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-2.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-3.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.9996 -0.7087 -0.1373  1.0939  2.6467 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 70.89089    0.19507 363.415   <2e-16 ***
## .rnorm      -0.05945    0.17751  -0.335    0.739    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.355 on 48 degrees of freedom
## Multiple R-squared:  0.002332,	Adjusted R-squared:  -0.01845 
## F-statistic: 0.1122 on 1 and 48 DF,  p-value: 0.7391
## 
##   model_id model_method  feats max.nTuningRuns min.elapsedtime.everything
## 1   MFO.lm           lm .rnorm               0                      0.617
##   min.elapsedtime.final max.R.sq.fit min.RMSE.fit max.R.sq.OOB
## 1                 0.002  0.002331715     1.327352   -0.0197079
##   min.RMSE.OOB max.Adj.R.sq.fit
## 1     1.341933      -0.01845304
```

```r
if (glb_is_classification)
    # "random" model - only for classification; none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_trnent_df, OOB_df=glb_newent_df)

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_trnent_df, OOB_df=glb_newent_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: Murder"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.479 on full training set
```

```
## Loading required package: rpart.plot
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 50 
## 
##        CP nsplit rel error
## 1 0.47911      0         1
## 
## Node number 1: 50 observations
##   mean=70.8786, MSE=1.76598 
## 
## n= 50 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 50 88.299 70.8786 *
##               model_id model_method  feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart Murder               0
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.483                 0.007            0
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1     1.328902            0     1.328902
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                        n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: Murder"
## Fitting cp = 0 on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 50 
## 
##           CP nsplit rel error
## 1 0.47910997      0 1.0000000
## 2 0.14868323      1 0.5208900
## 3 0.04379453      2 0.3722068
## 4 0.00000000      3 0.3284123
## 
## Variable importance
## Murder 
##    100 
## 
## Node number 1: 50 observations,    complexity param=0.47911
##   mean=70.8786, MSE=1.76598 
##   left son=2 (27 obs) right son=3 (23 obs)
##   Primary splits:
##       Murder < 6.55 to the right, improve=0.47911, (0 missing)
## 
## Node number 2: 27 observations,    complexity param=0.1486832
##   mean=70.02963, MSE=0.989848 
##   left son=4 (8 obs) right son=5 (19 obs)
##   Primary splits:
##       Murder < 11.2 to the right, improve=0.4912307, (0 missing)
## 
## Node number 3: 23 observations,    complexity param=0.04379453
##   mean=71.87522, MSE=0.8377467 
##   left son=6 (8 obs) right son=7 (15 obs)
##   Primary splits:
##       Murder < 4.75 to the right, improve=0.2006943, (0 missing)
## 
## Node number 4: 8 observations
##   mean=68.955, MSE=0.734025 
## 
## Node number 5: 19 observations
##   mean=70.48211, MSE=0.406585 
## 
## Node number 6: 8 observations
##   mean=71.31375, MSE=1.090298 
## 
## Node number 7: 15 observations
##   mean=72.17467, MSE=0.4452516 
## 
## n= 50 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 50 88.299000 70.87860  
##   2) Murder>=6.55 27 26.725900 70.02963  
##     4) Murder>=11.2 8  5.872200 68.95500 *
##     5) Murder< 11.2 19  7.725116 70.48211 *
##   3) Murder< 6.55 23 19.268170 71.87522  
##     6) Murder>=4.75 8  8.722387 71.31375 *
##     7) Murder< 4.75 15  6.678773 72.17467 *
##                    model_id model_method  feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart Murder               0
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.429                 0.006    0.6715877
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1    0.7615573    0.6715877    0.7615573
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: Murder"
## + Fold1: cp=0.04379 
## - Fold1: cp=0.04379 
## + Fold2: cp=0.04379 
## - Fold2: cp=0.04379 
## + Fold3: cp=0.04379 
## - Fold3: cp=0.04379 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0438 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-7.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-8.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 50 
## 
##           CP nsplit rel error
## 1 0.47910997      0 1.0000000
## 2 0.14868323      1 0.5208900
## 3 0.04379453      2 0.3722068
## 
## Variable importance
## Murder 
##    100 
## 
## Node number 1: 50 observations,    complexity param=0.47911
##   mean=70.8786, MSE=1.76598 
##   left son=2 (27 obs) right son=3 (23 obs)
##   Primary splits:
##       Murder < 6.55 to the right, improve=0.47911, (0 missing)
## 
## Node number 2: 27 observations,    complexity param=0.1486832
##   mean=70.02963, MSE=0.989848 
##   left son=4 (8 obs) right son=5 (19 obs)
##   Primary splits:
##       Murder < 11.2 to the right, improve=0.4912307, (0 missing)
## 
## Node number 3: 23 observations
##   mean=71.87522, MSE=0.8377467 
## 
## Node number 4: 8 observations
##   mean=68.955, MSE=0.734025 
## 
## Node number 5: 19 observations
##   mean=70.48211, MSE=0.406585 
## 
## n= 50 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 50 88.299000 70.87860  
##   2) Murder>=6.55 27 26.725900 70.02963  
##     4) Murder>=11.2 8  5.872200 68.95500 *
##     5) Murder< 11.2 19  7.725116 70.48211 *
##   3) Murder< 6.55 23 19.268170 71.87522 *
##          model_id model_method  feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart Murder               3
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.819                 0.006    0.6277932
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit min.RMSESD.fit
## 1     1.048316    0.6277932    0.8107464        0.4087926     0.08998812
##   max.RsquaredSD.fit
## 1          0.1741142
```

```r
# Used to compare vs. Interactions.High.cor.Y 
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_var,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.lm"
## [1] "    indep_vars: Murder"
## + Fold1: parameter=none 
## - Fold1: parameter=none 
## + Fold2: parameter=none 
## - Fold2: parameter=none 
## + Fold3: parameter=none 
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-9.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-10.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-11.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-12.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.81690 -0.48139  0.09591  0.39769  2.38691 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 72.97356    0.26997  270.30  < 2e-16 ***
## Murder      -0.28395    0.03279   -8.66 2.26e-11 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8473 on 48 degrees of freedom
## Multiple R-squared:  0.6097,	Adjusted R-squared:  0.6016 
## F-statistic: 74.99 on 1 and 48 DF,  p-value: 2.26e-11
## 
##       model_id model_method  feats max.nTuningRuns
## 1 Max.cor.Y.lm           lm Murder               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.721                 0.002    0.6097201
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit max.Rsquared.fit
## 1    0.8553751    0.6097201    0.8301967        0.6015893        0.6279054
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.1665302          0.1027377
```

```r
# Interactions.High.cor.Y
if (nrow(int_feats_df <- subset(glb_feats_df, (cor.low == 0) & 
                                              (exclude.as.feat == 0))) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    #   This does not work - why ???
#     indep_vars_vctr <- ifelse(glb_is_binomial, 
#         c(max_cor_y_x_var, paste(max_cor_y_x_var, 
#                         subset(glb_feats_df, is.na(cor.low))[, "id"], sep=":")),
#         union(max_cor_y_x_var, subset(glb_feats_df, is.na(cor.low))[, "id"]))
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_var, paste(max_cor_y_x_var, int_feats_df[, "id"], sep=":"))       
    } else { indep_vars_vctr <- union(max_cor_y_x_var, int_feats_df[, "id"]) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.y.lm"
## [1] "    indep_vars: Murder, Murder:y, Murder:Area, Murder:Illiteracy"
## + Fold1: parameter=none 
## - Fold1: parameter=none 
## + Fold2: parameter=none 
## - Fold2: parameter=none 
## + Fold3: parameter=none 
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-13.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-14.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-15.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.71746 -0.44965  0.04437  0.42027  2.32769 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)          7.303e+01  3.106e-01 235.147   <2e-16 ***
## Murder               1.991e-01  1.983e-01   1.004   0.3207    
## `Murder:y`          -1.093e-02  4.625e-03  -2.364   0.0224 *  
## `Murder:Area`        2.801e-07  1.552e-07   1.805   0.0778 .  
## `Murder:Illiteracy` -7.221e-02  3.253e-02  -2.220   0.0315 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.8151 on 45 degrees of freedom
## Multiple R-squared:  0.6614,	Adjusted R-squared:  0.6313 
## F-statistic: 21.98 on 4 and 45 DF,  p-value: 4.152e-10
## 
##                 model_id model_method
## 1 Interact.High.cor.y.lm           lm
##                                              feats max.nTuningRuns
## 1 Murder, Murder:y, Murder:Area, Murder:Illiteracy               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.715                 0.002    0.6614201
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit max.Rsquared.fit
## 1    0.9644864    0.6614201    0.7732563        0.6313241        0.5180279
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.1352115         0.05523021
```

```r
# Low.cor.X
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=subset(glb_feats_df, cor.low == 1)[, "id"],
                         model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.lm"
## [1] "    indep_vars: HS.Grad, state.region.fctr, Income, Frost, state.division.fctr, .rnorm, Population, state.area, x, Murder"
## + Fold1: parameter=none
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-17.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-18.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-19.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.06750 -0.37853  0.01932  0.40457  1.22850 
## 
## Coefficients: (3 not defined because of singularities)
##                                           Estimate Std. Error t value
## (Intercept)                              6.749e+01  3.463e+00  19.492
## HS.Grad                                  3.123e-02  3.004e-02   1.040
## state.region.fctrWest                   -8.070e-01  1.066e+00  -0.757
## state.region.fctrNortheast              -1.049e+00  8.416e-01  -1.246
## `state.region.fctrNorth Central`        -3.830e-02  7.541e-01  -0.051
## Income                                   3.724e-04  2.771e-04   1.344
## Frost                                   -4.133e-03  3.658e-03  -1.130
## state.division.fctrPacific              -4.429e-01  6.910e-01  -0.641
## state.division.fctrMountain                     NA         NA      NA
## `state.division.fctrWest South Central`  1.266e-01  5.922e-01   0.214
## `state.division.fctrNew England`         9.349e-01  6.320e-01   1.479
## `state.division.fctrSouth Atlantic`     -7.454e-01  5.506e-01  -1.354
## `state.division.fctrEast North Central` -3.752e-01  5.762e-01  -0.651
## `state.division.fctrWest North Central`         NA         NA      NA
## `state.division.fctrMiddle Atlantic`            NA         NA      NA
## .rnorm                                  -3.349e-02  9.985e-02  -0.335
## Population                               7.477e-05  3.545e-05   2.109
## state.area                              -2.850e-06  1.818e-06  -1.567
## x                                       -3.128e-02  3.720e-02  -0.841
## Murder                                  -2.714e-01  5.304e-02  -5.116
##                                         Pr(>|t|)    
## (Intercept)                              < 2e-16 ***
## HS.Grad                                   0.3060    
## state.region.fctrWest                     0.4544    
## state.region.fctrNortheast                0.2214    
## `state.region.fctrNorth Central`          0.9598    
## Income                                    0.1880    
## Frost                                     0.2667    
## state.division.fctrPacific                0.5260    
## state.division.fctrMountain                   NA    
## `state.division.fctrWest South Central`   0.8321    
## `state.division.fctrNew England`          0.1485    
## `state.division.fctrSouth Atlantic`       0.1850    
## `state.division.fctrEast North Central`   0.5195    
## `state.division.fctrWest North Central`       NA    
## `state.division.fctrMiddle Atlantic`          NA    
## .rnorm                                    0.7395    
## Population                                0.0426 *  
## state.area                                0.1266    
## x                                         0.4065    
## Murder                                  1.31e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6768 on 33 degrees of freedom
## Multiple R-squared:  0.8288,	Adjusted R-squared:  0.7458 
## F-statistic: 9.986 on 16 and 33 DF,  p-value: 1.92e-08
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-20.png) 

```
##       model_id model_method
## 1 Low.cor.X.lm           lm
##                                                                                                       feats
## 1 HS.Grad, state.region.fctr, Income, Frost, state.division.fctr, .rnorm, Population, state.area, x, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.727                 0.006
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.8288182    0.7679484    0.8248221    0.5562021         0.745821
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.7129405       0.125369         0.07850497
```

```r
# User specified
for (method in glb_models_method_vctr) {
    print(sprintf("iterating over method:%s", method))

    # All X that is not user excluded
    indep_vars_vctr <- setdiff(names(glb_trnent_df), 
        union(glb_rsp_var, glb_exclude_vars_as_features))
    
    # easier to exclude features
#     indep_vars_vctr <- setdiff(names(glb_trnent_df), 
#         union(union(glb_rsp_var, glb_exclude_vars_as_features), 
#               c("<feat1_name>", "<feat2_name>")))
    
    # easier to include features
#     indep_vars_vctr <- c("<feat1_name>", "<feat2_name>")

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_trnent_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]

#     glb_sel_mdl <- glb_sel_wlm_mdl <- ret_lst[["model"]]
#     rpart_sel_wlm_mdl <- rpart(reformulate(indep_vars_vctr, response=glb_rsp_var), 
#                                data=glb_trnent_df, method="class", 
#                                control=rpart.control(cp=glb_sel_wlm_mdl$bestTune$cp),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 
    model_id_pfx <- "All.X";
    ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ""), model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # Since caret does not optimize rpart well
    if (method == "rpart")
        ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_trnent_df, OOB_df=glb_newent_df,        
            n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
    
    # Compare how rf performs w/i & w/o .rnorm
    if (method == "rf")
        ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".no.rnorm"), model_method=method,
                                indep_vars_vctr=setdiff(indep_vars_vctr, c(".rnorm")),
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_trnent_df, OOB_df=glb_newent_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #   only for OOB in trainControl ?

#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_trnent_df, OOB_df=glb_newent_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)
}
```

```
## [1] "iterating over method:lm"
## [1] "fitting model: All.X.lm"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm"
## + Fold1: parameter=none
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-21.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-22.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-23.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.0260 -0.3662  0.0486  0.4035  1.0910 
## 
## Coefficients: (3 not defined because of singularities)
##                                           Estimate Std. Error t value
## (Intercept)                              7.268e+01  5.024e+00  14.467
## Population                               8.892e-05  3.884e-05   2.290
## Income                                   3.801e-04  2.723e-04   1.396
## Illiteracy                              -1.072e-01  4.422e-01  -0.242
## Murder                                  -3.213e-01  5.892e-02  -5.453
## HS.Grad                                  1.302e-02  3.809e-02   0.342
## Frost                                    3.955e-04  4.690e-03   0.084
## Area                                    -1.029e-04  8.678e-05  -1.186
## state.area                               9.736e-05  8.376e-05   1.162
## x                                       -2.049e-02  3.773e-02  -0.543
## y                                       -8.631e-02  4.984e-02  -1.732
## state.division.fctrPacific              -3.042e-01  1.542e+00  -0.197
## state.division.fctrMountain             -2.601e-01  1.112e+00  -0.234
## `state.division.fctrWest South Central`  6.599e-02  5.855e-01   0.113
## `state.division.fctrNew England`        -2.928e-01  8.927e-01  -0.328
## `state.division.fctrSouth Atlantic`     -1.031e+00  5.580e-01  -1.848
## `state.division.fctrEast North Central` -3.284e-01  6.428e-01  -0.511
## `state.division.fctrWest North Central`  7.216e-02  7.498e-01   0.096
## `state.division.fctrMiddle Atlantic`    -1.101e+00  8.698e-01  -1.266
## state.region.fctrWest                           NA         NA      NA
## state.region.fctrNortheast                      NA         NA      NA
## `state.region.fctrNorth Central`                NA         NA      NA
## .rnorm                                  -2.416e-02  9.895e-02  -0.244
##                                         Pr(>|t|)    
## (Intercept)                             4.57e-15 ***
## Population                                0.0292 *  
## Income                                    0.1730    
## Illiteracy                                0.8102    
## Murder                                  6.48e-06 ***
## HS.Grad                                   0.7348    
## Frost                                     0.9334    
## Area                                      0.2450    
## state.area                                0.2543    
## x                                         0.5912    
## y                                         0.0936 .  
## state.division.fctrPacific                0.8450    
## state.division.fctrMountain               0.8167    
## `state.division.fctrWest South Central`   0.9110    
## `state.division.fctrNew England`          0.7452    
## `state.division.fctrSouth Atlantic`       0.0745 .  
## `state.division.fctrEast North Central`   0.6132    
## `state.division.fctrWest North Central`   0.9240    
## `state.division.fctrMiddle Atlantic`      0.2154    
## state.region.fctrWest                         NA    
## state.region.fctrNortheast                    NA    
## `state.region.fctrNorth Central`              NA    
## .rnorm                                    0.8088    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6641 on 30 degrees of freedom
## Multiple R-squared:  0.8501,	Adjusted R-squared:  0.7552 
## F-statistic: 8.958 on 19 and 30 DF,  p-value: 9.655e-08
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
## Warning in predict.lm(modelFit, newdata): prediction from a rank-deficient
## fit may be misleading
```

```
##   model_id model_method
## 1 All.X.lm           lm
##                                                                                                                            feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                       0.75                 0.007
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.8501492    0.9978482    0.8471627    0.5195263        0.7552437
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1         0.554711      0.1337508         0.07368833
## [1] "iterating over method:glm"
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm"
## + Fold1: parameter=none
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold1: parameter=none 
## + Fold2: parameter=none
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## - Fold2: parameter=none 
## + Fold3: parameter=none
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-24.png) 

```
## - Fold3: parameter=none 
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-25.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-26.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-27.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.0260  -0.3662   0.0486   0.4035   1.0910  
## 
## Coefficients: (3 not defined because of singularities)
##                                           Estimate Std. Error t value
## (Intercept)                              7.268e+01  5.024e+00  14.467
## Population                               8.892e-05  3.884e-05   2.290
## Income                                   3.801e-04  2.723e-04   1.396
## Illiteracy                              -1.072e-01  4.422e-01  -0.242
## Murder                                  -3.213e-01  5.892e-02  -5.453
## HS.Grad                                  1.302e-02  3.809e-02   0.342
## Frost                                    3.955e-04  4.690e-03   0.084
## Area                                    -1.029e-04  8.678e-05  -1.186
## state.area                               9.736e-05  8.376e-05   1.162
## x                                       -2.049e-02  3.773e-02  -0.543
## y                                       -8.631e-02  4.984e-02  -1.732
## state.division.fctrPacific              -3.042e-01  1.542e+00  -0.197
## state.division.fctrMountain             -2.601e-01  1.112e+00  -0.234
## `state.division.fctrWest South Central`  6.599e-02  5.855e-01   0.113
## `state.division.fctrNew England`        -2.928e-01  8.927e-01  -0.328
## `state.division.fctrSouth Atlantic`     -1.031e+00  5.580e-01  -1.848
## `state.division.fctrEast North Central` -3.284e-01  6.428e-01  -0.511
## `state.division.fctrWest North Central`  7.216e-02  7.498e-01   0.096
## `state.division.fctrMiddle Atlantic`    -1.101e+00  8.698e-01  -1.266
## state.region.fctrWest                           NA         NA      NA
## state.region.fctrNortheast                      NA         NA      NA
## `state.region.fctrNorth Central`                NA         NA      NA
## .rnorm                                  -2.416e-02  9.895e-02  -0.244
##                                         Pr(>|t|)    
## (Intercept)                             4.57e-15 ***
## Population                                0.0292 *  
## Income                                    0.1730    
## Illiteracy                                0.8102    
## Murder                                  6.48e-06 ***
## HS.Grad                                   0.7348    
## Frost                                     0.9334    
## Area                                      0.2450    
## state.area                                0.2543    
## x                                         0.5912    
## y                                         0.0936 .  
## state.division.fctrPacific                0.8450    
## state.division.fctrMountain               0.8167    
## `state.division.fctrWest South Central`   0.9110    
## `state.division.fctrNew England`          0.7452    
## `state.division.fctrSouth Atlantic`       0.0745 .  
## `state.division.fctrEast North Central`   0.6132    
## `state.division.fctrWest North Central`   0.9240    
## `state.division.fctrMiddle Atlantic`      0.2154    
## state.region.fctrWest                         NA    
## state.region.fctrNortheast                    NA    
## `state.region.fctrNorth Central`              NA    
## .rnorm                                    0.8088    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.4410559)
## 
##     Null deviance: 88.299  on 49  degrees of freedom
## Residual deviance: 13.232  on 30  degrees of freedom
## AIC: 117.42
## 
## Number of Fisher Scoring iterations: 2
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

```
## Warning in predict.lm(object, newdata, se.fit, scale = 1, type =
## ifelse(type == : prediction from a rank-deficient fit may be misleading
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-28.png) 

```
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                            feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.761                 0.011
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.8501492    0.9978482    0.8471627    0.5195263    117.4234
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1         0.554711      0.1337508         0.07368833
## [1] "iterating over method:rpart"
## [1] "fitting model: All.X.rpart"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr"
## + Fold1: cp=0.0741 
## - Fold1: cp=0.0741 
## + Fold2: cp=0.0741 
## - Fold2: cp=0.0741 
## + Fold3: cp=0.0741 
## - Fold3: cp=0.0741 
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.0741 on full training set
```

```
## Warning in myfit_mdl(model_id = paste0(model_id_pfx, ""), model_method =
## method, : model's bestTune found at an extreme of tuneGrid for parameter:
## cp
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-29.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-30.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 50 
## 
##           CP nsplit rel error
## 1 0.47910997      0 1.0000000
## 2 0.14868323      1 0.5208900
## 3 0.07409923      2 0.3722068
## 
## Variable importance
##     Murder          y Illiteracy    HS.Grad      Frost Population 
##         31         18         16         13         12          8 
##       Area 
##          2 
## 
## Node number 1: 50 observations,    complexity param=0.47911
##   mean=70.8786, MSE=1.76598 
##   left son=2 (27 obs) right son=3 (23 obs)
##   Primary splits:
##       Murder     < 6.55      to the right, improve=0.4791100, (0 missing)
##       HS.Grad    < 44.3      to the left,  improve=0.4007892, (0 missing)
##       Income     < 3891      to the left,  improve=0.3185349, (0 missing)
##       Illiteracy < 1.35      to the right, improve=0.3150196, (0 missing)
##       x          < -92.94255 to the right, improve=0.2227949, (0 missing)
##   Surrogate splits:
##       y          < 40.56395  to the left,  agree=0.80, adj=0.565, (0 split)
##       Illiteracy < 1.2       to the right, agree=0.76, adj=0.478, (0 split)
##       HS.Grad    < 53.25     to the left,  agree=0.76, adj=0.478, (0 split)
##       Frost      < 125.5     to the left,  agree=0.74, adj=0.435, (0 split)
##       Population < 1671.5    to the right, agree=0.70, adj=0.348, (0 split)
## 
## Node number 2: 27 observations,    complexity param=0.1486832
##   mean=70.02963, MSE=0.989848 
##   left son=4 (8 obs) right son=5 (19 obs)
##   Primary splits:
##       Murder     < 11.2      to the right, improve=0.4912307, (0 missing)
##       HS.Grad    < 44.8      to the left,  improve=0.4129798, (0 missing)
##       Income     < 4139.5    to the left,  improve=0.3715959, (0 missing)
##       Illiteracy < 1.95      to the right, improve=0.3131356, (0 missing)
##       y          < 33.9191   to the left,  improve=0.2816457, (0 missing)
##   Surrogate splits:
##       Illiteracy < 1.95      to the right, agree=0.889, adj=0.625, (0 split)
##       y          < 33.9191   to the left,  agree=0.889, adj=0.625, (0 split)
##       HS.Grad    < 64.55     to the right, agree=0.778, adj=0.250, (0 split)
##       Frost      < 62.5      to the left,  agree=0.778, adj=0.250, (0 split)
##       Area       < 209247.5  to the right, agree=0.778, adj=0.250, (0 split)
## 
## Node number 3: 23 observations
##   mean=71.87522, MSE=0.8377467 
## 
## Node number 4: 8 observations
##   mean=68.955, MSE=0.734025 
## 
## Node number 5: 19 observations
##   mean=70.48211, MSE=0.406585 
## 
## n= 50 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 50 88.299000 70.87860  
##   2) Murder>=6.55 27 26.725900 70.02963  
##     4) Murder>=11.2 8  5.872200 68.95500 *
##     5) Murder< 11.2 19  7.725116 70.48211 *
##   3) Murder< 6.55 23 19.268170 71.87522 *
##      model_id model_method
## 1 All.X.rpart        rpart
##                                                                                                                    feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.823                 0.014
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.6277932     1.087313    0.6277932    0.8107464        0.3705278
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.1011328          0.1087759
## [1] "fitting model: All.X.cp.0.rpart"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr"
## Fitting cp = 0 on full training set
```

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 50 
## 
##           CP nsplit rel error
## 1 0.47910997      0 1.0000000
## 2 0.14868323      1 0.5208900
## 3 0.07409923      2 0.3722068
## 4 0.00000000      3 0.2981076
## 
## Variable importance
##                         Murder                              y 
##                             26                             15 
##                     Illiteracy                        HS.Grad 
##                             15                             11 
##                          Frost                     Population 
##                             10                              7 
##                           Area                              x 
##                              4                              3 
##                     state.area     state.region.fctrNortheast 
##                              3                              3 
## state.division.fctrNew England 
##                              2 
## 
## Node number 1: 50 observations,    complexity param=0.47911
##   mean=70.8786, MSE=1.76598 
##   left son=2 (27 obs) right son=3 (23 obs)
##   Primary splits:
##       Murder     < 6.55      to the right, improve=0.4791100, (0 missing)
##       HS.Grad    < 44.3      to the left,  improve=0.4007892, (0 missing)
##       Income     < 3891      to the left,  improve=0.3185349, (0 missing)
##       Illiteracy < 1.35      to the right, improve=0.3150196, (0 missing)
##       x          < -92.94255 to the right, improve=0.2227949, (0 missing)
##   Surrogate splits:
##       y          < 40.56395  to the left,  agree=0.80, adj=0.565, (0 split)
##       Illiteracy < 1.2       to the right, agree=0.76, adj=0.478, (0 split)
##       HS.Grad    < 53.25     to the left,  agree=0.76, adj=0.478, (0 split)
##       Frost      < 125.5     to the left,  agree=0.74, adj=0.435, (0 split)
##       Population < 1671.5    to the right, agree=0.70, adj=0.348, (0 split)
## 
## Node number 2: 27 observations,    complexity param=0.1486832
##   mean=70.02963, MSE=0.989848 
##   left son=4 (8 obs) right son=5 (19 obs)
##   Primary splits:
##       Murder     < 11.2      to the right, improve=0.4912307, (0 missing)
##       HS.Grad    < 44.8      to the left,  improve=0.4129798, (0 missing)
##       Income     < 4139.5    to the left,  improve=0.3715959, (0 missing)
##       Illiteracy < 1.95      to the right, improve=0.3131356, (0 missing)
##       y          < 33.9191   to the left,  improve=0.2816457, (0 missing)
##   Surrogate splits:
##       Illiteracy < 1.95      to the right, agree=0.889, adj=0.625, (0 split)
##       y          < 33.9191   to the left,  agree=0.889, adj=0.625, (0 split)
##       HS.Grad    < 64.55     to the right, agree=0.778, adj=0.250, (0 split)
##       Frost      < 62.5      to the left,  agree=0.778, adj=0.250, (0 split)
##       Area       < 209247.5  to the right, agree=0.778, adj=0.250, (0 split)
## 
## Node number 3: 23 observations,    complexity param=0.07409923
##   mean=71.87522, MSE=0.8377467 
##   left son=6 (9 obs) right son=7 (14 obs)
##   Primary splits:
##       x                              < -83.72205 to the right, improve=0.3395697, (0 missing)
##       state.region.fctrNorth Central < 0.5       to the left,  improve=0.2573045, (0 missing)
##       Murder                         < 4.75      to the right, improve=0.2006943, (0 missing)
##       HS.Grad                        < 59.25     to the left,  improve=0.1946401, (0 missing)
##       Income                         < 4458.5    to the left,  improve=0.1798387, (0 missing)
##   Surrogate splits:
##       Area                           < 49715     to the left,  agree=0.957, adj=0.889, (0 split)
##       state.area                     < 50743.5   to the left,  agree=0.957, adj=0.889, (0 split)
##       state.region.fctrNortheast     < 0.5       to the right, agree=0.957, adj=0.889, (0 split)
##       state.division.fctrNew England < 0.5       to the right, agree=0.870, adj=0.667, (0 split)
##       Illiteracy                     < 0.65      to the right, agree=0.783, adj=0.444, (0 split)
## 
## Node number 4: 8 observations
##   mean=68.955, MSE=0.734025 
## 
## Node number 5: 19 observations
##   mean=70.48211, MSE=0.406585 
## 
## Node number 6: 9 observations
##   mean=71.21, MSE=0.5933778 
## 
## Node number 7: 14 observations
##   mean=72.30286, MSE=0.5274918 
## 
## n= 50 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 50 88.299000 70.87860  
##   2) Murder>=6.55 27 26.725900 70.02963  
##     4) Murder>=11.2 8  5.872200 68.95500 *
##     5) Murder< 11.2 19  7.725116 70.48211 *
##   3) Murder< 6.55 23 19.268170 71.87522  
##     6) x>=-83.72205 9  5.340400 71.21000 *
##     7) x< -83.72205 14  7.384886 72.30286 *
##           model_id model_method
## 1 All.X.cp.0.rpart        rpart
##                                                                                                                    feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               0                      0.468                 0.015
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1    0.7018924    0.7255701    0.7018924    0.7255701
## [1] "iterating over method:rf"
## [1] "fitting model: All.X.rf"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-31.png) 

```
## + : mtry= 2 
## - : mtry= 2 
## + : mtry=12 
## - : mtry=12 
## + : mtry=22 
## - : mtry=22 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 12 on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-32.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-33.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted        50    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times        50    -none-     numeric  
## importance       22    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y                50    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           22    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
##   model_id model_method
## 1 All.X.rf           rf
##                                                                                                                            feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.177                 0.086
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.9193519    0.9136977    0.9122315    0.3936973        0.5272634
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr"
## + : mtry= 2 
## - : mtry= 2 
## + : mtry=11 
## - : mtry=11 
## + : mtry=21 
## - : mtry=21 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 11 on full training set
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_0-34.png) ![](USCensus1977_State_HW4_files/figure-html/fit.models_0-35.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted        50    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times        50    -none-     numeric  
## importance       21    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y                50    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           21    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                    feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.824                  0.08
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.9194504     0.891101    0.9194504    0.3771624        0.5503567
```

```r
# Simplify a model
# fit_df <- glb_trnent_df; glb_mdl <- step(<complex>_mdl)

print(glb_models_df)
```

```
##                     model_id model_method
## 1                     MFO.lm           lm
## 2       Max.cor.Y.cv.0.rpart        rpart
## 3  Max.cor.Y.cv.0.cp.0.rpart        rpart
## 4            Max.cor.Y.rpart        rpart
## 5               Max.cor.Y.lm           lm
## 6     Interact.High.cor.y.lm           lm
## 7               Low.cor.X.lm           lm
## 8                   All.X.lm           lm
## 9                  All.X.glm          glm
## 10               All.X.rpart        rpart
## 11          All.X.cp.0.rpart        rpart
## 12                  All.X.rf           rf
## 13         All.X.no.rnorm.rf           rf
##                                                                                                                             feats
## 1                                                                                                                          .rnorm
## 2                                                                                                                          Murder
## 3                                                                                                                          Murder
## 4                                                                                                                          Murder
## 5                                                                                                                          Murder
## 6                                                                                Murder, Murder:y, Murder:Area, Murder:Illiteracy
## 7                       HS.Grad, state.region.fctr, Income, Frost, state.division.fctr, .rnorm, Population, state.area, x, Murder
## 8  Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
## 9  Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
## 10         Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
## 11         Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
## 12 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
## 13         Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
##    max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1                0                      0.617                 0.002
## 2                0                      0.483                 0.007
## 3                0                      0.429                 0.006
## 4                3                      0.819                 0.006
## 5                1                      0.721                 0.002
## 6                1                      0.715                 0.002
## 7                1                      0.727                 0.006
## 8                1                      0.750                 0.007
## 9                1                      0.761                 0.011
## 10               3                      0.823                 0.014
## 11               0                      0.468                 0.015
## 12               3                      1.177                 0.086
## 13               3                      0.824                 0.080
##    max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1   0.002331715    1.3273516   -0.0197079    1.3419329      -0.01845304
## 2   0.000000000    1.3289018    0.0000000    1.3289018               NA
## 3   0.671587719    0.7615573    0.6715877    0.7615573               NA
## 4   0.627793192    1.0483158    0.6277932    0.8107464               NA
## 5   0.609720089    0.8553751    0.6097201    0.8301967       0.60158926
## 6   0.661420134    0.9644864    0.6614201    0.7732563       0.63132415
## 7   0.828818223    0.7679484    0.8248221    0.5562021       0.74582100
## 8   0.850149179    0.9978482    0.8471627    0.5195263       0.75524366
## 9   0.850149179    0.9978482    0.8471627    0.5195263               NA
## 10  0.627793192    1.0873126    0.6277932    0.8107464               NA
## 11  0.701892423    0.7255701    0.7018924    0.7255701               NA
## 12  0.919351922    0.9136977    0.9122315    0.3936973               NA
## 13  0.919450449    0.8911010    0.9194504    0.3771624               NA
##    max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit min.aic.fit
## 1                NA             NA                 NA          NA
## 2                NA             NA                 NA          NA
## 3                NA             NA                 NA          NA
## 4         0.4087926     0.08998812         0.17411417          NA
## 5         0.6279054     0.16653016         0.10273774          NA
## 6         0.5180279     0.13521150         0.05523021          NA
## 7         0.7129405     0.12536897         0.07850497          NA
## 8         0.5547110     0.13375082         0.07368833          NA
## 9         0.5547110     0.13375082         0.07368833    117.4234
## 10        0.3705278     0.10113275         0.10877585          NA
## 11               NA             NA                 NA          NA
## 12        0.5272634             NA                 NA          NA
## 13        0.5503567             NA                 NA          NA
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.models", 
    chunk_step_major=glb_script_df[nrow(glb_script_df), "chunk_step_major"], 
    chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,                              
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##          chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed8  fit.models                5                0   5.233
## elapsed9  fit.models                5                1  32.337
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_trnent_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_newent_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
#     tmp_models_df <- orderBy(~model_id, glb_models_df)
#     rownames(tmp_models_df) <- seq(1, nrow(tmp_models_df))
#     all.equal(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr"),
#               subset(stats_df, model_id != "Random.myrandom_classfr"))
#     print(subset(tmp_models_df[, names(stats_df)], model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])
#     print(subset(stats_df, model_id != "Random.myrandom_classfr")[, c("model_id", "max.Accuracy.fit")])

    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id", grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df), grep(glb_model_metric, names(stats_df), value=TRUE)))]
    tmp3_models_df <- merge(tmp2_models_df, stats_mrg_df, all.x=TRUE, sort=FALSE)
    print(tmp3_models_df)
    print(names(tmp3_models_df))
    print(glb_models_df <- subset(tmp3_models_df, select=-model_id.1))
}

plt_models_df <- glb_models_df[, -grep("SD|Upper|Lower", names(glb_models_df))]
for (var in grep("^min.", names(plt_models_df), value=TRUE)) {
    plt_models_df[, sub("min.", "inv.", var)] <- 
        #ifelse(all(is.na(tmp <- plt_models_df[, var])), NA, 1.0 / tmp)
        1.0 / plt_models_df[, var]
    plt_models_df <- plt_models_df[ , -grep(var, names(plt_models_df))]
}
print(plt_models_df)
```

```
##                     model_id model_method
## 1                     MFO.lm           lm
## 2       Max.cor.Y.cv.0.rpart        rpart
## 3  Max.cor.Y.cv.0.cp.0.rpart        rpart
## 4            Max.cor.Y.rpart        rpart
## 5               Max.cor.Y.lm           lm
## 6     Interact.High.cor.y.lm           lm
## 7               Low.cor.X.lm           lm
## 8                   All.X.lm           lm
## 9                  All.X.glm          glm
## 10               All.X.rpart        rpart
## 11          All.X.cp.0.rpart        rpart
## 12                  All.X.rf           rf
## 13         All.X.no.rnorm.rf           rf
##                                                                                                                             feats
## 1                                                                                                                          .rnorm
## 2                                                                                                                          Murder
## 3                                                                                                                          Murder
## 4                                                                                                                          Murder
## 5                                                                                                                          Murder
## 6                                                                                Murder, Murder:y, Murder:Area, Murder:Illiteracy
## 7                       HS.Grad, state.region.fctr, Income, Frost, state.division.fctr, .rnorm, Population, state.area, x, Murder
## 8  Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
## 9  Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
## 10         Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
## 11         Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
## 12 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr, .rnorm
## 13         Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area, state.area, x, y, state.division.fctr, state.region.fctr
##    max.nTuningRuns max.R.sq.fit max.R.sq.OOB max.Adj.R.sq.fit
## 1                0  0.002331715   -0.0197079      -0.01845304
## 2                0  0.000000000    0.0000000               NA
## 3                0  0.671587719    0.6715877               NA
## 4                3  0.627793192    0.6277932               NA
## 5                1  0.609720089    0.6097201       0.60158926
## 6                1  0.661420134    0.6614201       0.63132415
## 7                1  0.828818223    0.8248221       0.74582100
## 8                1  0.850149179    0.8471627       0.75524366
## 9                1  0.850149179    0.8471627               NA
## 10               3  0.627793192    0.6277932               NA
## 11               0  0.701892423    0.7018924               NA
## 12               3  0.919351922    0.9122315               NA
## 13               3  0.919450449    0.9194504               NA
##    max.Rsquared.fit inv.elapsedtime.everything inv.elapsedtime.final
## 1                NA                  1.6207455             500.00000
## 2                NA                  2.0703934             142.85714
## 3                NA                  2.3310023             166.66667
## 4         0.4087926                  1.2210012             166.66667
## 5         0.6279054                  1.3869626             500.00000
## 6         0.5180279                  1.3986014             500.00000
## 7         0.7129405                  1.3755158             166.66667
## 8         0.5547110                  1.3333333             142.85714
## 9         0.5547110                  1.3140604              90.90909
## 10        0.3705278                  1.2150668              71.42857
## 11               NA                  2.1367521              66.66667
## 12        0.5272634                  0.8496177              11.62791
## 13        0.5503567                  1.2135922              12.50000
##    inv.RMSE.fit inv.RMSE.OOB inv.aic.fit
## 1     0.7533799    0.7451938          NA
## 2     0.7525010    0.7525010          NA
## 3     1.3130988    1.3130988          NA
## 4     0.9539110    1.2334313          NA
## 5     1.1690778    1.2045338          NA
## 6     1.0368213    1.2932323          NA
## 7     1.3021708    1.7979078          NA
## 8     1.0021565    1.9248305          NA
## 9     1.0021565    1.9248305 0.008516191
## 10    0.9196987    1.2334313          NA
## 11    1.3782265    1.3782265          NA
## 12    1.0944539    2.5400222          NA
## 13    1.1222072    2.6513775          NA
```

```r
print(myplot_radar(radar_inp_df=plt_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 8 rows containing missing values (geom_path).
```

```
## Warning: Removed 81 rows containing missing values (geom_point).
```

```
## Warning: Removed 24 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_1-1.png) 

```r
# print(myplot_radar(radar_inp_df=subset(plt_models_df, 
#         !(model_id %in% grep("random|MFO", plt_models_df$model_id, value=TRUE)))))

# Compute CI for <metric>SD
glb_models_df <- mutate(glb_models_df, 
                max.df = ifelse(max.nTuningRuns > 1, max.nTuningRuns - 1, NA),
                min.sd2ci.scaler = ifelse(is.na(max.df), NA, qt(0.975, max.df)))
for (var in grep("SD", names(glb_models_df), value=TRUE)) {
    # Does CI alredy exist ?
    var_components <- unlist(strsplit(var, "SD"))
    varActul <- paste0(var_components[1],          var_components[2])
    varUpper <- paste0(var_components[1], "Upper", var_components[2])
    varLower <- paste0(var_components[1], "Lower", var_components[2])
    if (varUpper %in% names(glb_models_df)) {
        warning(varUpper, " already exists in glb_models_df")
        # Assuming Lower also exists
        next
    }    
    print(sprintf("var:%s", var))
    # CI is dependent on sample size in t distribution; df=n-1
    glb_models_df[, varUpper] <- glb_models_df[, varActul] + 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
    glb_models_df[, varLower] <- glb_models_df[, varActul] - 
        glb_models_df[, "min.sd2ci.scaler"] * glb_models_df[, var]
}
```

```
## [1] "var:min.RMSESD.fit"
## [1] "var:max.RsquaredSD.fit"
```

```r
# Plot metrics with CI
plt_models_df <- glb_models_df[, "model_id", FALSE]
pltCI_models_df <- glb_models_df[, "model_id", FALSE]
for (var in grep("Upper", names(glb_models_df), value=TRUE)) {
    var_components <- unlist(strsplit(var, "Upper"))
    col_name <- unlist(paste(var_components, collapse=""))
    plt_models_df[, col_name] <- glb_models_df[, col_name]
    for (name in paste0(var_components[1], c("Upper", "Lower"), var_components[2]))
        pltCI_models_df[, name] <- glb_models_df[, name]
}

build_statsCI_data <- function(plt_models_df) {
    mltd_models_df <- melt(plt_models_df, id.vars="model_id")
    mltd_models_df$data <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) tail(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), "[.]")), 1))
    mltd_models_df$label <- sapply(1:nrow(mltd_models_df), 
        function(row_ix) head(unlist(strsplit(as.character(
            mltd_models_df[row_ix, "variable"]), paste0(".", mltd_models_df[row_ix, "data"]))), 1))
    #print(mltd_models_df)
    
    return(mltd_models_df)
}
mltd_models_df <- build_statsCI_data(plt_models_df)

mltdCI_models_df <- melt(pltCI_models_df, id.vars="model_id")
for (row_ix in 1:nrow(mltdCI_models_df)) {
    for (type in c("Upper", "Lower")) {
        if (length(var_components <- unlist(strsplit(
                as.character(mltdCI_models_df[row_ix, "variable"]), type))) > 1) {
            #print(sprintf("row_ix:%d; type:%s; ", row_ix, type))
            mltdCI_models_df[row_ix, "label"] <- var_components[1]
            mltdCI_models_df[row_ix, "data"] <- unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
#print(mltdCI_models_df)
# castCI_models_df <- dcast(mltdCI_models_df, value ~ type, fun.aggregate=sum)
# print(castCI_models_df)
wideCI_models_df <- reshape(subset(mltdCI_models_df, select=-variable), 
                            timevar="type", 
        idvar=setdiff(names(mltdCI_models_df), c("type", "value", "variable")), 
                            direction="wide")
#print(wideCI_models_df)
mrgdCI_models_df <- merge(wideCI_models_df, mltd_models_df, all.x=TRUE)
#print(mrgdCI_models_df)

# Merge stats back in if CIs don't exist
goback_vars <- c()
for (var in unique(mltd_models_df$label)) {
    for (type in unique(mltd_models_df$data)) {
        var_type <- paste0(var, ".", type)
        # if this data is already present, next
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data, sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], all.x=TRUE)

# print(myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="data") + 
#         geom_errorbar(data=mrgdCI_models_df, 
#             mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
#           facet_grid(label ~ data, scales="free") + 
#           theme(axis.text.x = element_text(angle = 45,vjust = 1)))
# mltd_models_df <- orderBy(~ value +variable +data +label + model_method + model_id, 
#                           mltd_models_df)
print(myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning: Removed 4 rows containing missing values (position_stack).
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_1-2.png) 

```r
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                    ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
print(tmp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, c("model_id", glb_model_evl_criteria)])
```

```
##                     model_id min.RMSE.OOB max.R.sq.OOB max.Adj.R.sq.fit
## 13         All.X.no.rnorm.rf    0.3771624    0.9194504               NA
## 12                  All.X.rf    0.3936973    0.9122315               NA
## 8                   All.X.lm    0.5195263    0.8471627       0.75524366
## 9                  All.X.glm    0.5195263    0.8471627               NA
## 7               Low.cor.X.lm    0.5562021    0.8248221       0.74582100
## 11          All.X.cp.0.rpart    0.7255701    0.7018924               NA
## 3  Max.cor.Y.cv.0.cp.0.rpart    0.7615573    0.6715877               NA
## 6     Interact.High.cor.y.lm    0.7732563    0.6614201       0.63132415
## 4            Max.cor.Y.rpart    0.8107464    0.6277932               NA
## 10               All.X.rpart    0.8107464    0.6277932               NA
## 5               Max.cor.Y.lm    0.8301967    0.6097201       0.60158926
## 2       Max.cor.Y.cv.0.rpart    1.3289018    0.0000000               NA
## 1                     MFO.lm    1.3419329   -0.0197079      -0.01845304
```

```r
print(myplot_radar(radar_inp_df=tmp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

```
## Warning: Removed 6 rows containing missing values (geom_path).
```

```
## Warning: Removed 30 rows containing missing values (geom_point).
```

```
## Warning: Removed 8 rows containing missing values (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 13. Consider specifying shapes manually. if you must have them.
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_1-3.png) 

```r
print("Metrics used for model selection:"); print(model_sel_frmla)
```

```
## [1] "Metrics used for model selection:"
```

```
## ~+min.RMSE.OOB - max.R.sq.OOB - max.Adj.R.sq.fit
```

```r
print(sprintf("Best model id: %s", tmp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: All.X.no.rnorm.rf"
```

```r
if (is.null(glb_sel_mdl_id)) 
    { glb_sel_mdl_id <- tmp_models_df[1, "model_id"] } else 
        print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_1-4.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted        50    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times        50    -none-     numeric  
## importance       21    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y                50    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           21    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical
```

```
## [1] TRUE
```

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "model.selected")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0
```

![](USCensus1977_State_HW4_files/figure-html/fit.models_1-5.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.data.training.all", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                     chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed9             fit.models                5                1  32.337
## elapsed10 fit.data.training.all                6                0  39.180
```

## Step `6`: fit.data.training.all

```r
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
    print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, entity_df=glb_trnent_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    # Sync with parameters in mydsutils.R
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
                            indep_vars_vctr=mdl_feats_df$id, model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnent_df, OOB_df=NULL,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL,
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
##                                      id importance
## Murder                           Murder 100.000000
## HS.Grad                         HS.Grad  39.675598
## Illiteracy                   Illiteracy  25.511731
## Income                           Income  14.693130
## x                                     x  14.004421
## y                                     y   9.199612
## Population                   Population   8.571232
## Frost                             Frost   8.299797
## state.area                   state.area   7.138794
## Area                               Area   6.465033
## state.division.fctr state.division.fctr   3.729238
## state.region.fctr     state.region.fctr   1.756183
## [1] "fitting model: Final.rf"
## [1] "    indep_vars: Murder, HS.Grad, Illiteracy, Income, x, y, Population, Frost, state.area, Area, state.division.fctr, state.region.fctr"
## + : mtry= 2 
## - : mtry= 2 
## + : mtry=11 
## - : mtry=11 
## + : mtry=21 
## - : mtry=21 
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 21 on full training set
```

```
## Warning in myfit_mdl(model_id = "Final", model_method = model_method,
## indep_vars_vctr = mdl_feats_df$id, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_0-1.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_0-2.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted        50    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times        50    -none-     numeric  
## importance       21    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y                50    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           21    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
##   model_id model_method
## 1 Final.rf           rf
##                                                                                                                    feats
## 1 Murder, HS.Grad, Illiteracy, Income, x, y, Population, Frost, state.area, Area, state.division.fctr, state.region.fctr
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.879                 0.126
##   max.R.sq.fit min.RMSE.fit max.Rsquared.fit
## 1    0.9256304    0.8893656        0.5521064
```

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="fit.data.training.all", 
    chunk_step_major=glb_script_df[nrow(glb_script_df), "chunk_step_major"], 
    chunk_step_minor=glb_script_df[nrow(glb_script_df), "chunk_step_minor"]+1,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                     chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed10 fit.data.training.all                6                0  39.180
## elapsed11 fit.data.training.all                6                1  42.201
```


```r
glb_rsp_var_out <- paste0(glb_rsp_var_out, tail(names(glb_models_lst), 1))

# Used again in predict.data.new chunk
glb_get_predictions <- function(df) {
    if (glb_is_regression) {
        df[, glb_rsp_var_out] <- predict(glb_fin_mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, glb_rsp_var_out, 
                             smooth=TRUE))
        df[, paste0(glb_rsp_var_out, ".err")] <- 
            abs(df[, glb_rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(glb_rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        # incorporate glb_clf_proba_threshold
        #   shd it only be for glb_fin_mdl or for earlier models ?
        if ((prob_threshold <- 
                glb_models_df[glb_models_df$model_id == glb_fin_mdl_id, "opt.prob.threshold.fit"]) != 
            glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.fit"])
            stop("user specification for probability threshold required")
        
        df[, paste0(glb_rsp_var_out, ".prob")] <- 
            predict(glb_fin_mdl, newdata=df, type="prob")[, 2]
        df[, glb_rsp_var_out] <- 
    			factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(glb_rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, glb_rsp_var_out] <- predict(glb_fin_mdl, newdata=df, type="raw")
    }

    return(df)
}    
glb_trnent_df <- glb_get_predictions(glb_trnent_df)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-1.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 6        2541   4884        0.7    72.06    6.8    63.9   166 103766
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
##    state.abb state.area         x       y     state.division state.name
## 11        HI       6450 -126.2500 31.7500            Pacific     Hawaii
## 43        TX     267339  -98.7857 31.3897 West South Central      Texas
## 8         DE       2057  -74.9841 38.6777     South Atlantic   Delaware
## 19        ME      33215  -68.9801 45.6226        New England      Maine
## 6         CO     104247 -105.5130 38.6777           Mountain   Colorado
## 26        MT     147138 -109.3200 46.8230           Mountain    Montana
##    state.region state.division.fctr state.region.fctr     .rnorm
## 11         West             Pacific              West -1.0493528
## 43        South  West South Central             South  0.1410843
## 8         South      South Atlantic             South -1.2956717
## 19    Northeast         New England         Northeast  0.6453831
## 6          West            Mountain              West  1.5934885
## 26         West            Mountain              West -0.3079534
##    Life.Exp.predict.Final.rf Life.Exp.predict.Final.rf.err
## 11                  72.55698                     1.0430193
## 43                  70.15545                     0.7445467
## 8                   70.71212                     0.6521197
## 19                  70.99412                     0.6041163
## 6                   71.47361                     0.5863867
## 26                  71.13903                     0.5790330
```

```r
print(glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
                                               entity_df=glb_trnent_df))
```

```
##                     id       cor.y exclude.as.feat  cor.y.abs cor.low
## 7               Murder -0.78084575               0 0.78084575       1
## 4              HS.Grad  0.58221620               0 0.58221620       1
## 12                   x -0.24798347               0 0.24798347       1
## 5           Illiteracy -0.58847793               0 0.58847793       0
## 6               Income  0.34025534               0 0.34025534       1
## 8           Population -0.06805195               0 0.06805195       1
## 3                Frost  0.26206801               0 0.26206801       1
## 9           state.area -0.10963169               0 0.10963169       1
## 13                   y  0.40665458               0 0.40665458       0
## 2                 Area -0.10733194               0 0.10733194       0
## 10 state.division.fctr  0.19443418               0 0.19443418       1
## 11   state.region.fctr  0.56519612               0 0.56519612       1
## 1               .rnorm -0.04828783               0 0.04828783       1
##     importance
## 7  100.0000000
## 4   23.0440809
## 12   8.4350326
## 5    7.8261074
## 6    6.3098011
## 8    5.6596109
## 3    3.5855582
## 9    3.0366432
## 13   2.8615375
## 2    2.4883818
## 10   1.1764385
## 11   0.7336526
## 1           NA
```

```r
# Used again in predict.data.new chunk
glb_analytics_diag_plots <- function(obs_df) {
    for (var in subset(glb_feats_df, !is.na(importance))$id) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, glb_rsp_var_out))
#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
#         plot_vars_df <- subset(glb_feats_df, importance > 
#                         glb_feats_df[glb_feats_df$id == ".rnorm", "importance"])
        plot_vars_df <- orderBy(~ -importance, glb_feats_df)
        if (nrow(plot_vars_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2],
                                      ".rownames"), 
                                               feat_y=plot_vars_df$id[1],
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        id_vars=glb_id_vars)
    #               + facet_wrap(reformulate(plot_vars_df$id[2])) # if [1 or 2] is a factor                                                         
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(plot_vars_df <- subset(glb_feats_df, !is.na(importance))) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(plot_vars_df) > 1, plot_vars_df$id[2], 
                              ".rownames"),
                                               feat_y=plot_vars_df$id[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=glb_rsp_var_out, 
                     id_vars=glb_id_vars)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
glb_analytics_diag_plots(obs_df=glb_trnent_df)
```

![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-2.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-3.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-4.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-5.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-6.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-7.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-8.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-9.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-10.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-11.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-12.png) ![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-13.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 6        2541   4884        0.7    72.06    6.8    63.9   166 103766
##    state.abb state.area         x       y     state.division state.name
## 11        HI       6450 -126.2500 31.7500            Pacific     Hawaii
## 43        TX     267339  -98.7857 31.3897 West South Central      Texas
## 8         DE       2057  -74.9841 38.6777     South Atlantic   Delaware
## 19        ME      33215  -68.9801 45.6226        New England      Maine
## 6         CO     104247 -105.5130 38.6777           Mountain   Colorado
##    state.region state.division.fctr state.region.fctr     .rnorm
## 11         West             Pacific              West -1.0493528
## 43        South  West South Central             South  0.1410843
## 8         South      South Atlantic             South -1.2956717
## 19    Northeast         New England         Northeast  0.6453831
## 6          West            Mountain              West  1.5934885
##    Life.Exp.predict.Final.rf Life.Exp.predict.Final.rf.err .label
## 11                  72.55698                     1.0430193     HI
## 43                  70.15545                     0.7445467     TX
## 8                   70.71212                     0.6521197     DE
## 19                  70.99412                     0.6041163     ME
## 6                   71.47361                     0.5863867     CO
```

![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-14.png) 

```r
replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.training.all.prediction","model.final")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1
```

![](USCensus1977_State_HW4_files/figure-html/fit.data.training.all_1-15.png) 

```r
glb_script_df <- rbind(glb_script_df, 
                   data.frame(chunk_label="predict.data.new", 
                              chunk_step_major=max(glb_script_df$chunk_step_major)+1, 
                              chunk_step_minor=0,
                              elapsed=(proc.time() - glb_script_tm)["elapsed"]))
print(tail(glb_script_df, 2))
```

```
##                     chunk_label chunk_step_major chunk_step_minor elapsed
## elapsed11 fit.data.training.all                6                1  42.201
## elapsed12      predict.data.new                7                0  48.366
```

## Step `7`: predict data.new

```r
# Compute final model predictions
glb_newent_df <- glb_get_predictions(glb_newent_df)
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](USCensus1977_State_HW4_files/figure-html/predict.data.new-1.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 6        2541   4884        0.7    72.06    6.8    63.9   166 103766
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
##    state.abb state.area         x       y     state.division state.name
## 11        HI       6450 -126.2500 31.7500            Pacific     Hawaii
## 43        TX     267339  -98.7857 31.3897 West South Central      Texas
## 8         DE       2057  -74.9841 38.6777     South Atlantic   Delaware
## 19        ME      33215  -68.9801 45.6226        New England      Maine
## 6         CO     104247 -105.5130 38.6777           Mountain   Colorado
## 26        MT     147138 -109.3200 46.8230           Mountain    Montana
##    state.region state.division.fctr state.region.fctr      .rnorm
## 11         West             Pacific              West  0.32215158
## 43        South  West South Central             South -0.10862424
## 8         South      South Atlantic             South -0.06901731
## 19    Northeast         New England         Northeast  1.64219199
## 6          West            Mountain              West  0.62886063
## 26         West            Mountain              West  0.66892165
##    Life.Exp.predict.Final.rf Life.Exp.predict.Final.rf.err
## 11                  72.55698                     1.0430193
## 43                  70.15545                     0.7445467
## 8                   70.71212                     0.6521197
## 19                  70.99412                     0.6041163
## 6                   71.47361                     0.5863867
## 26                  71.13903                     0.5790330
```

```r
glb_analytics_diag_plots(obs_df=glb_newent_df)
```

![](USCensus1977_State_HW4_files/figure-html/predict.data.new-2.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-3.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-4.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-5.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-6.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-7.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-8.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-9.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-10.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-11.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-12.png) ![](USCensus1977_State_HW4_files/figure-html/predict.data.new-13.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 6        2541   4884        0.7    72.06    6.8    63.9   166 103766
##    state.abb state.area         x       y     state.division state.name
## 11        HI       6450 -126.2500 31.7500            Pacific     Hawaii
## 43        TX     267339  -98.7857 31.3897 West South Central      Texas
## 8         DE       2057  -74.9841 38.6777     South Atlantic   Delaware
## 19        ME      33215  -68.9801 45.6226        New England      Maine
## 6         CO     104247 -105.5130 38.6777           Mountain   Colorado
##    state.region state.division.fctr state.region.fctr      .rnorm
## 11         West             Pacific              West  0.32215158
## 43        South  West South Central             South -0.10862424
## 8         South      South Atlantic             South -0.06901731
## 19    Northeast         New England         Northeast  1.64219199
## 6          West            Mountain              West  0.62886063
##    Life.Exp.predict.Final.rf Life.Exp.predict.Final.rf.err .label
## 11                  72.55698                     1.0430193     HI
## 43                  70.15545                     0.7445467     TX
## 8                   70.71212                     0.6521197     DE
## 19                  70.99412                     0.6041163     ME
## 6                   71.47361                     0.5863867     CO
```

![](USCensus1977_State_HW4_files/figure-html/predict.data.new-14.png) 

```r
tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
    replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
        "data.new.prediction")), flip_coord=TRUE)
```

```
## time	trans	 "bgn " "fit.data.training.all " "predict.data.new " "end " 
## 0.0000 	multiple enabled transitions:  data.training.all data.new model.selected 	firing:  data.training.all 
## 1.0000 	 1 	 2 1 0 0 
## 1.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction 	firing:  data.new 
## 2.0000 	 2 	 1 1 1 0 
## 2.0000 	multiple enabled transitions:  data.training.all data.new model.selected model.final data.training.all.prediction data.new.prediction 	firing:  model.selected 
## 3.0000 	 3 	 0 2 1 0 
## 3.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  data.training.all.prediction 
## 4.0000 	 5 	 0 1 1 1 
## 4.0000 	multiple enabled transitions:  model.final data.training.all.prediction data.new.prediction 	firing:  model.final 
## 5.0000 	 4 	 0 0 2 1 
## 6.0000 	 6 	 0 0 1 2
```

![](USCensus1977_State_HW4_files/figure-html/predict.data.new-15.png) 

```r
print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())
```

![](USCensus1977_State_HW4_files/figure-html/predict.data.new-16.png) 

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                   chunk_label chunk_step_major chunk_step_minor elapsed
## 10                 fit.models                5                1  32.337
## 11      fit.data.training.all                6                0  39.180
## 13           predict.data.new                7                0  48.366
## 12      fit.data.training.all                6                1  42.201
## 9                  fit.models                5                0   5.233
## 4         manage_missing_data                2                2   1.479
## 7             select_features                4                0   2.759
## 5          encode_retype_data                2                3   1.847
## 2                cleanse_data                2                0   0.368
## 8  remove_correlated_features                4                1   2.946
## 6            extract_features                3                0   1.901
## 3       inspectORexplore.data                2                1   0.405
## 1                 import_data                1                0   0.002
##    elapsed_diff
## 10       27.104
## 11        6.843
## 13        6.165
## 12        3.021
## 9         2.287
## 4         1.074
## 7         0.858
## 5         0.368
## 2         0.366
## 8         0.187
## 6         0.054
## 3         0.037
## 1         0.000
```

```
## [1] "Total Elapsed Time: 48.366 secs"
```

![](USCensus1977_State_HW4_files/figure-html/print_sessionInfo-1.png) 

```
## R version 3.1.3 (2015-03-09)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
##  [1] randomForest_4.6-10 rpart.plot_1.5.2    rpart_4.1-9        
##  [4] caret_6.0-41        lattice_0.20-31     reshape2_1.4.1     
##  [7] plyr_1.8.1          doBy_4.5-13         survival_2.38-1    
## [10] ggplot2_1.0.1      
## 
## loaded via a namespace (and not attached):
##  [1] BradleyTerry2_1.0-6 brglm_0.5-9         car_2.0-25         
##  [4] codetools_0.2-11    colorspace_1.2-6    compiler_3.1.3     
##  [7] digest_0.6.8        evaluate_0.5.5      foreach_1.4.2      
## [10] formatR_1.1         gtable_0.1.2        gtools_3.4.1       
## [13] htmltools_0.2.6     iterators_1.0.7     knitr_1.9          
## [16] labeling_0.3        lme4_1.1-7          MASS_7.3-40        
## [19] Matrix_1.2-0        mgcv_1.8-6          minqa_1.2.4        
## [22] munsell_0.4.2       nlme_3.1-120        nloptr_1.0.4       
## [25] nnet_7.3-9          parallel_3.1.3      pbkrtest_0.4-2     
## [28] proto_0.3-10        quantreg_5.11       RColorBrewer_1.1-2 
## [31] Rcpp_0.11.5         rmarkdown_0.5.1     scales_0.2.4       
## [34] SparseM_1.6         splines_3.1.3       stringr_0.6.2      
## [37] tools_3.1.3         yaml_2.1.13
```
