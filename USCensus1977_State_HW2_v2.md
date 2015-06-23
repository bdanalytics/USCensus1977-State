# US Census 1977: Life.Exp regression:: HW2_v2
bdanalytics  

**  **    
**Date: (Tue) Jun 23, 2015**    

# Introduction:  

Data: 
Source: 
    Training:   https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/statedata.csv  
    New:        <newdt_url>  
Time period: 



# Synopsis:

Based on analysis utilizing <> techniques, <conclusion heading>:  

Regression results:
First run:
    <glb_sel_mdl_id>: 
        OOB_RMSE=<0.4f>; new_RMSE=<0.4f>; <feat1>=<imp>; <feat2>=<imp>

Classification results:
First run:
    <glb_sel_mdl_id>: Leaderboard: <accuracy>
        newobs_tbl=[0=, 1=]; submit_filename=
        OOB_conf_mtrx=[YN=, NY=]=; max.Accuracy.OOB=; opt.prob.threshold.OOB=
            <feat1>=<imp>; <feat1>=<imp>; <feat1>=<imp>; 
            <txt.feat1>=<imp>; <txt.feat1>=<imp>; <txt.feat1>=<imp>; 

### Prediction Accuracy Enhancement Options:
- import.data chunk:
    - which obs should be in fit vs. OOB (currently dirty.0 vs .1 is split 50%)
    
- inspect.data chunk:
    - For date variables
        - Appropriate factors ?
        - Different / More last* features ?
        
- scrub.data chunk:        
- transform.data chunk:
    - derive features from multiple features
    
- manage.missing.data chunk:
    - Not fill missing vars
    - Fill missing numerics with a different algorithm
    - Fill missing chars with data based on clusters 
    
- extract.features chunk:
    - Text variables: move to date extraction chunk ???
        - Mine acronyms
        - Mine places

- Review set_global_options chunk after features are finalized

### ![](<filename>.png)

## Potential next steps include:
- Organization:
    - Categorize by chunk
    - Priority criteria:
        0. Ease of change
        1. Impacts report
        2. Cleans innards
        3. Bug report
        
- all chunks:
    - at chunk-end rm(!glb_<var>)
    
- manage.missing.data chunk:
    - cleaner way to manage re-splitting of training vs. new entity

- extract.features chunk:
    - Add n-grams for glb_txt_vars
        - "RTextTools", "tau", "RWeka", and "textcat" packages
    - Convert user-specified mutate code to config specs
    
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

- fit.all.training chunk:
    - myplot_prediction_classification: displays 'x' instead of '+' when there are no prediction errors 
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
source("~/Dropbox/datascience/R/myscript.R")
source("~/Dropbox/datascience/R/mydsutils.R")
```

```
## Loading required package: caret
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
source("~/Dropbox/datascience/R/myplot.R")
source("~/Dropbox/datascience/R/mypetrinet.R")
source("~/Dropbox/datascience/R/myplclust.R")
# Gather all package requirements here
suppressPackageStartupMessages(require(doMC))
registerDoMC(4) # max(length(glb_txt_vars), glb_n_cv_folds) + 1
#packageVersion("snow")
#require(sos); findFn("cosine", maxPages=2, sortby="MaxScore")

# Analysis control global variables
glb_trnng_url <- "https://courses.edx.org/asset-v1:MITx+15.071x_2a+2T2015+type@asset+block/statedata.csv"
glb_newdt_url <- "<newdt_url>"
glb_out_pfx <- "HW2_v2_"
glb_save_envir <- FALSE # or TRUE

glb_is_separate_newobs_dataset <- FALSE    # or TRUE
    glb_split_entity_newobs_datasets <- TRUE   # or FALSE
    glb_split_newdata_method <- "sample"          # "condition" or "sample" or "copy"
    glb_split_newdata_condition <- NULL # or "is.na(<var>)"; "<var> <condition_operator> <value>"
    glb_split_newdata_size_ratio <- 0.3               # > 0 & < 1
    glb_split_sample.seed <- 123               # or any integer

glb_max_fitobs <- NULL # or any integer                         
glb_is_regression <- TRUE; glb_is_classification <- !glb_is_regression; 
    glb_is_binomial <- NULL # or TRUE or FALSE

glb_rsp_var_raw <- "Life.Exp"

# for classification, the response variable has to be a factor
glb_rsp_var <- glb_rsp_var_raw # or "Life.Exp.fctr"

# if the response factor is based on numbers/logicals e.g (0/1 OR TRUE/FALSE vs. "A"/"B"), 
#   or contains spaces (e.g. "Not in Labor Force")
#   caret predict(..., type="prob") crashes
glb_map_rsp_raw_to_var <- NULL # or function(raw) {
#     return(log(raw))
#     ret_vals <- rep_len(NA, length(raw)); ret_vals[!is.na(raw)] <- ifelse(raw[!is.na(raw)] == 1, "Y", "N"); return(relevel(as.factor(ret_vals), ref="N"))
#     #as.factor(paste0("B", raw))
#     #as.factor(gsub(" ", "\\.", raw))    
# }
# glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0))

glb_map_rsp_var_to_raw <- NULL # or function(var) {
#     return(exp(var))
#     as.numeric(var) - 1
#     #as.numeric(var)
#     #gsub("\\.", " ", levels(var)[as.numeric(var)])
#     c("<=50K", " >50K")[as.numeric(var)]
#     #c(FALSE, TRUE)[as.numeric(var)]
# }
# glb_map_rsp_var_to_raw(glb_map_rsp_raw_to_var(c(1, 1, 0, 0, 0)))

if ((glb_rsp_var != glb_rsp_var_raw) & is.null(glb_map_rsp_raw_to_var))
    stop("glb_map_rsp_raw_to_var function expected")
glb_rsp_var_out <- paste0(glb_rsp_var, ".predict.") # model_id is appended later

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

# If multiple vars are parts of id, consider concatenating them to create one id var
# If glb_id_var == NULL, ".rownames <- row.names()" is the default
glb_id_var <- c("state.abb")
glb_category_vars <- NULL # or c("<var1>", "<var2>")
glb_drop_vars <- c(NULL) # or c("<col_name>")

glb_map_vars <- NULL # or c("<var1>", "<var2>")
glb_map_urls <- list();
# glb_map_urls[["<var1>"]] <- "<var1.url>"

glb_assign_pairs_lst <- NULL; 
# glb_assign_pairs_lst[["<var1>"]] <- list(from=c(NA),
#                                            to=c("NA.my"))
glb_assign_vars <- names(glb_assign_pairs_lst)

# Derived features
glb_derive_lst <- NULL;
# glb_derive_lst[["Week.bgn"]] <- list(
#     mapfn=function(Week) { return(substr(Week, 1, 10)) }
#     , args=c("Week"))

# require(zoo)
# # If glb_allobs_df is not sorted in the desired manner
# glb_derive_lst[["ILI.2.lag"]] <- list(
#     mapfn=function(Week) { return(coredata(lag(zoo(orderBy(~Week, glb_allobs_df)$ILI), -2, na.pad=TRUE))) }
#     , args=c("Week"))
# glb_derive_lst[["ILI.2.lag"]] <- list(
#     mapfn=function(ILI) { return(coredata(lag(zoo(ILI), -2, na.pad=TRUE))) }
#     , args=c("ILI"))
# glb_derive_lst[["ILI.2.lag.log"]] <- list(
#     mapfn=function(ILI.2.lag) { return(log(ILI.2.lag)) }
#     , args=c("ILI.2.lag"))

#     mapfn=function(PTS, oppPTS) { return(PTS - oppPTS) }
#     , args=c("PTS", "oppPTS"))

# Add logs of numerics that are not distributed normally ->  do automatically ???

#     mapfn=function(raw) { tfr_raw <- as.character(cut(raw, 5)); 
#                           tfr_raw[is.na(tfr_raw)] <- "NA.my";
#                           return(as.factor(tfr_raw)) }

# glb_derive_lst[["<txt_var>.niso8859.log"]] <- list(
#     mapfn=function(<txt_var>) { match_lst <- gregexpr("&#[[:digit:]]{3};", <txt_var>)
#                         match_num_vctr <- unlist(lapply(match_lst, 
#                                                         function(elem) length(elem)))
#                         return(log(1 + match_num_vctr)) }
#     , args=c("<txt_var>"))

#     mapfn=function(raw) { mod_raw <- raw;
#         mod_raw <- gsub("&#[[:digit:]]{3};", " ", mod_raw);
#         # Modifications for this exercise only
#         mod_raw <- gsub("\\bgoodIn ", "good In", mod_raw);
#                           return(mod_raw)

#         # Create user-specified pattern vectors 
# #sum(mycount_pattern_occ("Metropolitan Diary:", glb_allobs_df$Abstract) > 0)
#         if (txt_var %in% c("Snippet", "Abstract")) {
#             txt_X_df[, paste0(txt_var_pfx, ".P.metropolitan.diary.colon")] <-
#                 as.integer(0 + mycount_pattern_occ("Metropolitan Diary:", 
#                                                    glb_allobs_df[, txt_var]))
#summary(glb_allobs_df[ ,grep("P.on.this.day", names(glb_allobs_df), value=TRUE)])

# args_lst <- NULL; for (arg in glb_derive_lst[["Week.bgn"]]$args) args_lst[[arg]] <- glb_allobs_df[, arg]; do.call(mapfn, args_lst)

# glb_derive_lst[["<var1>"]] <- glb_derive_lst[["<var2>"]]
glb_derive_vars <- names(glb_derive_lst)

glb_date_vars <- NULL # or c("<date_var>")
glb_date_fmts <- list(); #glb_date_fmts[["<date_var>"]] <- "%m/%e/%y"
glb_date_tzs <- list();  #glb_date_tzs[["<date_var>"]] <- "America/New_York"
#grep("America/New", OlsonNames(), value=TRUE)

glb_txt_vars <- NULL # or c("<txt_var1>", "<txt_var2>")   
#Sys.setlocale("LC_ALL", "C") # For english

glb_append_stop_words <- list()
# Remember to use unstemmed words
#orderBy(~ -cor.y.abs, subset(glb_feats_df, grepl("[HSA]\\.T\\.", id) & !is.na(cor.high.X)))
#dsp_obs(Headline.contains="polit")
#subset(glb_allobs_df, H.T.compani > 0)[, c("UniqueID", "Headline", "H.T.compani")]
# glb_append_stop_words[["<txt_var1>"]] <- c(NULL
# #                             ,"<word1>" # <reason1>
#                             )
#subset(glb_allobs_df, S.T.newyorktim > 0)[, c("UniqueID", "Snippet", "S.T.newyorktim")]
#glb_txt_lst[["Snippet"]][which(glb_allobs_df$UniqueID %in% c(8394, 8317, 8339, 8350, 8307))]

glb_important_terms <- list()
# Remember to use stemmed terms 

glb_sprs_thresholds <- NULL # or c(0.988, 0.970, 0.970) # Generates 29, 22, 22 terms
# Properties:
#   numrows(glb_feats_df) << numrows(glb_fitobs_df)
#   Select terms that appear in at least 0.2 * O(FP/FN(glb_OOBobs_df))
#       numrows(glb_OOBobs_df) = 1.1 * numrows(glb_newobs_df)
names(glb_sprs_thresholds) <- glb_txt_vars

# User-specified exclusions  
glb_exclude_vars_as_features <- c("state.name") 
if (glb_rsp_var_raw != glb_rsp_var)
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                            glb_rsp_var_raw)

# List feats that shd be excluded due to known causation by prediction variable
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                      c(NULL)) # or c("<col_name>")

glb_impute_na_data <- FALSE # or TRUE
glb_mice_complete.seed <- 144 # or any integer

glb_cluster <- FALSE # or TRUE

glb_interaction_only_features <- NULL # or ???

glb_models_lst <- list(); glb_models_df <- data.frame()
# Regression
if (glb_is_regression)
    glb_models_method_vctr <- c("lm", "glm", "bayesglm", "rpart", "rf") else
# Classification
    if (glb_is_binomial)
        glb_models_method_vctr <- c("glm", "bayesglm", "rpart", "rf") else  
        glb_models_method_vctr <- c("rpart", "rf")

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
    #data.frame(parameter="mtry",  min=080, max=100, by=10),
    #data.frame(parameter="mtry",  min=08, max=10, by=1),    
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
        glb_model_evl_criteria <- 
            c("max.Accuracy.OOB", "max.auc.OOB", "max.Kappa.OOB", "min.aic.fit") else
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

![](USCensus1977_State_HW2_v2_files/figure-html/set_global_options-1.png) 

```r
glb_analytics_avl_objs <- NULL

glb_chunks_df <- myadd_chunk(NULL, "import.data")
```

```
##         label step_major step_minor   bgn end elapsed
## 1 import.data          1          0 8.926  NA      NA
```

## Step `1.0: import data`
#### chunk option: eval=<r condition>

```r
#glb_chunks_df <- myadd_chunk(NULL, "import.data")

glb_trnobs_df <- myimport_data(url=glb_trnng_url, comment="glb_trnobs_df", 
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
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
## 36       2715   3983        1.1    71.42    6.4    51.6    82  68782
## 48       1799   3617        1.4    69.48    6.7    41.6   100  24070
##    state.abb state.area         x       y     state.division    state.name
## 2         AK     589757 -127.2500 49.2500            Pacific        Alaska
## 8         DE       2057  -74.9841 38.6777     South Atlantic      Delaware
## 26        MT     147138 -109.3200 46.8230           Mountain       Montana
## 34        ND      70665 -100.0990 47.2517 West North Central  North Dakota
## 36        OK      69919  -97.1239 35.5053 West South Central      Oklahoma
## 48        WV      24181  -80.6665 38.4204     South Atlantic West Virginia
##     state.region
## 2           West
## 8          South
## 26          West
## 34 North Central
## 36         South
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
##  - attr(*, "comment")= chr "glb_trnobs_df"
## NULL
```

```r
# glb_trnobs_df <- read.delim("data/hygiene.txt", header=TRUE, fill=TRUE, sep="\t",
#                             fileEncoding='iso-8859-1')
# glb_trnobs_df <- read.table("data/hygiene.dat.labels", col.names=c("dirty"),
#                             na.strings="[none]")
# glb_trnobs_df$review <- readLines("data/hygiene.dat", n =-1)
# comment(glb_trnobs_df) <- "glb_trnobs_df"                                

# glb_trnobs_df <- data.frame()
# for (symbol in c("Boeing", "CocaCola", "GE", "IBM", "ProcterGamble")) {
#     sym_trnobs_df <- 
#         myimport_data(url=gsub("IBM", symbol, glb_trnng_url), comment="glb_trnobs_df", 
#                                     force_header=TRUE)
#     sym_trnobs_df$Symbol <- symbol
#     glb_trnobs_df <- myrbind_df(glb_trnobs_df, sym_trnobs_df)
# }
                                
# glb_trnobs_df <- 
#     glb_trnobs_df %>% dplyr::filter(Year >= 1999)
                                
if (glb_is_separate_newobs_dataset) {
    glb_newobs_df <- myimport_data(url=glb_newdt_url, comment="glb_newobs_df", 
                                   force_header=TRUE)
    
    # To make plots / stats / checks easier in chunk:inspectORexplore.data
    glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df); 
    comment(glb_allobs_df) <- "glb_allobs_df"
} else {
    glb_allobs_df <- glb_trnobs_df; comment(glb_allobs_df) <- "glb_allobs_df"
    if (!glb_split_entity_newobs_datasets) {
        stop("Not implemented yet") 
        glb_newobs_df <- glb_trnobs_df[sample(1:nrow(glb_trnobs_df),
                                          max(2, nrow(glb_trnobs_df) / 1000)),]                    
    } else      if (glb_split_newdata_method == "condition") {
            glb_newobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=glb_split_newdata_condition)))
            glb_trnobs_df <- do.call("subset", 
                list(glb_trnobs_df, parse(text=paste0("!(", 
                                                      glb_split_newdata_condition,
                                                      ")"))))
        } else if (glb_split_newdata_method == "sample") {
                require(caTools)
                
                set.seed(glb_split_sample.seed)
                split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
                                      SplitRatio=(1-glb_split_newdata_size_ratio))
                glb_newobs_df <- glb_trnobs_df[!split, ] 
                glb_trnobs_df <- glb_trnobs_df[split ,]
        } else if (glb_split_newdata_method == "copy") {  
            glb_trnobs_df <- glb_allobs_df
            comment(glb_trnobs_df) <- "glb_trnobs_df"
            glb_newobs_df <- glb_allobs_df
            comment(glb_newobs_df) <- "glb_newobs_df"
        } else stop("glb_split_newdata_method should be %in% c('condition', 'sample', 'copy')")   

    comment(glb_newobs_df) <- "glb_newobs_df"
    myprint_df(glb_newobs_df)
    str(glb_newobs_df)

    if (glb_split_entity_newobs_datasets) {
        myprint_df(glb_trnobs_df)
        str(glb_trnobs_df)        
    }
}         
```

```
## Loading required package: caTools
```

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 4        2110   3378        1.9    70.66   10.1    39.9    65  51945
## 5       21198   5114        1.1    71.71   10.3    62.6    20 156361
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 16       2280   4669        0.6    72.58    4.5    59.9   114  81787
## 20       4122   5299        0.9    70.22    8.5    52.3   101   9891
## 21       5814   4755        1.1    71.83    3.3    58.5   103   7826
## 24       2341   3098        2.4    68.09   12.5    41.0    50  47296
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
## 31       1144   3601        2.2    70.32    9.7    55.2   120 121412
## 32      18076   4903        1.4    70.55   10.9    52.7    82  47831
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
## 37       2284   4660        0.6    72.13    4.2    60.0    44  96184
## 50        376   4566        0.6    70.29    6.9    62.9   173  97203
##    state.abb state.area         x       y     state.division    state.name
## 2         AK     589757 -127.2500 49.2500            Pacific        Alaska
## 4         AR      53104  -92.2992 34.7336 West South Central      Arkansas
## 5         CA     158693 -119.7730 36.5341            Pacific    California
## 8         DE       2057  -74.9841 38.6777     South Atlantic      Delaware
## 11        HI       6450 -126.2500 31.7500            Pacific        Hawaii
## 16        KS      82264  -98.1156 38.4204 West North Central        Kansas
## 20        MD      10577  -76.6459 39.2778     South Atlantic      Maryland
## 21        MA       8257  -71.5800 42.3645        New England Massachusetts
## 24        MS      47716  -89.8065 32.6758 East South Central   Mississippi
## 26        MT     147138 -109.3200 46.8230           Mountain       Montana
## 31        NM     121666 -105.9420 34.4764           Mountain    New Mexico
## 32        NY      49576  -75.1449 43.1361    Middle Atlantic      New York
## 34        ND      70665 -100.0990 47.2517 West North Central  North Dakota
## 37        OR      96981 -120.0680 43.9078            Pacific        Oregon
## 50        WY      97914 -107.2560 43.0504           Mountain       Wyoming
##     state.region
## 2           West
## 4          South
## 5           West
## 8          South
## 11          West
## 16 North Central
## 20         South
## 21     Northeast
## 24         South
## 26          West
## 31          West
## 32     Northeast
## 34 North Central
## 37          West
## 50          West
## 'data.frame':	15 obs. of  15 variables:
##  $ Population    : int  365 2110 21198 579 868 2280 4122 5814 2341 746 ...
##  $ Income        : int  6315 3378 5114 4809 4963 4669 5299 4755 3098 4347 ...
##  $ Illiteracy    : num  1.5 1.9 1.1 0.9 1.9 0.6 0.9 1.1 2.4 0.6 ...
##  $ Life.Exp      : num  69.3 70.7 71.7 70.1 73.6 ...
##  $ Murder        : num  11.3 10.1 10.3 6.2 6.2 4.5 8.5 3.3 12.5 5 ...
##  $ HS.Grad       : num  66.7 39.9 62.6 54.6 61.9 59.9 52.3 58.5 41 59.2 ...
##  $ Frost         : int  152 65 20 103 0 114 101 103 50 155 ...
##  $ Area          : int  566432 51945 156361 1982 6425 81787 9891 7826 47296 145587 ...
##  $ state.abb     : chr  "AK" "AR" "CA" "DE" ...
##  $ state.area    : int  589757 53104 158693 2057 6450 82264 10577 8257 47716 147138 ...
##  $ x             : num  -127.2 -92.3 -119.8 -75 -126.2 ...
##  $ y             : num  49.2 34.7 36.5 38.7 31.8 ...
##  $ state.division: chr  "Pacific" "West South Central" "Pacific" "South Atlantic" ...
##  $ state.name    : chr  "Alaska" "Arkansas" "California" "Delaware" ...
##  $ state.region  : chr  "West" "South" "West" "South" ...
##  - attr(*, "comment")= chr "glb_newobs_df"
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 1        3615   3624        2.1    69.05   15.1    41.3    20  50708
## 3        2212   4530        1.8    70.55    7.8    58.1    15 113417
## 6        2541   4884        0.7    72.06    6.8    63.9   166 103766
## 7        3100   5348        1.1    72.48    3.1    56.0   139   4862
## 9        8277   4815        1.3    70.66   10.7    52.6    11  54090
## 10       4931   4091        2.0    68.54   13.9    40.6    60  58073
##    state.abb state.area         x       y     state.division  state.name
## 1         AL      51609  -86.7509 32.5901 East South Central     Alabama
## 3         AZ     113909 -111.6250 34.2192           Mountain     Arizona
## 6         CO     104247 -105.5130 38.6777           Mountain    Colorado
## 7         CT       5009  -72.3573 41.5928        New England Connecticut
## 9         FL      58560  -81.6850 27.8744     South Atlantic     Florida
## 10        GA      58876  -83.3736 32.3329     South Atlantic     Georgia
##    state.region
## 1         South
## 3          West
## 6          West
## 7     Northeast
## 9         South
## 10        South
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 3        2212   4530        1.8    70.55    7.8    58.1    15 113417
## 7        3100   5348        1.1    72.48    3.1    56.0   139   4862
## 12        813   4119        0.6    71.87    5.3    59.5   126  82677
## 25       4767   4254        0.8    70.69    9.3    48.8   108  68995
## 28        590   5149        0.5    69.03   11.5    65.2   188 109889
## 41        681   4167        0.5    72.08    1.7    53.3   172  75955
##    state.abb state.area         x       y     state.division   state.name
## 3         AZ     113909 -111.6250 34.2192           Mountain      Arizona
## 7         CT       5009  -72.3573 41.5928        New England  Connecticut
## 12        ID      83557 -113.9300 43.5648           Mountain        Idaho
## 25        MO      69686  -92.5137 38.3347 West North Central     Missouri
## 28        NV     110540 -116.8510 39.1063           Mountain       Nevada
## 41        SD      77047  -99.7238 44.3365 West North Central South Dakota
##     state.region
## 3           West
## 7      Northeast
## 12          West
## 25 North Central
## 28          West
## 41 North Central
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost  Area
## 44       1203   4022        0.6    72.90    4.5    67.3   137 82096
## 45        472   3907        0.6    71.64    5.5    57.1   168  9267
## 46       4981   4701        1.4    70.08    9.5    47.8    85 39780
## 47       3559   4864        0.6    71.72    4.3    63.5    32 66570
## 48       1799   3617        1.4    69.48    6.7    41.6   100 24070
## 49       4589   4468        0.7    72.48    3.0    54.5   149 54464
##    state.abb state.area         x       y     state.division    state.name
## 44        UT      84916 -111.3300 39.1063           Mountain          Utah
## 45        VT       9609  -72.5450 44.2508        New England       Vermont
## 46        VA      40815  -78.2005 37.5630     South Atlantic      Virginia
## 47        WA      68192 -119.7460 47.4231            Pacific    Washington
## 48        WV      24181  -80.6665 38.4204     South Atlantic West Virginia
## 49        WI      56154  -89.9941 44.5937 East North Central     Wisconsin
##     state.region
## 44          West
## 45     Northeast
## 46         South
## 47          West
## 48         South
## 49 North Central
## 'data.frame':	35 obs. of  15 variables:
##  $ Population    : int  3615 2212 2541 3100 8277 4931 813 11197 5313 2861 ...
##  $ Income        : int  3624 4530 4884 5348 4815 4091 4119 5107 4458 4628 ...
##  $ Illiteracy    : num  2.1 1.8 0.7 1.1 1.3 2 0.6 0.9 0.7 0.5 ...
##  $ Life.Exp      : num  69 70.5 72.1 72.5 70.7 ...
##  $ Murder        : num  15.1 7.8 6.8 3.1 10.7 13.9 5.3 10.3 7.1 2.3 ...
##  $ HS.Grad       : num  41.3 58.1 63.9 56 52.6 40.6 59.5 52.6 52.9 59 ...
##  $ Frost         : int  20 15 166 139 11 60 126 127 122 140 ...
##  $ Area          : int  50708 113417 103766 4862 54090 58073 82677 55748 36097 55941 ...
##  $ state.abb     : chr  "AL" "AZ" "CO" "CT" ...
##  $ state.area    : int  51609 113909 104247 5009 58560 58876 83557 56400 36291 56290 ...
##  $ x             : num  -86.8 -111.6 -105.5 -72.4 -81.7 ...
##  $ y             : num  32.6 34.2 38.7 41.6 27.9 ...
##  $ state.division: chr  "East South Central" "Mountain" "Mountain" "New England" ...
##  $ state.name    : chr  "Alabama" "Arizona" "Colorado" "Connecticut" ...
##  $ state.region  : chr  "South" "West" "West" "Northeast" ...
##  - attr(*, "comment")= chr "glb_trnobs_df"
```

```r
if ((num_nas <- sum(is.na(glb_trnobs_df[, glb_rsp_var_raw]))) > 0)
    stop("glb_trnobs_df$", glb_rsp_var_raw, " contains NAs for ", num_nas, " obs")

if (nrow(glb_trnobs_df) == nrow(glb_allobs_df))
    warning("glb_trnobs_df same as glb_allobs_df")
if (nrow(glb_newobs_df) == nrow(glb_allobs_df))
    warning("glb_newobs_df same as glb_allobs_df")

if (length(glb_drop_vars) > 0) {
    warning("dropping vars: ", paste0(glb_drop_vars, collapse=", "))
    glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), glb_drop_vars)]
    glb_trnobs_df <- glb_trnobs_df[, setdiff(names(glb_trnobs_df), glb_drop_vars)]    
    glb_newobs_df <- glb_newobs_df[, setdiff(names(glb_newobs_df), glb_drop_vars)]    
}

#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Combine trnent & newobs into glb_allobs_df for easier manipulation
glb_trnobs_df$.src <- "Train"; glb_newobs_df$.src <- "Test"; 
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, ".src")
glb_allobs_df <- myrbind_df(glb_trnobs_df, glb_newobs_df)
comment(glb_allobs_df) <- "glb_allobs_df"

# Check for duplicates in glb_id_var
if (length(glb_id_var) == 0) {
    warning("using .rownames as identifiers for observations")
    glb_allobs_df$.rownames <- rownames(glb_allobs_df)
    glb_trnobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Train"))
    glb_newobs_df$.rownames <- rownames(subset(glb_allobs_df, .src == "Test"))    
    glb_id_var <- ".rownames"
}
if (sum(duplicated(glb_allobs_df[, glb_id_var, FALSE])) > 0)
    stop(glb_id_var, " duplicated in glb_allobs_df")
glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_id_var)

glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
glb_trnobs_df <- glb_newobs_df <- NULL

glb_chunks_df <- myadd_chunk(glb_chunks_df, "inspect.data", major.inc=TRUE)
```

```
##          label step_major step_minor   bgn   end elapsed
## 1  import.data          1          0 8.926 9.365    0.44
## 2 inspect.data          2          0 9.366    NA      NA
```

## Step `2.0: inspect data`

```r
#print(str(glb_allobs_df))
#View(glb_allobs_df)

dsp_class_dstrb <- function(var) {
    xtab_df <- mycreate_xtab_df(glb_allobs_df, c(".src", var))
    rownames(xtab_df) <- xtab_df$.src
    xtab_df <- subset(xtab_df, select=-.src)
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Performed repeatedly in other chunks
glb_chk_data <- function() {
    # Histogram of predictor in glb_trnobs_df & glb_newobs_df
    print(myplot_histogram(glb_allobs_df, glb_rsp_var_raw) + facet_wrap(~ .src))
    
    if (glb_is_classification) 
        dsp_class_dstrb(var=ifelse(glb_rsp_var %in% names(glb_allobs_df), 
                                   glb_rsp_var, glb_rsp_var_raw))
    mycheck_problem_data(glb_allobs_df)
}
glb_chk_data()
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-1.png) 

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Frost 
##     1 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      state.abb state.division     state.name   state.region 
##              0              0              0              0
```

```r
# Create new features that help diagnostics
if (!is.null(glb_map_rsp_raw_to_var)) {
    glb_allobs_df[, glb_rsp_var] <- 
        glb_map_rsp_raw_to_var(glb_allobs_df[, glb_rsp_var_raw])
    mycheck_map_results(mapd_df=glb_allobs_df, 
                        from_col_name=glb_rsp_var_raw, to_col_name=glb_rsp_var)
        
    if (glb_is_classification) dsp_class_dstrb(glb_rsp_var)
}

# check distribution of all numeric data
dsp_numeric_feats_dstrb <- function(feats_vctr) {
    for (feat in feats_vctr) {
        print(sprintf("feat: %s", feat))
        if (glb_is_regression)
            gp <- myplot_scatter(df=glb_allobs_df, ycol_name=glb_rsp_var, xcol_name=feat,
                                 smooth=TRUE)
        if (glb_is_classification)
            gp <- myplot_box(df=glb_allobs_df, ycol_names=feat, xcol_name=glb_rsp_var)
        if (inherits(glb_allobs_df[, feat], "factor"))
            gp <- gp + facet_wrap(reformulate(feat))
        print(gp)
    }
}
# dsp_numeric_vars_dstrb(setdiff(names(glb_allobs_df), 
#                                 union(myfind_chr_cols_df(glb_allobs_df), 
#                                       c(glb_rsp_var_raw, glb_rsp_var))))                                      

add_new_diag_feats <- function(obs_df, ref_df=glb_allobs_df) {
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

#         <col_name>=<table>[as.character(<col2_name>)],
#         <col_name>=as.numeric(<col2_name>),

#         <col_name> = trunc(<col2_name> / 100),

        .rnorm = rnorm(n=nrow(obs_df))
                        )

    # If levels of a factor are different across obs_df & glb_newobs_df; predict.glm fails  
    # Transformations not handled by mutate
#     obs_df$<col_name>.fctr.num <- sapply(1:nrow(obs_df), 
#         function(row_ix) grep(obs_df[row_ix, "<col_name>"],
#                               levels(obs_df[row_ix, "<col_name>.fctr"])))
    
    #print(summary(obs_df))
    #print(sapply(names(obs_df), function(col) sum(is.na(obs_df[, col]))))
    return(obs_df)
}
glb_allobs_df <- add_new_diag_feats(glb_allobs_df)
```

```
## Loading required package: plyr
```

```r
require(dplyr)
```

```
## Loading required package: dplyr
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df # glb_allobs_df <- sav_allobs_df
# Merge some <descriptor>
# glb_allobs_df$<descriptor>.my <- glb_allobs_df$<descriptor>
# glb_allobs_df[grepl("\\bAIRPORT\\b", glb_allobs_df$<descriptor>.my),
#               "<descriptor>.my"] <- "AIRPORT"
# glb_allobs_df$<descriptor>.my <-
#     plyr::revalue(glb_allobs_df$<descriptor>.my, c(
#         "ABANDONED BUILDING" = "OTHER",
#         "##"                      = "##"
#     ))
# print(<descriptor>_freq_df <- mycreate_sqlxtab_df(glb_allobs_df, c("<descriptor>.my")))
# # print(dplyr::filter(<descriptor>_freq_df, grepl("(MEDICAL|DENTAL|OFFICE)", <descriptor>.my)))
# # print(dplyr::filter(dplyr::select(glb_allobs_df, -<var.zoo>), 
# #                     grepl("STORE", <descriptor>.my)))
# glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, "<descriptor>")

# Check distributions of newly transformed / extracted vars
#   Enhancement: remove vars that were displayed ealier
dsp_numeric_feats_dstrb(feats_vctr=setdiff(names(glb_allobs_df), 
        c(myfind_chr_cols_df(glb_allobs_df), glb_rsp_var_raw, glb_rsp_var, 
          glb_exclude_vars_as_features)))
```

```
## [1] "feat: Population"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-2.png) 

```
## [1] "feat: Income"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-3.png) 

```
## [1] "feat: Illiteracy"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-4.png) 

```
## [1] "feat: Murder"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-5.png) 

```
## [1] "feat: HS.Grad"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-6.png) 

```
## [1] "feat: Frost"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-7.png) 

```
## [1] "feat: Area"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-8.png) 

```
## [1] "feat: state.area"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-9.png) 

```
## [1] "feat: x"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-10.png) 

```
## [1] "feat: y"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-11.png) 

```
## [1] "feat: .rnorm"
```

![](USCensus1977_State_HW2_v2_files/figure-html/inspect.data-12.png) 

```r
#   Convert factors to dummy variables
#   Build splines   require(splines); bsBasis <- bs(training$age, df=3)

#pairs(subset(glb_trnobs_df, select=-c(col_symbol)))
# Check for glb_newobs_df & glb_trnobs_df features range mismatches

# Other diagnostics:
# print(subset(glb_trnobs_df, <col1_name> == max(glb_trnobs_df$<col1_name>, na.rm=TRUE) & 
#                         <col2_name> <= mean(glb_trnobs_df$<col1_name>, na.rm=TRUE)))

# print(glb_trnobs_df[which.max(glb_trnobs_df$<col_name>),])

# print(<col_name>_freq_glb_trnobs_df <- mycreate_tbl_df(glb_trnobs_df, "<col_name>"))
# print(which.min(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col_name>)))
# print(which.max(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>)[, 2]))
# print(table(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>))
# print(table(is.na(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(table(sign(glb_trnobs_df$<col1_name>), glb_trnobs_df$<col2_name>))
# print(mycreate_xtab_df(glb_trnobs_df, <col1_name>))
# print(mycreate_xtab_df(glb_trnobs_df, c(<col1_name>, <col2_name>)))
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mycreate_xtab_df(glb_trnobs_df, c("<col1_name>", "<col2_name>")))
# <col1_name>_<col2_name>_xtab_glb_trnobs_df[is.na(<col1_name>_<col2_name>_xtab_glb_trnobs_df)] <- 0
# print(<col1_name>_<col2_name>_xtab_glb_trnobs_df <- 
#   mutate(<col1_name>_<col2_name>_xtab_glb_trnobs_df, 
#             <col3_name>=(<col1_name> * 1.0) / (<col1_name> + <col2_name>))) 
# print(mycreate_sqlxtab_df(glb_allobs_df, c("<col1_name>", "<col2_name>")))

# print(<col2_name>_min_entity_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>, glb_trnobs_df$<col2_name>, min, na.rm=TRUE)))
# print(<col1_name>_na_by_<col2_name>_arr <- 
#    sort(tapply(glb_trnobs_df$<col1_name>.NA, glb_trnobs_df$<col2_name>, mean, na.rm=TRUE)))

# Other plots:
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>"))
# print(myplot_box(df=glb_trnobs_df, ycol_names="<col1_name>", xcol_name="<col2_name>"))
# print(myplot_line(subset(glb_trnobs_df, Symbol %in% c("CocaCola", "ProcterGamble")), 
#                   "Date.POSIX", "StockPrice", facet_row_colnames="Symbol") + 
#     geom_vline(xintercept=as.numeric(as.POSIXlt("2003-03-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1983-01-01")))        
#         )
# print(myplot_line(subset(glb_trnobs_df, Date.POSIX > as.POSIXct("2004-01-01")), 
#                   "Date.POSIX", "StockPrice") +
#     geom_line(aes(color=Symbol)) + 
#     coord_cartesian(xlim=c(as.POSIXct("1990-01-01"),
#                            as.POSIXct("2000-01-01"))) +     
#     coord_cartesian(ylim=c(0, 250)) +     
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-09-01"))) +
#     geom_vline(xintercept=as.numeric(as.POSIXlt("1997-11-01")))        
#         )
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))
# print(myplot_scatter(glb_allobs_df, "<col1_name>", "<col2_name>", colorcol_name="<Pred.fctr>") + 
#         geom_point(data=subset(glb_allobs_df, <condition>), 
#                     mapping=aes(x=<x_var>, y=<y_var>), color="red", shape=4, size=5) +
#         geom_vline(xintercept=84))

glb_chunks_df <- myadd_chunk(glb_chunks_df, "scrub.data", major.inc=FALSE)
```

```
##          label step_major step_minor    bgn    end elapsed
## 2 inspect.data          2          0  9.366 13.968   4.602
## 3   scrub.data          2          1 13.968     NA      NA
```

### Step `2.1: scrub data`

```r
mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Frost 
##     1 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      state.abb state.division     state.name   state.region 
##              0              0              0              0
```

```r
dsp_catgs <- function() {
    print("NewsDesk:")
    print(table(glb_allobs_df$NewsDesk))
    print("SectionName:")    
    print(table(glb_allobs_df$SectionName))
    print("SubsectionName:")        
    print(table(glb_allobs_df$SubsectionName))
}

# sel_obs <- function(Popular=NULL, 
#                     NewsDesk=NULL, SectionName=NULL, SubsectionName=NULL,
#         Headline.contains=NULL, Snippet.contains=NULL, Abstract.contains=NULL,
#         Headline.pfx=NULL, NewsDesk.nb=NULL, .clusterid=NULL, myCategory=NULL,
#         perl=FALSE) {
sel_obs <- function(vars_lst) {
    tmp_df <- glb_allobs_df
    # Does not work for Popular == NAs ???
    if (!is.null(Popular)) {
        if (is.na(Popular))
            tmp_df <- tmp_df[is.na(tmp_df$Popular), ] else   
            tmp_df <- tmp_df[tmp_df$Popular == Popular, ]    
    }    
    if (!is.null(NewsDesk)) 
        tmp_df <- tmp_df[tmp_df$NewsDesk == NewsDesk, ]
    if (!is.null(SectionName)) 
        tmp_df <- tmp_df[tmp_df$SectionName == SectionName, ]
    if (!is.null(SubsectionName)) 
        tmp_df <- tmp_df[tmp_df$SubsectionName == SubsectionName, ]
    if (!is.null(Headline.contains))
        tmp_df <- 
            tmp_df[grep(Headline.contains, tmp_df$Headline, perl=perl), ]
    if (!is.null(Snippet.contains))
        tmp_df <- 
            tmp_df[grep(Snippet.contains, tmp_df$Snippet, perl=perl), ]
    if (!is.null(Abstract.contains))
        tmp_df <- 
            tmp_df[grep(Abstract.contains, tmp_df$Abstract, perl=perl), ]
    if (!is.null(Headline.pfx)) {
        if (length(grep("Headline.pfx", names(tmp_df), fixed=TRUE, value=TRUE))
            > 0) tmp_df <- 
                tmp_df[tmp_df$Headline.pfx == Headline.pfx, ] else
        warning("glb_allobs_df does not contain Headline.pfx; ignoring that filter")                    
    }    
    if (!is.null(NewsDesk.nb)) {
        if (any(grepl("NewsDesk.nb", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$NewsDesk.nb == NewsDesk.nb, ] else
        warning("glb_allobs_df does not contain NewsDesk.nb; ignoring that filter")                    
    }    
    if (!is.null(.clusterid)) {
        if (any(grepl(".clusterid", names(tmp_df), fixed=TRUE)) > 0) 
            tmp_df <- 
                tmp_df[tmp_df$clusterid == clusterid, ] else
        warning("glb_allobs_df does not contain clusterid; ignoring that filter")                       }
    if (!is.null(myCategory)) {    
        if (!(myCategory %in% names(glb_allobs_df)))
            tmp_df <-
                tmp_df[tmp_df$myCategory == myCategory, ] else
        warning("glb_allobs_df does not contain myCategory; ignoring that filter")                    
    }    
    
    return(glb_allobs_df$UniqueID %in% tmp_df$UniqueID)
}

dsp_obs <- function(..., cols=c(NULL), all=FALSE) {
    tmp_df <- glb_allobs_df[sel_obs(...), 
                            union(c("UniqueID", "Popular", "myCategory", "Headline"), cols), FALSE]
    if(all) { print(tmp_df) } else { myprint_df(tmp_df) }
}
#dsp_obs(Popular=1, NewsDesk="", SectionName="", Headline.contains="Boehner")
# dsp_obs(Popular=1, NewsDesk="", SectionName="")
# dsp_obs(Popular=NA, NewsDesk="", SectionName="")

dsp_tbl <- function(...) {
    tmp_entity_df <- glb_allobs_df[sel_obs(...), ]
    tmp_tbl <- table(tmp_entity_df$NewsDesk, 
                     tmp_entity_df$SectionName,
                     tmp_entity_df$SubsectionName, 
                     tmp_entity_df$Popular, useNA="ifany")
    #print(names(tmp_tbl))
    #print(dimnames(tmp_tbl))
    print(tmp_tbl)
}

dsp_hdlxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
                           c("Headline.pfx", "Headline", glb_rsp_var)))
#dsp_hdlxtab("(1914)|(1939)")

dsp_catxtab <- function(str) 
    print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains=str), ],
        c("Headline.pfx", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# dsp_catxtab("1914)|(1939)")
# dsp_catxtab("19(14|39|64):")
# dsp_catxtab("19..:")

# Create myCategory <- NewsDesk#SectionName#SubsectionName
#   Fix some data before merging categories
# glb_allobs_df[sel_obs(Headline.contains="Your Turn:", NewsDesk=""),
#               "NewsDesk"] <- "Styles"
# glb_allobs_df[sel_obs(Headline.contains="School", NewsDesk="", SectionName="U.S.",
#                       SubsectionName=""),
#               "SubsectionName"] <- "Education"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SectionName"] <- "Business Day"
# glb_allobs_df[sel_obs(Headline.contains="Today in Small Business:", NewsDesk="Business"),
#               "SubsectionName"] <- "Small Business"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SectionName"] <- "Opinion"
# glb_allobs_df[sel_obs(Headline.contains="Readers Respond:"),
#               "SubsectionName"] <- "Room For Debate"

# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName="", Popular=NA),
#               "SubsectionName"] <- "Small Business"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(7973), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(NewsDesk="Business", SectionName="", SubsectionName=""),
#               "SectionName"] <- "Technology"
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5076, 5736, 5924, 5911, 6532), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df[sel_obs(SectionName="Health"),
#               "NewsDesk"] <- "Science"
# glb_allobs_df[sel_obs(SectionName="Travel"),
#               "NewsDesk"] <- "Travel"
# 
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SectionName"] <- ""
# glb_allobs_df[sel_obs(SubsectionName="Fashion & Style"),
#               "SubsectionName"] <- ""
# glb_allobs_df[sel_obs(NewsDesk="Styles", SectionName="", SubsectionName="", Popular=1),
#               "SectionName"] <- "U.S."
# print(glb_allobs_df[glb_allobs_df$UniqueID %in% c(5486), 
#     c("UniqueID", "Headline", "myCategory", "NewsDesk", "SectionName", "SubsectionName")])
# 
# glb_allobs_df$myCategory <- paste(glb_allobs_df$NewsDesk, 
#                                   glb_allobs_df$SectionName,
#                                   glb_allobs_df$SubsectionName,
#                                   sep="#")

# dsp_obs( Headline.contains="Music:"
#         #,NewsDesk=""
#         #,SectionName=""  
#         #,SubsectionName="Fashion & Style"
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
# dsp_obs( Headline.contains="."
#         ,NewsDesk=""
#         ,SectionName="Opinion"  
#         ,SubsectionName=""
#         #,Popular=1 #NA
#         ,cols= c("UniqueID", "Headline", "Popular", "myCategory", 
#                 "NewsDesk", "SectionName", "SubsectionName"),
#         all=TRUE)
                                        
# Merge some categories
# glb_allobs_df$myCategory <-
#     plyr::revalue(glb_allobs_df$myCategory, c(      
#         "#Business Day#Dealbook"            = "Business#Business Day#Dealbook",
#         "#Business Day#Small Business"      = "Business#Business Day#Small Business",
#         "#Crosswords/Games#"                = "Business#Crosswords/Games#",
#         "Business##"                        = "Business#Technology#",
#         "#Open#"                            = "Business#Technology#",
#         "#Technology#"                      = "Business#Technology#",
#         
#         "#Arts#"                            = "Culture#Arts#",        
#         "Culture##"                         = "Culture#Arts#",        
#         
#         "#World#Asia Pacific"               = "Foreign#World#Asia Pacific",        
#         "Foreign##"                         = "Foreign#World#",    
#         
#         "#N.Y. / Region#"                   = "Metro#N.Y. / Region#",  
#         
#         "#Opinion#"                         = "OpEd#Opinion#",                
#         "OpEd##"                            = "OpEd#Opinion#",        
# 
#         "#Health#"                          = "Science#Health#",
#         "Science##"                         = "Science#Health#",        
#         
#         "Styles##"                          = "Styles##Fashion",                        
#         "Styles#Health#"                    = "Science#Health#",                
#         "Styles#Style#Fashion & Style"      = "Styles##Fashion",        
# 
#         "#Travel#"                          = "Travel#Travel#",                
#         
#         "Magazine#Magazine#"                = "myOther",
#         "National##"                        = "myOther",
#         "National#U.S.#Politics"            = "myOther",        
#         "Sports##"                          = "myOther",
#         "Sports#Sports#"                    = "myOther",
#         "#U.S.#"                            = "myOther",        
#         
# 
# #         "Business##Small Business"        = "Business#Business Day#Small Business",        
# #         
# #         "#Opinion#"                       = "#Opinion#Room For Debate",        
#         "##"                                = "##"
# #         "Business##" = "Business#Business Day#Dealbook",
# #         "Foreign#World#" = "Foreign##",
# #         "#Open#" = "Other",
# #         "#Opinion#The Public Editor" = "OpEd#Opinion#",
# #         "Styles#Health#" = "Styles##",
# #         "Styles#Style#Fashion & Style" = "Styles##",
# #         "#U.S.#" = "#U.S.#Education",
#     ))

# ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
#                           mycreate_sqlxtab_df(glb_allobs_df,
#     c("myCategory", "NewsDesk", "SectionName", "SubsectionName", glb_rsp_var)))
# myprint_df(ctgry_xtab_df)
# write.table(ctgry_xtab_df, paste0(glb_out_pfx, "ctgry_xtab.csv"), 
#             row.names=FALSE)

# ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
#                        myCategory + NewsDesk + SectionName + SubsectionName ~ 
#                            Popular.fctr, sum, value.var=".n"))
# myprint_df(ctgry_cast_df)
# write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_cast.csv"), 
#             row.names=FALSE)

# print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df[, glb_rsp_var], 
#                              useNA="ifany"))

dsp_chisq.test <- function(...) {
    sel_df <- glb_allobs_df[sel_obs(...) & 
                            !is.na(glb_allobs_df$Popular), ]
    sel_df$.marker <- 1
    ref_df <- glb_allobs_df[!is.na(glb_allobs_df$Popular), ]
    mrg_df <- merge(ref_df[, c(glb_id_var, "Popular")],
                    sel_df[, c(glb_id_var, ".marker")], all.x=TRUE)
    mrg_df[is.na(mrg_df)] <- 0
    print(mrg_tbl <- table(mrg_df$.marker, mrg_df$Popular))
    print("Rows:Selected; Cols:Popular")
    #print(mrg_tbl)
    print(chisq.test(mrg_tbl))
}
# dsp_chisq.test(Headline.contains="[Ee]bola")
# dsp_chisq.test(Snippet.contains="[Ee]bola")
# dsp_chisq.test(Abstract.contains="[Ee]bola")

# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola"), ], 
#                           c(glb_rsp_var, "NewsDesk", "SectionName", "SubsectionName")))

# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName))
# print(table(glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))
# print(table(glb_allobs_df$NewsDesk, glb_allobs_df$SectionName, glb_allobs_df$SubsectionName))

# glb_allobs_df$myCategory.fctr <- as.factor(glb_allobs_df$myCategory)
# glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
#                                       c("myCategory", "NewsDesk", "SectionName", "SubsectionName"))

# Copy Headline into Snipper & Abstract if they are empty
# print(glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, c("Headline", "Snippet")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Snippet, 
#                     c("UniqueID", "Headline", "Snippet")])
# glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Snippet"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Snippet"]) == 0, "Headline"]
# 
# print(glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, c("Headline", "Abstract")])
# print(glb_allobs_df[glb_allobs_df$Headline == glb_allobs_df$Abstract, 
#                     c("UniqueID", "Headline", "Abstract")])
# glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Abstract"] <- 
#     glb_allobs_df[nchar(glb_allobs_df[, "Abstract"]) == 0, "Headline"]

# WordCount_0_df <- subset(glb_allobs_df, WordCount == 0)
# table(WordCount_0_df$Popular, WordCount_0_df$WordCount, useNA="ifany")
# myprint_df(WordCount_0_df[, 
#                 c("UniqueID", "Popular", "WordCount", "Headline")])
```

### Step `2.1: scrub data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "transform.data", major.inc=FALSE)
```

```
##            label step_major step_minor    bgn    end elapsed
## 3     scrub.data          2          1 13.968 15.244   1.276
## 4 transform.data          2          2 15.244     NA      NA
```

```r
### Mapping dictionary
#sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_map_vars)) {
    for (feat in glb_map_vars) {
        map_df <- myimport_data(url=glb_map_urls[[feat]], 
                                            comment="map_df", 
                                           print_diagn=TRUE)
        glb_allobs_df <- mymap_codes(glb_allobs_df, feat, names(map_df)[2], 
                                     map_df, map_join_col_name=names(map_df)[1], 
                                     map_tgt_col_name=names(map_df)[2])
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_map_vars)
}

### Forced Assignments
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (feat in glb_assign_vars) {
    new_feat <- paste0(feat, ".my")
    print(sprintf("Forced Assignments for: %s -> %s...", feat, new_feat))
    glb_allobs_df[, new_feat] <- glb_allobs_df[, feat]
    
    pairs <- glb_assign_pairs_lst[[feat]]
    for (pair_ix in 1:length(pairs$from)) {
        if (is.na(pairs$from[pair_ix]))
            nobs <- nrow(filter(glb_allobs_df, 
                                is.na(eval(parse(text=feat),
                                            envir=glb_allobs_df)))) else
            nobs <- sum(glb_allobs_df[, feat] == pairs$from[pair_ix])
        #nobs <- nrow(filter(glb_allobs_df, is.na(Married.fctr)))    ; print(nobs)
        
        if ((is.na(pairs$from[pair_ix])) && (is.na(pairs$to[pair_ix])))
            stop("what are you trying to do ???")
        if (is.na(pairs$from[pair_ix]))
            glb_allobs_df[is.na(glb_allobs_df[, feat]), new_feat] <- 
                pairs$to[pair_ix] else
            glb_allobs_df[glb_allobs_df[, feat] == pairs$from[pair_ix], new_feat] <- 
                pairs$to[pair_ix]
                    
        print(sprintf("    %s -> %s for %s obs", 
                      pairs$from[pair_ix], pairs$to[pair_ix], format(nobs, big.mark=",")))
    }

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, glb_assign_vars)
}

### Derivations using mapping functions
#stop(here"); sav_allobs_df <- glb_allobs_df; glb_allobs_df <- sav_allobs_df
for (new_feat in glb_derive_vars) {
    print(sprintf("Creating new feature: %s...", new_feat))
    args_lst <- NULL 
    for (arg in glb_derive_lst[[new_feat]]$args) 
        args_lst[[arg]] <- glb_allobs_df[, arg]
    glb_allobs_df[, new_feat] <- do.call(glb_derive_lst[[new_feat]]$mapfn, args_lst)
}
```

## Step `2.2: transform data`

```r
#```{r extract_features, cache=FALSE, eval=!is.null(glb_txt_vars)}
glb_chunks_df <- myadd_chunk(glb_chunks_df, "extract.features", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn  end elapsed
## 4   transform.data          2          2 15.244 15.3   0.056
## 5 extract.features          3          0 15.300   NA      NA
```

```r
extract.features_chunk_df <- myadd_chunk(NULL, "extract.features_bgn")
```

```
##                  label step_major step_minor    bgn end elapsed
## 1 extract.features_bgn          1          0 15.305  NA      NA
```

```r
# Options:
#   Select Tf, log(1 + Tf), Tf-IDF or BM25Tf-IDf

# Create new features that help prediction
# <col_name>.lag.2 <- lag(zoo(glb_trnobs_df$<col_name>), -2, na.pad=TRUE)
# glb_trnobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# <col_name>.lag.2 <- lag(zoo(glb_newobs_df$<col_name>), -2, na.pad=TRUE)
# glb_newobs_df[, "<col_name>.lag.2"] <- coredata(<col_name>.lag.2)
# 
# glb_newobs_df[1, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df) - 1, 
#                                                    "<col_name>"]
# glb_newobs_df[2, "<col_name>.lag.2"] <- glb_trnobs_df[nrow(glb_trnobs_df), 
#                                                    "<col_name>"]
                                                   
# glb_allobs_df <- mutate(glb_allobs_df,
#     A.P.http=ifelse(grepl("http",Added,fixed=TRUE), 1, 0)
#                     )
# 
# glb_trnobs_df <- mutate(glb_trnobs_df,
#                     )
# 
# glb_newobs_df <- mutate(glb_newobs_df,
#                     )

#   Convert dates to numbers 
#       typically, dates come in as chars; 
#           so this must be done before converting chars to factors

#stop(here"); sav_allobs_df <- glb_allobs_df #; glb_allobs_df <- sav_allobs_df
if (!is.null(glb_date_vars)) {
    glb_allobs_df <- cbind(glb_allobs_df, 
        myextract_dates_df(df=glb_allobs_df, vars=glb_date_vars, 
                           id_vars=glb_id_var, rsp_var=glb_rsp_var))
    for (sfx in c("", ".POSIX"))
        glb_exclude_vars_as_features <- 
            union(glb_exclude_vars_as_features, 
                    paste(glb_date_vars, sfx, sep=""))

    for (feat in glb_date_vars) {
        glb_allobs_df <- orderBy(reformulate(paste0(feat, ".POSIX")), glb_allobs_df)
#         print(myplot_scatter(glb_allobs_df, xcol_name=paste0(feat, ".POSIX"),
#                              ycol_name=glb_rsp_var, colorcol_name=glb_rsp_var))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[, paste0(feat, ".POSIX")] >=
                                               strptime("2012-12-01", "%Y-%m-%d"), ], 
                             xcol_name=paste0(feat, ".POSIX"),
                             ycol_name=glb_rsp_var, colorcol_name=paste0(feat, ".wkend")))

        # Create features that measure the gap between previous timestamp in the data
        require(zoo)
        z <- zoo(as.numeric(as.POSIXlt(glb_allobs_df[, paste0(feat, ".POSIX")])))
        glb_allobs_df[, paste0(feat, ".zoo")] <- z
        print(head(glb_allobs_df[, c(glb_id_var, feat, paste0(feat, ".zoo"))]))
        print(myplot_scatter(glb_allobs_df[glb_allobs_df[,  paste0(feat, ".POSIX")] >
                                            strptime("2012-10-01", "%Y-%m-%d"), ], 
                            xcol_name=paste0(feat, ".zoo"), ycol_name=glb_rsp_var,
                            colorcol_name=glb_rsp_var))
        b <- zoo(, seq(nrow(glb_allobs_df)))
        
        last1 <- as.numeric(merge(z-lag(z, -1), b, all=TRUE)); last1[is.na(last1)] <- 0
        glb_allobs_df[, paste0(feat, ".last1.log")] <- log(1 + last1)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last1.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last1.log"), 
                               xcol_name=glb_rsp_var))
        
        last2 <- as.numeric(merge(z-lag(z, -2), b, all=TRUE)); last2[is.na(last2)] <- 0
        glb_allobs_df[, paste0(feat, ".last2.log")] <- log(1 + last2)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last2.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last2.log"), 
                               xcol_name=glb_rsp_var))
        
        last10 <- as.numeric(merge(z-lag(z, -10), b, all=TRUE)); last10[is.na(last10)] <- 0
        glb_allobs_df[, paste0(feat, ".last10.log")] <- log(1 + last10)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last10.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last10.log"), 
                               xcol_name=glb_rsp_var))
        
        last100 <- as.numeric(merge(z-lag(z, -100), b, all=TRUE)); last100[is.na(last100)] <- 0
        glb_allobs_df[, paste0(feat, ".last100.log")] <- log(1 + last100)
        print(gp <- myplot_box(df=glb_allobs_df[glb_allobs_df[, 
                                                    paste0(feat, ".last100.log")] > 0, ], 
                               ycol_names=paste0(feat, ".last100.log"), 
                               xcol_name=glb_rsp_var))
        
        glb_allobs_df <- orderBy(reformulate(glb_id_var), glb_allobs_df)
        glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                                c(paste0(feat, ".zoo")))
        # all2$last3 = as.numeric(merge(z-lag(z, -3), b, all = TRUE))
        # all2$last5 = as.numeric(merge(z-lag(z, -5), b, all = TRUE))
        # all2$last10 = as.numeric(merge(z-lag(z, -10), b, all = TRUE))
        # all2$last20 = as.numeric(merge(z-lag(z, -20), b, all = TRUE))
        # all2$last50 = as.numeric(merge(z-lag(z, -50), b, all = TRUE))
        # 
        # 
        # # order table
        # all2 = all2[order(all2$id),]
        # 
        # ## fill in NAs
        # # count averages
        # na.avg = all2 %>% group_by(weekend, hour) %>% dplyr::summarise(
        #     last1=mean(last1, na.rm=TRUE),
        #     last3=mean(last3, na.rm=TRUE),
        #     last5=mean(last5, na.rm=TRUE),
        #     last10=mean(last10, na.rm=TRUE),
        #     last20=mean(last20, na.rm=TRUE),
        #     last50=mean(last50, na.rm=TRUE)
        # )
        # 
        # # fill in averages
        # na.merge = merge(all2, na.avg, by=c("weekend","hour"))
        # na.merge = na.merge[order(na.merge$id),]
        # for(i in c("last1", "last3", "last5", "last10", "last20", "last50")) {
        #     y = paste0(i, ".y")
        #     idx = is.na(all2[[i]])
        #     all2[idx,][[i]] <- na.merge[idx,][[y]]
        # }
        # rm(na.avg, na.merge, b, i, idx, n, pd, sec, sh, y, z)
    }
}
rm(last1, last10, last100)
```

```
## Warning in rm(last1, last10, last100): object 'last1' not found
```

```
## Warning in rm(last1, last10, last100): object 'last10' not found
```

```
## Warning in rm(last1, last10, last100): object 'last100' not found
```

```r
#   Create factors of string variables
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "factorize.str.vars"), major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn    end
## 1                extract.features_bgn          1          0 15.305 15.317
## 2 extract.features_factorize.str.vars          2          0 15.318     NA
##   elapsed
## 1   0.012
## 2      NA
```

```r
#stop(here"); sav_allobs_df <- glb_allobs_df; #glb_allobs_df <- sav_allobs_df
print(str_vars <- myfind_chr_cols_df(glb_allobs_df))
```

```
##        state.abb   state.division       state.name     state.region 
##      "state.abb" "state.division"     "state.name"   "state.region" 
##             .src 
##           ".src"
```

```r
if (length(str_vars <- setdiff(str_vars, 
                               c(glb_exclude_vars_as_features, glb_txt_vars))) > 0) {
    for (var in str_vars) {
        warning("Creating factors of string variable: ", var, 
                ": # of unique values: ", length(unique(glb_allobs_df[, var])))
        glb_allobs_df[, paste0(var, ".fctr")] <- 
            relevel(factor(glb_allobs_df[, var]),
                    names(which.max(table(glb_allobs_df[, var], useNA = "ifany"))))
    }
    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, str_vars)
}
```

```
## Warning: Creating factors of string variable: state.division: # of unique
## values: 9
```

```
## Warning: Creating factors of string variable: state.region: # of unique
## values: 4
```

```r
if (!is.null(glb_txt_vars)) {
    require(foreach)
    require(gsubfn)
    require(stringr)
    require(tm)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text"), major.inc=TRUE)
    
    chk_pattern_freq <- function(rex_str, ignore.case=TRUE) {
        match_mtrx <- str_extract_all(txt_vctr, regex(rex_str, ignore_case=ignore.case), 
                                      simplify=TRUE)
        match_df <- as.data.frame(match_mtrx[match_mtrx != ""])
        names(match_df) <- "pattern"
        return(mycreate_sqlxtab_df(match_df, "pattern"))        
    }

#     match_lst <- gregexpr("\\bok(?!ay)", txt_vctr[746], ignore.case = FALSE, perl=TRUE); print(match_lst)
    dsp_pattern <- function(rex_str, ignore.case=TRUE, print.all=TRUE) {
        match_lst <- gregexpr(rex_str, txt_vctr, ignore.case = ignore.case, perl=TRUE)
        match_lst <- regmatches(txt_vctr, match_lst)
        match_df <- data.frame(matches=sapply(match_lst, 
                                              function (elems) paste(elems, collapse="#")))
        match_df <- subset(match_df, matches != "")
        if (print.all)
            print(match_df)
        return(match_df)
    }
    
    dsp_matches <- function(rex_str, ix) {
        print(match_pos <- gregexpr(rex_str, txt_vctr[ix], perl=TRUE))
        print(str_sub(txt_vctr[ix], (match_pos[[1]] / 100) *  99 +   0, 
                                    (match_pos[[1]] / 100) * 100 + 100))        
    }

    myapply_gsub <- function(...) {
        if ((length_lst <- length(names(gsub_map_lst))) == 0)
            return(txt_vctr)
        for (ptn_ix in 1:length_lst) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                length(names(gsub_map_lst)), names(gsub_map_lst)[ptn_ix]))
            txt_vctr <- gsub(names(gsub_map_lst)[ptn_ix], gsub_map_lst[[ptn_ix]], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    myapply_txtmap <- function(txt_vctr, ...) {
        nrows <- nrow(glb_txt_map_df)
        for (ptn_ix in 1:nrows) {
            if ((ptn_ix %% 10) == 0)
                print(sprintf("running gsub for %02d (of %02d): #%s#...", ptn_ix, 
                                nrows, glb_txt_map_df[ptn_ix, "rex_str"]))
            txt_vctr <- gsub(glb_txt_map_df[ptn_ix, "rex_str"], 
                             glb_txt_map_df[ptn_ix, "rpl_str"], 
                               txt_vctr, ...)
        }
        return(txt_vctr)
    }    

    chk.equal <- function(bgn, end) {
        print(all.equal(sav_txt_lst[["Headline"]][bgn:end], 
                        glb_txt_lst[["Headline"]][bgn:end]))
    }    
    dsp.equal <- function(bgn, end) {
        print(sav_txt_lst[["Headline"]][bgn:end])
        print(glb_txt_lst[["Headline"]][bgn:end])
    }    
#sav_txt_lst <- glb_txt_lst; all.equal(sav_txt_lst, glb_txt_lst)
#all.equal(sav_txt_lst[["Headline"]][1:4200], glb_txt_lst[["Headline"]][1:4200])
#chk.equal( 1, 100)
#dsp.equal(86, 90)
    
    glb_txt_map_df <- read.csv("mytxt_map.csv", comment.char="#", strip.white=TRUE)
    glb_txt_lst <- list(); 
    print(sprintf("Building glb_txt_lst..."))
    glb_txt_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_vctr <- glb_allobs_df[, txt_var]
        
        # myapply_txtmap shd be created as a tm_map::content_transformer ?
        #print(glb_txt_map_df)
        #txt_var=glb_txt_vars[3]; txt_vctr <- glb_txt_lst[[txt_var]]
        #print(rex_str <- glb_txt_map_df[163, "rex_str"])
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rex_str == "\\bWall St\\.", "rex_str"])
        #print(rex_str <- glb_txt_map_df[grepl("du Pont", glb_txt_map_df$rex_str), "rex_str"])        
        #print(rex_str <- glb_txt_map_df[glb_txt_map_df$rpl_str == "versus", "rex_str"])             
        #print(tmp_vctr <- grep(rex_str, txt_vctr, value=TRUE, ignore.case=FALSE))
        #ret_lst <- regexec(rex_str, txt_vctr, ignore.case=FALSE); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])
        #gsub(rex_str, glb_txt_map_df[glb_txt_map_df$rex_str == rex_str, "rpl_str"], tmp_vctr, ignore.case=FALSE)
        #grep("Hong Hong", txt_vctr, value=TRUE)
    
        txt_vctr <- myapply_txtmap(txt_vctr, ignore.case=FALSE)    
    }
    names(glb_txt_lst) <- glb_txt_vars

    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining OK in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "(?<!(BO|HO|LO))OK(?!(E\\!|ED|IE|IN|S ))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "Ok(?!(a\\.|ay|in|ra|um))", ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))

        print(chk_pattern_freq(rex_str <- "(?<!( b| B| c| C| g| G| j| M| p| P| w| W| r| Z|\\(b|ar|bo|Bo|co|Co|Ew|gk|go|ho|ig|jo|kb|ke|Ke|ki|lo|Lo|mo|mt|no|No|po|ra|ro|sm|Sm|Sp|to|To))ok(?!(ay|bo|e |e\\)|e,|e\\.|eb|ed|el|en|er|es|ey|i |ie|in|it|ka|ke|ki|ly|on|oy|ra|st|u |uc|uy|yl|yo))",
                               ignore.case=FALSE))
        match_df <- dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
        for (row in row.names(match_df))
            dsp_matches(rex_str, ix=as.numeric(row))
    }    
    # txt_vctr <- glb_txt_lst[[glb_txt_vars[1]]]
    # print(chk_pattern_freq(rex_str <- "(?<!( b| c| C| p|\\(b|bo|co|lo|Lo|Sp|to|To))ok(?!(ay|e |e\\)|e,|e\\.|ed|el|en|es|ey|ie|in|on|ra))", ignore.case=FALSE))
    # print(chk_pattern_freq(rex_str <- "ok(?!(ay|el|on|ra))", ignore.case=FALSE))
    # dsp_pattern(rex_str, ignore.case=FALSE, print.all=FALSE)
    # dsp_matches(rex_str, ix=8)
    # substr(txt_vctr[86], 5613, 5620)
    # substr(glb_allobs_df[301, "review"], 550, 650)

#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining Acronyms in %s:", txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]
        
        print(chk_pattern_freq(rex_str <- "([[:upper:]]\\.( *)){2,}", ignore.case=FALSE))
        
        # Check for names
        print(subset(chk_pattern_freq(rex_str <- "(([[:upper:]]+)\\.( *)){1}",
                                      ignore.case=FALSE),
                     .n > 1))
        # dsp_pattern(rex_str="(OK\\.( *)){1}", ignore.case=FALSE)
        # dsp_matches(rex_str="(OK\\.( *)){1}", ix=557)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)(\\B)", ix=461)
        #dsp_matches(rex_str="\\bR\\.I\\.P(\\.*)", ix=461)        
        #print(str_sub(txt_vctr[676], 10100, 10200))
        #print(str_sub(txt_vctr[74], 1, -1))        
    }

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(Fort|Ft\\.|Hong|Las|Los|New|Puerto|Saint|San|St\\.)( |-)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))
        txt_vctr <- glb_txt_lst[[txt_var]]        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl("( |-)[[:upper:]]", pattern))))
        print("    consider cleaning if relevant to problem domain; geography name; .n > 1")
        #grep("New G", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Wins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }        
        
#stop(here"); sav_txt_lst <- glb_txt_lst    
    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(N|S|E|W|C)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("N Weaver", txt_vctr, value=TRUE, ignore.case=FALSE)        
    }    

    for (txt_var in glb_txt_vars) {
        re_str <- "\\b(North|South|East|West|Central)( |\\.)(\\w)+"
        print(sprintf("Remaining #%s# terms in %s: ", re_str, txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
        print(orderBy(~ -.n +pattern, subset(chk_pattern_freq(re_str, ignore.case=FALSE), 
                                             grepl(".", pattern))))
        #grep("Central (African|Bankers|Cast|Italy|Role|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("East (Africa|Berlin|London|Poland|Rivals|Spring)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("North (American|Korean|West)", txt_vctr, value=TRUE, ignore.case=FALSE)        
        #grep("South (Pacific|Street)", txt_vctr, value=TRUE, ignore.case=FALSE)
        #grep("St\\. Martins", txt_vctr, value=TRUE, ignore.case=FALSE)
    }    

    find_cmpnd_wrds <- function(txt_vctr) {
        txt_corpus <- Corpus(VectorSource(txt_vctr))
        txt_corpus <- tm_map(txt_corpus, tolower)
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation, 
                             preserve_intra_word_dashes=TRUE)
        full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTf))
        print("   Full TermMatrix:"); print(full_Tf_DTM)
        full_Tf_mtrx <- as.matrix(full_Tf_DTM)
        rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_Tf_vctr <- colSums(full_Tf_mtrx)
        names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
        #grep("year", names(full_Tf_vctr), value=TRUE)
        #which.max(full_Tf_mtrx[, "yearlong"])
        full_Tf_df <- as.data.frame(full_Tf_vctr)
        names(full_Tf_df) <- "Tf.full"
        full_Tf_df$term <- rownames(full_Tf_df)
        #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
        full_Tf_df <- orderBy(~ -Tf.full, full_Tf_df)
        cmpnd_Tf_df <- full_Tf_df[grep("-", full_Tf_df$term, value=TRUE) ,]
        
        filter_df <- read.csv("mytxt_compound.csv", comment.char="#", strip.white=TRUE)
        cmpnd_Tf_df$filter <- FALSE
        for (row_ix in 1:nrow(filter_df))
            cmpnd_Tf_df[!cmpnd_Tf_df$filter, "filter"] <- 
            grepl(filter_df[row_ix, "rex_str"], 
                  cmpnd_Tf_df[!cmpnd_Tf_df$filter, "term"], ignore.case=TRUE)
        cmpnd_Tf_df <- subset(cmpnd_Tf_df, !filter)
        # Bug in tm_map(txt_corpus, removePunctuation, preserve_intra_word_dashes=TRUE) ???
        #   "net-a-porter" gets converted to "net-aporter"
        #grep("net-a-porter", txt_vctr, ignore.case=TRUE, value=TRUE)
        #grep("maser-laser", txt_vctr, ignore.case=TRUE, value=TRUE)
        #txt_corpus[[which(grepl("net-a-porter", txt_vctr, ignore.case=TRUE))]]
        #grep("\\b(across|longer)-(\\w)", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        #grep("(\\w)-(affected|term)\\b", cmpnd_Tf_df$term, ignore.case=TRUE, value=TRUE)
        
        print(sprintf("nrow(cmpnd_Tf_df): %d", nrow(cmpnd_Tf_df)))
        myprint_df(cmpnd_Tf_df)
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "process.text_reporting_compound_terms"), major.inc=FALSE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Remaining compound terms in %s: ", txt_var))        
        txt_vctr <- glb_txt_lst[[txt_var]]                        
#         find_cmpnd_wrds(txt_vctr)
        #grep("thirty-five", txt_vctr, ignore.case=TRUE, value=TRUE)
        #rex_str <- glb_txt_map_df[grepl("hirty", glb_txt_map_df$rex_str), "rex_str"]
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "build.corpus"), major.inc=TRUE)
    
    glb_corpus_lst <- list()
    print(sprintf("Building glb_corpus_lst..."))
    glb_corpus_lst <- foreach(txt_var=glb_txt_vars) %dopar% {   
#     for (txt_var in glb_txt_vars) {
        txt_corpus <- Corpus(VectorSource(glb_txt_lst[[txt_var]]))
        txt_corpus <- tm_map(txt_corpus, tolower) #nuppr
        txt_corpus <- tm_map(txt_corpus, PlainTextDocument)
        txt_corpus <- tm_map(txt_corpus, removePunctuation) #npnct<chr_ix>
#         txt-corpus <- tm_map(txt_corpus, content_transformer(function(x, pattern) gsub(pattern, "", x))   

        # Not to be run in production
        inspect_terms <- function() {
            full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
                                              control=list(weighting=weightTf))
            print("   Full TermMatrix:"); print(full_Tf_DTM)
            full_Tf_mtrx <- as.matrix(full_Tf_DTM)
            rownames(full_Tf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
            full_Tf_vctr <- colSums(full_Tf_mtrx)
            names(full_Tf_vctr) <- dimnames(full_Tf_DTM)[[2]]
            #grep("year", names(full_Tf_vctr), value=TRUE)
            #which.max(full_Tf_mtrx[, "yearlong"])
            full_Tf_df <- as.data.frame(full_Tf_vctr)
            names(full_Tf_df) <- "Tf.full"
            full_Tf_df$term <- rownames(full_Tf_df)
            #full_Tf_df$freq.full <- colSums(full_Tf_mtrx != 0)
            full_Tf_df <- orderBy(~ -Tf.full +term, full_Tf_df)
            print(myplot_histogram(full_Tf_df, "Tf.full"))
            myprint_df(full_Tf_df)
            #txt_corpus[[which(grepl("zun", txt_vctr, ignore.case=TRUE))]]
            digit_terms_df <- subset(full_Tf_df, grepl("[[:digit:]]", term))
            myprint_df(digit_terms_df)
            return(full_Tf_df)
        }    
        #print("RemovePunct:"); remove_punct_Tf_df <- inspect_terms()

        txt_corpus <- tm_map(txt_corpus, removeWords, 
                             c(glb_append_stop_words[[txt_var]], 
                               stopwords("english"))) #nstopwrds
        #print("StoppedWords:"); stopped_words_Tf_df <- inspect_terms()
        txt_corpus <- tm_map(txt_corpus, stemDocument) #Features for lost information: Difference/ratio in density of full_TfIdf_DTM ???
        #txt_corpus <- tm_map(txt_corpus, content_transformer(stemDocument))        
        #print("StemmedWords:"); stemmed_words_Tf_df <- inspect_terms()
        #stemmed_stopped_Tf_df <- merge(stemmed_words_Tf_df, stopped_words_Tf_df, by="term", all=TRUE, suffixes=c(".stem", ".stop"))
        #myprint_df(stemmed_stopped_Tf_df)
        #print(subset(stemmed_stopped_Tf_df, grepl("compan", term)))
        #glb_corpus_lst[[txt_var]] <- txt_corpus
    }
    names(glb_corpus_lst) <- glb_txt_vars
        
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "extract.DTM"), major.inc=TRUE)

    glb_full_DTM_lst <- list(); glb_sprs_DTM_lst <- list();
    for (txt_var in glb_txt_vars) {
        print(sprintf("Extracting TfIDf terms for %s...", txt_var))        
        txt_corpus <- glb_corpus_lst[[txt_var]]
        
#         full_Tf_DTM <- DocumentTermMatrix(txt_corpus, 
#                                           control=list(weighting=weightTf))
        full_TfIdf_DTM <- DocumentTermMatrix(txt_corpus, 
                                          control=list(weighting=weightTfIdf))
        sprs_TfIdf_DTM <- removeSparseTerms(full_TfIdf_DTM, 
                                            glb_sprs_thresholds[txt_var])
        
#         glb_full_DTM_lst[[txt_var]] <- full_Tf_DTM
#         glb_sprs_DTM_lst[[txt_var]] <- sprs_Tf_DTM
        glb_full_DTM_lst[[txt_var]] <- full_TfIdf_DTM
        glb_sprs_DTM_lst[[txt_var]] <- sprs_TfIdf_DTM
    }

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
            paste0("extract.features_", "report.DTM"), major.inc=TRUE)
    
    for (txt_var in glb_txt_vars) {
        print(sprintf("Reporting TfIDf terms for %s...", txt_var))        
        full_TfIdf_DTM <- glb_full_DTM_lst[[txt_var]]
        sprs_TfIdf_DTM <- glb_sprs_DTM_lst[[txt_var]]        

        print("   Full TermMatrix:"); print(full_TfIdf_DTM)
        full_TfIdf_mtrx <- as.matrix(full_TfIdf_DTM)
        rownames(full_TfIdf_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        full_TfIdf_vctr <- colSums(full_TfIdf_mtrx)
        names(full_TfIdf_vctr) <- dimnames(full_TfIdf_DTM)[[2]]
        #grep("scene", names(full_TfIdf_vctr), value=TRUE)
        #which.max(full_TfIdf_mtrx[, "yearlong"])
        full_TfIdf_df <- as.data.frame(full_TfIdf_vctr)
        names(full_TfIdf_df) <- "TfIdf.full"
        full_TfIdf_df$term <- rownames(full_TfIdf_df)
        full_TfIdf_df$freq.full <- colSums(full_TfIdf_mtrx != 0)
        full_TfIdf_df <- orderBy(~ -TfIdf.full, full_TfIdf_df)

        print("   Sparse TermMatrix:"); print(sprs_TfIdf_DTM)
        sprs_TfIdf_vctr <- colSums(as.matrix(sprs_TfIdf_DTM))
        names(sprs_TfIdf_vctr) <- dimnames(sprs_TfIdf_DTM)[[2]]
        sprs_TfIdf_df <- as.data.frame(sprs_TfIdf_vctr)
        names(sprs_TfIdf_df) <- "TfIdf.sprs"
        sprs_TfIdf_df$term <- rownames(sprs_TfIdf_df)
        sprs_TfIdf_df$freq.sprs <- colSums(as.matrix(sprs_TfIdf_DTM) != 0)        
        sprs_TfIdf_df <- orderBy(~ -TfIdf.sprs, sprs_TfIdf_df)
        
        terms_TfIdf_df <- merge(full_TfIdf_df, sprs_TfIdf_df, all.x=TRUE)
        terms_TfIdf_df$in.sprs <- !is.na(terms_TfIdf_df$freq.sprs)
        plt_TfIdf_df <- subset(terms_TfIdf_df, 
                               TfIdf.full >= min(terms_TfIdf_df$TfIdf.sprs, na.rm=TRUE))
        plt_TfIdf_df$label <- ""
        plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "label"] <- 
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"]
        glb_important_terms[[txt_var]] <- union(glb_important_terms[[txt_var]],
            plt_TfIdf_df[is.na(plt_TfIdf_df$TfIdf.sprs), "term"])
        print(myplot_scatter(plt_TfIdf_df, "freq.full", "TfIdf.full", 
                             colorcol_name="in.sprs") + 
                  geom_text(aes(label=label), color="Black", size=3.5))
        
        melt_TfIdf_df <- orderBy(~ -value, melt(terms_TfIdf_df, id.var="term"))
        print(ggplot(melt_TfIdf_df, aes(value, color=variable)) + stat_ecdf() + 
                  geom_hline(yintercept=glb_sprs_thresholds[txt_var], 
                             linetype = "dotted"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, !is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(melt_TfIdf_df, "term", "value", 
                          colorcol_name="variable"))
        
        melt_TfIdf_df <- orderBy(~ -value, 
                        melt(subset(terms_TfIdf_df, is.na(TfIdf.sprs)), id.var="term"))
        print(myplot_hbar(head(melt_TfIdf_df, 10), "term", "value", 
                          colorcol_name="variable"))
    }

#     sav_full_DTM_lst <- glb_full_DTM_lst
#     sav_sprs_DTM_lst <- glb_sprs_DTM_lst
#     print(identical(sav_glb_corpus_lst, glb_corpus_lst))
#     print(all.equal(length(sav_glb_corpus_lst), length(glb_corpus_lst)))
#     print(all.equal(names(sav_glb_corpus_lst), names(glb_corpus_lst)))
#     print(all.equal(sav_glb_corpus_lst[["Headline"]], glb_corpus_lst[["Headline"]]))

#     print(identical(sav_full_DTM_lst, glb_full_DTM_lst))
#     print(identical(sav_sprs_DTM_lst, glb_sprs_DTM_lst))
        
    rm(full_TfIdf_mtrx, full_TfIdf_df, melt_TfIdf_df, terms_TfIdf_df)

    # Create txt features
    if ((length(glb_txt_vars) > 1) &&
        (length(unique(pfxs <- sapply(glb_txt_vars, 
                    function(txt) toupper(substr(txt, 1, 1))))) < length(glb_txt_vars)))
            stop("Prefixes for corpus freq terms not unique: ", pfxs)
    
    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DTM"), 
                                         major.inc=TRUE)
    for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DTM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))
        txt_X_df <- as.data.frame(as.matrix(glb_sprs_DTM_lst[[txt_var]]))
        colnames(txt_X_df) <- paste(txt_var_pfx, ".T.",
                                    make.names(colnames(txt_X_df)), sep="")
        rownames(txt_X_df) <- rownames(glb_allobs_df) # warning otherwise
#         plt_X_df <- cbind(txt_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today", xcol_name=glb_rsp_var))

#         log_X_df <- log(1 + txt_X_df)
#         colnames(log_X_df) <- paste(colnames(txt_X_df), ".log", sep="")
#         plt_X_df <- cbind(log_X_df, glb_allobs_df[, c(glb_id_var, glb_rsp_var)])
#         print(myplot_box(df=plt_X_df, ycol_names="H.T.today.log", xcol_name=glb_rsp_var))
        glb_allobs_df <- cbind(glb_allobs_df, txt_X_df) # TfIdf is normalized
        #glb_allobs_df <- cbind(glb_allobs_df, log_X_df) # if using non-normalized metrics 
    }
    #identical(chk_entity_df, glb_allobs_df)
    #chk_entity_df <- glb_allobs_df

    extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, 
                            paste0("extract.features_", "bind.DXM"), 
                                         major.inc=TRUE)

#sav_allobs_df <- glb_allobs_df
    glb_punct_vctr <- c("!", "\"", "#", "\\$", "%", "&", "'", 
                        "\\(|\\)",# "\\(", "\\)", 
                        "\\*", "\\+", ",", "-", "\\.", "/", ":", ";", 
                        "<|>", # "<", 
                        "=", 
                        # ">", 
                        "\\?", "@", "\\[", "\\\\", "\\]", "^", "_", "`", 
                        "\\{", "\\|", "\\}", "~")
    txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
    txt_X_df <- foreach(txt_var=glb_txt_vars, .combine=cbind) %dopar% {   
    #for (txt_var in glb_txt_vars) {
        print(sprintf("Binding DXM for %s...", txt_var))
        txt_var_pfx <- toupper(substr(txt_var, 1, 1))        
        #txt_X_df <- glb_allobs_df[, c(glb_id_var, ".rnorm"), FALSE]
        
        txt_full_DTM_mtrx <- as.matrix(glb_full_DTM_lst[[txt_var]])
        rownames(txt_full_DTM_mtrx) <- rownames(glb_allobs_df) # print undreadable otherwise
        #print(txt_full_DTM_mtrx[txt_full_DTM_mtrx[, "ebola"] != 0, "ebola"])
        
        # Create <txt_var>.T.<term> for glb_important_terms
        for (term in glb_important_terms[[txt_var]])
            txt_X_df[, paste0(txt_var_pfx, ".T.", make.names(term))] <- 
                txt_full_DTM_mtrx[, term]
                
        # Create <txt_var>.nwrds.log & .nwrds.unq.log
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")] <- 
            log(1 + mycount_pattern_occ("\\w+", glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".nwrds.unq.log")] <- 
            log(1 + rowSums(txt_full_DTM_mtrx != 0))
        txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] <- 
            rowSums(txt_full_DTM_mtrx) 
        txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 
            txt_X_df[, paste0(txt_var_pfx, ".sum.TfIdf")] / 
            (exp(txt_X_df[, paste0(txt_var_pfx, ".nwrds.log")]) - 1)
        txt_X_df[is.nan(txt_X_df[, paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")]),
                 paste0(txt_var_pfx, ".ratio.sum.TfIdf.nwrds")] <- 0

        # Create <txt_var>.nchrs.log
        txt_X_df[, paste0(txt_var_pfx, ".nchrs.log")] <- 
            log(1 + mycount_pattern_occ(".", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".nuppr.log")] <- 
            log(1 + mycount_pattern_occ("[[:upper:]]", glb_allobs_df[, txt_var]))
        txt_X_df[, paste0(txt_var_pfx, ".ndgts.log")] <- 
            log(1 + mycount_pattern_occ("[[:digit:]]", glb_allobs_df[, txt_var]))

        # Create <txt_var>.npnct?.log
        # would this be faster if it's iterated over each row instead of 
        #   each created column ???
        for (punct_ix in 1:length(glb_punct_vctr)) { 
#             smp0 <- " "
#             smp1 <- "! \" # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~"
#             smp2 <- paste(smp1, smp1, sep=" ")
#             print(sprintf("Testing %s pattern:", glb_punct_vctr[punct_ix])) 
#             results <- mycount_pattern_occ(glb_punct_vctr[punct_ix], c(smp0, smp1, smp2))
#             names(results) <- NULL; print(results)
            txt_X_df[, 
                paste0(txt_var_pfx, ".npnct", sprintf("%02d", punct_ix), ".log")] <-
                log(1 + mycount_pattern_occ(glb_punct_vctr[punct_ix], 
                                            glb_allobs_df[, txt_var]))
        }
#         print(head(glb_allobs_df[glb_allobs_df[, "A.npnct23.log"] > 0, 
#                                     c("UniqueID", "Popular", "Abstract", "A.npnct23.log")]))    
        
        # Create <txt_var>.nstopwrds.log & <txt_var>ratio.nstopwrds.nwrds
        stop_words_rex_str <- paste0("\\b(", paste0(c(glb_append_stop_words[[txt_var]], 
                                       stopwords("english")), collapse="|"),
                                     ")\\b")
        txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] <-
            log(1 + mycount_pattern_occ(stop_words_rex_str, glb_txt_lst[[txt_var]]))
        txt_X_df[, paste0(txt_var_pfx, ".ratio.nstopwrds.nwrds")] <-
            exp(txt_X_df[, paste0(txt_var_pfx, ".nstopwrds", ".log")] - 
                txt_X_df[, paste0(txt_var_pfx, ".nwrds", ".log")])

        # Create <txt_var>.P.http
        txt_X_df[, paste(txt_var_pfx, ".P.http", sep="")] <- 
            as.integer(0 + mycount_pattern_occ("http", glb_allobs_df[, txt_var]))    
    
        txt_X_df <- subset(txt_X_df, select=-.rnorm)
        txt_X_df <- txt_X_df[, -grep(glb_id_var, names(txt_X_df), fixed=TRUE), FALSE]
        #glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    }
    glb_allobs_df <- cbind(glb_allobs_df, txt_X_df)
    #myplot_box(glb_allobs_df, "A.sum.TfIdf", glb_rsp_var)

    # Generate summaries
#     print(summary(glb_allobs_df))
#     print(sapply(names(glb_allobs_df), function(col) sum(is.na(glb_allobs_df[, col]))))
#     print(summary(glb_trnobs_df))
#     print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
#     print(summary(glb_newobs_df))
#     print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

    glb_exclude_vars_as_features <- union(glb_exclude_vars_as_features, 
                                          glb_txt_vars)
    rm(log_X_df, txt_X_df)
}

# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))

# print(myplot_scatter(glb_trnobs_df, "<col1_name>", "<col2_name>", smooth=TRUE))

rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr, 
   glb_full_DTM_lst, glb_sprs_DTM_lst, txt_corpus, txt_vctr)
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'corpus_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_DTM' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'full_TfIdf_vctr' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_full_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'glb_sprs_DTM_lst' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_corpus' not found
```

```
## Warning in rm(corpus_lst, full_TfIdf_DTM, full_TfIdf_vctr,
## glb_full_DTM_lst, : object 'txt_vctr' not found
```

```r
extract.features_chunk_df <- myadd_chunk(extract.features_chunk_df, "extract.features_end", 
                                     major.inc=TRUE)
```

```
##                                 label step_major step_minor    bgn   end
## 2 extract.features_factorize.str.vars          2          0 15.318 15.34
## 3                extract.features_end          3          0 15.340    NA
##   elapsed
## 2   0.022
## 3      NA
```

```r
myplt_chunk(extract.features_chunk_df)
```

```
##                                 label step_major step_minor    bgn    end
## 2 extract.features_factorize.str.vars          2          0 15.318 15.340
## 1                extract.features_bgn          1          0 15.305 15.317
##   elapsed duration
## 2   0.022    0.022
## 1   0.012    0.012
## [1] "Total Elapsed Time: 15.34 secs"
```

![](USCensus1977_State_HW2_v2_files/figure-html/extract.features-1.png) 

```r
# if (glb_save_envir)
#     save(glb_feats_df, 
#          glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
#          file=paste0(glb_out_pfx, "extract_features_dsk.RData"))
# load(paste0(glb_out_pfx, "extract_features_dsk.RData"))

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

![](USCensus1977_State_HW2_v2_files/figure-html/extract.features-2.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "cluster.data", major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 5 extract.features          3          0 15.300 16.438   1.139
## 6     cluster.data          4          0 16.439     NA      NA
```

### Step `4.0: cluster data`

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "manage.missing.data", major.inc=FALSE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 6        cluster.data          4          0 16.439 16.707   0.268
## 7 manage.missing.data          4          1 16.707     NA      NA
```

```r
# print(sapply(names(glb_trnobs_df), function(col) sum(is.na(glb_trnobs_df[, col]))))
# print(sapply(names(glb_newobs_df), function(col) sum(is.na(glb_newobs_df[, col]))))
# glb_trnobs_df <- na.omit(glb_trnobs_df)
# glb_newobs_df <- na.omit(glb_newobs_df)
# df[is.na(df)] <- 0

mycheck_problem_data(glb_allobs_df)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Frost 
##     1 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      state.abb state.division     state.name   state.region 
##              0              0              0              0
```

```r
# glb_allobs_df <- na.omit(glb_allobs_df)

# Not refactored into mydsutils.R since glb_*_df might be reassigned
glb_impute_missing_data <- function() {
    
    require(mice)
    set.seed(glb_mice_complete.seed)
    inp_impent_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                union(glb_exclude_vars_as_features, glb_rsp_var))]
    print("Summary before imputation: ")
    print(summary(inp_impent_df))
    out_impent_df <- complete(mice(inp_impent_df))
    print(summary(out_impent_df))
    
    # complete(mice()) changes attributes of factors even though values don't change
    ret_vars <- sapply(names(out_impent_df), 
                       function(col) ifelse(!identical(out_impent_df[, col], inp_impent_df[, col]), 
                                            col, ""))
    ret_vars <- ret_vars[ret_vars != ""]
    return(out_impent_df[, ret_vars])
}

if (glb_impute_na_data && 
    (length(myfind_numerics_missing(glb_allobs_df)) > 0) &&
    (ncol(nonna_df <- glb_impute_missing_data()) > 0)) {
    for (col in names(nonna_df)) {
        glb_allobs_df[, paste0(col, ".nonNA")] <- nonna_df[, col]
        glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, col)        
    }
}    
    
mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ 0s in glb_allobs_df: "
## Frost 
##     1 
## [1] "numeric data w/ Infs in glb_allobs_df: "
## named integer(0)
## [1] "numeric data w/ NaNs in glb_allobs_df: "
## named integer(0)
## [1] "string data missing in glb_allobs_df: "
##      state.abb state.division     state.name   state.region 
##              0              0              0              0
```

## Step `4.1: manage missing data`

```r
if (glb_cluster) {
    require(proxy)
    #require(hash)
    require(dynamicTreeCut)

#     glb_hash <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#     glb_hash_lst <- hash(key=unique(glb_allobs_df$myCategory), 
#                      values=1:length(unique(glb_allobs_df$myCategory)))
#stophere; sav_allobs_df <- glb_allobs_df; 
    print("Clustering features: ")
    print(cluster_vars <- grep("[HSA]\\.[PT]\\.", names(glb_allobs_df), value=TRUE))
    #print(cluster_vars <- grep("[HSA]\\.", names(glb_allobs_df), value=TRUE))
    glb_allobs_df$.clusterid <- 1    
    #print(max(table(glb_allobs_df$myCategory.fctr) / 20))
    for (myCategory in c("##", "Business#Business Day#Dealbook", "OpEd#Opinion#", 
                         "Styles#U.S.#", "Business#Technology#", "Science#Health#",
                         "Culture#Arts#")) {
        ctgry_allobs_df <- glb_allobs_df[glb_allobs_df$myCategory == myCategory, ]
        
        dstns_dist <- dist(ctgry_allobs_df[, cluster_vars], method = "cosine")
        dstns_mtrx <- as.matrix(dstns_dist)
        print(sprintf("max distance(%0.4f) pair:", max(dstns_mtrx)))
        row_ix <- ceiling(which.max(dstns_mtrx) / ncol(dstns_mtrx))
        col_ix <- which.max(dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])
    
        min_dstns_mtrx <- dstns_mtrx
        diag(min_dstns_mtrx) <- 1
        print(sprintf("min distance(%0.4f) pair:", min(min_dstns_mtrx)))
        row_ix <- ceiling(which.min(min_dstns_mtrx) / ncol(min_dstns_mtrx))
        col_ix <- which.min(min_dstns_mtrx[row_ix, ])
        print(ctgry_allobs_df[c(row_ix, col_ix), 
            c("UniqueID", "Popular", "myCategory", "Headline", cluster_vars)])                          
    
        clusters <- hclust(dstns_dist, method = "ward.D2")
        #plot(clusters, labels=NULL, hang=-1)
        myplclust(clusters, lab.col=unclass(ctgry_allobs_df[, glb_rsp_var]))
        
        #clusterGroups = cutree(clusters, k=7)
        clusterGroups <- cutreeDynamic(clusters, minClusterSize=20, method="tree", deepSplit=0)
        # Unassigned groups are labeled 0; the largest group has label 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")   
        #print(ctgry_allobs_df[which(clusterGroups == 1), c("UniqueID", "Popular", "Headline")])
        #print(ctgry_allobs_df[(clusterGroups == 1) & !is.na(ctgry_allobs_df$Popular) & (ctgry_allobs_df$Popular==1), c("UniqueID", "Popular", "Headline")])
        clusterGroups[clusterGroups == 0] <- 1
        table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
        #summary(factor(clusterGroups))
#         clusterGroups <- clusterGroups + 
#                 100 * # has to be > max(table(glb_allobs_df$myCategory.fctr) / minClusterSize=20)
#                             which(levels(glb_allobs_df$myCategory.fctr) == myCategory)
#         table(clusterGroups, ctgry_allobs_df[, glb_rsp_var], useNA="ifany")        
    
        # add to glb_allobs_df - then split the data again
        glb_allobs_df[glb_allobs_df$myCategory==myCategory,]$.clusterid <- clusterGroups
        #print(unique(glb_allobs_df$.clusterid))
        #print(glb_feats_df[glb_feats_df$id == ".clusterid.fctr", ])
    }
    
    ctgry_xtab_df <- orderBy(reformulate(c("-", ".n")),
                              mycreate_sqlxtab_df(glb_allobs_df,
        c("myCategory", ".clusterid", glb_rsp_var)))
    ctgry_cast_df <- orderBy(~ -Y -NA, dcast(ctgry_xtab_df, 
                           myCategory + .clusterid ~ 
                               Popular.fctr, sum, value.var=".n"))
    print(ctgry_cast_df)
    #print(orderBy(~ myCategory -Y -NA, ctgry_cast_df))
    # write.table(ctgry_cast_df, paste0(glb_out_pfx, "ctgry_clst.csv"), 
    #             row.names=FALSE)
    
    print(ctgry_sum_tbl <- table(glb_allobs_df$myCategory, glb_allobs_df$.clusterid, 
                                 glb_allobs_df[, glb_rsp_var], 
                                 useNA="ifany"))
#     dsp_obs(.clusterid=1, myCategory="OpEd#Opinion#", 
#             cols=c("UniqueID", "Popular", "myCategory", ".clusterid", "Headline"),
#             all=TRUE)
    
    glb_allobs_df$.clusterid.fctr <- as.factor(glb_allobs_df$.clusterid)
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      ".clusterid")
    glb_interaction_only_features["myCategory.fctr"] <- c(".clusterid.fctr")
    glb_exclude_vars_as_features <- c(glb_exclude_vars_as_features, 
                                      cluster_vars)
}

# Re-partition
glb_trnobs_df <- subset(glb_allobs_df, .src == "Train")
glb_newobs_df <- subset(glb_allobs_df, .src == "Test")

glb_chunks_df <- myadd_chunk(glb_chunks_df, "select.features", major.inc=TRUE)
```

```
##                 label step_major step_minor    bgn    end elapsed
## 7 manage.missing.data          4          1 16.707 16.762   0.055
## 8     select.features          5          0 16.763     NA      NA
```

## Step `5.0: select features`

```r
print(glb_feats_df <- myselect_features(entity_df=glb_trnobs_df, 
                       exclude_vars_as_features=glb_exclude_vars_as_features, 
                       rsp_var=glb_rsp_var))
```

```
##                                      id       cor.y exclude.as.feat
## Murder                           Murder -0.82083018               0
## Illiteracy                   Illiteracy -0.70214027               0
## HS.Grad                         HS.Grad  0.69343552               0
## y                                     y  0.61621841               0
## Frost                             Frost  0.49341775               0
## state.region.fctr     state.region.fctr  0.46752060               0
## Income                           Income  0.39971784               0
## x                                     x -0.23179465               0
## Population                   Population -0.19279277               0
## .rnorm                           .rnorm -0.18778648               0
## Area                               Area  0.08153632               0
## state.area                   state.area  0.08080809               0
## state.division.fctr state.division.fctr -0.05931248               0
##                      cor.y.abs
## Murder              0.82083018
## Illiteracy          0.70214027
## HS.Grad             0.69343552
## y                   0.61621841
## Frost               0.49341775
## state.region.fctr   0.46752060
## Income              0.39971784
## x                   0.23179465
## Population          0.19279277
## .rnorm              0.18778648
## Area                0.08153632
## state.area          0.08080809
## state.division.fctr 0.05931248
```

```r
# sav_feats_df <- glb_feats_df; glb_feats_df <- sav_feats_df
print(glb_feats_df <- orderBy(~-cor.y, 
          myfind_cor_features(feats_df=glb_feats_df, obs_df=glb_trnobs_df, 
                              rsp_var=glb_rsp_var)))
```

```
## Loading required package: reshape2
```

```
## [1] "cor(HS.Grad, state.region.fctr)=0.8066"
## [1] "cor(Life.Exp, HS.Grad)=0.6934"
## [1] "cor(Life.Exp, state.region.fctr)=0.4675"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified state.region.fctr as highly correlated with
## HS.Grad
```

```
## [1] "cor(Illiteracy, y)=-0.8007"
## [1] "cor(Life.Exp, Illiteracy)=-0.7021"
## [1] "cor(Life.Exp, y)=0.6162"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified y as highly correlated with Illiteracy
```

```
## [1] "cor(Frost, Illiteracy)=-0.7817"
## [1] "cor(Life.Exp, Frost)=0.4934"
## [1] "cor(Life.Exp, Illiteracy)=-0.7021"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified Frost as highly correlated with Illiteracy
```

```
## [1] "cor(HS.Grad, Illiteracy)=-0.7806"
## [1] "cor(Life.Exp, HS.Grad)=0.6934"
## [1] "cor(Life.Exp, Illiteracy)=-0.7021"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified HS.Grad as highly correlated with Illiteracy
```

```
## [1] "cor(Illiteracy, Murder)=0.7194"
## [1] "cor(Life.Exp, Illiteracy)=-0.7021"
## [1] "cor(Life.Exp, Murder)=-0.8208"
```

```
## Warning in myfind_cor_features(feats_df = glb_feats_df, obs_df =
## glb_trnobs_df, : Identified Illiteracy as highly correlated with Murder
```

```
##                     id       cor.y exclude.as.feat  cor.y.abs cor.high.X
## 4              HS.Grad  0.69343552               0 0.69343552 Illiteracy
## 13                   y  0.61621841               0 0.61621841 Illiteracy
## 3                Frost  0.49341775               0 0.49341775 Illiteracy
## 11   state.region.fctr  0.46752060               0 0.46752060    HS.Grad
## 6               Income  0.39971784               0 0.39971784       <NA>
## 2                 Area  0.08153632               0 0.08153632       <NA>
## 9           state.area  0.08080809               0 0.08080809       <NA>
## 10 state.division.fctr -0.05931248               0 0.05931248       <NA>
## 1               .rnorm -0.18778648               0 0.18778648       <NA>
## 8           Population -0.19279277               0 0.19279277       <NA>
## 12                   x -0.23179465               0 0.23179465       <NA>
## 5           Illiteracy -0.70214027               0 0.70214027     Murder
## 7               Murder -0.82083018               0 0.82083018       <NA>
##    freqRatio percentUnique zeroVar   nzv myNearZV is.cor.y.abs.low
## 4        1.0      91.42857   FALSE FALSE    FALSE            FALSE
## 13       1.0      91.42857   FALSE FALSE    FALSE            FALSE
## 3        1.0      91.42857   FALSE FALSE    FALSE            FALSE
## 11       1.2      11.42857   FALSE FALSE    FALSE            FALSE
## 6        1.0     100.00000   FALSE FALSE    FALSE            FALSE
## 2        1.0     100.00000   FALSE FALSE    FALSE             TRUE
## 9        1.0     100.00000   FALSE FALSE    FALSE             TRUE
## 10       1.2      25.71429   FALSE FALSE    FALSE             TRUE
## 1        1.0     100.00000   FALSE FALSE    FALSE            FALSE
## 8        1.0     100.00000   FALSE FALSE    FALSE            FALSE
## 12       1.0     100.00000   FALSE FALSE    FALSE            FALSE
## 5        1.2      48.57143   FALSE FALSE    FALSE            FALSE
## 7        1.0      94.28571   FALSE FALSE    FALSE            FALSE
```

```r
#subset(glb_feats_df, id %in% c("A.nuppr.log", "S.nuppr.log"))
print(myplot_scatter(glb_feats_df, "percentUnique", "freqRatio", 
                     colorcol_name="myNearZV", jitter=TRUE) + 
          geom_point(aes(shape=nzv)) + xlim(-5, 25))
```

```
## Warning in myplot_scatter(glb_feats_df, "percentUnique", "freqRatio",
## colorcol_name = "myNearZV", : converting myNearZV to class:factor
```

```
## Warning in loop_apply(n, do.ply): Removed 12 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 12 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 12 rows containing missing values
## (geom_point).
```

![](USCensus1977_State_HW2_v2_files/figure-html/select.features-1.png) 

```r
print(subset(glb_feats_df, myNearZV))
```

```
##  [1] id               cor.y            exclude.as.feat  cor.y.abs       
##  [5] cor.high.X       freqRatio        percentUnique    zeroVar         
##  [9] nzv              myNearZV         is.cor.y.abs.low
## <0 rows> (or 0-length row.names)
```

```r
glb_allobs_df <- glb_allobs_df[, setdiff(names(glb_allobs_df), 
                                         subset(glb_feats_df, myNearZV)$id)]

if (!is.null(glb_interaction_only_features))
    glb_feats_df[glb_feats_df$id %in% glb_interaction_only_features, "interaction.feat"] <-
        names(glb_interaction_only_features) else
    glb_feats_df$interaction.feat <- NA        

mycheck_problem_data(glb_allobs_df, terminate = TRUE)
```

```
## [1] "numeric data missing in : "
## named integer(0)
## [1] "numeric data w/ 0s in : "
## Frost 
##     1 
## [1] "numeric data w/ Infs in : "
## named integer(0)
## [1] "numeric data w/ NaNs in : "
## named integer(0)
## [1] "string data missing in : "
##      state.abb state.division     state.name   state.region 
##              0              0              0              0
```

```r
# glb_allobs_df %>% filter(is.na(Married.fctr)) %>% tbl_df()
# glb_allobs_df %>% count(Married.fctr)
# levels(glb_allobs_df$Married.fctr)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "partition.data.training", major.inc=TRUE)
```

```
##                     label step_major step_minor    bgn    end elapsed
## 8         select.features          5          0 16.763 17.353    0.59
## 9 partition.data.training          6          0 17.354     NA      NA
```

## Step `6.0: partition data training`

```r
if (all(is.na(glb_newobs_df[, glb_rsp_var]))) {
    require(caTools)
    
    set.seed(glb_split_sample.seed)
    split <- sample.split(glb_trnobs_df[, glb_rsp_var_raw], 
        SplitRatio=1 - (nrow(glb_newobs_df) * 1.1 / nrow(glb_trnobs_df)))
    glb_fitobs_df <- glb_trnobs_df[split, ] 
    glb_OOBobs_df <- glb_trnobs_df[!split ,]    
} else {
    print(sprintf("Newdata contains non-NA data for %s; setting OOB to Newdata", 
                  glb_rsp_var))
    glb_fitobs_df <- glb_trnobs_df; glb_OOBobs_df <- glb_newobs_df
}
```

```
## [1] "Newdata contains non-NA data for Life.Exp; setting OOB to Newdata"
```

```r
if (!is.null(glb_max_fitobs) && (nrow(glb_fitobs_df) > glb_max_fitobs)) {
    warning("glb_fitobs_df restricted to glb_max_fitobs: ", 
            format(glb_max_fitobs, big.mark=","))
    org_fitobs_df <- glb_fitobs_df
    glb_fitobs_df <- 
        org_fitobs_df[split <- sample.split(org_fitobs_df[, glb_rsp_var_raw], 
                                            SplitRatio=glb_max_fitobs), ]
    org_fitobs_df <- NULL
}

glb_allobs_df$.lcn <- ""
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_fitobs_df[, glb_id_var], ".lcn"] <- "Fit"
glb_allobs_df[glb_allobs_df[, glb_id_var] %in% 
              glb_OOBobs_df[, glb_id_var], ".lcn"] <- "OOB"

dsp_class_dstrb <- function(obs_df, location_var, partition_var) {
    xtab_df <- mycreate_xtab_df(obs_df, c(location_var, partition_var))
    rownames(xtab_df) <- xtab_df[, location_var]
    xtab_df <- xtab_df[, -grepl(location_var, names(xtab_df))]
    print(xtab_df)
    print(xtab_df / rowSums(xtab_df, na.rm=TRUE))    
}    

# Ensure proper splits by glb_rsp_var_raw & user-specified feature for OOB vs. new
if (!is.null(glb_category_vars)) {
    if (glb_is_classification)
        dsp_class_dstrb(glb_allobs_df, ".lcn", glb_rsp_var_raw)
    newobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .src == "Test"), 
                                           glb_category_vars)
    OOBobs_ctgry_df <- mycreate_sqlxtab_df(subset(glb_allobs_df, .lcn == "OOB"), 
                                           glb_category_vars)
    glb_ctgry_df <- merge(newobs_ctgry_df, OOBobs_ctgry_df, by=glb_category_vars
                          , all=TRUE, suffixes=c(".Tst", ".OOB"))
    glb_ctgry_df$.freqRatio.Tst <- glb_ctgry_df$.n.Tst / sum(glb_ctgry_df$.n.Tst, na.rm=TRUE)
    glb_ctgry_df$.freqRatio.OOB <- glb_ctgry_df$.n.OOB / sum(glb_ctgry_df$.n.OOB, na.rm=TRUE)
    print(orderBy(~-.freqRatio.Tst-.freqRatio.OOB, glb_ctgry_df))
}

# Run this line by line
print("glb_feats_df:");   print(dim(glb_feats_df))
```

```
## [1] "glb_feats_df:"
```

```
## [1] 13 12
```

```r
sav_feats_df <- glb_feats_df
glb_feats_df <- sav_feats_df

glb_feats_df[, "rsp_var_raw"] <- FALSE
glb_feats_df[glb_feats_df$id == glb_rsp_var_raw, "rsp_var_raw"] <- TRUE 
glb_feats_df$exclude.as.feat <- (glb_feats_df$exclude.as.feat == 1)
if (!is.null(glb_id_var) && glb_id_var != ".rownames")
    glb_feats_df[glb_feats_df$id %in% glb_id_var, "id_var"] <- TRUE 
add_feats_df <- data.frame(id=glb_rsp_var, exclude.as.feat=TRUE, rsp_var=TRUE)
row.names(add_feats_df) <- add_feats_df$id; print(add_feats_df)
```

```
##                id exclude.as.feat rsp_var
## Life.Exp Life.Exp            TRUE    TRUE
```

```r
glb_feats_df <- myrbind_df(glb_feats_df, add_feats_df)
if (glb_id_var != ".rownames")
    print(subset(glb_feats_df, rsp_var_raw | rsp_var | id_var)) else
    print(subset(glb_feats_df, rsp_var_raw | rsp_var))    
```

```
##                id cor.y exclude.as.feat cor.y.abs cor.high.X freqRatio
## Life.Exp Life.Exp    NA            TRUE        NA       <NA>        NA
##          percentUnique zeroVar nzv myNearZV is.cor.y.abs.low
## Life.Exp            NA      NA  NA       NA               NA
##          interaction.feat rsp_var_raw id_var rsp_var
## Life.Exp               NA          NA     NA    TRUE
```

```r
print("glb_feats_df vs. glb_allobs_df: "); 
```

```
## [1] "glb_feats_df vs. glb_allobs_df: "
```

```r
print(setdiff(glb_feats_df$id, names(glb_allobs_df)))
```

```
## character(0)
```

```r
print("glb_allobs_df vs. glb_feats_df: "); 
```

```
## [1] "glb_allobs_df vs. glb_feats_df: "
```

```r
# Ensure these are only chr vars
print(setdiff(setdiff(names(glb_allobs_df), glb_feats_df$id), 
                myfind_chr_cols_df(glb_allobs_df)))
```

```
## character(0)
```

```r
#print(setdiff(setdiff(names(glb_allobs_df), glb_exclude_vars_as_features), 
#                glb_feats_df$id))

print("glb_allobs_df: "); print(dim(glb_allobs_df))
```

```
## [1] "glb_allobs_df: "
```

```
## [1] 50 20
```

```r
print("glb_trnobs_df: "); print(dim(glb_trnobs_df))
```

```
## [1] "glb_trnobs_df: "
```

```
## [1] 35 19
```

```r
print("glb_fitobs_df: "); print(dim(glb_fitobs_df))
```

```
## [1] "glb_fitobs_df: "
```

```
## [1] 35 19
```

```r
print("glb_OOBobs_df: "); print(dim(glb_OOBobs_df))
```

```
## [1] "glb_OOBobs_df: "
```

```
## [1] 15 19
```

```r
print("glb_newobs_df: "); print(dim(glb_newobs_df))
```

```
## [1] "glb_newobs_df: "
```

```
## [1] 15 19
```

```r
# # Does not handle NULL or length(glb_id_var) > 1
# glb_allobs_df$.src.trn <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_trnobs_df[, glb_id_var], 
#                 ".src.trn"] <- 1 
# glb_allobs_df$.src.fit <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_fitobs_df[, glb_id_var], 
#                 ".src.fit"] <- 1 
# glb_allobs_df$.src.OOB <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_OOBobs_df[, glb_id_var], 
#                 ".src.OOB"] <- 1 
# glb_allobs_df$.src.new <- 0
# glb_allobs_df[glb_allobs_df[, glb_id_var] %in% glb_newobs_df[, glb_id_var], 
#                 ".src.new"] <- 1 
# #print(unique(glb_allobs_df[, ".src.trn"]))
# write_cols <- c(glb_feats_df$id, 
#                 ".src.trn", ".src.fit", ".src.OOB", ".src.new")
# glb_allobs_df <- glb_allobs_df[, write_cols]
# 
# tmp_feats_df <- glb_feats_df
# tmp_entity_df <- glb_allobs_df

if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         file=paste0(glb_out_pfx, "blddfs_dsk.RData"))
# load(paste0(glb_out_pfx, "blddfs_dsk.RData"))

# if (!all.equal(tmp_feats_df, glb_feats_df))
#     stop("glb_feats_df r/w not working")
# if (!all.equal(tmp_entity_df, glb_allobs_df))
#     stop("glb_allobs_df r/w not working")

rm(split)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=TRUE)
```

```
##                      label step_major step_minor    bgn    end elapsed
## 9  partition.data.training          6          0 17.354 17.652   0.299
## 10              fit.models          7          0 17.653     NA      NA
```

## Step `7.0: fit models`

```r
# load(paste0(glb_out_pfx, "dsk.RData"))
# keep_cols <- setdiff(names(glb_allobs_df), 
#                      grep("^.src", names(glb_allobs_df), value=TRUE))
# glb_trnobs_df <- glb_allobs_df[glb_allobs_df$.src.trn == 1, keep_cols]
# glb_fitobs_df <- glb_allobs_df[glb_allobs_df$.src.fit == 1, keep_cols]
# glb_OOBobs_df <- glb_allobs_df[glb_allobs_df$.src.OOB == 1, keep_cols]
# glb_newobs_df <- glb_allobs_df[glb_allobs_df$.src.new == 1, keep_cols]
# 
# glb_models_lst <- list(); glb_models_df <- data.frame()
# 
if (glb_is_classification && glb_is_binomial && 
        (length(unique(glb_fitobs_df[, glb_rsp_var])) < 2))
    stop("glb_fitobs_df$", glb_rsp_var, ": contains less than 2 unique values: ",
         paste0(unique(glb_fitobs_df[, glb_rsp_var]), collapse=", "))

max_cor_y_x_vars <- orderBy(~ -cor.y.abs, 
        subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low & 
                                is.na(cor.high.X)))[1:2, "id"]
# while(length(max_cor_y_x_vars) < 2) {
#     max_cor_y_x_vars <- c(max_cor_y_x_vars, orderBy(~ -cor.y.abs, 
#             subset(glb_feats_df, (exclude.as.feat == 0) & !is.cor.y.abs.low))[3, "id"])    
# }
if (!is.null(glb_Baseline_mdl_var)) {
    if ((max_cor_y_x_vars[1] != glb_Baseline_mdl_var) & 
        (glb_feats_df[max_cor_y_x_vars[1], "cor.y.abs"] > 
         glb_feats_df[glb_Baseline_mdl_var, "cor.y.abs"]))
        stop(max_cor_y_x_vars[1], " has a lower correlation with ", glb_rsp_var, 
             " than the Baseline var: ", glb_Baseline_mdl_var)
}

glb_model_type <- ifelse(glb_is_regression, "regression", "classification")
    
# Baseline
if (!is.null(glb_Baseline_mdl_var)) 
    ret_lst <- myfit_mdl_fn(model_id="Baseline", model_method="mybaseln_classfr",
                            indep_vars_vctr=glb_Baseline_mdl_var,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Most Frequent Outcome "MFO" model: mean(y) for regression
#   Not using caret's nullModel since model stats not avl
#   Cannot use rpart for multinomial classification since it predicts non-MFO
ret_lst <- myfit_mdl(model_id="MFO", 
                     model_method=ifelse(glb_is_regression, "lm", "myMFO_classfr"), 
                     model_type=glb_model_type,
                        indep_vars_vctr=".rnorm",
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: MFO.lm"
## [1] "    indep_vars: .rnorm"
## Fitting parameter = none on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-1.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-2.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-3.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -2.8679 -0.4467 -0.1146  1.0084  2.1875 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  70.8425     0.2219 319.260   <2e-16 ***
## .rnorm       -0.2763     0.2516  -1.098     0.28    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 1.312 on 33 degrees of freedom
## Multiple R-squared:  0.03526,	Adjusted R-squared:  0.006029 
## F-statistic: 1.206 on 1 and 33 DF,  p-value: 0.28
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method  feats max.nTuningRuns min.elapsedtime.everything
## 1   MFO.lm           lm .rnorm               0                      0.649
##   min.elapsedtime.final max.R.sq.fit min.RMSE.fit max.R.sq.OOB
## 1                 0.003   0.03526376     1.274159   -0.1638247
##   min.RMSE.OOB max.Adj.R.sq.fit
## 1     1.512725      0.006029329
```

```r
if (glb_is_classification)
    # "random" model - only for classification; 
    #   none needed for regression since it is same as MFO
    ret_lst <- myfit_mdl(model_id="Random", model_method="myrandom_classfr",
                            model_type=glb_model_type,                         
                            indep_vars_vctr=".rnorm",
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)

# Any models that have tuning parameters has "better" results with cross-validation
#   (except rf) & "different" results for different outcome metrics

# Max.cor.Y
#   Check impact of cv
#       rpart is not a good candidate since caret does not optimize cp (only tuning parameter of rpart) well
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df)
```

```
## [1] "fitting model: Max.cor.Y.cv.0.rpart"
## [1] "    indep_vars: Murder, Income"
```

```
## Loading required package: rpart
```

```
## Fitting cp = 0.52 on full training set
```

```
## Loading required package: rpart.plot
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-5.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 35 
## 
##          CP nsplit rel error
## 1 0.5202823      0         1
## 
## Node number 1: 35 observations
##   mean=70.83543, MSE=1.682825 
## 
## n= 35 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 35 58.89887 70.83543 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method          feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.rpart        rpart Murder, Income               0
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.549                 0.007            0
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1     1.297237            0      1.40222
```

```r
ret_lst <- myfit_mdl(model_id="Max.cor.Y.cv.0.cp.0", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=0, 
            tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
```

```
## [1] "fitting model: Max.cor.Y.cv.0.cp.0.rpart"
## [1] "    indep_vars: Murder, Income"
## Fitting cp = 0 on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-6.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 35 
## 
##          CP nsplit rel error
## 1 0.5202823      0 1.0000000
## 2 0.1444640      1 0.4797177
## 3 0.0000000      2 0.3352537
## 
## Variable importance
## Murder Income 
##     86     14 
## 
## Node number 1: 35 observations,    complexity param=0.5202823
##   mean=70.83543, MSE=1.682825 
##   left son=2 (21 obs) right son=3 (14 obs)
##   Primary splits:
##       Murder < 5.8    to the right, improve=0.5202823, (0 missing)
##       Income < 3891   to the left,  improve=0.3716857, (0 missing)
##   Surrogate splits:
##       Income < 5193   to the left,  agree=0.657, adj=0.143, (0 split)
## 
## Node number 2: 21 observations,    complexity param=0.144464
##   mean=70.07143, MSE=0.9915456 
##   left son=4 (8 obs) right son=5 (13 obs)
##   Primary splits:
##       Murder < 11.05  to the right, improve=0.4086340, (0 missing)
##       Income < 3929   to the left,  improve=0.3497827, (0 missing)
##   Surrogate splits:
##       Income < 3673.5 to the left,  agree=0.714, adj=0.25, (0 split)
## 
## Node number 3: 14 observations
##   mean=71.98143, MSE=0.5308837 
## 
## Node number 4: 8 observations
##   mean=69.26, MSE=0.89055 
## 
## Node number 5: 13 observations
##   mean=70.57077, MSE=0.3991763 
## 
## n= 35 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 35 58.898870 70.83543  
##   2) Murder>=5.8 21 20.822460 70.07143  
##     4) Murder>=11.05 8  7.124400 69.26000 *
##     5) Murder< 11.05 13  5.189292 70.57077 *
##   3) Murder< 5.8 14  7.432371 71.98143 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                    model_id model_method          feats max.nTuningRuns
## 1 Max.cor.Y.cv.0.cp.0.rpart        rpart Murder, Income               0
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.437                 0.006    0.6647463
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB
## 1    0.7511147    0.4764582     1.014593
```

```r
if (glb_is_regression || glb_is_binomial) # For multinomials this model will be run next by default
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method="rpart",
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.rpart"
## [1] "    indep_vars: Murder, Income"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.52 on full training set
```

```
## Warning in myfit_mdl(model_id = "Max.cor.Y", model_method = "rpart",
## model_type = glb_model_type, : model's bestTune found at an extreme of
## tuneGrid for parameter: cp
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-7.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-8.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 35 
## 
##          CP nsplit rel error
## 1 0.5202823      0         1
## 
## Node number 1: 35 observations
##   mean=70.83543, MSE=1.682825 
## 
## n= 35 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 35 58.89887 70.83543 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##          model_id model_method          feats max.nTuningRuns
## 1 Max.cor.Y.rpart        rpart Murder, Income               3
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.924                 0.007            0
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit min.RMSESD.fit
## 1      1.13825            0      1.40222        0.3110708      0.1557052
##   max.RsquaredSD.fit
## 1          0.1418048
```

```r
# Used to compare vs. Interactions.High.cor.Y and/or Max.cor.Y.TmSrs
ret_lst <- myfit_mdl(model_id="Max.cor.Y", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=max_cor_y_x_vars,
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Max.cor.Y.lm"
## [1] "    indep_vars: Murder, Income"
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-9.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-10.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-11.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-12.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4230 -0.3956 -0.0785  0.5158  1.4756 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 70.6375648  1.2345627  57.217  < 2e-16 ***
## Murder      -0.2609401  0.0338571  -7.707 8.71e-09 ***
## Income       0.0004876  0.0002611   1.868    0.071 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7358 on 32 degrees of freedom
## Multiple R-squared:  0.7058,	Adjusted R-squared:  0.6874 
## F-statistic: 38.39 on 2 and 32 DF,  p-value: 3.145e-09
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method          feats max.nTuningRuns
## 1 Max.cor.Y.lm           lm Murder, Income               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      1.023                 0.002     0.705827
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit max.Rsquared.fit
## 1    0.8349469     0.491967    0.9994526        0.6874411        0.6946483
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.1059423          0.1244454
```

```r
if (!is.null(glb_date_vars) && 
    (sum(grepl(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
               names(glb_allobs_df))) > 0)) {
# ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly1", 
#                         model_method=ifelse(glb_is_regression, "lm", 
#                                         ifelse(glb_is_binomial, "glm", "rpart")),
#                      model_type=glb_model_type,
#                         indep_vars_vctr=c(max_cor_y_x_vars, paste0(glb_date_vars, ".day.minutes")),
#                         rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                         fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                         n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
# 
ret_lst <- myfit_mdl(model_id="Max.cor.Y.TmSrs.poly", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                     model_type=glb_model_type,
                        indep_vars_vctr=c(max_cor_y_x_vars, 
            grep(paste(glb_date_vars, "\\.day\\.minutes\\.poly\\.", sep=""),
                        names(glb_allobs_df), value=TRUE)),
                        rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
}

# Interactions.High.cor.Y
if (length(int_feats <- setdiff(unique(glb_feats_df$cor.high.X), NA)) > 0) {
    # lm & glm handle interaction terms; rpart & rf do not
    if (glb_is_regression || glb_is_binomial) {
        indep_vars_vctr <- 
            c(max_cor_y_x_vars, paste(max_cor_y_x_vars[1], int_feats, sep=":"))            
    } else { indep_vars_vctr <- union(max_cor_y_x_vars, int_feats) }
    
    ret_lst <- myfit_mdl(model_id="Interact.High.cor.Y", 
                            model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                         model_type=glb_model_type,
                            indep_vars_vctr,
                            glb_rsp_var, glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)                        
}    
```

```
## [1] "fitting model: Interact.High.cor.Y.lm"
## [1] "    indep_vars: Murder, Income, Murder:Illiteracy, Murder:HS.Grad, Murder:Murder"
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-13.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-14.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-15.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-16.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.5001 -0.3157 -0.0616  0.5171  1.2410 
## 
## Coefficients:
##                       Estimate Std. Error t value Pr(>|t|)    
## (Intercept)         71.4520821  1.4857446  48.092   <2e-16 ***
## Murder              -0.4576543  0.1985981  -2.304   0.0283 *  
## Income               0.0002815  0.0003403   0.827   0.4147    
## `Murder:Illiteracy`  0.0170160  0.0373674   0.455   0.6521    
## `Murder:HS.Grad`     0.0037262  0.0032615   1.142   0.2623    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.7431 on 30 degrees of freedom
## Multiple R-squared:  0.7187,	Adjusted R-squared:  0.6812 
## F-statistic: 19.16 on 4 and 30 DF,  p-value: 6.433e-08
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##                 model_id model_method
## 1 Interact.High.cor.Y.lm           lm
##                                                              feats
## 1 Murder, Income, Murder:Illiteracy, Murder:HS.Grad, Murder:Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.805                 0.003
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.7187136    0.8694013     0.492048    0.9993728        0.6812088
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1         0.644158      0.1370642          0.1714077
```

```r
# Low.cor.X
# if (glb_is_classification && glb_is_binomial)
#     indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & 
#                                             is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"] else
indep_vars_vctr <- subset(glb_feats_df, is.na(cor.high.X) & !myNearZV & 
                              (exclude.as.feat != 1))[, "id"]  
myadjust_interaction_feats <- function(vars_vctr) {
    for (feat in subset(glb_feats_df, !is.na(interaction.feat))$id)
        if (feat %in% vars_vctr)
            vars_vctr <- union(setdiff(vars_vctr, feat), 
                paste0(glb_feats_df[glb_feats_df$id == feat, "interaction.feat"], ":", feat))
    return(vars_vctr)
}
indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)
ret_lst <- myfit_mdl(model_id="Low.cor.X", 
                        model_method=ifelse(glb_is_regression, "lm", 
                                        ifelse(glb_is_binomial, "glm", "rpart")),
                        indep_vars_vctr=indep_vars_vctr,
                        model_type=glb_model_type,                     
                        glb_rsp_var, glb_rsp_var_out,
                        fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                        n_cv_folds=glb_n_cv_folds, tune_models_df=NULL)
```

```
## [1] "fitting model: Low.cor.X.lm"
## [1] "    indep_vars: Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Murder"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: not plotting observations with leverage one:
##   33
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-17.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-18.png) 

```
## Warning: not plotting observations with leverage one:
##   33
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-19.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_0-20.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -0.9374 -0.2350  0.0000  0.3009  0.9680 
## 
## Coefficients:
##                                           Estimate Std. Error t value
## (Intercept)                              7.139e+01  4.846e+00  14.732
## Income                                   8.360e-04  3.200e-04   2.613
## Area                                    -1.092e-04  9.568e-05  -1.141
## state.area                               1.112e-04  9.478e-05   1.174
## `state.division.fctrEast North Central` -1.024e+00  1.092e+00  -0.938
## `state.division.fctrEast South Central`  1.005e+00  1.165e+00   0.862
## `state.division.fctrMiddle Atlantic`    -2.292e+00  1.545e+00  -1.483
## `state.division.fctrNew England`        -1.118e+00  1.713e+00  -0.653
## state.division.fctrPacific              -9.447e-01  8.626e-01  -1.095
## `state.division.fctrSouth Atlantic`     -1.068e+00  1.291e+00  -0.828
## `state.division.fctrWest North Central` -5.256e-01  7.510e-01  -0.700
## `state.division.fctrWest South Central` -3.825e-01  8.169e-01  -0.468
## .rnorm                                  -3.369e-01  1.663e-01  -2.026
## Population                               8.614e-05  8.586e-05   1.003
## x                                        1.814e-02  4.680e-02   0.388
## Murder                                  -3.422e-01  4.719e-02  -7.251
##                                         Pr(>|t|)    
## (Intercept)                             7.55e-12 ***
## Income                                    0.0171 *  
## Area                                      0.2681    
## state.area                                0.2551    
## `state.division.fctrEast North Central`   0.3599    
## `state.division.fctrEast South Central`   0.3993    
## `state.division.fctrMiddle Atlantic`      0.1544    
## `state.division.fctrNew England`          0.5218    
## state.division.fctrPacific                0.2872    
## `state.division.fctrSouth Atlantic`       0.4182    
## `state.division.fctrWest North Central`   0.4925    
## `state.division.fctrWest South Central`   0.6450    
## .rnorm                                    0.0570 .  
## Population                                0.3284    
## x                                         0.7027    
## Murder                                  7.00e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5656 on 19 degrees of freedom
## Multiple R-squared:  0.8968,	Adjusted R-squared:  0.8153 
## F-statistic: 11.01 on 15 and 19 DF,  p-value: 2.228e-06
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##       model_id model_method
## 1 Low.cor.X.lm           lm
##                                                                          feats
## 1 Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.866                 0.006
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.8968055     1.310286   -0.4350738     1.679784        0.8153362
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.4598377      0.4605754          0.1158658
```

```r
rm(ret_lst)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 10 fit.models          7          0 17.653 31.074  13.422
## 11 fit.models          7          1 31.075     NA      NA
```


```r
fit.models_1_chunk_df <- myadd_chunk(NULL, "fit.models_1_bgn")
```

```
##              label step_major step_minor    bgn end elapsed
## 1 fit.models_1_bgn          1          0 32.618  NA      NA
```

```r
# Options:
#   1. rpart & rf manual tuning
#   2. rf without pca (default: with pca)

#stop(here); sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
#glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df

# All X that is not user excluded
# if (glb_is_classification && glb_is_binomial) {
#     model_id_pfx <- "Conditional.X"
# # indep_vars_vctr <- setdiff(names(glb_fitobs_df), union(glb_rsp_var, glb_exclude_vars_as_features))
#     indep_vars_vctr <- subset(glb_feats_df, is.ConditionalX.y & 
#                                             (exclude.as.feat != 1))[, "id"]
# } else {
    model_id_pfx <- "All.X"
    indep_vars_vctr <- subset(glb_feats_df, !myNearZV &
                                            (exclude.as.feat != 1))[, "id"]
# }

indep_vars_vctr <- myadjust_interaction_feats(indep_vars_vctr)

for (method in glb_models_method_vctr) {
    fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, 
                                paste0("fit.models_1_", method), major.inc=TRUE)
    if (method %in% c("rpart", "rf")) {
        # rpart:    fubar's the tree
        # rf:       skip the scenario w/ .rnorm for speed
        indep_vars_vctr <- setdiff(indep_vars_vctr, c(".rnorm"))
        model_id <- paste0(model_id_pfx, ".no.rnorm")
    } else model_id <- model_id_pfx
    
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                            indep_vars_vctr=indep_vars_vctr,
                            model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                            fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    
    # If All.X.glm is less accurate than Low.Cor.X.glm
    #   check NA coefficients & filter appropriate terms in indep_vars_vctr
#     if (method == "glm") {
#         orig_glm <- glb_models_lst[[paste0(model_id, ".", model_method)]]$finalModel
#         orig_glm <- glb_models_lst[["All.X.glm"]]$finalModel; print(summary(orig_glm))
#           vif_orig_glm <- vif(orig_glm); print(vif_orig_glm)
#           print(vif_orig_glm[!is.na(vif_orig_glm) & (vif_orig_glm == Inf)])
#           print(which.max(vif_orig_glm))
#           print(sort(vif_orig_glm[vif_orig_glm >= 1.0e+03], decreasing=TRUE))
#           glb_fitobs_df[c(1143, 3637, 3953, 4105), c("UniqueID", "Popular", "H.P.quandary", "Headline")]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.nchrs.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%    grep("[HSA]\\.npnct14.log", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.T.scen", glb_feats_df$id, value=TRUE), ]
#           glb_feats_df[glb_feats_df$id %in% grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE) | glb_feats_df$cor.high.X %in%         grep("[HSA]\\.P.first", glb_feats_df$id, value=TRUE), ]
#           all.equal(glb_allobs_df$S.nuppr.log, glb_allobs_df$A.nuppr.log)
#           all.equal(glb_allobs_df$S.npnct19.log, glb_allobs_df$A.npnct19.log)
#           all.equal(glb_allobs_df$S.P.year.colon, glb_allobs_df$A.P.year.colon)
#           all.equal(glb_allobs_df$S.T.share, glb_allobs_df$A.T.share)
#           all.equal(glb_allobs_df$H.T.clip, glb_allobs_df$H.P.daily.clip.report)
#           cor(glb_allobs_df$S.T.herald, glb_allobs_df$S.T.tribun)
#           dsp_obs(Abstract.contains="[Dd]iar", cols=("Abstract"), all=TRUE)
#           dsp_obs(Abstract.contains="[Ss]hare", cols=("Abstract"), all=TRUE)
#           subset(glb_feats_df, cor.y.abs <= glb_feats_df[glb_feats_df$id == ".rnorm", "cor.y.abs"])
#         corxx_mtrx <- cor(data.matrix(glb_allobs_df[, setdiff(names(glb_allobs_df), myfind_chr_cols_df(glb_allobs_df))]), use="pairwise.complete.obs"); abs_corxx_mtrx <- abs(corxx_mtrx); diag(abs_corxx_mtrx) <- 0
#           which.max(abs_corxx_mtrx["S.T.tribun", ])
#           abs_corxx_mtrx["A.npnct08.log", "S.npnct08.log"]
#         step_glm <- step(orig_glm)
#     }
    # Since caret does not optimize rpart well
#     if (method == "rpart")
#         ret_lst <- myfit_mdl(model_id=paste0(model_id_pfx, ".cp.0"), model_method=method,
#                                 indep_vars_vctr=indep_vars_vctr,
#                                 model_type=glb_model_type,
#                                 rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                                 fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,        
#             n_cv_folds=0, tune_models_df=data.frame(parameter="cp", min=0.0, max=0.0, by=0.1))
}
```

```
##              label step_major step_minor    bgn    end elapsed
## 1 fit.models_1_bgn          1          0 32.618 32.631   0.013
## 2  fit.models_1_lm          2          0 32.631     NA      NA
## [1] "fitting model: All.X.lm"
## [1] "    indep_vars: HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: not plotting observations with leverage one:
##   33
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-1.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-2.png) 

```
## Warning: not plotting observations with leverage one:
##   33
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-3.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.1017 -0.1744  0.0000  0.2392  0.7914 
## 
## Coefficients: (3 not defined because of singularities)
##                                           Estimate Std. Error t value
## (Intercept)                              7.198e+01  6.650e+00  10.824
## HS.Grad                                  1.257e-02  5.190e-02   0.242
## y                                        1.703e-02  8.052e-02   0.211
## Frost                                   -4.546e-03  6.638e-03  -0.685
## `state.region.fctrNorth Central`        -3.991e-01  7.427e-01  -0.537
## state.region.fctrNortheast              -1.075e+00  1.398e+00  -0.769
## state.region.fctrWest                    1.207e-01  1.005e+00   0.120
## Income                                   7.524e-04  4.188e-04   1.797
## Area                                    -9.414e-05  1.120e-04  -0.840
## state.area                               9.721e-05  1.113e-04   0.873
## `state.division.fctrEast North Central` -5.053e-01  6.867e-01  -0.736
## `state.division.fctrEast South Central`  1.119e+00  8.900e-01   1.257
## `state.division.fctrMiddle Atlantic`    -9.746e-01  9.227e-01  -1.056
## `state.division.fctrNew England`                NA         NA      NA
## state.division.fctrPacific              -1.331e+00  1.440e+00  -0.925
## `state.division.fctrSouth Atlantic`     -8.597e-01  8.867e-01  -0.970
## `state.division.fctrWest North Central`         NA         NA      NA
## `state.division.fctrWest South Central`         NA         NA      NA
## .rnorm                                  -2.927e-01  1.921e-01  -1.524
## Population                               6.330e-05  9.991e-05   0.634
## x                                        2.809e-02  5.134e-02   0.547
## Illiteracy                              -4.387e-01  6.378e-01  -0.688
## Murder                                  -3.088e-01  6.394e-02  -4.830
##                                         Pr(>|t|)    
## (Intercept)                             1.75e-08 ***
## HS.Grad                                 0.811903    
## y                                       0.835357    
## Frost                                   0.503909    
## `state.region.fctrNorth Central`        0.598856    
## state.region.fctrNortheast              0.453771    
## state.region.fctrWest                   0.906057    
## Income                                  0.092525 .  
## Area                                    0.413945    
## state.area                              0.396368    
## `state.division.fctrEast North Central` 0.473224    
## `state.division.fctrEast South Central` 0.227928    
## `state.division.fctrMiddle Atlantic`    0.307561    
## `state.division.fctrNew England`              NA    
## state.division.fctrPacific              0.369859    
## `state.division.fctrSouth Atlantic`     0.347615    
## `state.division.fctrWest North Central`       NA    
## `state.division.fctrWest South Central`       NA    
## .rnorm                                  0.148340    
## Population                              0.535914    
## x                                       0.592257    
## Illiteracy                              0.502058    
## Murder                                  0.000221 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6102 on 15 degrees of freedom
## Multiple R-squared:  0.9052,	Adjusted R-squared:  0.7851 
## F-statistic: 7.536 on 19 and 15 DF,  p-value: 0.0001267
## 
## [1] "    calling mypredict_mdl for fit:"
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
## [1] "    calling mypredict_mdl for OOB:"
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
## 1 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.841                  0.01
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.9051765      1.83507   -0.4939665     1.713905        0.7850667
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.3370194      0.5357182          0.1074956
##              label step_major step_minor    bgn    end elapsed
## 2  fit.models_1_lm          2          0 32.631 35.277   2.646
## 3 fit.models_1_glm          3          0 35.277     NA      NA
## [1] "fitting model: All.X.glm"
## [1] "    indep_vars: HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder"
## Aggregating results
## Fitting final model on full training set
```

```
## Warning: not plotting observations with leverage one:
##   33
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-4.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-5.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-6.png) 

```
## Warning: not plotting observations with leverage one:
##   33
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-7.png) 

```
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.1017  -0.1744   0.0000   0.2392   0.7914  
## 
## Coefficients: (3 not defined because of singularities)
##                                           Estimate Std. Error t value
## (Intercept)                              7.198e+01  6.650e+00  10.824
## HS.Grad                                  1.257e-02  5.190e-02   0.242
## y                                        1.703e-02  8.052e-02   0.211
## Frost                                   -4.546e-03  6.638e-03  -0.685
## `state.region.fctrNorth Central`        -3.991e-01  7.427e-01  -0.537
## state.region.fctrNortheast              -1.075e+00  1.398e+00  -0.769
## state.region.fctrWest                    1.207e-01  1.005e+00   0.120
## Income                                   7.524e-04  4.188e-04   1.797
## Area                                    -9.414e-05  1.120e-04  -0.840
## state.area                               9.721e-05  1.113e-04   0.873
## `state.division.fctrEast North Central` -5.053e-01  6.867e-01  -0.736
## `state.division.fctrEast South Central`  1.119e+00  8.900e-01   1.257
## `state.division.fctrMiddle Atlantic`    -9.746e-01  9.227e-01  -1.056
## `state.division.fctrNew England`                NA         NA      NA
## state.division.fctrPacific              -1.331e+00  1.440e+00  -0.925
## `state.division.fctrSouth Atlantic`     -8.597e-01  8.867e-01  -0.970
## `state.division.fctrWest North Central`         NA         NA      NA
## `state.division.fctrWest South Central`         NA         NA      NA
## .rnorm                                  -2.927e-01  1.921e-01  -1.524
## Population                               6.330e-05  9.991e-05   0.634
## x                                        2.809e-02  5.134e-02   0.547
## Illiteracy                              -4.387e-01  6.378e-01  -0.688
## Murder                                  -3.088e-01  6.394e-02  -4.830
##                                         Pr(>|t|)    
## (Intercept)                             1.75e-08 ***
## HS.Grad                                 0.811903    
## y                                       0.835357    
## Frost                                   0.503909    
## `state.region.fctrNorth Central`        0.598856    
## state.region.fctrNortheast              0.453771    
## state.region.fctrWest                   0.906057    
## Income                                  0.092525 .  
## Area                                    0.413945    
## state.area                              0.396368    
## `state.division.fctrEast North Central` 0.473224    
## `state.division.fctrEast South Central` 0.227928    
## `state.division.fctrMiddle Atlantic`    0.307561    
## `state.division.fctrNew England`              NA    
## state.division.fctrPacific              0.369859    
## `state.division.fctrSouth Atlantic`     0.347615    
## `state.division.fctrWest North Central`       NA    
## `state.division.fctrWest South Central`       NA    
## .rnorm                                  0.148340    
## Population                              0.535914    
## x                                       0.592257    
## Illiteracy                              0.502058    
## Murder                                  0.000221 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.3723332)
## 
##     Null deviance: 58.899  on 34  degrees of freedom
## Residual deviance:  5.585  on 15  degrees of freedom
## AIC: 77.091
## 
## Number of Fisher Scoring iterations: 2
## 
## [1] "    calling mypredict_mdl for fit:"
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
## [1] "    calling mypredict_mdl for OOB:"
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
##    model_id model_method
## 1 All.X.glm          glm
##                                                                                                                            feats
## 1 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.865                 0.014
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.9051765      1.83507   -0.4939665     1.713905    77.09146
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.3370194      0.5357182          0.1074956
##                   label step_major step_minor    bgn   end elapsed
## 3      fit.models_1_glm          3          0 35.277 37.63   2.354
## 4 fit.models_1_bayesglm          4          0 37.631    NA      NA
## [1] "fitting model: All.X.bayesglm"
## [1] "    indep_vars: HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder"
```

```
## Loading required package: arm
## Loading required package: MASS
## 
## Attaching package: 'MASS'
## 
## The following object is masked from 'package:dplyr':
## 
##     select
## 
## Loading required package: Matrix
## Loading required package: lme4
## Loading required package: Rcpp
## 
## arm (Version 1.8-5, built: 2015-05-13)
## 
## Working directory is /Users/bbalaji-2012/Documents/Work/Courses/MIT/Analytics_Edge_15_071x/Assignments/HW2_USCensus1977_State
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-8.png) 

```
## Aggregating results
## Fitting final model on full training set
## 
## Call:
## NULL
## 
## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.10504  -0.17040  -0.01897   0.24553   0.79188  
## 
## Coefficients:
##                                           Estimate Std. Error t value
## (Intercept)                              7.192e+01  7.528e+00   9.554
## HS.Grad                                  1.321e-02  5.755e-02   0.230
## y                                        1.217e-02  8.805e-02   0.138
## Frost                                   -4.223e-03  7.278e-03  -0.580
## `state.region.fctrNorth Central`        -2.938e-01  3.757e+00  -0.078
## state.region.fctrNortheast              -9.020e-01  3.826e+00  -0.236
## state.region.fctrWest                    2.895e-01  2.673e+00   0.108
## Income                                   7.390e-04  4.668e-04   1.583
## Area                                    -9.516e-05  1.250e-04  -0.761
## state.area                               9.834e-05  1.242e-04   0.792
## `state.division.fctrEast North Central` -3.820e-01  3.484e+00  -0.110
## `state.division.fctrEast South Central`  1.293e+00  2.524e+00   0.512
## `state.division.fctrMiddle Atlantic`    -9.086e-01  3.524e+00  -0.258
## `state.division.fctrNew England`         3.418e-02  3.534e+00   0.010
## state.division.fctrPacific              -1.222e+00  1.539e+00  -0.794
## `state.division.fctrSouth Atlantic`     -6.689e-01  2.526e+00  -0.265
## `state.division.fctrWest North Central`  9.593e-02  3.475e+00   0.028
## `state.division.fctrWest South Central`  1.711e-01  2.510e+00   0.068
## .rnorm                                  -2.876e-01  2.141e-01  -1.343
## Population                               6.041e-05  1.106e-04   0.546
## x                                        2.772e-02  5.572e-02   0.497
## Illiteracy                              -4.247e-01  7.040e-01  -0.603
## Murder                                  -3.091e-01  7.128e-02  -4.336
##                                         Pr(>|t|)    
## (Intercept)                             5.85e-07 ***
## HS.Grad                                 0.822266    
## y                                       0.892325    
## Frost                                   0.572491    
## `state.region.fctrNorth Central`        0.938959    
## state.region.fctrNortheast              0.817580    
## state.region.fctrWest                   0.915541    
## Income                                  0.139372    
## Area                                    0.461288    
## state.area                              0.443917    
## `state.division.fctrEast North Central` 0.914511    
## `state.division.fctrEast South Central` 0.617788    
## `state.division.fctrMiddle Atlantic`    0.800934    
## `state.division.fctrNew England`        0.992442    
## state.division.fctrPacific              0.442753    
## `state.division.fctrSouth Atlantic`     0.795642    
## `state.division.fctrWest North Central` 0.978427    
## `state.division.fctrWest South Central` 0.946786    
## .rnorm                                  0.204068    
## Population                              0.595071    
## x                                       0.627856    
## Illiteracy                              0.557542    
## Murder                                  0.000968 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.4657058)
## 
##     Null deviance: 58.8989  on 34  degrees of freedom
## Residual deviance:  5.5885  on 12  degrees of freedom
## AIC: 83.113
## 
## Number of Fisher Scoring iterations: 31
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##         model_id model_method
## 1 All.X.bayesglm     bayesglm
##                                                                                                                            feats
## 1 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      1.976                 0.051
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB min.aic.fit
## 1    0.9051175     1.673164    -0.485121     1.708855    83.11321
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.3637272      0.3906281          0.0886578
##                   label step_major step_minor    bgn    end elapsed
## 4 fit.models_1_bayesglm          4          0 37.631 40.515   2.884
## 5    fit.models_1_rpart          5          0 40.515     NA      NA
## [1] "fitting model: All.X.no.rnorm.rpart"
## [1] "    indep_vars: HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder"
## Aggregating results
## Selecting tuning parameters
## Fitting cp = 0.52 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: cp
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-9.png) 

```
## Call:
## rpart(formula = .outcome ~ ., control = list(minsplit = 20, minbucket = 7, 
##     cp = 0, maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, 
##     surrogatestyle = 0, maxdepth = 30, xval = 0))
##   n= 35 
## 
##          CP nsplit rel error
## 1 0.5202823      0         1
## 
## Node number 1: 35 observations
##   mean=70.83543, MSE=1.682825 
## 
## n= 35 
## 
## node), split, n, deviance, yval
##       * denotes terminal node
## 
## 1) root 35 58.89887 70.83543 *
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##               model_id model_method
## 1 All.X.no.rnorm.rpart        rpart
##                                                                                                                    feats
## 1 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      0.921                 0.013
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1            0     1.155693            0      1.40222        0.3011671
##   min.RMSESD.fit max.RsquaredSD.fit
## 1       0.185915          0.1579136
##                label step_major step_minor    bgn    end elapsed
## 5 fit.models_1_rpart          5          0 40.515 43.934   3.419
## 6    fit.models_1_rf          6          0 43.934     NA      NA
## [1] "fitting model: All.X.no.rnorm.rf"
## [1] "    indep_vars: HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder"
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attaching package: 'randomForest'
## 
## The following object is masked from 'package:dplyr':
## 
##     combine
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-10.png) 

```
## Aggregating results
## Selecting tuning parameters
## Fitting mtry = 2 on full training set
```

```
## Warning in myfit_mdl(model_id = model_id, model_method = method,
## indep_vars_vctr = indep_vars_vctr, : model's bestTune found at an extreme
## of tuneGrid for parameter: mtry
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-11.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-12.png) 

```
##                 Length Class      Mode     
## call              4    -none-     call     
## type              1    -none-     character
## predicted        35    -none-     numeric  
## mse             500    -none-     numeric  
## rsq             500    -none-     numeric  
## oob.times        35    -none-     numeric  
## importance       21    -none-     numeric  
## importanceSD      0    -none-     NULL     
## localImportance   0    -none-     NULL     
## proximity         0    -none-     NULL     
## ntree             1    -none-     numeric  
## mtry              1    -none-     numeric  
## forest           11    -none-     list     
## coefs             0    -none-     NULL     
## y                35    -none-     numeric  
## test              0    -none-     NULL     
## inbag             0    -none-     NULL     
## xNames           21    -none-     character
## problemType       1    -none-     character
## tuneValue         1    data.frame list     
## obsLevels         1    -none-     logical  
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##            model_id model_method
## 1 All.X.no.rnorm.rf           rf
##                                                                                                                    feats
## 1 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               3                      1.782                 0.024
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Rsquared.fit
## 1    0.8648624    0.7878053    0.3724496     1.110307        0.6989583
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.2050785          0.1760712
```

```r
# User specified
#   Ensure at least 2 vars in each regression; else varImp crashes
# sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df; sav_featsimp_df <- glb_featsimp_df
# glb_models_lst <- sav_models_lst; glb_models_df <- sav_models_df; glm_featsimp_df <- sav_featsimp_df

    # easier to exclude features
#model_id <- "";
# indep_vars_vctr <- head(subset(glb_models_df, grepl("All\\.X\\.", model_id), select=feats), 1)
# indep_vars_vctr <- setdiff(indep_vars_vctr, ".rnorm")

    # easier to include features
model_id <- "Csm.1"; indep_vars_vctr <- c("Population", "Income", "Illiteracy", "Murder", "HS.Grad", "Frost", "Area")
for (method in c("lm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: Csm.1.lm"
## [1] "    indep_vars: Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area"
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-13.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-14.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-15.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-16.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.3740 -0.2984  0.1442  0.4722  0.8784 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.161e+01  2.121e+00  33.759  < 2e-16 ***
## Population   2.336e-05  4.786e-05   0.488   0.6294    
## Income       1.215e-04  3.436e-04   0.354   0.7263    
## Illiteracy  -3.890e-01  4.807e-01  -0.809   0.4255    
## Murder      -2.614e-01  4.630e-02  -5.647 5.39e-06 ***
## HS.Grad      1.943e-02  3.316e-02   0.586   0.5629    
## Frost       -3.684e-03  3.875e-03  -0.951   0.3502    
## Area         6.115e-06  3.258e-06   1.877   0.0714 .  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6674 on 27 degrees of freedom
## Multiple R-squared:  0.7958,	Adjusted R-squared:  0.7429 
## F-statistic: 15.03 on 7 and 27 DF,  p-value: 7.81e-08
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method
## 1 Csm.1.lm           lm
##                                                          feats
## 1 Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area
##   max.nTuningRuns min.elapsedtime.everything min.elapsedtime.final
## 1               1                      0.845                 0.004
##   max.R.sq.fit min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## 1    0.7957973     1.070277   0.09058028     1.337206        0.7428559
##   max.Rsquared.fit min.RMSESD.fit max.RsquaredSD.fit
## 1        0.5047781       0.222815         0.06898573
##            importance
## Murder     100.000000
## Area        28.773018
## Frost       11.278727
## Illiteracy   8.604534
## HS.Grad      4.384197
## Population   2.537741
```

```r
model_id <- "Csm.2"; indep_vars_vctr <- c("Population", "Murder", "HS.Grad", "Frost")
for (method in c("lm")) {
    ret_lst <- myfit_mdl(model_id=model_id, model_method=method,
                                indep_vars_vctr=indep_vars_vctr,
                                model_type=glb_model_type,
                                rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
                                fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
                    n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df)
    csm_mdl_id <- paste0(model_id, ".", method)
    csm_featsimp_df <- myget_feats_importance(glb_models_lst[[paste0(model_id, ".", method)]]);         print(head(csm_featsimp_df))
}
```

```
## [1] "fitting model: Csm.2.lm"
## [1] "    indep_vars: Population, Murder, HS.Grad, Frost"
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-17.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-18.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-19.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_1-20.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4481 -0.4838  0.1375  0.4930  0.8952 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.977e+01  1.139e+00  61.245  < 2e-16 ***
## Population   5.537e-05  3.712e-05   1.492  0.14619    
## Murder      -2.501e-01  4.308e-02  -5.806  2.4e-06 ***
## HS.Grad      5.710e-02  1.818e-02   3.140  0.00378 ** 
## Frost       -2.704e-03  2.988e-03  -0.905  0.37282    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6748 on 30 degrees of freedom
## Multiple R-squared:  0.7681,	Adjusted R-squared:  0.7372 
## F-statistic: 24.84 on 4 and 30 DF,  p-value: 3.781e-09
## 
## [1] "    calling mypredict_mdl for fit:"
## [1] "    calling mypredict_mdl for OOB:"
##   model_id model_method                              feats max.nTuningRuns
## 1 Csm.2.lm           lm Population, Murder, HS.Grad, Frost               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.808                 0.003    0.7680836
##   min.RMSE.fit max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit max.Rsquared.fit
## 1    0.8483694    0.6186259     0.865948        0.7371614        0.6383817
##   min.RMSESD.fit max.RsquaredSD.fit
## 1      0.1624027         0.04550407
##            importance
## Murder      100.00000
## HS.Grad      45.60984
## Population   11.97951
## Frost         0.00000
```

```r
# Ntv.1.lm <- lm(reformulate(indep_vars_vctr, glb_rsp_var), glb_trnobs_df); print(summary(Ntv.1.lm))

#print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
#csm_featsimp_df[grepl("H.npnct19.log", row.names(csm_featsimp_df)), , FALSE]
#csm_OOBobs_df <- glb_get_predictions(glb_OOBobs_df, mdl_id=csm_mdl_id, rsp_var_out=glb_rsp_var_out, prob_threshold_def=glb_models_df[glb_models_df$model_id == csm_mdl_id, "opt.prob.threshold.OOB"])
#print(sprintf("%s OOB confusion matrix & accuracy: ", csm_mdl_id)); print(t(confusionMatrix(csm_OOBobs_df[, paste0(glb_rsp_var_out, csm_mdl_id)], csm_OOBobs_df[, glb_rsp_var])$table))

#glb_models_df[, "max.Accuracy.OOB", FALSE]
#varImp(glb_models_lst[["Low.cor.X.glm"]])
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.2.glm"]])$importance)
#orderBy(~ -Overall, varImp(glb_models_lst[["All.X.3.glm"]])$importance)
#glb_feats_df[grepl("npnct28", glb_feats_df$id), ]
#print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id)); print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], glb_OOBobs_df[, glb_rsp_var])$table))

    # User specified bivariate models
#     indep_vars_vctr_lst <- list()
#     for (feat in setdiff(names(glb_fitobs_df), 
#                          union(glb_rsp_var, glb_exclude_vars_as_features)))
#         indep_vars_vctr_lst[["feat"]] <- feat

    # User specified combinatorial models
#     indep_vars_vctr_lst <- list()
#     combn_mtrx <- combn(c("<feat1_name>", "<feat2_name>", "<featn_name>"), 
#                           <num_feats_to_choose>)
#     for (combn_ix in 1:ncol(combn_mtrx))
#         #print(combn_mtrx[, combn_ix])
#         indep_vars_vctr_lst[[combn_ix]] <- combn_mtrx[, combn_ix]
    
    # template for myfit_mdl
    #   rf is hard-coded in caret to recognize only Accuracy / Kappa evaluation metrics
    #       only for OOB in trainControl ?
    
#     ret_lst <- myfit_mdl_fn(model_id=paste0(model_id_pfx, ""), model_method=method,
#                             indep_vars_vctr=indep_vars_vctr,
#                             rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out,
#                             fit_df=glb_fitobs_df, OOB_df=glb_OOBobs_df,
#                             n_cv_folds=glb_n_cv_folds, tune_models_df=glb_tune_models_df,
#                             model_loss_mtrx=glb_model_metric_terms,
#                             model_summaryFunction=glb_model_metric_smmry,
#                             model_metric=glb_model_metric,
#                             model_metric_maximize=glb_model_metric_maximize)

# Simplify a model
# fit_df <- glb_fitobs_df; glb_mdl <- step(<complex>_mdl)

# Non-caret models
#     rpart_area_mdl <- rpart(reformulate("Area", response=glb_rsp_var), 
#                                data=glb_fitobs_df, #method="class", 
#                                control=rpart.control(cp=0.12),
#                            parms=list(loss=glb_model_metric_terms))
#     print("rpart_sel_wlm_mdl"); prp(rpart_sel_wlm_mdl)
# 

print(glb_models_df)
```

```
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.bayesglm                       All.X.bayesglm     bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
## Csm.1.lm                                   Csm.1.lm           lm
## Csm.2.lm                                   Csm.2.lm           lm
##                                                                                                                                                    feats
## MFO.lm                                                                                                                                            .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                      Murder, Income
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                 Murder, Income
## Max.cor.Y.rpart                                                                                                                           Murder, Income
## Max.cor.Y.lm                                                                                                                              Murder, Income
## Interact.High.cor.Y.lm                                                                  Murder, Income, Murder:Illiteracy, Murder:HS.Grad, Murder:Murder
## Low.cor.X.lm                                                                Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Murder
## All.X.lm                  HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
## All.X.glm                 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
## All.X.bayesglm            HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
## All.X.no.rnorm.rpart              HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder
## All.X.no.rnorm.rf                 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder
## Csm.1.lm                                                                                    Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area
## Csm.2.lm                                                                                                              Population, Murder, HS.Grad, Frost
##                           max.nTuningRuns min.elapsedtime.everything
## MFO.lm                                  0                      0.649
## Max.cor.Y.cv.0.rpart                    0                      0.549
## Max.cor.Y.cv.0.cp.0.rpart               0                      0.437
## Max.cor.Y.rpart                         3                      0.924
## Max.cor.Y.lm                            1                      1.023
## Interact.High.cor.Y.lm                  1                      0.805
## Low.cor.X.lm                            1                      0.866
## All.X.lm                                1                      0.841
## All.X.glm                               1                      0.865
## All.X.bayesglm                          1                      1.976
## All.X.no.rnorm.rpart                    3                      0.921
## All.X.no.rnorm.rf                       3                      1.782
## Csm.1.lm                                1                      0.845
## Csm.2.lm                                1                      0.808
##                           min.elapsedtime.final max.R.sq.fit min.RMSE.fit
## MFO.lm                                    0.003   0.03526376    1.2741594
## Max.cor.Y.cv.0.rpart                      0.007   0.00000000    1.2972374
## Max.cor.Y.cv.0.cp.0.rpart                 0.006   0.66474630    0.7511147
## Max.cor.Y.rpart                           0.007   0.00000000    1.1382505
## Max.cor.Y.lm                              0.002   0.70582695    0.8349469
## Interact.High.cor.Y.lm                    0.003   0.71871361    0.8694013
## Low.cor.X.lm                              0.006   0.89680550    1.3102855
## All.X.lm                                  0.010   0.90517649    1.8350704
## All.X.glm                                 0.014   0.90517649    1.8350704
## All.X.bayesglm                            0.051   0.90511753    1.6731644
## All.X.no.rnorm.rpart                      0.013   0.00000000    1.1556929
## All.X.no.rnorm.rf                         0.024   0.86486236    0.7878053
## Csm.1.lm                                  0.004   0.79579730    1.0702772
## Csm.2.lm                                  0.003   0.76808360    0.8483694
##                           max.R.sq.OOB min.RMSE.OOB max.Adj.R.sq.fit
## MFO.lm                     -0.16382466    1.5127250      0.006029329
## Max.cor.Y.cv.0.rpart        0.00000000    1.4022202               NA
## Max.cor.Y.cv.0.cp.0.rpart   0.47645818    1.0145931               NA
## Max.cor.Y.rpart             0.00000000    1.4022202               NA
## Max.cor.Y.lm                0.49196697    0.9994526      0.687441138
## Interact.High.cor.Y.lm      0.49204804    0.9993728      0.681208758
## Low.cor.X.lm               -0.43507375    1.6797836      0.815336155
## All.X.lm                   -0.49396649    1.7139046      0.785066700
## All.X.glm                  -0.49396649    1.7139046               NA
## All.X.bayesglm             -0.48512097    1.7088547               NA
## All.X.no.rnorm.rpart        0.00000000    1.4022202               NA
## All.X.no.rnorm.rf           0.37244959    1.1103073               NA
## Csm.1.lm                    0.09058028    1.3372063      0.742855858
## Csm.2.lm                    0.61862592    0.8659480      0.737161419
##                           max.Rsquared.fit min.RMSESD.fit
## MFO.lm                                  NA             NA
## Max.cor.Y.cv.0.rpart                    NA             NA
## Max.cor.Y.cv.0.cp.0.rpart               NA             NA
## Max.cor.Y.rpart                  0.3110708      0.1557052
## Max.cor.Y.lm                     0.6946483      0.1059423
## Interact.High.cor.Y.lm           0.6441580      0.1370642
## Low.cor.X.lm                     0.4598377      0.4605754
## All.X.lm                         0.3370194      0.5357182
## All.X.glm                        0.3370194      0.5357182
## All.X.bayesglm                   0.3637272      0.3906281
## All.X.no.rnorm.rpart             0.3011671      0.1859150
## All.X.no.rnorm.rf                0.6989583      0.2050785
## Csm.1.lm                         0.5047781      0.2228150
## Csm.2.lm                         0.6383817      0.1624027
##                           max.RsquaredSD.fit min.aic.fit
## MFO.lm                                    NA          NA
## Max.cor.Y.cv.0.rpart                      NA          NA
## Max.cor.Y.cv.0.cp.0.rpart                 NA          NA
## Max.cor.Y.rpart                   0.14180475          NA
## Max.cor.Y.lm                      0.12444542          NA
## Interact.High.cor.Y.lm            0.17140770          NA
## Low.cor.X.lm                      0.11586579          NA
## All.X.lm                          0.10749559          NA
## All.X.glm                         0.10749559    77.09146
## All.X.bayesglm                    0.08865780    83.11321
## All.X.no.rnorm.rpart              0.15791358          NA
## All.X.no.rnorm.rf                 0.17607120          NA
## Csm.1.lm                          0.06898573          NA
## Csm.2.lm                          0.04550407          NA
```

```r
rm(ret_lst)
fit.models_1_chunk_df <- myadd_chunk(fit.models_1_chunk_df, "fit.models_1_end", 
                                     major.inc=TRUE)
```

```
##              label step_major step_minor    bgn    end elapsed
## 6  fit.models_1_rf          6          0 43.934 52.044   8.111
## 7 fit.models_1_end          7          0 52.045     NA      NA
```

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn   end elapsed
## 11 fit.models          7          1 31.075 52.05  20.975
## 12 fit.models          7          2 52.051    NA      NA
```


```r
if (!is.null(glb_model_metric_smmry)) {
    stats_df <- glb_models_df[, "model_id", FALSE]

    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_fitobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "fit",
        						glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    stats_mdl_df <- data.frame()
    for (model_id in stats_df$model_id) {
        stats_mdl_df <- rbind(stats_mdl_df, 
            mypredict_mdl(glb_models_lst[[model_id]], glb_OOBobs_df, glb_rsp_var, 
                          glb_rsp_var_out, model_id, "OOB",
            					glb_model_metric_smmry, glb_model_metric, 
        						glb_model_metric_maximize, ret_type="stats"))
    }
    stats_df <- merge(stats_df, stats_mdl_df, all.x=TRUE)
    
    print("Merging following data into glb_models_df:")
    print(stats_mrg_df <- stats_df[, c(1, grep(glb_model_metric, names(stats_df)))])
    print(tmp_models_df <- orderBy(~model_id, glb_models_df[, c("model_id",
                                    grep(glb_model_metric, names(stats_df), value=TRUE))]))

    tmp2_models_df <- glb_models_df[, c("model_id", setdiff(names(glb_models_df),
                                    grep(glb_model_metric, names(stats_df), value=TRUE)))]
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
##                                            model_id model_method
## MFO.lm                                       MFO.lm           lm
## Max.cor.Y.cv.0.rpart           Max.cor.Y.cv.0.rpart        rpart
## Max.cor.Y.cv.0.cp.0.rpart Max.cor.Y.cv.0.cp.0.rpart        rpart
## Max.cor.Y.rpart                     Max.cor.Y.rpart        rpart
## Max.cor.Y.lm                           Max.cor.Y.lm           lm
## Interact.High.cor.Y.lm       Interact.High.cor.Y.lm           lm
## Low.cor.X.lm                           Low.cor.X.lm           lm
## All.X.lm                                   All.X.lm           lm
## All.X.glm                                 All.X.glm          glm
## All.X.bayesglm                       All.X.bayesglm     bayesglm
## All.X.no.rnorm.rpart           All.X.no.rnorm.rpart        rpart
## All.X.no.rnorm.rf                 All.X.no.rnorm.rf           rf
## Csm.1.lm                                   Csm.1.lm           lm
## Csm.2.lm                                   Csm.2.lm           lm
##                                                                                                                                                    feats
## MFO.lm                                                                                                                                            .rnorm
## Max.cor.Y.cv.0.rpart                                                                                                                      Murder, Income
## Max.cor.Y.cv.0.cp.0.rpart                                                                                                                 Murder, Income
## Max.cor.Y.rpart                                                                                                                           Murder, Income
## Max.cor.Y.lm                                                                                                                              Murder, Income
## Interact.High.cor.Y.lm                                                                  Murder, Income, Murder:Illiteracy, Murder:HS.Grad, Murder:Murder
## Low.cor.X.lm                                                                Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Murder
## All.X.lm                  HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
## All.X.glm                 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
## All.X.bayesglm            HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, .rnorm, Population, x, Illiteracy, Murder
## All.X.no.rnorm.rpart              HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder
## All.X.no.rnorm.rf                 HS.Grad, y, Frost, state.region.fctr, Income, Area, state.area, state.division.fctr, Population, x, Illiteracy, Murder
## Csm.1.lm                                                                                    Population, Income, Illiteracy, Murder, HS.Grad, Frost, Area
## Csm.2.lm                                                                                                              Population, Murder, HS.Grad, Frost
##                           max.nTuningRuns max.R.sq.fit max.R.sq.OOB
## MFO.lm                                  0   0.03526376  -0.16382466
## Max.cor.Y.cv.0.rpart                    0   0.00000000   0.00000000
## Max.cor.Y.cv.0.cp.0.rpart               0   0.66474630   0.47645818
## Max.cor.Y.rpart                         3   0.00000000   0.00000000
## Max.cor.Y.lm                            1   0.70582695   0.49196697
## Interact.High.cor.Y.lm                  1   0.71871361   0.49204804
## Low.cor.X.lm                            1   0.89680550  -0.43507375
## All.X.lm                                1   0.90517649  -0.49396649
## All.X.glm                               1   0.90517649  -0.49396649
## All.X.bayesglm                          1   0.90511753  -0.48512097
## All.X.no.rnorm.rpart                    3   0.00000000   0.00000000
## All.X.no.rnorm.rf                       3   0.86486236   0.37244959
## Csm.1.lm                                1   0.79579730   0.09058028
## Csm.2.lm                                1   0.76808360   0.61862592
##                           max.Adj.R.sq.fit max.Rsquared.fit
## MFO.lm                         0.006029329               NA
## Max.cor.Y.cv.0.rpart                    NA               NA
## Max.cor.Y.cv.0.cp.0.rpart               NA               NA
## Max.cor.Y.rpart                         NA        0.3110708
## Max.cor.Y.lm                   0.687441138        0.6946483
## Interact.High.cor.Y.lm         0.681208758        0.6441580
## Low.cor.X.lm                   0.815336155        0.4598377
## All.X.lm                       0.785066700        0.3370194
## All.X.glm                               NA        0.3370194
## All.X.bayesglm                          NA        0.3637272
## All.X.no.rnorm.rpart                    NA        0.3011671
## All.X.no.rnorm.rf                       NA        0.6989583
## Csm.1.lm                       0.742855858        0.5047781
## Csm.2.lm                       0.737161419        0.6383817
##                           inv.elapsedtime.everything inv.elapsedtime.final
## MFO.lm                                     1.5408320             333.33333
## Max.cor.Y.cv.0.rpart                       1.8214936             142.85714
## Max.cor.Y.cv.0.cp.0.rpart                  2.2883295             166.66667
## Max.cor.Y.rpart                            1.0822511             142.85714
## Max.cor.Y.lm                               0.9775171             500.00000
## Interact.High.cor.Y.lm                     1.2422360             333.33333
## Low.cor.X.lm                               1.1547344             166.66667
## All.X.lm                                   1.1890606             100.00000
## All.X.glm                                  1.1560694              71.42857
## All.X.bayesglm                             0.5060729              19.60784
## All.X.no.rnorm.rpart                       1.0857763              76.92308
## All.X.no.rnorm.rf                          0.5611672              41.66667
## Csm.1.lm                                   1.1834320             250.00000
## Csm.2.lm                                   1.2376238             333.33333
##                           inv.RMSE.fit inv.RMSE.OOB inv.aic.fit
## MFO.lm                       0.7848312    0.6610587          NA
## Max.cor.Y.cv.0.rpart         0.7708689    0.7131548          NA
## Max.cor.Y.cv.0.cp.0.rpart    1.3313546    0.9856168          NA
## Max.cor.Y.rpart              0.8785412    0.7131548          NA
## Max.cor.Y.lm                 1.1976809    1.0005477          NA
## Interact.High.cor.Y.lm       1.1502168    1.0006276          NA
## Low.cor.X.lm                 0.7631924    0.5953148          NA
## All.X.lm                     0.5449382    0.5834630          NA
## All.X.glm                    0.5449382    0.5834630  0.01297161
## All.X.bayesglm               0.5976699    0.5851873  0.01203178
## All.X.no.rnorm.rpart         0.8652818    0.7131548          NA
## All.X.no.rnorm.rf            1.2693492    0.9006516          NA
## Csm.1.lm                     0.9343374    0.7478278          NA
## Csm.2.lm                     1.1787319    1.1548038          NA
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
## 14. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 88 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 22 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually. if you must have them.
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-1.png) 

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
            mltd_models_df[row_ix, "variable"]), 
            paste0(".", mltd_models_df[row_ix, "data"]))), 1))
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
            mltdCI_models_df[row_ix, "data"] <- 
                unlist(strsplit(var_components[2], "[.]"))[2]
            mltdCI_models_df[row_ix, "type"] <- type
            break
        }
    }    
}
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
        if (var_type %in% unique(paste(mltd_models_df$label, mltd_models_df$data,
                                       sep=".")))
            next
        #print(sprintf("var_type:%s", var_type))
        goback_vars <- c(goback_vars, var_type)
    }
}

if (length(goback_vars) > 0) {
    mltd_goback_df <- build_statsCI_data(glb_models_df[, c("model_id", goback_vars)])
    mltd_models_df <- rbind(mltd_models_df, mltd_goback_df)
}

mltd_models_df <- merge(mltd_models_df, glb_models_df[, c("model_id", "model_method")], 
                        all.x=TRUE)

png(paste0(glb_out_pfx, "models_bar.png"), width=480*3, height=480*2)
print(gp <- myplot_bar(mltd_models_df, "model_id", "value", colorcol_name="model_method") + 
        geom_errorbar(data=mrgdCI_models_df, 
            mapping=aes(x=model_id, ymax=value.Upper, ymin=value.Lower), width=0.5) + 
          facet_grid(label ~ data, scales="free") + 
          theme(axis.text.x = element_text(angle = 90,vjust = 0.5)))
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

```r
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
print(gp)
```

```
## Warning in loop_apply(n, do.ply): Removed 3 rows containing missing values
## (position_stack).
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-2.png) 

```r
# used for console inspection
model_evl_terms <- c(NULL)
for (metric in glb_model_evl_criteria)
    model_evl_terms <- c(model_evl_terms, 
                         ifelse(length(grep("max", metric)) > 0, "-", "+"), metric)
if (glb_is_classification && glb_is_binomial)
    model_evl_terms <- c(model_evl_terms, "-", "opt.prob.threshold.OOB")
model_sel_frmla <- as.formula(paste(c("~ ", model_evl_terms), collapse=" "))
dsp_models_cols <- c("model_id", glb_model_evl_criteria) 
if (glb_is_classification && glb_is_binomial) 
    dsp_models_cols <- c(dsp_models_cols, "opt.prob.threshold.OOB")
print(dsp_models_df <- orderBy(model_sel_frmla, glb_models_df)[, dsp_models_cols])
```

```
##                     model_id min.RMSE.OOB max.R.sq.OOB max.Adj.R.sq.fit
## 14                  Csm.2.lm    0.8659480   0.61862592      0.737161419
## 6     Interact.High.cor.Y.lm    0.9993728   0.49204804      0.681208758
## 5               Max.cor.Y.lm    0.9994526   0.49196697      0.687441138
## 3  Max.cor.Y.cv.0.cp.0.rpart    1.0145931   0.47645818               NA
## 12         All.X.no.rnorm.rf    1.1103073   0.37244959               NA
## 13                  Csm.1.lm    1.3372063   0.09058028      0.742855858
## 2       Max.cor.Y.cv.0.rpart    1.4022202   0.00000000               NA
## 4            Max.cor.Y.rpart    1.4022202   0.00000000               NA
## 11      All.X.no.rnorm.rpart    1.4022202   0.00000000               NA
## 1                     MFO.lm    1.5127250  -0.16382466      0.006029329
## 7               Low.cor.X.lm    1.6797836  -0.43507375      0.815336155
## 10            All.X.bayesglm    1.7088547  -0.48512097               NA
## 8                   All.X.lm    1.7139046  -0.49396649      0.785066700
## 9                  All.X.glm    1.7139046  -0.49396649               NA
```

```r
print(myplot_radar(radar_inp_df=dsp_models_df))
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually. if you must have them.
```

```
## Warning in loop_apply(n, do.ply): Removed 4 rows containing missing values
## (geom_path).
```

```
## Warning in loop_apply(n, do.ply): Removed 33 rows containing missing values
## (geom_point).
```

```
## Warning in loop_apply(n, do.ply): Removed 7 rows containing missing values
## (geom_text).
```

```
## Warning in RColorBrewer::brewer.pal(n, pal): n too large, allowed maximum for palette Set1 is 9
## Returning the palette you asked for with that many colors
```

```
## Warning: The shape palette can deal with a maximum of 6 discrete values
## because more than 6 becomes difficult to discriminate; you have
## 14. Consider specifying shapes manually. if you must have them.
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-3.png) 

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
print(sprintf("Best model id: %s", dsp_models_df[1, "model_id"]))
```

```
## [1] "Best model id: Csm.2.lm"
```

```r
if (is.null(glb_sel_mdl_id)) { 
    glb_sel_mdl_id <- dsp_models_df[1, "model_id"]
    if (glb_sel_mdl_id == "Interact.High.cor.Y.glm") {
        warning("glb_sel_mdl_id: Interact.High.cor.Y.glm; myextract_mdl_feats does not currently support interaction terms")
        glb_sel_mdl_id <- dsp_models_df[2, "model_id"]
    }
} else 
    print(sprintf("User specified selection: %s", glb_sel_mdl_id))   
    
myprint_mdl(glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]])
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-4.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-5.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-6.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-7.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4481 -0.4838  0.1375  0.4930  0.8952 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.977e+01  1.139e+00  61.245  < 2e-16 ***
## Population   5.537e-05  3.712e-05   1.492  0.14619    
## Murder      -2.501e-01  4.308e-02  -5.806  2.4e-06 ***
## HS.Grad      5.710e-02  1.818e-02   3.140  0.00378 ** 
## Frost       -2.704e-03  2.988e-03  -0.905  0.37282    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6748 on 30 degrees of freedom
## Multiple R-squared:  0.7681,	Adjusted R-squared:  0.7372 
## F-statistic: 24.84 on 4 and 30 DF,  p-value: 3.781e-09
```

```
## [1] TRUE
```

```r
# From here to save(), this should all be in one function
#   these are executed in the same seq twice more:
#       fit.data.training & predict.data.new chunks
glb_get_predictions <- function(df, mdl_id, rsp_var_out, prob_threshold_def=NULL) {
    mdl <- glb_models_lst[[mdl_id]]
    rsp_var_out <- paste0(rsp_var_out, mdl_id)

    if (glb_is_regression) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        print(myplot_scatter(df, glb_rsp_var, rsp_var_out, smooth=TRUE))
        df[, paste0(rsp_var_out, ".err")] <- 
            abs(df[, rsp_var_out] - df[, glb_rsp_var])
        print(head(orderBy(reformulate(c("-", paste0(rsp_var_out, ".err"))), 
                           df)))                             
    }

    if (glb_is_classification && glb_is_binomial) {
        prob_threshold <- glb_models_df[glb_models_df$model_id == mdl_id, 
                                        "opt.prob.threshold.OOB"]
        if (is.null(prob_threshold) || is.na(prob_threshold)) {
            warning("Using default probability threshold: ", prob_threshold_def)
            if (is.null(prob_threshold <- prob_threshold_def))
                stop("Default probability threshold is NULL")
        }
        
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")[, 2]
        df[, rsp_var_out] <- 
        		factor(levels(df[, glb_rsp_var])[
    				(df[, paste0(rsp_var_out, ".prob")] >=
    					prob_threshold) * 1 + 1], levels(df[, glb_rsp_var]))
    
        # prediction stats already reported by myfit_mdl ???
    }    
    
    if (glb_is_classification && !glb_is_binomial) {
        df[, rsp_var_out] <- predict(mdl, newdata=df, type="raw")
        df[, paste0(rsp_var_out, ".prob")] <- 
            predict(mdl, newdata=df, type="prob")
    }

    return(df)
}    
glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out)
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-8.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 4        2110   3378        1.9    70.66   10.1    39.9    65  51945
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
##    state.abb state.area         x       y     state.division   state.name
## 11        HI       6450 -126.2500 31.7500            Pacific       Hawaii
## 4         AR      53104  -92.2992 34.7336 West South Central     Arkansas
## 2         AK     589757 -127.2500 49.2500            Pacific       Alaska
## 8         DE       2057  -74.9841 38.6777     South Atlantic     Delaware
## 34        ND      70665 -100.0990 47.2517 West North Central North Dakota
## 26        MT     147138 -109.3200 46.8230           Mountain      Montana
##     state.region .src     .rnorm state.division.fctr state.region.fctr
## 11          West Test -0.3059627             Pacific              West
## 4          South Test  0.4264642  West South Central             South
## 2           West Test -1.1381369             Pacific              West
## 8          South Test  0.6886403      South Atlantic             South
## 34 North Central Test  1.5164706  West North Central     North Central
## 26          West Test  1.3686023            Mountain              West
##    Life.Exp.predict.Csm.2.lm Life.Exp.predict.Csm.2.lm.err
## 11                  71.79852                     1.8014793
## 4                   69.45995                     1.2000501
## 2                   70.35815                     1.0481505
## 8                   71.08724                     1.0272417
## 34                  71.82113                     0.9588669
## 26                  71.51869                     0.9586932
```

```r
predct_accurate_var_name <- paste0(glb_rsp_var_out, glb_sel_mdl_id, ".accurate")
glb_OOBobs_df[, predct_accurate_var_name] <-
                    (glb_OOBobs_df[, glb_rsp_var] == 
                     glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)])

#stop(here"); #sav_models_lst <- glb_models_lst; sav_models_df <- glb_models_df
glb_featsimp_df <- 
    myget_feats_importance(mdl=glb_sel_mdl, featsimp_df=NULL)
glb_featsimp_df[, paste0(glb_sel_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##            importance Csm.2.lm.importance
## Murder      100.00000           100.00000
## HS.Grad      45.60984            45.60984
## Population   11.97951            11.97951
## Frost         0.00000             0.00000
```

```r
# Used again in fit.data.training & predict.data.new chunks
glb_analytics_diag_plots <- function(obs_df, mdl_id, prob_threshold=NULL) {
    featsimp_df <- glb_featsimp_df
    featsimp_df$feat <- gsub("`(.*?)`", "\\1", row.names(featsimp_df))    
    featsimp_df$feat.interact <- gsub("(.*?):(.*)", "\\2", featsimp_df$feat)
    featsimp_df$feat <- gsub("(.*?):(.*)", "\\1", featsimp_df$feat)    
    featsimp_df$feat.interact <- ifelse(featsimp_df$feat.interact == featsimp_df$feat, 
                                        NA, featsimp_df$feat.interact)
    featsimp_df$feat <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat)
    featsimp_df$feat.interact <- gsub("(.*?)\\.fctr(.*)", "\\1\\.fctr", featsimp_df$feat.interact) 
    featsimp_df <- orderBy(~ -importance.max, summaryBy(importance ~ feat + feat.interact, 
                                                        data=featsimp_df, FUN=max))    
    #rex_str=":(.*)"; txt_vctr=tail(featsimp_df$feat); ret_lst <- regexec(rex_str, txt_vctr); ret_lst <- regmatches(txt_vctr, ret_lst); ret_vctr <- sapply(1:length(ret_lst), function(pos_ix) ifelse(length(ret_lst[[pos_ix]]) > 0, ret_lst[[pos_ix]], "")); print(ret_vctr <- ret_vctr[ret_vctr != ""])    
    if (nrow(featsimp_df) > 5) {
        warning("Limiting important feature scatter plots to 5 out of ", nrow(featsimp_df))
        featsimp_df <- head(featsimp_df, 5)
    }
    
#     if (!all(is.na(featsimp_df$feat.interact)))
#         stop("not implemented yet")
    rsp_var_out <- paste0(glb_rsp_var_out, mdl_id)
    for (var in featsimp_df$feat) {
        plot_df <- melt(obs_df, id.vars=var, 
                        measure.vars=c(glb_rsp_var, rsp_var_out))

#         if (var == "<feat_name>") print(myplot_scatter(plot_df, var, "value", 
#                                              facet_colcol_name="variable") + 
#                       geom_vline(xintercept=<divider_val>, linetype="dotted")) else     
            print(myplot_scatter(plot_df, var, "value", colorcol_name="variable",
                                 facet_colcol_name="variable", jitter=TRUE) + 
                      guides(color=FALSE))
    }
    
    if (glb_is_regression) {
        if (nrow(featsimp_df) == 0)
            warning("No important features in glb_fin_mdl") else
            print(myplot_prediction_regression(df=obs_df, 
                        feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2],
                                      ".rownames"), 
                                               feat_y=featsimp_df$feat[1],
                        rsp_var=glb_rsp_var, rsp_var_out=rsp_var_out,
                        id_vars=glb_id_var)
    #               + facet_wrap(reformulate(featsimp_df$feat[2])) # if [1 or 2] is a factor
    #               + geom_point(aes_string(color="<col_name>.fctr")) #  to color the plot
                  )
    }    
    
    if (glb_is_classification) {
        if (nrow(featsimp_df) == 0)
            warning("No features in selected model are statistically important")
        else print(myplot_prediction_classification(df=obs_df, 
                feat_x=ifelse(nrow(featsimp_df) > 1, featsimp_df$feat[2], 
                              ".rownames"),
                                               feat_y=featsimp_df$feat[1],
                     rsp_var=glb_rsp_var, 
                     rsp_var_out=rsp_var_out, 
                     id_vars=glb_id_var,
                    prob_threshold=prob_threshold)
#               + geom_hline(yintercept=<divider_val>, linetype = "dotted")
                )
    }    
}
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_OOBobs_df, mdl_id=glb_sel_mdl_id)                  
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-9.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-10.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-11.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-12.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 4        2110   3378        1.9    70.66   10.1    39.9    65  51945
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
##    state.abb state.area         x       y     state.division   state.name
## 11        HI       6450 -126.2500 31.7500            Pacific       Hawaii
## 4         AR      53104  -92.2992 34.7336 West South Central     Arkansas
## 2         AK     589757 -127.2500 49.2500            Pacific       Alaska
## 8         DE       2057  -74.9841 38.6777     South Atlantic     Delaware
## 34        ND      70665 -100.0990 47.2517 West North Central North Dakota
##     state.region .src     .rnorm state.division.fctr state.region.fctr
## 11          West Test -0.3059627             Pacific              West
## 4          South Test  0.4264642  West South Central             South
## 2           West Test -1.1381369             Pacific              West
## 8          South Test  0.6886403      South Atlantic             South
## 34 North Central Test  1.5164706  West North Central     North Central
##    Life.Exp.predict.Csm.2.lm Life.Exp.predict.Csm.2.lm.err
## 11                  71.79852                     1.8014793
## 4                   69.45995                     1.2000501
## 2                   70.35815                     1.0481505
## 8                   71.08724                     1.0272417
## 34                  71.82113                     0.9588669
##    Life.Exp.predict.Csm.2.lm.accurate .label
## 11                              FALSE     HI
## 4                               FALSE     AR
## 2                               FALSE     AK
## 8                               FALSE     DE
## 34                              FALSE     ND
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_2-13.png) 

```r
# gather predictions from models better than MFO.*
#mdl_id <- "Conditional.X.rf"
#mdl_id <- "Conditional.X.cp.0.rpart"
#mdl_id <- "Conditional.X.rpart"
# glb_OOBobs_df <- glb_get_predictions(df=glb_OOBobs_df, mdl_id,
#                                      glb_rsp_var_out)
# print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, mdl_id)], 
#                         glb_OOBobs_df[, glb_rsp_var])$table))
# FN_OOB_ids <- c(4721, 4020, 693, 92)
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_feats_df$id[1:5]])
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
write.csv(glb_OOBobs_df[, c(glb_id_var, 
                grep(glb_rsp_var, names(glb_OOBobs_df), fixed=TRUE, value=TRUE))], 
    paste0(gsub(".", "_", paste0(glb_out_pfx, glb_sel_mdl_id), fixed=TRUE), 
           "_OOBobs.csv"), row.names=FALSE)

# print(glb_allobs_df[glb_allobs_df$UniqueID %in% FN_OOB_ids, 
#                     glb_txt_vars])
# dsp_tbl(Headline.contains="[Ee]bola")
# sum(sel_obs(Headline.contains="[Ee]bola"))
# ftable(xtabs(Popular ~ NewsDesk.fctr, data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,]))
# xtabs(NewsDesk ~ Popular, #Popular ~ NewsDesk.fctr, 
#       data=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,],
#       exclude=NULL)
# print(mycreate_xtab_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular", "NewsDesk", "SectionName", "SubsectionName")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], c("Popular")))
# print(mycreate_tbl_df(df=glb_allobs_df[sel_obs(Headline.contains="[Ee]bola") ,], 
#                       tbl_col_names=c("Popular", "NewsDesk")))

# write.csv(glb_chunks_df, paste0(glb_out_pfx, tail(glb_chunks_df, 1)$label, "_",
#                                 tail(glb_chunks_df, 1)$step_minor,  "_chunks1.csv"),
#           row.names=FALSE)

glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.models", major.inc=FALSE)
```

```
##         label step_major step_minor    bgn    end elapsed
## 12 fit.models          7          2 52.051 62.656  10.606
## 13 fit.models          7          3 62.657     NA      NA
```


```r
print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## [1] "Life.Exp.predict.Csm.2.lm"         
## [2] "Life.Exp.predict.Csm.2.lm.err"     
## [3] "Life.Exp.predict.Csm.2.lm.accurate"
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, 
         glb_allobs_df, #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_sel_mdl, glb_sel_mdl_id,
         glb_model_type,
        file=paste0(glb_out_pfx, "selmdl_dsk.RData"))
#load(paste0(glb_out_pfx, "selmdl_dsk.RData"))

rm(ret_lst)
```

```
## Warning in rm(ret_lst): object 'ret_lst' not found
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

![](USCensus1977_State_HW2_v2_files/figure-html/fit.models_3-1.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn  end elapsed
## 13        fit.models          7          3 62.657 66.2   3.543
## 14 fit.data.training          8          0 66.201   NA      NA
```

## Step `8.0: fit data training`

```r
#load(paste0(glb_inp_pfx, "dsk.RData"))

# To create specific models
# glb_fin_mdl_id <- NULL; glb_fin_mdl <- NULL; 
# glb_sel_mdl_id <- "Conditional.X.cp.0.rpart"; 
# glb_sel_mdl <- glb_models_lst[[glb_sel_mdl_id]]; print(glb_sel_mdl)
    
if (!is.null(glb_fin_mdl_id) && (glb_fin_mdl_id %in% names(glb_models_lst))) {
    warning("Final model same as user selected model")
    glb_fin_mdl <- glb_sel_mdl
} else {    
#     print(mdl_feats_df <- myextract_mdl_feats(sel_mdl=glb_sel_mdl, 
#                                               entity_df=glb_fitobs_df))
    
    if ((model_method <- glb_sel_mdl$method) == "custom")
        # get actual method from the model_id
        model_method <- tail(unlist(strsplit(glb_sel_mdl_id, "[.]")), 1)
        
    tune_finmdl_df <- NULL
    if (nrow(glb_sel_mdl$bestTune) > 0) {
        for (param in names(glb_sel_mdl$bestTune)) {
            #print(sprintf("param: %s", param))
            if (glb_sel_mdl$bestTune[1, param] != "none")
                tune_finmdl_df <- rbind(tune_finmdl_df, 
                    data.frame(parameter=param, 
                               min=glb_sel_mdl$bestTune[1, param], 
                               max=glb_sel_mdl$bestTune[1, param], 
                               by=1)) # by val does not matter
        }
    } 
    
    # Sync with parameters in mydsutils.R
    require(gdata)
    ret_lst <- myfit_mdl(model_id="Final", model_method=model_method,
        indep_vars_vctr=trim(unlist(strsplit(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id,
                                                    "feats"], "[,]"))), 
                         model_type=glb_model_type,
                            rsp_var=glb_rsp_var, rsp_var_out=glb_rsp_var_out, 
                            fit_df=glb_trnobs_df, OOB_df=NULL,
                            n_cv_folds=glb_n_cv_folds, tune_models_df=tune_finmdl_df,
                         # Automate from here
                         #  Issues if glb_sel_mdl$method == "rf" b/c trainControl is "oob"; not "cv"
                            model_loss_mtrx=glb_model_metric_terms,
                            model_summaryFunction=glb_sel_mdl$control$summaryFunction,
                            model_metric=glb_sel_mdl$metric,
                            model_metric_maximize=glb_sel_mdl$maximize)
    glb_fin_mdl <- glb_models_lst[[length(glb_models_lst)]] 
    glb_fin_mdl_id <- glb_models_df[length(glb_models_lst), "model_id"]
}
```

```
## Loading required package: gdata
## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.
## 
## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.
## 
## Attaching package: 'gdata'
## 
## The following object is masked from 'package:randomForest':
## 
##     combine
## 
## The following objects are masked from 'package:dplyr':
## 
##     combine, first, last
## 
## The following object is masked from 'package:stats':
## 
##     nobs
## 
## The following object is masked from 'package:utils':
## 
##     object.size
```

```
## [1] "fitting model: Final.lm"
## [1] "    indep_vars: Population, Murder, HS.Grad, Frost"
## Aggregating results
## Fitting final model on full training set
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_0-1.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_0-2.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_0-3.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_0-4.png) 

```
## 
## Call:
## lm(formula = .outcome ~ ., data = dat)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -1.4481 -0.4838  0.1375  0.4930  0.8952 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  6.977e+01  1.139e+00  61.245  < 2e-16 ***
## Population   5.537e-05  3.712e-05   1.492  0.14619    
## Murder      -2.501e-01  4.308e-02  -5.806  2.4e-06 ***
## HS.Grad      5.710e-02  1.818e-02   3.140  0.00378 ** 
## Frost       -2.704e-03  2.988e-03  -0.905  0.37282    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.6748 on 30 degrees of freedom
## Multiple R-squared:  0.7681,	Adjusted R-squared:  0.7372 
## F-statistic: 24.84 on 4 and 30 DF,  p-value: 3.781e-09
## 
## [1] "    calling mypredict_mdl for fit:"
##   model_id model_method                              feats max.nTuningRuns
## 1 Final.lm           lm Population, Murder, HS.Grad, Frost               1
##   min.elapsedtime.everything min.elapsedtime.final max.R.sq.fit
## 1                      0.854                 0.003    0.7680836
##   min.RMSE.fit max.Adj.R.sq.fit max.Rsquared.fit min.RMSESD.fit
## 1    0.8483694        0.7371614        0.6383817      0.1624027
##   max.RsquaredSD.fit
## 1         0.04550407
```

```r
rm(ret_lst)
glb_chunks_df <- myadd_chunk(glb_chunks_df, "fit.data.training", major.inc=FALSE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 14 fit.data.training          8          0 66.201 69.795   3.595
## 15 fit.data.training          8          1 69.796     NA      NA
```


```r
glb_trnobs_df <- glb_get_predictions(df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "opt.prob.threshold.OOB"], NULL))
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-1.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 28        590   5149        0.5    69.03   11.5    65.2   188 109889
## 40       2816   3635        2.3    67.96   11.6    37.8    65  30225
## 38      11860   4449        1.0    70.43    6.1    50.2   126  44966
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
## 17       3387   3712        1.6    70.10   10.6    38.5    95  39650
##    state.abb state.area         x       y     state.division
## 19        ME      33215  -68.9801 45.6226        New England
## 28        NV     110540 -116.8510 39.1063           Mountain
## 40        SC      31055  -80.5056 33.6190     South Atlantic
## 38        PA      45333  -77.4500 40.9069    Middle Atlantic
## 43        TX     267339  -98.7857 31.3897 West South Central
## 17        KY      40395  -84.7674 37.3915 East South Central
##        state.name state.region  .src      .rnorm state.division.fctr
## 19          Maine    Northeast Train  0.77996512         New England
## 28         Nevada         West Train  0.37963948            Mountain
## 40 South Carolina        South Train  0.05300423      South Atlantic
## 38   Pennsylvania    Northeast Train  0.30352864     Middle Atlantic
## 43          Texas        South Train -0.49103117  West South Central
## 17       Kentucky        South Train  1.20796200  East South Central
##    state.region.fctr Life.Exp.predict.Final.lm
## 19         Northeast                  71.83810
## 28              West                  70.13761
## 40             South                  69.00395
## 38         Northeast                  71.42348
## 43             South                  70.00476
## 17             South                  69.24455
##    Life.Exp.predict.Final.lm.err
## 19                     1.4480981
## 28                     1.1076092
## 40                     1.0439531
## 38                     0.9934843
## 43                     0.8952381
## 17                     0.8554471
```

```r
sav_featsimp_df <- glb_featsimp_df
#glb_feats_df <- sav_feats_df
# glb_feats_df <- mymerge_feats_importance(feats_df=glb_feats_df, sel_mdl=glb_fin_mdl, 
#                                                entity_df=glb_trnobs_df)
glb_featsimp_df <- myget_feats_importance(mdl=glb_fin_mdl, featsimp_df=glb_featsimp_df)
glb_featsimp_df[, paste0(glb_fin_mdl_id, ".importance")] <- glb_featsimp_df$importance
print(glb_featsimp_df)
```

```
##            Csm.2.lm.importance importance Final.lm.importance
## Murder               100.00000  100.00000           100.00000
## HS.Grad               45.60984   45.60984            45.60984
## Population            11.97951   11.97951            11.97951
## Frost                  0.00000    0.00000             0.00000
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_trnobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-2.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-3.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-4.png) ![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-5.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 19       1058   3694        0.7    70.39    2.7    54.7   161  30920
## 28        590   5149        0.5    69.03   11.5    65.2   188 109889
## 40       2816   3635        2.3    67.96   11.6    37.8    65  30225
## 38      11860   4449        1.0    70.43    6.1    50.2   126  44966
## 43      12237   4188        2.2    70.90   12.2    47.4    35 262134
##    state.abb state.area         x       y     state.division
## 19        ME      33215  -68.9801 45.6226        New England
## 28        NV     110540 -116.8510 39.1063           Mountain
## 40        SC      31055  -80.5056 33.6190     South Atlantic
## 38        PA      45333  -77.4500 40.9069    Middle Atlantic
## 43        TX     267339  -98.7857 31.3897 West South Central
##        state.name state.region  .src      .rnorm state.division.fctr
## 19          Maine    Northeast Train  0.77996512         New England
## 28         Nevada         West Train  0.37963948            Mountain
## 40 South Carolina        South Train  0.05300423      South Atlantic
## 38   Pennsylvania    Northeast Train  0.30352864     Middle Atlantic
## 43          Texas        South Train -0.49103117  West South Central
##    state.region.fctr Life.Exp.predict.Final.lm
## 19         Northeast                  71.83810
## 28              West                  70.13761
## 40             South                  69.00395
## 38         Northeast                  71.42348
## 43             South                  70.00476
##    Life.Exp.predict.Final.lm.err .label
## 19                     1.4480981     ME
## 28                     1.1076092     NV
## 40                     1.0439531     SC
## 38                     0.9934843     PA
## 43                     0.8952381     TX
```

![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-6.png) 

```r
dsp_feats_vctr <- c(NULL)
for(var in grep(".importance", names(glb_feats_df), fixed=TRUE, value=TRUE))
    dsp_feats_vctr <- union(dsp_feats_vctr, 
                            glb_feats_df[!is.na(glb_feats_df[, var]), "id"])

# print(glb_trnobs_df[glb_trnobs_df$UniqueID %in% FN_OOB_ids, 
#                     grep(glb_rsp_var, names(glb_trnobs_df), value=TRUE)])

print(setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
```

```
## [1] "Life.Exp.predict.Final.lm"     "Life.Exp.predict.Final.lm.err"
```

```r
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

print(setdiff(names(glb_fitobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
print(setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
print(setdiff(names(glb_newobs_df), names(glb_allobs_df)))
```

```
## character(0)
```

```r
if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "dsk.RData"))

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

![](USCensus1977_State_HW2_v2_files/figure-html/fit.data.training_1-7.png) 

```r
glb_chunks_df <- myadd_chunk(glb_chunks_df, "predict.data.new", major.inc=TRUE)
```

```
##                label step_major step_minor    bgn    end elapsed
## 15 fit.data.training          8          1 69.796 73.264   3.468
## 16  predict.data.new          9          0 73.264     NA      NA
```

## Step `9.0: predict data new`

```r
# Compute final model predictions
# sav_newobs_df <- glb_newobs_df
glb_newobs_df <- glb_get_predictions(glb_newobs_df, mdl_id=glb_fin_mdl_id, 
                                     rsp_var_out=glb_rsp_var_out,
    prob_threshold_def=ifelse(glb_is_classification && glb_is_binomial, 
        glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                      "opt.prob.threshold.OOB"], NULL))
```

![](USCensus1977_State_HW2_v2_files/figure-html/predict.data.new-1.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 4        2110   3378        1.9    70.66   10.1    39.9    65  51945
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
## 26        746   4347        0.6    70.56    5.0    59.2   155 145587
##    state.abb state.area         x       y     state.division   state.name
## 11        HI       6450 -126.2500 31.7500            Pacific       Hawaii
## 4         AR      53104  -92.2992 34.7336 West South Central     Arkansas
## 2         AK     589757 -127.2500 49.2500            Pacific       Alaska
## 8         DE       2057  -74.9841 38.6777     South Atlantic     Delaware
## 34        ND      70665 -100.0990 47.2517 West North Central North Dakota
## 26        MT     147138 -109.3200 46.8230           Mountain      Montana
##     state.region .src     .rnorm state.division.fctr state.region.fctr
## 11          West Test -0.3059627             Pacific              West
## 4          South Test  0.4264642  West South Central             South
## 2           West Test -1.1381369             Pacific              West
## 8          South Test  0.6886403      South Atlantic             South
## 34 North Central Test  1.5164706  West North Central     North Central
## 26          West Test  1.3686023            Mountain              West
##    Life.Exp.predict.Final.lm Life.Exp.predict.Final.lm.err
## 11                  71.79852                     1.8014793
## 4                   69.45995                     1.2000501
## 2                   70.35815                     1.0481505
## 8                   71.08724                     1.0272417
## 34                  71.82113                     0.9588669
## 26                  71.51869                     0.9586932
```

```r
if (glb_is_classification && glb_is_binomial)
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id, 
            prob_threshold=glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                                         "opt.prob.threshold.OOB"]) else
    glb_analytics_diag_plots(obs_df=glb_newobs_df, mdl_id=glb_fin_mdl_id)                  
```

![](USCensus1977_State_HW2_v2_files/figure-html/predict.data.new-2.png) ![](USCensus1977_State_HW2_v2_files/figure-html/predict.data.new-3.png) ![](USCensus1977_State_HW2_v2_files/figure-html/predict.data.new-4.png) ![](USCensus1977_State_HW2_v2_files/figure-html/predict.data.new-5.png) 

```
##    Population Income Illiteracy Life.Exp Murder HS.Grad Frost   Area
## 11        868   4963        1.9    73.60    6.2    61.9     0   6425
## 4        2110   3378        1.9    70.66   10.1    39.9    65  51945
## 2         365   6315        1.5    69.31   11.3    66.7   152 566432
## 8         579   4809        0.9    70.06    6.2    54.6   103   1982
## 34        637   5087        0.8    72.78    1.4    50.3   186  69273
##    state.abb state.area         x       y     state.division   state.name
## 11        HI       6450 -126.2500 31.7500            Pacific       Hawaii
## 4         AR      53104  -92.2992 34.7336 West South Central     Arkansas
## 2         AK     589757 -127.2500 49.2500            Pacific       Alaska
## 8         DE       2057  -74.9841 38.6777     South Atlantic     Delaware
## 34        ND      70665 -100.0990 47.2517 West North Central North Dakota
##     state.region .src     .rnorm state.division.fctr state.region.fctr
## 11          West Test -0.3059627             Pacific              West
## 4          South Test  0.4264642  West South Central             South
## 2           West Test -1.1381369             Pacific              West
## 8          South Test  0.6886403      South Atlantic             South
## 34 North Central Test  1.5164706  West North Central     North Central
##    Life.Exp.predict.Final.lm Life.Exp.predict.Final.lm.err .label
## 11                  71.79852                     1.8014793     HI
## 4                   69.45995                     1.2000501     AR
## 2                   70.35815                     1.0481505     AK
## 8                   71.08724                     1.0272417     DE
## 34                  71.82113                     0.9588669     ND
```

![](USCensus1977_State_HW2_v2_files/figure-html/predict.data.new-6.png) 

```r
if (glb_is_classification && glb_is_binomial) {
    submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id, ".prob"))]
    names(submit_df)[2] <- "Probability1"
#     submit_df <- glb_newobs_df[, c(paste0(glb_rsp_var_out, glb_fin_mdl_id)), FALSE]
#     names(submit_df)[1] <- "BDscience"
#     submit_df$BDscience <- as.numeric(submit_df$BDscience) - 1
#     #submit_df <-rbind(submit_df, data.frame(bdanalytics=c(" ")))
#     print("Submission Stats:")
#     print(table(submit_df$BDscience, useNA = "ifany"))
} else submit_df <- glb_newobs_df[, c(glb_id_var, 
                                   paste0(glb_rsp_var_out, glb_fin_mdl_id))]
submit_fname <- paste0(gsub(".", "_", paste0(glb_out_pfx, glb_fin_mdl_id), fixed=TRUE), 
                    "_submit.csv")
write.csv(submit_df, submit_fname, quote=FALSE, row.names=FALSE)
#cat(" ", "\n", file=submit_fn, append=TRUE)

# print(orderBy(~ -max.auc.OOB, glb_models_df[, c("model_id", 
#             "max.auc.OOB", "max.Accuracy.OOB")]))
if (glb_is_classification && glb_is_binomial)
    print(glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, 
                        "opt.prob.threshold.OOB"])
print(sprintf("glb_sel_mdl_id: %s", glb_sel_mdl_id))
```

```
## [1] "glb_sel_mdl_id: Csm.2.lm"
```

```r
print(sprintf("glb_fin_mdl_id: %s", glb_fin_mdl_id))
```

```
## [1] "glb_fin_mdl_id: Final.lm"
```

```r
print(dim(glb_fitobs_df))
```

```
## [1] 35 19
```

```r
print(dsp_models_df)
```

```
##                     model_id min.RMSE.OOB max.R.sq.OOB max.Adj.R.sq.fit
## 14                  Csm.2.lm    0.8659480   0.61862592      0.737161419
## 6     Interact.High.cor.Y.lm    0.9993728   0.49204804      0.681208758
## 5               Max.cor.Y.lm    0.9994526   0.49196697      0.687441138
## 3  Max.cor.Y.cv.0.cp.0.rpart    1.0145931   0.47645818               NA
## 12         All.X.no.rnorm.rf    1.1103073   0.37244959               NA
## 13                  Csm.1.lm    1.3372063   0.09058028      0.742855858
## 2       Max.cor.Y.cv.0.rpart    1.4022202   0.00000000               NA
## 4            Max.cor.Y.rpart    1.4022202   0.00000000               NA
## 11      All.X.no.rnorm.rpart    1.4022202   0.00000000               NA
## 1                     MFO.lm    1.5127250  -0.16382466      0.006029329
## 7               Low.cor.X.lm    1.6797836  -0.43507375      0.815336155
## 10            All.X.bayesglm    1.7088547  -0.48512097               NA
## 8                   All.X.lm    1.7139046  -0.49396649      0.785066700
## 9                  All.X.glm    1.7139046  -0.49396649               NA
```

```r
if (glb_is_regression) {
    print(sprintf("%s OOB RMSE: %0.4f", glb_sel_mdl_id,
                  glb_models_df[glb_models_df$model_id == glb_sel_mdl_id, "min.RMSE.OOB"]))

    if (!is.null(glb_category_vars)) {
        stop("not implemented yet")
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
            pred_stats_df <- 
                mypredict_mdl(mdl=glb_models_lst[[glb_fin_mdl_id]], 
                              df=glb_newobs_df, 
                              rsp_var=glb_rsp_var, 
                              rsp_var_out=glb_rsp_var_out, 
                              model_id_method=glb_fin_mdl_id, 
                              label="new",
						      model_summaryFunction=glb_sel_mdl$control$summaryFunction, 
						      model_metric=glb_sel_mdl$metric,
						      model_metric_maximize=glb_sel_mdl$maximize,
						      ret_type="stats")        
            print(sprintf("%s prediction stats for glb_newobs_df:", glb_fin_mdl_id))
            print(pred_stats_df)
    }    
}    
```

```
## [1] "Csm.2.lm OOB RMSE: 0.8659"
## [1] "Final.lm prediction stats for glb_newobs_df:"
##   model_id max.R.sq.new min.RMSE.new
## 1 Final.lm    0.6186259     0.865948
```

```r
if (glb_is_classification) {
    print(sprintf("%s OOB confusion matrix & accuracy: ", glb_sel_mdl_id))
    print(t(confusionMatrix(glb_OOBobs_df[, paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
                            glb_OOBobs_df[, glb_rsp_var])$table))

    if (!is.null(glb_category_vars)) {
        tmp_OOBobs_df <- glb_OOBobs_df[, c(glb_category_vars, predct_accurate_var_name)]
        names(tmp_OOBobs_df)[length(names(tmp_OOBobs_df))] <- "accurate.OOB"
        aOOB_ctgry_df <- mycreate_xtab_df(tmp_OOBobs_df, names(tmp_OOBobs_df)) 
        aOOB_ctgry_df[is.na(aOOB_ctgry_df)] <- 0
        aOOB_ctgry_df <- mutate(aOOB_ctgry_df, 
                                .n.OOB = accurate.OOB.FALSE + accurate.OOB.TRUE,
                                max.accuracy.OOB = accurate.OOB.TRUE / .n.OOB)
        #intersect(names(glb_ctgry_df), names(aOOB_ctgry_df))
        glb_ctgry_df <- merge(glb_ctgry_df, aOOB_ctgry_df, all=TRUE)
        print(orderBy(~-accurate.OOB.FALSE, glb_ctgry_df))
    }
    
    if ((glb_rsp_var %in% names(glb_newobs_df)) &&
        !(any(is.na(glb_newobs_df[, glb_rsp_var])))) {
        print(sprintf("%s new confusion matrix & accuracy: ", glb_fin_mdl_id))
        print(t(confusionMatrix(glb_newobs_df[, paste0(glb_rsp_var_out, glb_fin_mdl_id)], 
                                glb_newobs_df[, glb_rsp_var])$table))
    }    

}    

dsp_myCategory_conf_mtrx <- function(myCategory) {
    print(sprintf("%s OOB::myCategory=%s confusion matrix & accuracy: ", 
                  glb_sel_mdl_id, myCategory))
    print(t(confusionMatrix(
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                      paste0(glb_rsp_var_out, glb_sel_mdl_id)], 
        glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, glb_rsp_var])$table))
    print(sum(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, 
                            predct_accurate_var_name]) / 
         nrow(glb_OOBobs_df[glb_OOBobs_df$myCategory == myCategory, ]))
    err_ids <- glb_OOBobs_df[(glb_OOBobs_df$myCategory == myCategory) & 
                             (!glb_OOBobs_df[, predct_accurate_var_name]), glb_id_var]

    OOB_FNerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 1), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FN errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FNerr_df)))
    print(OOB_FNerr_df)

    OOB_FPerr_df <- glb_OOBobs_df[(glb_OOBobs_df$UniqueID %in% err_ids) & 
                               (glb_OOBobs_df$Popular == 0), 
                        c(
                            ".clusterid", 
                            "Popular", "Headline", "Snippet", "Abstract")]
    print(sprintf("%s OOB::myCategory=%s FP errors: %d", glb_sel_mdl_id, myCategory,
                  nrow(OOB_FPerr_df)))
    print(OOB_FPerr_df)
}
#dsp_myCategory_conf_mtrx(myCategory="OpEd#Opinion#")
#dsp_myCategory_conf_mtrx(myCategory="Business#Business Day#Dealbook")
#dsp_myCategory_conf_mtrx(myCategory="##")

# if (glb_is_classification) {
#     print("FN_OOB_ids:")
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
#     print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         glb_txt_vars])
#     print(dsp_vctr <- colSums(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                         setdiff(grep("[HSA].", names(glb_OOBobs_df), value=TRUE),
#                                 union(myfind_chr_cols_df(glb_OOBobs_df),
#                     grep(".fctr", names(glb_OOBobs_df), fixed=TRUE, value=TRUE)))]))
# }

dsp_hdlpfx_results <- function(hdlpfx) {
    print(hdlpfx)
    print(glb_OOBobs_df[glb_OOBobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        grep(glb_rsp_var, names(glb_newobs_df), value=TRUE)])
    print(dsp_vctr <- colSums(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        setdiff(grep("[HSA]\\.", names(glb_newobs_df), value=TRUE),
                                union(myfind_chr_cols_df(glb_newobs_df),
                    grep(".fctr", names(glb_newobs_df), fixed=TRUE, value=TRUE)))]))
    print(dsp_vctr <- dsp_vctr[dsp_vctr != 0])
    print(glb_newobs_df[glb_newobs_df$Headline.pfx %in% c(hdlpfx), 
                        union(names(dsp_vctr), myfind_chr_cols_df(glb_newobs_df))])
}
#dsp_hdlpfx_results(hdlpfx="Ask Well::")

# print("myMisc::|OpEd|blank|blank|1:")
# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% c(6446), 
#                     grep(glb_rsp_var, names(glb_OOBobs_df), value=TRUE)])

# print(glb_OOBobs_df[glb_OOBobs_df$UniqueID %in% FN_OOB_ids, 
#                     c("WordCount", "WordCount.log", "myMultimedia",
#                       "NewsDesk", "SectionName", "SubsectionName")])
# print(mycreate_sqlxtab_df(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), ], 
#                           c(glb_rsp_var, "myMultimedia")))
# dsp_chisq.test(Headline.contains="[Vi]deo")
# print(glb_allobs_df[sel_obs(Headline.contains="[Vv]ideo"), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline")])
# print(glb_allobs_df[sel_obs(Headline.contains="[Ee]bola", Popular=1), 
#                           c(glb_rsp_var, "Popular", "myMultimedia", "Headline",
#                             "NewsDesk", "SectionName", "SubsectionName")])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.ConditionalX.y & is.na(importance))[,
#     c("is.ConditionalX.y", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, !is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
# print(subset(glb_feats_df, is.na(importance))[,
#     c("zeroVar", "nzv", "myNearZV", 
#       grep("importance", names(glb_feats_df), fixed=TRUE, value=TRUE))])
print(orderBy(as.formula(paste0("~ -", glb_sel_mdl_id, ".importance")), glb_featsimp_df))
```

```
##            Csm.2.lm.importance importance Final.lm.importance
## Murder               100.00000  100.00000           100.00000
## HS.Grad               45.60984   45.60984            45.60984
## Population            11.97951   11.97951            11.97951
## Frost                  0.00000    0.00000             0.00000
```

```r
# players_df <- data.frame(id=c("Chavez", "Giambi", "Menechino", "Myers", "Pena"),
#                          OBP=c(0.338, 0.391, 0.369, 0.313, 0.361),
#                          SLG=c(0.540, 0.450, 0.374, 0.447, 0.500),
#                         cost=c(1400000, 1065000, 295000, 800000, 300000))
# players_df$RS.predict <- predict(glb_models_lst[[csm_mdl_id]], players_df)
# print(orderBy(~ -RS.predict, players_df))

if (length(diff <- setdiff(names(glb_trnobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
for (col in setdiff(names(glb_trnobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.src == "Train", col] <- glb_trnobs_df[, col]

if (length(diff <- setdiff(names(glb_fitobs_df), names(glb_allobs_df))) > 0)   
    print(diff)
if (length(diff <- setdiff(names(glb_OOBobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

for (col in setdiff(names(glb_OOBobs_df), names(glb_allobs_df)))
    # Merge or cbind ?
    glb_allobs_df[glb_allobs_df$.lcn == "OOB", col] <- glb_OOBobs_df[, col]
    
if (length(diff <- setdiff(names(glb_newobs_df), names(glb_allobs_df))) > 0)   
    print(diff)

if (glb_save_envir)
    save(glb_feats_df, glb_allobs_df, 
         #glb_trnobs_df, glb_fitobs_df, glb_OOBobs_df, glb_newobs_df,
         glb_models_df, dsp_models_df, glb_models_lst, glb_model_type,
         glb_sel_mdl, glb_sel_mdl_id,
         glb_fin_mdl, glb_fin_mdl_id,
        file=paste0(glb_out_pfx, "prdnew_dsk.RData"))

rm(submit_df, tmp_OOBobs_df)
```

```
## Warning in rm(submit_df, tmp_OOBobs_df): object 'tmp_OOBobs_df' not found
```

```r
# tmp_replay_lst <- replay.petrisim(pn=glb_analytics_pn, 
#     replay.trans=(glb_analytics_avl_objs <- c(glb_analytics_avl_objs, 
#         "data.new.prediction")), flip_coord=TRUE)
# print(ggplot.petrinet(tmp_replay_lst[["pn"]]) + coord_flip())

glb_chunks_df <- myadd_chunk(glb_chunks_df, "display.session.info", major.inc=TRUE)
```

```
##                   label step_major step_minor    bgn    end elapsed
## 16     predict.data.new          9          0 73.264 76.327   3.064
## 17 display.session.info         10          0 76.328     NA      NA
```

Null Hypothesis ($\sf{H_{0}}$): mpg is not impacted by am_fctr.  
The variance by am_fctr appears to be independent. 
#```{r q1, cache=FALSE}
# print(t.test(subset(cars_df, am_fctr == "automatic")$mpg, 
#              subset(cars_df, am_fctr == "manual")$mpg, 
#              var.equal=FALSE)$conf)
#```
We reject the null hypothesis i.e. we have evidence to conclude that am_fctr impacts mpg (95% confidence). Manual transmission is better for miles per gallon versus automatic transmission.


```
##                      label step_major step_minor    bgn    end elapsed
## 11              fit.models          7          1 31.075 52.050  20.975
## 10              fit.models          7          0 17.653 31.074  13.422
## 12              fit.models          7          2 52.051 62.656  10.606
## 2             inspect.data          2          0  9.366 13.968   4.602
## 14       fit.data.training          8          0 66.201 69.795   3.595
## 13              fit.models          7          3 62.657 66.200   3.543
## 15       fit.data.training          8          1 69.796 73.264   3.468
## 16        predict.data.new          9          0 73.264 76.327   3.064
## 3               scrub.data          2          1 13.968 15.244   1.276
## 5         extract.features          3          0 15.300 16.438   1.139
## 8          select.features          5          0 16.763 17.353   0.590
## 1              import.data          1          0  8.926  9.365   0.440
## 9  partition.data.training          6          0 17.354 17.652   0.299
## 6             cluster.data          4          0 16.439 16.707   0.268
## 4           transform.data          2          2 15.244 15.300   0.056
## 7      manage.missing.data          4          1 16.707 16.762   0.055
##    duration
## 11   20.975
## 10   13.421
## 12   10.605
## 2     4.602
## 14    3.594
## 13    3.543
## 15    3.468
## 16    3.063
## 3     1.276
## 5     1.138
## 8     0.590
## 1     0.439
## 9     0.298
## 6     0.268
## 4     0.056
## 7     0.055
## [1] "Total Elapsed Time: 76.327 secs"
```

![](USCensus1977_State_HW2_v2_files/figure-html/display.session.info-1.png) 

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## Running under: OS X 10.10.3 (Yosemite)
## 
## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
## 
## attached base packages:
## [1] grid      parallel  stats     graphics  grDevices utils     datasets 
## [8] methods   base     
## 
## other attached packages:
##  [1] gdata_2.16.1        randomForest_4.6-10 arm_1.8-5          
##  [4] lme4_1.1-7          Rcpp_0.11.6         Matrix_1.2-1       
##  [7] MASS_7.3-40         rpart.plot_1.5.2    rpart_4.1-9        
## [10] reshape2_1.4.1      dplyr_0.4.1         plyr_1.8.2         
## [13] caTools_1.17.1      doMC_1.3.3          iterators_1.0.7    
## [16] foreach_1.4.2       doBy_4.5-13         survival_2.38-1    
## [19] caret_6.0-47        ggplot2_1.0.1       lattice_0.20-31    
## 
## loaded via a namespace (and not attached):
##  [1] gtools_3.5.0        splines_3.2.0       colorspace_1.2-6   
##  [4] htmltools_0.2.6     yaml_2.1.13         mgcv_1.8-6         
##  [7] nloptr_1.0.4        DBI_0.3.1           RColorBrewer_1.1-2 
## [10] stringr_1.0.0       munsell_0.4.2       gtable_0.1.2       
## [13] codetools_0.2-11    coda_0.17-1         evaluate_0.7       
## [16] labeling_0.3        knitr_1.10.5        SparseM_1.6        
## [19] quantreg_5.11       pbkrtest_0.4-2      proto_0.3-10       
## [22] scales_0.2.4        formatR_1.2         BradleyTerry2_1.0-6
## [25] abind_1.4-3         digest_0.6.8        stringi_0.4-1      
## [28] brglm_0.5-9         tools_3.2.0         bitops_1.0-6       
## [31] magrittr_1.5        lazyeval_0.1.10     car_2.0-25         
## [34] assertthat_0.1      minqa_1.2.4         rmarkdown_0.6.1    
## [37] nnet_7.3-9          nlme_3.1-120        compiler_3.2.0
```
