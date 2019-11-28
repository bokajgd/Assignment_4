Assignment 4 - Heart rate, respiration and interpersonal coordination
=====================================================================

Physiological data (here heart rate \[variability\], and respiration)
are increasingly popular. Historically treated as pernicious noise to be
regressed out of neuro-imaging data, there is now increasing research on
how these signals tell us something important about cognition and beyond
being just a signal of cognitive processes also impact them in
interesting ways. Advanced sport science, and the quantified self
movement (closely followed by marketing and communication) have hailed
continuous physiological tracking as a powerful way to access and modify
attitudes, habits, and performance. Further, as team coordination (in
the military, in decision processes and organizational contexts) is more
and more in focus, research has attempted to measure how interpersonal
coordination between physiological systems might tell us something
important about e.g. emotional and cognitive coordination. See
references in the reading list for more on this.

In this assignment, you will learn to: - collect physiological data -
pre-process physiological data (and grow further your mad R skills) -
model the continuous interdependence between two signals (using a
multilevel model as proxy for a dynamical system approach) -
conservatively assess the presence of coordination between to signals in
a controlled context

This assignment has two parts. The first part familiarizes you with
heart rate, and respiration data and their preprocessing. The second
part explores how to analyze interpersonal coordination of these
signals.

These are the questions you need to be able to answer at the end of the
assignment (aka that you need to submit as part of the portfolio)

1.  How do you preprocess heart rate and respiration data? Describe the
    process. If any data needs to be excluded, list the excluded data
    and motivate the exclusion.

2.  Do you observe interpersonal coordination in heart rate and
    respiration? Describe your control baseline, the method used to
    quantify coordination, and the statistical models used to infer
    whether coordination was higher than in the baseline. Report the
    results of the models.

3.  Do you observe differences in coordination between conditions?
    Report the models and results.

4.  Is respiration coordination a likely driver of heart rate
    coordination? Describe how you would test for it. Bonus points if
    you actually run the tests and report methods and results.

N.B. to give you a bit more data I included data from previous years
(Study1, Study2 and Study 3). Note that synchronouns and turn-taking are
the same across both studies, but the third condition is different: in
the first year it was self-paced joint reading; in the second year it
was the tv-series conversation.

Let’s get started
-----------------

### Exploring physiological signals

-   Choose one pair (one pair, three conditions)
-   Load the logs
-   Produce a plot of the participants’ respiration signal and a
    different one of the participants’ HR signal. N.B: remember the
    slides: artifacts, downsampling, scaling. N.B. The
    gridExtra::grid.arrange() function allows you to display the plots
    side by side. E.g. grid.arrange(plot1, plot2, plot3, ncol=3). There
    are also smarter packages, like cowplot and ggpubr.
-   Can you eye-ball which condition if any displays more physiological
    coordination?

### First we read one data file and identify the procedure

-   Load the file
-   correctly identify all columns
-   plot the data
-   deal with the artifacts
-   downsample the dat
-   Add a column for study, group, trial and condition

``` r
# Load the libraries
library(pacman)
p_load(tidyverse, tidymodels, goeveg, DescTools, lmerTest,
       caret, knitr, groupdata2, jtools, ggstance)

# Load the file
data <- read.csv('~/Desktop/folders/R/Assignment4/data/Study1_G1_T1_Synchronous.csv')

# Plot
#Respiration 
p1 <- ggplot(data=data, aes(x=time)) +
  geom_line(aes(y= Resp1, color = "Resp1"))+
  geom_line(aes(y= Resp2, color = "Resp2"))+
  labs( x= "Time", y= "Resp")+
  theme_classic()
p1
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
#Heart Rate 
p2 <- ggplot(data=data, aes(x=time)) +
  geom_line(aes(y= HR1, color = "HR1"))+
  geom_line(aes(y= HR2, color = "HR2"))+
  labs( x= "Time", y= "Heart Rate")+
  theme_classic()
p2
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
## Remove outliers

### Tip, check the function below
removeOuts <- function(ts,threshold){
  ts[ts > (mean(ts,na.rm=T) +
             (threshold*sd(ts,na.rm=T))) | 
       ts < (mean(ts,na.rm=T) -
             (threshold*sd(ts,na.rm=T)))] = mean(ts,na.rm=T)
  return(ts)
}
threshold=2.5 # Default value at 2.5 sds from the mean

#adding columns with the outliers removed for Respiration 
data$RespR1_noOuts <- removeOuts(data$Resp1, threshold)
data$RespR2_noOuts <- removeOuts(data$Resp2, threshold)

#adding columns for the outliers removed for Heart Rate 
data$HR1R_noOuts <- removeOuts(data$HR1, threshold)
data$HR2R_noOuts <- removeOuts(data$HR2, threshold)

### Tip: if scale() gives some issues, try the one below
z_scale <- function(column){
  column_d <- (column - mean(column)) / sd(column)
}

## Scale
data$RespR1_scaled <- z_scale(data$RespR1_noOuts)
data$RespR2_scaled <- scale(data$RespR2_noOuts)

data$HR1R_scaled <- z_scale(data$HR1R_noOuts)
data$HR2R_scaled <- z_scale(data$HR2R_noOuts)


# Plot raw data againt those with the artiacts removed
#Here we are just plotting all the columns as individual y-axis on the same x-axis 
p3 <- ggplot(data=data, aes(x=time)) +
  geom_line(aes(y= Resp1, color = "Resp1"))+
  geom_line(aes(y= Resp2, color = "Resp2"))+
  geom_line(aes(y= RespR1_noOuts, color = "RespR1 - no outlier"))+
  geom_line(aes(y= RespR2_noOuts, color = "RespR2"))+
  labs( x= "Time", y= "Resp")
p3
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-3.png)

``` r
p4 <- ggplot(data=data, aes(x=time)) +
  geom_line(aes(y= HR1, color = "HR1"))+
  geom_line(aes(y= HR2, color = "HR2"))+
  geom_line(aes(y= HR1R_noOuts, color = "HR1R"))+
  geom_line(aes(y= HR2R_noOuts, color = "HR2R"))+
  labs( x= "Time", y= "Resp")
  p4
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-4.png)

``` r
# Plotting normal outlier removed data and scaled data

p5 <- ggplot(data=data, aes(x=time)) +
  geom_line(aes(y= RespR1_scaled, color = "RespR1S"))+
  geom_line(aes(y= RespR2_scaled, color = "RespR2S"))+
  geom_line(aes(y= RespR1_noOuts, color = "RespR1"))+
  geom_line(aes(y= RespR2_noOuts, color = "RespR2"))+
  labs( x= "Time", y= "Resp")
p5
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-5.png)

``` r
p6 <- ggplot(data=data, aes(x=time)) +
  geom_line(aes(y= HR1R_scaled, color = "HR1"))+
  geom_line(aes(y= HR2R_scaled, color = "HR2"))+
  geom_line(aes(y= HR1R_noOuts, color = "HR1R"))+
  geom_line(aes(y= HR2R_noOuts, color = "HR2R"))
  labs( x= "Time", y= "Resp")
```

    ## $x
    ## [1] "Time"
    ## 
    ## $y
    ## [1] "Resp"
    ## 
    ## attr(,"class")
    ## [1] "labels"

``` r
p6
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-6.png)

``` r
## Downsample
### This is tricky, so you can have a look at my code  (relying on Ludvig's groupdata2) if you get stuck
d1 = data %>%
 group(n = 100, method = 'greedy') %>%
 dplyr::summarise(
   time = mean(time,na.rm=T),
   HR1 = mean(HR1R_scaled,na.rm=T),
   HR2 = mean(HR2R_scaled,na.rm=T),
   Resp1 = mean(RespR1_scaled,na.rm=T),
   Resp2 = mean(RespR2_scaled,na.rm=T))

## Plot the downsampled data
p7 <- ggplot(data = data) +
  geom_path(aes(time, Resp1, color = "P1")) +
  geom_path(aes(time, Resp2, color = "P2")) +
  labs(x = "time", y = "Resp") +
  theme(legend.position="bottom")
p7
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-1-7.png)

``` r
## Now add the group, trial, condition to the cleaned up, scaled, downsampled data
## Tip the info is in the file name
```

Now we are ready to go to load and pre-process all files
--------------------------------------------------------

Go through all the files (with a function passed onto map\_df), check
which files should be excluded, if any, and save the pre-processed
time-series

A couple of tips: - looping is oh so slow. Making a function and using
Map/Map\_df is your salvation. - each study restarts the group
numbering, so you should make sure to change that (e.g. 100 \* Study +
Group) - you need to make sure all the data are meaningful or something
has to be removed. Plotting is your friend. E.g.
“Study1\_G1\_T1\_Synchronous” has one bad respiration signal. We could
replace it with NAs

``` r
data_preprocess <- function(filename, threshold = 2.5){
  
  # load the data
  data <- read_csv(filename)
  
  # remove outliers
  data$Resp1 <- removeOuts(data$Resp1, threshold)
  data$Resp2 <- removeOuts(data$Resp2, threshold)
  data$HR1 <- removeOuts(data$HR1, threshold)
  data$HR2 <- removeOuts(data$HR2, threshold)
  
  # scale variables
  data$HR1Scaled <- scale(data$HR1)
  data$HR2Scaled <- scale(data$HR2)
  data$Resp1Scaled <- scale(data$Resp1)
  data$Resp2Scaled <- scale(data$Resp2)
  
  # downsample the data
  data <- data %>% 
    group(n = 1000, method = 'greedy') %>% 
    dplyr::summarise(
      time = mean(time, na.rm=T),
      HR1 = mean(HR1Scaled, na.rm=T),
      HR2 = mean(HR2Scaled, na.rm=T),
      Resp1 = mean(Resp1Scaled, na.rm=T),
      Resp2 = mean(Resp2Scaled, na.rm=T))
  
  # create list with new variable indeces
  fileIndex <- unlist(str_extract_all(filename, "\\d"))
  # create list with condition indeces
  conditionIndex <- unlist(str_split(filename, "T*_"))
  conditionIndex <- str_remove(conditionIndex[4], ".csv")
  
  data <- data %>%
    add_column(study = fileIndex[2]) %>% 
    add_column(group = fileIndex[3]) %>% 
    add_column(trial = fileIndex[4]) %>% 
    add_column(condition = conditionIndex) %>% 
    add_column(ID = paste(fileIndex[2], fileIndex[3], fileIndex[4],
                          conditionIndex))
  
  return(data)
}


# Run function on all files
data <- list.files(path = "/Users/bertram/Desktop/folders/R/Assignment4/data", pattern = ".csv", full.names=T) %>% 
    purrr::map_df(data_preprocess)
```


``` r
# remove spaces from ID-column
data$ID <- str_replace_all(data$ID, " ", "")

# make ID-column into type factor
data$ID <- as.numeric(as.factor(data$ID))

# Save data frame
# write_csv(data, "full_data.csv")
```

``` r
# Loading real data
full_data <- read.csv('/Users/bertram/Desktop/folders/R/Assignment4/dataAssignment1000.csv')

# Now we need to make sure all the data are meaningful or something has to be removed
# E.g. "Study1_G1_T1_Synchronous" has one bad respiration signal. We could replace it with NAs

# plots plots plots
# Study 4 plots two at a time
for(i in seq(from=58, to=97, by=2)) {
  pl <- full_data %>%
    filter(study==4, ID==c(i, (i+1))) %>% 
    group_by(group,trial) %>% 
    mutate(time = seq(n())) %>% 
    ggplot()+
    geom_line(aes(time,HR1, colour="blue", group=ID))+
    geom_line(aes(time,HR2, colour="red",group=ID))+
    theme_classic()+
    facet_wrap(group~trial)
  plot(pl)
}
```

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-1.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-2.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-3.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-4.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-5.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-6.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-7.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-8.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-9.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-10.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-11.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-12.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-13.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-14.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-15.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-16.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-17.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-18.png)

    ## Warning in ID == c(i, (i + 1)): longer object length is not a multiple of
    ## shorter object length

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-19.png)![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-3-20.png)

``` r
# Remove bad data

data4 <- full_data %>% 
  filter(study==4) %>% 
  group_by(ID) %>%
  mutate(time = seq(n())) 



# Remove bad data(detects straight lines) - pitfall: removes 
for (i in seq(NROW(data4))){
  if (i+5 > length(seq(NROW(data4)))) {
    data4$HR1[i]=NA
  } else if (mean(data4$ID[i:(i+5)]) != data4$ID[i]) {
    data4$HR1[i] = NA
  } else if ((sd(data4$HR1[i:(i+5)])) < 0.1) {
    data4$HR1[i] = NA
  } 
}

for (j in seq(NROW(data4))){
  if (j+5 > length(seq(NROW(data4)))) {
    data4$HR2[j]=NA
  } else if (mean(data4$ID[j:(j+5)]) != data4$ID[j]) {
    data4$HR2[j] = NA
  } else if ((sd(data4$HR2[j:(j+5)])) < 0.1) {
    data4$HR2[j] = NA
  } 
}

# Save the data
```

Now we need to run some analysis
--------------------------------

Let’s start with a multilevel model that accounts for - stability (how
each signal is autocorrelated) - interpersonal dependence (each signal
is dependent from the previous state of the other signal)

The data needs to be further prepared, so we can analyze both
participants in the same model. We need to turn the data into a long
format: - a column indicating own hr and one own respiration - a column
indicating other hr and one other respiration - a column indicating
change in hr from previous round and one in respiration

We can then run an analysis where change is a function of one’s previous
state (stability, see slides), and the other’s previous state
(coupling). Make sure to: - set up the most interesting contrasts: how
do these parameters vary by condition? which condition should be
baseline? - set up the right random effects. - N.B. the model will be
slow. Make sure it works on a subset of the data first!

Bonus question: what if we include an additional layer? Is my heart rate
just adjusting to yours, or also to how much you are adjusting to mine?
- to start answering this we can add a column indicating the previous
change in hr in the other and one in respiration - we can then build on
the previous models by also adding the previous change in the other

``` r
# Genearate a column for each: previous HR1, HR2, Resp1, Resp2
data4 <- data4 %>% 
  group_by(ID )%>% 
  mutate(HR1_change = lead(HR1)-HR1, HR2_change = lead(HR2)-HR2)

# Make the data long, so we can analyze both participants at the same time
HR_other <- data4 %>%
  select(HR1, HR2, ID) %>% 
  gather("P", "HR_other", 2:1) %>% 
  ungroup() %>%
  select(HR_other)

HR_other <- as.data.frame(HR_other)

HR_change <- data4 %>%
  select(HR1_change, HR2_change, ID) %>% 
  gather("Person", "HR_change", 1:2) %>% 
  ungroup() %>%
  select(HR_change, Person) %>% 
  mutate(Person = as.factor(Person))

HR_change <- as.data.frame(HR_change)

HR_change_other <- data4 %>%
  select(HR1_change, HR2_change, ID) %>% 
  gather("Person", "HR_change_other",2:1) %>% 
  ungroup() %>%
  select(HR_change_other)

HR_change_other <- as.data.frame(HR_change_other)

model_data <- data4 %>%
  gather("Person", "HR_self", 3:4) %>% 
  select(time, study, group, trial, condition, ID, HR_self)


model_data <- as.data.frame(model_data)
model_data$type <- 1

model_data <- cbind(model_data, HR_other, HR_change, HR_change_other)


# Set the most interesting contrast e.g. by defining synchronous or conversation as the baseline
model_1 <- lm(HR_change ~ 0 + condition + (HR_self+HR_other):condition, data=model_data)
summary(model_1)
```

    ## 
    ## Call:
    ## lm(formula = HR_change ~ 0 + condition + (HR_self + HR_other):condition, 
    ##     data = model_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5638 -0.3425  0.0416  0.3825  3.8728 
    ## 
    ## Coefficients:
    ##                                    Estimate Std. Error t value Pr(>|t|)
    ## conditionConversation             0.0002741  0.0119932   0.023  0.98176
    ## conditionMovementCoop            -0.0034799  0.0160849  -0.216  0.82872
    ## conditionMovementGuided           0.0071644  0.0150215   0.477  0.63341
    ## conditionSynchronous              0.0012823  0.0123861   0.104  0.91755
    ## conditionTurnTaking               0.0003828  0.0123479   0.031  0.97527
    ## conditionConversation:HR_self    -0.2777431  0.0131620 -21.102  < 2e-16
    ## conditionMovementCoop:HR_self    -0.2306488  0.0178927 -12.891  < 2e-16
    ## conditionMovementGuided:HR_self  -0.2615608  0.0160927 -16.253  < 2e-16
    ## conditionSynchronous:HR_self     -0.2995344  0.0136317 -21.973  < 2e-16
    ## conditionTurnTaking:HR_self      -0.2680886  0.0133059 -20.148  < 2e-16
    ## conditionConversation:HR_other    0.0343033  0.0131713   2.604  0.00922
    ## conditionMovementCoop:HR_other    0.0281471  0.0179529   1.568  0.11695
    ## conditionMovementGuided:HR_other  0.0074144  0.0160994   0.461  0.64514
    ## conditionSynchronous:HR_other     0.0093582  0.0136563   0.685  0.49319
    ## conditionTurnTaking:HR_other     -0.0243130  0.0133206  -1.825  0.06799
    ##                                     
    ## conditionConversation               
    ## conditionMovementCoop               
    ## conditionMovementGuided             
    ## conditionSynchronous                
    ## conditionTurnTaking                 
    ## conditionConversation:HR_self    ***
    ## conditionMovementCoop:HR_self    ***
    ## conditionMovementGuided:HR_self  ***
    ## conditionSynchronous:HR_self     ***
    ## conditionTurnTaking:HR_self      ***
    ## conditionConversation:HR_other   ** 
    ## conditionMovementCoop:HR_other      
    ## conditionMovementGuided:HR_other    
    ## conditionSynchronous:HR_other       
    ## conditionTurnTaking:HR_other     .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6277 on 11147 degrees of freedom
    ##   (818 observations deleted due to missingness)
    ## Multiple R-squared:  0.1369, Adjusted R-squared:  0.1357 
    ## F-statistic: 117.9 on 15 and 11147 DF,  p-value: < 2.2e-16

``` r
plot_summs(model_1)
```

![](A4---Physiological-Coordination-Knit_files/figure-markdown_github/unnamed-chunk-4-1.png)

``` r
model_2 <- lmer(HR_change ~ 0 + condition + (HR_self+HR_other):condition + (1+HR_self| ID) + (1+HR_self| group), data=model_data)
```

    ## boundary (singular) fit: see ?isSingular

    ## Warning: Model failed to converge with 1 negative eigenvalue: -1.4e+01

``` r
summary(model_2)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: HR_change ~ 0 + condition + (HR_self + HR_other):condition +  
    ##     (1 + HR_self | ID) + (1 + HR_self | group)
    ##    Data: model_data
    ## 
    ## REML criterion at convergence: 21269.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4226 -0.5571  0.0708  0.6214  6.4840 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.  Corr 
    ##  ID       (Intercept) 0.000e+00 0.0000000      
    ##           HR_self     1.845e-03 0.0429590  NaN 
    ##  group    (Intercept) 5.586e-08 0.0002363      
    ##           HR_self     4.408e-03 0.0663890 -0.14
    ##  Residual             3.893e-01 0.6239028      
    ## Number of obs: 11162, groups:  ID, 40; group, 8
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## conditionConversation             3.783e-04  1.192e-02  1.067e+04   0.032
    ## conditionMovementCoop            -4.239e-03  1.600e-02  1.098e+04  -0.265
    ## conditionMovementGuided           8.685e-03  1.494e-02  1.087e+04   0.581
    ## conditionSynchronous              1.208e-03  1.232e-02  1.071e+04   0.098
    ## conditionTurnTaking               4.027e-04  1.228e-02  1.070e+04   0.033
    ## conditionConversation:HR_self    -2.840e-01  3.088e-02  1.455e+01  -9.196
    ## conditionMovementCoop:HR_self    -2.336e-01  3.317e-02  1.935e+01  -7.043
    ## conditionMovementGuided:HR_self  -2.650e-01  3.235e-02  1.748e+01  -8.192
    ## conditionSynchronous:HR_self     -3.021e-01  3.112e-02  1.499e+01  -9.709
    ## conditionTurnTaking:HR_self      -2.732e-01  3.101e-02  1.476e+01  -8.811
    ## conditionConversation:HR_other    2.839e-02  1.323e-02  1.080e+04   2.145
    ## conditionMovementCoop:HR_other    2.127e-02  1.802e-02  1.033e+04   1.180
    ## conditionMovementGuided:HR_other  1.532e-03  1.612e-02  1.086e+04   0.095
    ## conditionSynchronous:HR_other     7.437e-03  1.372e-02  1.070e+04   0.542
    ## conditionTurnTaking:HR_other     -2.619e-02  1.350e-02  9.714e+03  -1.940
    ##                                  Pr(>|t|)    
    ## conditionConversation              0.9747    
    ## conditionMovementCoop              0.7911    
    ## conditionMovementGuided            0.5610    
    ## conditionSynchronous               0.9219    
    ## conditionTurnTaking                0.9738    
    ## conditionConversation:HR_self    1.91e-07 ***
    ## conditionMovementCoop:HR_self    9.50e-07 ***
    ## conditionMovementGuided:HR_self  2.16e-07 ***
    ## conditionSynchronous:HR_self     7.39e-08 ***
    ## conditionTurnTaking:HR_self      2.91e-07 ***
    ## conditionConversation:HR_other     0.0319 *  
    ## conditionMovementCoop:HR_other     0.2380    
    ## conditionMovementGuided:HR_other   0.9243    
    ## conditionSynchronous:HR_other      0.5877    
    ## conditionTurnTaking:HR_other       0.0524 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
summ(model_2, digits=3)
```

    ## Warning in summ.merMod(model_2, digits = 3): Could not calculate r-squared. Try removing missing data
    ## before fitting the model.

    ## MODEL INFO:
    ## Observations: 11162
    ## Dependent Variable: HR_change
    ## Type: Mixed effects linear regression 
    ## 
    ## MODEL FIT:
    ## AIC = 21313.570, BIC = 21474.616 
    ## 
    ## FIXED EFFECTS:
    ## ----------------------------------------------------------------
    ##                                            Est.    S.E.   t val.
    ## -------------------------------------- -------- ------- --------
    ## conditionConversation                     0.000   0.012    0.032
    ## conditionMovementCoop                    -0.004   0.016   -0.265
    ## conditionMovementGuided                   0.009   0.015    0.581
    ## conditionSynchronous                      0.001   0.012    0.098
    ## conditionTurnTaking                       0.000   0.012    0.033
    ## conditionConversation:HR_self            -0.284   0.031   -9.196
    ## conditionMovementCoop:HR_self            -0.234   0.033   -7.043
    ## conditionMovementGuided:HR_self          -0.265   0.032   -8.192
    ## conditionSynchronous:HR_self             -0.302   0.031   -9.709
    ## conditionTurnTaking:HR_self              -0.273   0.031   -8.811
    ## conditionConversation:HR_other            0.028   0.013    2.145
    ## conditionMovementCoop:HR_other            0.021   0.018    1.180
    ## conditionMovementGuided:HR_other          0.002   0.016    0.095
    ## conditionSynchronous:HR_other             0.007   0.014    0.542
    ## conditionTurnTaking:HR_other             -0.026   0.014   -1.940
    ## ----------------------------------------------------------------
    ##  
    ## ----------------------------------------------------------
    ##                                               d.f.       p
    ## -------------------------------------- ----------- -------
    ## conditionConversation                    10670.990   0.975
    ## conditionMovementCoop                    10975.104   0.791
    ## conditionMovementGuided                  10867.780   0.561
    ## conditionSynchronous                     10709.384   0.922
    ## conditionTurnTaking                      10695.682   0.974
    ## conditionConversation:HR_self               14.550   0.000
    ## conditionMovementCoop:HR_self               19.348   0.000
    ## conditionMovementGuided:HR_self             17.483   0.000
    ## conditionSynchronous:HR_self                14.993   0.000
    ## conditionTurnTaking:HR_self                 14.764   0.000
    ## conditionConversation:HR_other           10798.136   0.032
    ## conditionMovementCoop:HR_other           10333.776   0.238
    ## conditionMovementGuided:HR_other         10864.875   0.924
    ## conditionSynchronous:HR_other            10695.948   0.588
    ## conditionTurnTaking:HR_other              9713.694   0.052
    ## ----------------------------------------------------------
    ## 
    ## p values calculated using Satterthwaite d.f.
    ## 
    ## RANDOM EFFECTS:
    ## ------------------------------------
    ##   Group      Parameter    Std. Dev. 
    ## ---------- ------------- -----------
    ##     ID      (Intercept)     0.000   
    ##     ID        HR_self       0.043   
    ##   group     (Intercept)     0.000   
    ##   group       HR_self       0.066   
    ##  Residual                   0.624   
    ## ------------------------------------
    ## 
    ## Grouping variables:
    ## --------------------------
    ##  Group   # groups    ICC  
    ## ------- ---------- -------
    ##   ID        40      0.000 
    ##  group      8       0.000 
    ## --------------------------

``` r
model_0 <- lmer(HR_change ~ 1 + (HR_self+HR_other)*condition + (1+HR_self| ID) + (1+HR_self| group), data=model_data)
```

    ## boundary (singular) fit: see ?isSingular

    ## Warning: Model failed to converge with 1 negative eigenvalue: -1.4e+01

``` r
summary(model_0)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## HR_change ~ 1 + (HR_self + HR_other) * condition + (1 + HR_self |  
    ##     ID) + (1 + HR_self | group)
    ##    Data: model_data
    ## 
    ## REML criterion at convergence: 21269.6
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -7.4226 -0.5571  0.0708  0.6214  6.4840 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev.  Corr 
    ##  ID       (Intercept) 0.000e+00 0.0000000      
    ##           HR_self     1.845e-03 0.0429584  NaN 
    ##  group    (Intercept) 5.637e-08 0.0002374      
    ##           HR_self     4.407e-03 0.0663844 -0.14
    ##  Residual             3.893e-01 0.6239028      
    ## Number of obs: 11162, groups:  ID, 40; group, 8
    ## 
    ## Fixed effects:
    ##                                    Estimate Std. Error         df t value
    ## (Intercept)                       3.783e-04  1.192e-02  1.067e+04   0.032
    ## HR_self                          -2.840e-01  3.088e-02  1.455e+01  -9.197
    ## HR_other                          2.839e-02  1.323e-02  1.080e+04   2.145
    ## conditionMovementCoop            -4.617e-03  1.996e-02  1.112e+04  -0.231
    ## conditionMovementGuided           8.307e-03  1.911e-02  1.110e+04   0.435
    ## conditionSynchronous              8.299e-04  1.714e-02  1.112e+04   0.048
    ## conditionTurnTaking               2.437e-05  1.711e-02  1.111e+04   0.001
    ## HR_self:conditionMovementCoop     5.041e-02  3.085e-02  3.132e+01   1.634
    ## HR_self:conditionMovementGuided   1.896e-02  2.997e-02  2.766e+01   0.633
    ## HR_self:conditionSynchronous     -1.815e-02  2.864e-02  2.327e+01  -0.634
    ## HR_self:conditionTurnTaking       1.081e-02  2.852e-02  2.282e+01   0.379
    ## HR_other:conditionMovementCoop   -7.118e-03  2.234e-02  1.034e+04  -0.319
    ## HR_other:conditionMovementGuided -2.685e-02  2.086e-02  1.088e+04  -1.287
    ## HR_other:conditionSynchronous    -2.095e-02  1.905e-02  1.062e+04  -1.100
    ## HR_other:conditionTurnTaking     -5.458e-02  1.889e-02  1.012e+04  -2.889
    ##                                  Pr(>|t|)    
    ## (Intercept)                       0.97469    
    ## HR_self                           1.9e-07 ***
    ## HR_other                          0.03194 *  
    ## conditionMovementCoop             0.81702    
    ## conditionMovementGuided           0.66383    
    ## conditionSynchronous              0.96139    
    ## conditionTurnTaking               0.99886    
    ## HR_self:conditionMovementCoop     0.11225    
    ## HR_self:conditionMovementGuided   0.53218    
    ## HR_self:conditionSynchronous      0.53256    
    ## HR_self:conditionTurnTaking       0.70805    
    ## HR_other:conditionMovementCoop    0.75003    
    ## HR_other:conditionMovementGuided  0.19804    
    ## HR_other:conditionSynchronous     0.27144    
    ## HR_other:conditionTurnTaking      0.00387 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 15 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
summ(model_0, digits=3)
```

    ## MODEL INFO:
    ## Observations: 11162
    ## Dependent Variable: HR_change
    ## Type: Mixed effects linear regression 
    ## 
    ## MODEL FIT:
    ## AIC = 21313.570, BIC = 21474.616
    ## Pseudo-R² (fixed effects) = 0.140
    ## Pseudo-R² (total) = 0.152 
    ## 
    ## FIXED EFFECTS:
    ## ----------------------------------------------------------------
    ##                                            Est.    S.E.   t val.
    ## -------------------------------------- -------- ------- --------
    ## (Intercept)                               0.000   0.012    0.032
    ## HR_self                                  -0.284   0.031   -9.197
    ## HR_other                                  0.028   0.013    2.145
    ## conditionMovementCoop                    -0.005   0.020   -0.231
    ## conditionMovementGuided                   0.008   0.019    0.435
    ## conditionSynchronous                      0.001   0.017    0.048
    ## conditionTurnTaking                       0.000   0.017    0.001
    ## HR_self:conditionMovementCoop             0.050   0.031    1.634
    ## HR_self:conditionMovementGuided           0.019   0.030    0.633
    ## HR_self:conditionSynchronous             -0.018   0.029   -0.634
    ## HR_self:conditionTurnTaking               0.011   0.029    0.379
    ## HR_other:conditionMovementCoop           -0.007   0.022   -0.319
    ## HR_other:conditionMovementGuided         -0.027   0.021   -1.287
    ## HR_other:conditionSynchronous            -0.021   0.019   -1.100
    ## HR_other:conditionTurnTaking             -0.055   0.019   -2.889
    ## ----------------------------------------------------------------
    ##  
    ## ----------------------------------------------------------
    ##                                               d.f.       p
    ## -------------------------------------- ----------- -------
    ## (Intercept)                              10667.278   0.975
    ## HR_self                                     14.553   0.000
    ## HR_other                                 10798.128   0.032
    ## conditionMovementCoop                    11119.289   0.817
    ## conditionMovementGuided                  11098.039   0.664
    ## conditionSynchronous                     11116.172   0.961
    ## conditionTurnTaking                      11114.717   0.999
    ## HR_self:conditionMovementCoop               31.320   0.112
    ## HR_self:conditionMovementGuided             27.664   0.532
    ## HR_self:conditionSynchronous                23.267   0.533
    ## HR_self:conditionTurnTaking                 22.820   0.708
    ## HR_other:conditionMovementCoop           10340.040   0.750
    ## HR_other:conditionMovementGuided         10876.500   0.198
    ## HR_other:conditionSynchronous            10620.949   0.271
    ## HR_other:conditionTurnTaking             10123.713   0.004
    ## ----------------------------------------------------------
    ## 
    ## p values calculated using Satterthwaite d.f.
    ## 
    ## RANDOM EFFECTS:
    ## ------------------------------------
    ##   Group      Parameter    Std. Dev. 
    ## ---------- ------------- -----------
    ##     ID      (Intercept)     0.000   
    ##     ID        HR_self       0.043   
    ##   group     (Intercept)     0.000   
    ##   group       HR_self       0.066   
    ##  Residual                   0.624   
    ## ------------------------------------
    ## 
    ## Grouping variables:
    ## --------------------------
    ##  Group   # groups    ICC  
    ## ------- ---------- -------
    ##   ID        40      0.000 
    ##  group      8       0.000 
    ## --------------------------

``` r
# Model change as a function of own and other previous state 


# Bonus points: Add to the previous model also change in the other to see whether my adaptation is influenced by the other's adaptation.
```

Now we need to create control baselines.
----------------------------------------

First shuffled controls, then surrogate pairs.

### Creating controls: shuffled controls

Shuffled controls break the temporal dependencies of time-series by
shuffling the value within one time-series. This ensures the
“coordination” observed is not due to the actual values in the series
and not their sequence. Tip: sample() is your friend, but make sure to
shuffle things within participant/condition and not throughout the whole
dataset

``` r
# Create a shuffled dataset
shuffled_data <- model_data %>% 
  group_by(ID, Person) %>% 
  mutate(HR_self = sample(HR_self), HR_other = sample(HR_other)) %>% 
  mutate(HR_change = lead(HR_self)-HR_self, HR_change_other = lead(HR_other)-HR_other, type = 0)

shuffled_data <- as.data.frame(shuffled_data)
# Concatenate it to the original dataset (and remember to have a column telling you which is which)
shuffled_model_data <- rbind(shuffled_data, model_data)
shuffled_model_data$type <- as.factor(shuffled_model_data$type)

# Create the same models as in the previous chunk, but adding an interaction by shuffled vs. real
model_3 <- lmerTest::lmer(HR_change ~ (HR_self+HR_other):condition:type + (1 | ID) +(1 | group), data=shuffled_model_data)
```

    ## boundary (singular) fit: see ?isSingular

``` r
summary(model_3)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: HR_change ~ (HR_self + HR_other):condition:type + (1 | ID) +  
    ##     (1 | group)
    ##    Data: shuffled_model_data
    ## 
    ## REML criterion at convergence: 50720.5
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -5.8131 -0.5787  0.0546  0.6126  4.9396 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. 
    ##  ID       (Intercept) 1.790e-18 1.338e-09
    ##  group    (Intercept) 3.948e-16 1.987e-08
    ##  Residual             6.146e-01 7.840e-01
    ## Number of obs: 21526, groups:  ID, 40; group, 8
    ## 
    ## Fixed effects:
    ##                                          Estimate Std. Error         df
    ## (Intercept)                             6.952e-04  5.344e-03  2.150e+04
    ## HR_self:conditionConversation:type0    -1.043e+00  1.682e-02  2.150e+04
    ## HR_self:conditionMovementCoop:type0    -1.048e+00  2.302e-02  2.150e+04
    ## HR_self:conditionMovementGuided:type0  -1.010e+00  2.142e-02  2.150e+04
    ## HR_self:conditionSynchronous:type0     -1.018e+00  1.762e-02  2.150e+04
    ## HR_self:conditionTurnTaking:type0      -1.010e+00  1.711e-02  2.150e+04
    ## HR_self:conditionConversation:type1    -2.777e-01  1.644e-02  2.150e+04
    ## HR_self:conditionMovementCoop:type1    -2.306e-01  2.235e-02  2.150e+04
    ## HR_self:conditionMovementGuided:type1  -2.616e-01  2.010e-02  2.150e+04
    ## HR_self:conditionSynchronous:type1     -2.995e-01  1.702e-02  2.150e+04
    ## HR_self:conditionTurnTaking:type1      -2.681e-01  1.662e-02  2.150e+04
    ## HR_other:conditionConversation:type0    2.415e-02  1.677e-02  2.150e+04
    ## HR_other:conditionMovementCoop:type0    8.761e-04  2.310e-02  2.150e+04
    ## HR_other:conditionMovementGuided:type0 -2.464e-02  2.103e-02  2.150e+04
    ## HR_other:conditionSynchronous:type0    -1.105e-02  1.750e-02  2.150e+04
    ## HR_other:conditionTurnTaking:type0     -2.719e-02  1.719e-02  2.150e+04
    ## HR_other:conditionConversation:type1    3.430e-02  1.645e-02  2.150e+04
    ## HR_other:conditionMovementCoop:type1    2.823e-02  2.242e-02  2.150e+04
    ## HR_other:conditionMovementGuided:type1  7.443e-03  2.011e-02  2.150e+04
    ## HR_other:conditionSynchronous:type1     9.367e-03  1.706e-02  2.150e+04
    ## HR_other:conditionTurnTaking:type1     -2.431e-02  1.664e-02  2.150e+04
    ##                                        t value Pr(>|t|)    
    ## (Intercept)                              0.130   0.8965    
    ## HR_self:conditionConversation:type0    -61.988   <2e-16 ***
    ## HR_self:conditionMovementCoop:type0    -45.512   <2e-16 ***
    ## HR_self:conditionMovementGuided:type0  -47.152   <2e-16 ***
    ## HR_self:conditionSynchronous:type0     -57.768   <2e-16 ***
    ## HR_self:conditionTurnTaking:type0      -59.046   <2e-16 ***
    ## HR_self:conditionConversation:type1    -16.895   <2e-16 ***
    ## HR_self:conditionMovementCoop:type1    -10.318   <2e-16 ***
    ## HR_self:conditionMovementGuided:type1  -13.013   <2e-16 ***
    ## HR_self:conditionSynchronous:type1     -17.593   <2e-16 ***
    ## HR_self:conditionTurnTaking:type1      -16.131   <2e-16 ***
    ## HR_other:conditionConversation:type0     1.440   0.1500    
    ## HR_other:conditionMovementCoop:type0     0.038   0.9697    
    ## HR_other:conditionMovementGuided:type0  -1.172   0.2414    
    ## HR_other:conditionSynchronous:type0     -0.632   0.5277    
    ## HR_other:conditionTurnTaking:type0      -1.581   0.1138    
    ## HR_other:conditionConversation:type1     2.085   0.0371 *  
    ## HR_other:conditionMovementCoop:type1     1.259   0.2079    
    ## HR_other:conditionMovementGuided:type1   0.370   0.7113    
    ## HR_other:conditionSynchronous:type1      0.549   0.5829    
    ## HR_other:conditionTurnTaking:type1      -1.461   0.1439    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 21 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

### TRICKY! Creating controls: surrogate pair controls

-   Per each real pair, identify at least one surrogate pair (matching
    one of the participants, with somebody doing the same task, but in a
    different pair)

``` r
# Identify unique pairs within a given study (to keep things manageable) and create list of possible surrogate pairs (e.g. individual 1 from pair 1 and individual 2 from pair 2)
# creating a list based on gorup
Groups <- as.numeric(as.character(unique(model_data$group)))
# expaniding the list into two values
SurrogateList <- expand.grid(a = Groups, b = Groups)
SurrogateList <- subset(SurrogateList, a != b)

# transoform into data-frame and ungroup the data
surrogate_data <- data.frame()
data4 <- data4 %>% ungroup()

# specifying pairs and creating surrogate pairs
for (d in 1:nrow(SurrogateList)) {
  x <- subset(data4, group==SurrogateList$a[d])
  y <- subset(data4, group==SurrogateList$b[d])
  
  for (co in c("Synchronous","TurnTaking","SelfPaced","Conversation", "MovementGuided")) {
      
      z1 <- as.data.frame(subset(x,condition==co))
      z2 <- as.data.frame(subset(y,condition==co))
      z1 <- z1 %>% mutate(ID=paste(as.character(SurrogateList$a[d]),as.character(SurrogateList$b[d]),co, sep=""))
      
      if (NROW(z1) > NROW(z2)) {
        z1 <-  z1[c(1:nrow(z2)),]
      }
      if (NROW(z2) > NROW(z1)) {
        z2 <-  z2[c(1:nrow(z1)),]
        }
        
        w1 <- z1 %>% mutate(
          HR2 = z2$HR2,
          HR2_change = z2$HR2_change) 
        
        surrogate_data <- rbind(surrogate_data,w1)
  }}
      

sur_other <- surrogate_data %>%
  select(HR1, HR2) %>% 
  gather("P", "HR_other", 2:1) %>% 
  select(HR_other)

sur_other <- as.data.frame(sur_other)

sur_change <- surrogate_data %>%
  select(HR1_change, HR2_change) %>% 
  gather("Person", "HR_change", 1:2) %>% 
  select(HR_change, Person) %>% 
  mutate(Person = as.factor(Person))

sur_change <- as.data.frame(sur_change)

sur_change_other <- surrogate_data %>%
  select(HR1_change, HR2_change) %>% 
  gather("Person", "HR_change_other",2:1) %>% 
  select(HR_change_other)

sur_change_other <- as.data.frame(sur_change_other)

sur_data <- surrogate_data %>%
  gather("Person", "HR_self", 3:4) %>% 
  select(time, study, group, trial, condition, ID, HR_self)


sur_data <- as.data.frame(sur_data)
sur_data$type <- 0

sur_data <- cbind(sur_data, sur_other, sur_change, sur_change_other)

sur_model_data <- rbind(sur_data, model_data)
sur_model_data$type <- as.factor(sur_model_data$type)


# Creating models based on the surrogate pairs
model_4 <- lmer(HR_change ~ 0 + condition + (HR_self+HR_other):condition:type + (1+HR_self| ID) + (1+HR_self| group), data=sur_model_data)
```

    ## fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients

    ## boundary (singular) fit: see ?isSingular

    ## Warning: Model failed to converge with 1 negative eigenvalue: -4.2e+01

``` r
summary(model_4)
```

    ## Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    ## lmerModLmerTest]
    ## Formula: 
    ## HR_change ~ 0 + condition + (HR_self + HR_other):condition:type +  
    ##     (1 + HR_self | ID) + (1 + HR_self | group)
    ##    Data: sur_model_data
    ## 
    ## REML criterion at convergence: 143084.8
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -7.3472 -0.5482  0.0704  0.6170  6.3783 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr 
    ##  ID       (Intercept) 0.0000038 0.001949      
    ##           HR_self     0.0046030 0.067845 -1.00
    ##  group    (Intercept) 0.0000000 0.000000      
    ##           HR_self     0.0022391 0.047319  NaN 
    ##  Residual             0.3955770 0.628949      
    ## Number of obs: 74630, groups:  ID, 264; group, 8
    ## 
    ## Fixed effects:
    ##                                          Estimate Std. Error         df
    ## conditionConversation                   4.221e-04  4.260e-03  2.239e+04
    ## conditionMovementCoop                  -3.795e-03  1.615e-02  4.502e+04
    ## conditionMovementGuided                 1.239e-02  5.698e-03  4.329e+04
    ## conditionSynchronous                    2.540e-03  4.526e-03  2.580e+04
    ## conditionTurnTaking                     1.633e-03  4.503e-03  2.493e+04
    ## conditionConversation:HR_self:type0    -2.808e-01  1.966e-02  1.119e+01
    ## conditionMovementGuided:HR_self:type0  -2.610e-01  2.014e-02  1.232e+01
    ## conditionSynchronous:HR_self:type0     -3.065e-01  1.979e-02  1.147e+01
    ## conditionTurnTaking:HR_self:type0      -2.718e-01  1.977e-02  1.143e+01
    ## conditionConversation:HR_self:type1    -2.844e-01  3.207e-02  6.733e+01
    ## conditionMovementCoop:HR_self:type1    -2.329e-01  3.434e-02  8.842e+01
    ## conditionMovementGuided:HR_self:type1  -2.631e-01  3.355e-02  8.048e+01
    ## conditionSynchronous:HR_self:type1     -3.025e-01  3.231e-02  6.931e+01
    ## conditionTurnTaking:HR_self:type1      -2.757e-01  3.221e-02  6.842e+01
    ## conditionConversation:HR_other:type0    5.360e-04  4.971e-03  7.460e+04
    ## conditionMovementGuided:HR_other:type0  2.072e-03  6.634e-03  7.317e+04
    ## conditionSynchronous:HR_other:type0     1.001e-03  5.356e-03  7.451e+04
    ## conditionTurnTaking:HR_other:type0     -1.241e-02  5.272e-03  7.444e+04
    ## conditionConversation:HR_other:type1    2.900e-02  1.336e-02  7.421e+04
    ## conditionMovementCoop:HR_other:type1    2.242e-02  1.822e-02  7.267e+04
    ## conditionMovementGuided:HR_other:type1  1.327e-03  1.628e-02  7.422e+04
    ## conditionSynchronous:HR_other:type1     7.268e-03  1.386e-02  7.398e+04
    ## conditionTurnTaking:HR_other:type1     -2.467e-02  1.366e-02  7.191e+04
    ##                                        t value Pr(>|t|)    
    ## conditionConversation                    0.099   0.9211    
    ## conditionMovementCoop                   -0.235   0.8142    
    ## conditionMovementGuided                  2.175   0.0297 *  
    ## conditionSynchronous                     0.561   0.5747    
    ## conditionTurnTaking                      0.363   0.7169    
    ## conditionConversation:HR_self:type0    -14.280 1.57e-08 ***
    ## conditionMovementGuided:HR_self:type0  -12.962 1.52e-08 ***
    ## conditionSynchronous:HR_self:type0     -15.493 4.77e-09 ***
    ## conditionTurnTaking:HR_self:type0      -13.752 1.84e-08 ***
    ## conditionConversation:HR_self:type1     -8.868 6.34e-13 ***
    ## conditionMovementCoop:HR_self:type1     -6.782 1.28e-09 ***
    ## conditionMovementGuided:HR_self:type1   -7.844 1.58e-11 ***
    ## conditionSynchronous:HR_self:type1      -9.362 6.30e-14 ***
    ## conditionTurnTaking:HR_self:type1       -8.560 2.01e-12 ***
    ## conditionConversation:HR_other:type0     0.108   0.9141    
    ## conditionMovementGuided:HR_other:type0   0.312   0.7548    
    ## conditionSynchronous:HR_other:type0      0.187   0.8518    
    ## conditionTurnTaking:HR_other:type0      -2.353   0.0186 *  
    ## conditionConversation:HR_other:type1     2.170   0.0300 *  
    ## conditionMovementCoop:HR_other:type1     1.230   0.2186    
    ## conditionMovementGuided:HR_other:type1   0.082   0.9350    
    ## conditionSynchronous:HR_other:type1      0.525   0.5999    
    ## conditionTurnTaking:HR_other:type1      -1.806   0.0709 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    ## 
    ## Correlation matrix not shown by default, as p = 23 > 12.
    ## Use print(x, correlation=TRUE)  or
    ##     vcov(x)        if you need it

    ## fit warnings:
    ## fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients
    ## convergence code: 0
    ## boundary (singular) fit: see ?isSingular

``` r
summ(model_4, digits=3)
```

    ## Warning in summ.merMod(model_4, digits = 3): Could not calculate r-squared. Try removing missing data
    ## before fitting the model.

    ## MODEL INFO:
    ## Observations: 74630
    ## Dependent Variable: HR_change
    ## Type: Mixed effects linear regression 
    ## 
    ## MODEL FIT:
    ## AIC = 143144.755, BIC = 143421.364 
    ## 
    ## FIXED EFFECTS:
    ## -----------------------------------------------------------------------
    ##                                                  Est.    S.E.    t val.
    ## -------------------------------------------- -------- ------- ---------
    ## conditionConversation                           0.000   0.004     0.099
    ## conditionMovementCoop                          -0.004   0.016    -0.235
    ## conditionMovementGuided                         0.012   0.006     2.175
    ## conditionSynchronous                            0.003   0.005     0.561
    ## conditionTurnTaking                             0.002   0.005     0.363
    ## conditionConversation:HR_self:type0            -0.281   0.020   -14.280
    ## conditionMovementGuided:HR_self:type0          -0.261   0.020   -12.962
    ## conditionSynchronous:HR_self:type0             -0.307   0.020   -15.493
    ## conditionTurnTaking:HR_self:type0              -0.272   0.020   -13.752
    ## conditionConversation:HR_self:type1            -0.284   0.032    -8.868
    ## conditionMovementCoop:HR_self:type1            -0.233   0.034    -6.782
    ## conditionMovementGuided:HR_self:type1          -0.263   0.034    -7.844
    ## conditionSynchronous:HR_self:type1             -0.302   0.032    -9.362
    ## conditionTurnTaking:HR_self:type1              -0.276   0.032    -8.560
    ## conditionConversation:HR_other:type0            0.001   0.005     0.108
    ## conditionMovementGuided:HR_other:type0          0.002   0.007     0.312
    ## conditionSynchronous:HR_other:type0             0.001   0.005     0.187
    ## conditionTurnTaking:HR_other:type0             -0.012   0.005    -2.353
    ## conditionConversation:HR_other:type1            0.029   0.013     2.170
    ## conditionMovementCoop:HR_other:type1            0.022   0.018     1.230
    ## conditionMovementGuided:HR_other:type1          0.001   0.016     0.082
    ## conditionSynchronous:HR_other:type1             0.007   0.014     0.525
    ## conditionTurnTaking:HR_other:type1             -0.025   0.014    -1.806
    ## -----------------------------------------------------------------------
    ##  
    ## ----------------------------------------------------------------
    ##                                                     d.f.       p
    ## -------------------------------------------- ----------- -------
    ## conditionConversation                          22394.992   0.921
    ## conditionMovementCoop                          45015.705   0.814
    ## conditionMovementGuided                        43290.254   0.030
    ## conditionSynchronous                           25799.139   0.575
    ## conditionTurnTaking                            24934.578   0.717
    ## conditionConversation:HR_self:type0               11.188   0.000
    ## conditionMovementGuided:HR_self:type0             12.316   0.000
    ## conditionSynchronous:HR_self:type0                11.474   0.000
    ## conditionTurnTaking:HR_self:type0                 11.429   0.000
    ## conditionConversation:HR_self:type1               67.327   0.000
    ## conditionMovementCoop:HR_self:type1               88.417   0.000
    ## conditionMovementGuided:HR_self:type1             80.481   0.000
    ## conditionSynchronous:HR_self:type1                69.309   0.000
    ## conditionTurnTaking:HR_self:type1                 68.418   0.000
    ## conditionConversation:HR_other:type0           74595.879   0.914
    ## conditionMovementGuided:HR_other:type0         73173.138   0.755
    ## conditionSynchronous:HR_other:type0            74512.217   0.852
    ## conditionTurnTaking:HR_other:type0             74437.949   0.019
    ## conditionConversation:HR_other:type1           74210.929   0.030
    ## conditionMovementCoop:HR_other:type1           72674.632   0.219
    ## conditionMovementGuided:HR_other:type1         74219.695   0.935
    ## conditionSynchronous:HR_other:type1            73978.542   0.600
    ## conditionTurnTaking:HR_other:type1             71907.950   0.071
    ## ----------------------------------------------------------------
    ## 
    ## p values calculated using Satterthwaite d.f.
    ## 
    ## RANDOM EFFECTS:
    ## ------------------------------------
    ##   Group      Parameter    Std. Dev. 
    ## ---------- ------------- -----------
    ##     ID      (Intercept)     0.002   
    ##     ID        HR_self       0.068   
    ##   group     (Intercept)     0.000   
    ##   group       HR_self       0.047   
    ##  Residual                   0.629   
    ## ------------------------------------
    ## 
    ## Grouping variables:
    ## --------------------------
    ##  Group   # groups    ICC  
    ## ------- ---------- -------
    ##   ID       264      0.000 
    ##  group      8       0.000 
    ## --------------------------

``` r
model_5 <- lm(HR_change ~ 0 + condition + (HR_self+HR_other):condition:type, data=sur_model_data)
summary(model_5)
```

    ## 
    ## Call:
    ## lm(formula = HR_change ~ 0 + condition + (HR_self + HR_other):condition:type, 
    ##     data = sur_model_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -4.5722 -0.3437  0.0427  0.3866  3.9222 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                                          Estimate Std. Error t value
    ## conditionConversation                   0.0001705  0.0042805   0.040
    ## conditionMovementCoop                  -0.0034799  0.0162248  -0.214
    ## conditionMovementGuided                 0.0120923  0.0057243   2.112
    ## conditionSynchronous                    0.0024854  0.0045466   0.547
    ## conditionTurnTaking                     0.0015827  0.0045248   0.350
    ## conditionConversation:HR_self:type0    -0.2750416  0.0049682 -55.360
    ## conditionMovementCoop:HR_self:type0            NA         NA      NA
    ## conditionMovementGuided:HR_self:type0  -0.2557810  0.0065899 -38.814
    ## conditionSynchronous:HR_self:type0     -0.2994635  0.0053408 -56.071
    ## conditionTurnTaking:HR_self:type0      -0.2674668  0.0052546 -50.901
    ## conditionConversation:HR_self:type1    -0.2777430  0.0132765 -20.920
    ## conditionMovementCoop:HR_self:type1    -0.2306488  0.0180483 -12.780
    ## conditionMovementGuided:HR_self:type1  -0.2615629  0.0162327 -16.113
    ## conditionSynchronous:HR_self:type1     -0.2995528  0.0137491 -21.787
    ## conditionTurnTaking:HR_self:type1      -0.2680944  0.0134215 -19.975
    ## conditionConversation:HR_other:type0    0.0025675  0.0049779   0.516
    ## conditionMovementCoop:HR_other:type0           NA         NA      NA
    ## conditionMovementGuided:HR_other:type0  0.0035494  0.0065909   0.539
    ## conditionSynchronous:HR_other:type0     0.0008750  0.0053500   0.164
    ## conditionTurnTaking:HR_other:type0     -0.0164264  0.0052559  -3.125
    ## conditionConversation:HR_other:type1    0.0343032  0.0132859   2.582
    ## conditionMovementCoop:HR_other:type1    0.0281471  0.0181091   1.554
    ## conditionMovementGuided:HR_other:type1  0.0073926  0.0162394   0.455
    ## conditionSynchronous:HR_other:type1     0.0093399  0.0137740   0.678
    ## conditionTurnTaking:HR_other:type1     -0.0243187  0.0134363  -1.810
    ##                                        Pr(>|t|)    
    ## conditionConversation                   0.96823    
    ## conditionMovementCoop                   0.83017    
    ## conditionMovementGuided                 0.03465 *  
    ## conditionSynchronous                    0.58463    
    ## conditionTurnTaking                     0.72650    
    ## conditionConversation:HR_self:type0     < 2e-16 ***
    ## conditionMovementCoop:HR_self:type0          NA    
    ## conditionMovementGuided:HR_self:type0   < 2e-16 ***
    ## conditionSynchronous:HR_self:type0      < 2e-16 ***
    ## conditionTurnTaking:HR_self:type0       < 2e-16 ***
    ## conditionConversation:HR_self:type1     < 2e-16 ***
    ## conditionMovementCoop:HR_self:type1     < 2e-16 ***
    ## conditionMovementGuided:HR_self:type1   < 2e-16 ***
    ## conditionSynchronous:HR_self:type1      < 2e-16 ***
    ## conditionTurnTaking:HR_self:type1       < 2e-16 ***
    ## conditionConversation:HR_other:type0    0.60601    
    ## conditionMovementCoop:HR_other:type0         NA    
    ## conditionMovementGuided:HR_other:type0  0.59022    
    ## conditionSynchronous:HR_other:type0     0.87009    
    ## conditionTurnTaking:HR_other:type0      0.00178 ** 
    ## conditionConversation:HR_other:type1    0.00983 ** 
    ## conditionMovementCoop:HR_other:type1    0.12012    
    ## conditionMovementGuided:HR_other:type1  0.64895    
    ## conditionSynchronous:HR_other:type1     0.49772    
    ## conditionTurnTaking:HR_other:type1      0.07031 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.6331 on 74607 degrees of freedom
    ##   (4858 observations deleted due to missingness)
    ## Multiple R-squared:  0.139,  Adjusted R-squared:  0.1388 
    ## F-statistic: 523.9 on 23 and 74607 DF,  p-value: < 2.2e-16

``` r
summ(model_5, digits=3)
```

    ## MODEL INFO:
    ## Observations: 74630 (4858 missing obs. deleted)
    ## Dependent Variable: HR_change
    ## Type: OLS linear regression 
    ## 
    ## MODEL FIT:
    ## F(23,74607) = 523.851, p = 0.000
    ## R² = 0.139
    ## Adj. R² = 0.139 
    ## 
    ## Standard errors: OLS
    ## -----------------------------------------------------------------------
    ##                                                  Est.    S.E.    t val.
    ## -------------------------------------------- -------- ------- ---------
    ## conditionConversation                           0.000   0.004     0.040
    ## conditionMovementCoop                          -0.003   0.016    -0.214
    ## conditionMovementGuided                         0.012   0.006     2.112
    ## conditionSynchronous                            0.002   0.005     0.547
    ## conditionTurnTaking                             0.002   0.005     0.350
    ## conditionConversation:HR_self:type0            -0.275   0.005   -55.360
    ## conditionMovementCoop:HR_self:type0                                    
    ## conditionMovementGuided:HR_self:type0          -0.256   0.007   -38.814
    ## conditionSynchronous:HR_self:type0             -0.299   0.005   -56.071
    ## conditionTurnTaking:HR_self:type0              -0.267   0.005   -50.901
    ## conditionConversation:HR_self:type1            -0.278   0.013   -20.920
    ## conditionMovementCoop:HR_self:type1            -0.231   0.018   -12.780
    ## conditionMovementGuided:HR_self:type1          -0.262   0.016   -16.113
    ## conditionSynchronous:HR_self:type1             -0.300   0.014   -21.787
    ## conditionTurnTaking:HR_self:type1              -0.268   0.013   -19.975
    ## conditionConversation:HR_other:type0            0.003   0.005     0.516
    ## conditionMovementCoop:HR_other:type0                                   
    ## conditionMovementGuided:HR_other:type0          0.004   0.007     0.539
    ## conditionSynchronous:HR_other:type0             0.001   0.005     0.164
    ## conditionTurnTaking:HR_other:type0             -0.016   0.005    -3.125
    ## conditionConversation:HR_other:type1            0.034   0.013     2.582
    ## conditionMovementCoop:HR_other:type1            0.028   0.018     1.554
    ## conditionMovementGuided:HR_other:type1          0.007   0.016     0.455
    ## conditionSynchronous:HR_other:type1             0.009   0.014     0.678
    ## conditionTurnTaking:HR_other:type1             -0.024   0.013    -1.810
    ## -----------------------------------------------------------------------
    ##  
    ## ----------------------------------------------------
    ##                                                    p
    ## -------------------------------------------- -------
    ## conditionConversation                          0.968
    ## conditionMovementCoop                          0.830
    ## conditionMovementGuided                        0.035
    ## conditionSynchronous                           0.585
    ## conditionTurnTaking                            0.727
    ## conditionConversation:HR_self:type0            0.000
    ## conditionMovementCoop:HR_self:type0                 
    ## conditionMovementGuided:HR_self:type0          0.000
    ## conditionSynchronous:HR_self:type0             0.000
    ## conditionTurnTaking:HR_self:type0              0.000
    ## conditionConversation:HR_self:type1            0.000
    ## conditionMovementCoop:HR_self:type1            0.000
    ## conditionMovementGuided:HR_self:type1          0.000
    ## conditionSynchronous:HR_self:type1             0.000
    ## conditionTurnTaking:HR_self:type1              0.000
    ## conditionConversation:HR_other:type0           0.606
    ## conditionMovementCoop:HR_other:type0                
    ## conditionMovementGuided:HR_other:type0         0.590
    ## conditionSynchronous:HR_other:type0            0.870
    ## conditionTurnTaking:HR_other:type0             0.002
    ## conditionConversation:HR_other:type1           0.010
    ## conditionMovementCoop:HR_other:type1           0.120
    ## conditionMovementGuided:HR_other:type1         0.649
    ## conditionSynchronous:HR_other:type1            0.498
    ## conditionTurnTaking:HR_other:type1             0.070
    ## ----------------------------------------------------

### Effects of respiration coordination on heart rate coordination

-   describe how you would test those.
-   Optional: run the models and report them
