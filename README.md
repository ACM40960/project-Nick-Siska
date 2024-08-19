# An Analysis of Depression

Depression is defined as a mood disorder that is characterised by persistent feelings of sadness and hopelessness. According to the World Health Organisation, around 280 million people live with depression. It causes severe symptoms that affect how you feel, think, and handle daily activities. Many people who suffer from depression report disrupted sleep, lack of concentration, and thoughts of suicide. The cause of depression is complex and can be due to several psychological, biological, and social factors.

<p align="center">
  <img src="images/mean_pre_drep_g.png" alt="Image 1" width="300"/>
  <img src="images/male_dep_g.png" alt="Image 2" width="300"/>
</p>

# Purpose 
This repository contains R code that analyzes two datasets pertaining to depression. The first dataset was used to analyze which factors are influential in predicting depression, and the second dataset was used to analyze the effects of exercise as a treatment for depression.

<p align="center">
  <img src="images/class_count_g.png" alt="Image 3" width="300"/>
  <img src="images/mean_diff_g.png" alt="Image 4" width="300"/>
</p>

# Code and File Descriptions

There are two files that contain R code for the analysis. The R script called Depression_Analysis_Complete.R is the main file and contains all necessary calculations for the analysis. The second file is an R Markdown file called "An Analysis of Depression.Rmd", which contains the code to create the PDF for the academic poster. The second file is dependent on the main script Depression_Analysis_Complete.R.

### Depression_Analysis_Complete.R

```r
# Script for analyzing depression
# Loading packages 
library(data.table) # Fread and data.table functionality
library(tidyverse) # Multiple packages including dplyr
library(kableExtra) # Output tables
library(caret) # Cross-validation
library(ggplot2) # Plots
library(ggthemr) # Theme of ggplots
library(rcompanion) # Cramer v test
library(randomForest) # Random forest model
library(pROC) # ROC plots
library(rcompanion)
library(DescTools)

# Function for applying chi-squared test
apply_chi_cramer = function(pairs,dt){
  # column_pairs : Names of categorical columns
  # dt: Data table that is passed
  
  # Getting the first and second categorical columns
  v1 = pairs[1]
  v2 = pairs[2]
  
  # Creating a table with frequencies
  tbl =  table(dt[[v1]],
               dt[[v2]])
  
```

### An Analysis of Depression.Rmd

```r
title: An Analysis of Depression
author:
  - name: Nicolas Siska
affiliation:
    address: School of Mathematics and Statistics
column_numbers: 4
logoright_name: UCDlogo.png
logoleft_name: UCDlogo.png
output: 
  posterdown::posterdown_html:
    self_contained: TRUE 
    css: mytheme.css 
poster_width: 84.1cm
poster_height: 118.9cm
bibliography: packages.bib
knit: pagedown::chrome_print
  
```

### Folders

* The folder called "data" contains the two datasets used in the analysis and saved models from the R script  Depression_Analysis_Complete.R.
* The images folder contains plots used in this README file.
*HTML and PDF folder contains the outputs of the "An Analysis of Depression.Rmd" script.
	
### CSS and BibTex Files

* mythem.css contains css code for the colors and size of the font for the academic poster.
* packages.bib is a bibliography file format used for managing references containing citation entries for various R packages.
 
 
 