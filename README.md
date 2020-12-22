
<!-- README.md is generated from README.Rmd. Please edit that file -->

# doRadiomics

<!-- badges: start -->

<!-- badges: end -->

The goal of doRadiomics is to do radiomics analysis.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("yzhizai/doRadiomics")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(doRadiomics)
## basic example code
dt <- read_csv('dataset_radiomics.csv')
model <- new('Radiomics')
model <- run.radiomics(model, dt)
# before run the figure.radiomics function, you should create a pptx file at first,
# and confirm the file is closed.
radi.out.train <- predict.radiomics(model, dt) %>% validate.radiomics()
figure.radiomics(model, 'output.pptx')



dt <- read_csv('data_clinics.csv')
nomogram.model <- new('Nomogram')
nomogram.model@uni_p_thresh <- 0.05
nomogram.model <- run.nomogram(nomogram.model, dt)
nomo.out.train <- predict.nomogram(nomogram.model, dt)

figure.nomogram(nomogram.model, dt, 'output.pptx')
```
