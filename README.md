
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
dt <- dataset.radiomics

model <- new('Radiomics')

model <- run.radiomics(model, dt)

out.1 <- predict.radiomics(model, dt) %>% validate.radiomics()


dt <- dataset.nomogram

model <- new('Nomogram', uni_p_thresh = 0.05)
model <- run.nomogram(model, dt)

out.3 <- predict.nomogram(model, dt) %>% validate.nomogram()

compare.model(out.1, out.3, 'output.pptx')
```
