
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
#> 载入需要的程辑包：tidyverse
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> √ ggplot2 3.3.2     √ purrr   0.3.4
#> √ tibble  3.0.4     √ dplyr   1.0.2
#> √ tidyr   1.1.2     √ stringr 1.4.0
#> √ readr   1.4.0     √ forcats 0.5.0
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> 载入需要的程辑包：glmnet
#> 载入需要的程辑包：Matrix
#> 
#> 载入程辑包：'Matrix'
#> The following objects are masked from 'package:tidyr':
#> 
#>     expand, pack, unpack
#> Loaded glmnet 4.0-2
#> 载入需要的程辑包：pROC
#> Type 'citation("pROC")' for a citation.
#> 
#> 载入程辑包：'pROC'
#> The following objects are masked from 'package:stats':
#> 
#>     cov, smooth, var
#> 载入需要的程辑包：caret
#> 载入需要的程辑包：lattice
#> 
#> 载入程辑包：'caret'
#> The following object is masked from 'package:purrr':
#> 
#>     lift
#> 载入需要的程辑包：mRMRe
#> 载入需要的程辑包：survival
#> 
#> 载入程辑包：'survival'
#> The following object is masked from 'package:caret':
#> 
#>     cluster
#> 载入需要的程辑包：igraph
#> 
#> 载入程辑包：'igraph'
#> The following objects are masked from 'package:dplyr':
#> 
#>     as_data_frame, groups, union
#> The following objects are masked from 'package:purrr':
#> 
#>     compose, simplify
#> The following object is masked from 'package:tidyr':
#> 
#>     crossing
#> The following object is masked from 'package:tibble':
#> 
#>     as_data_frame
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
#> 载入需要的程辑包：officer
#> 载入需要的程辑包：rvg
#> 载入需要的程辑包：Publish
#> 载入需要的程辑包：prodlim
#> 
#> 载入程辑包：'prodlim'
#> The following object is masked from 'package:igraph':
#> 
#>     neighborhood
#> 载入需要的程辑包：rms
#> 载入需要的程辑包：Hmisc
#> 载入需要的程辑包：Formula
#> 
#> 载入程辑包：'Hmisc'
#> The following objects are masked from 'package:dplyr':
#> 
#>     src, summarize
#> The following objects are masked from 'package:base':
#> 
#>     format.pval, units
#> 载入需要的程辑包：SparseM
#> 
#> 载入程辑包：'SparseM'
#> The following object is masked from 'package:base':
#> 
#>     backsolve
## basic example code
dt <- dataset.radiomics
model <- new('Radiomics')
model <- run.radiomics(model, dt)
# before run the figure.radiomics function, you should create a pptx file at first,
# and confirm the file is closed.
radi.out.train <- predict.radiomics(model, dt) %>% validate.radiomics()
#> Warning in Ops.ordered(left, right): '-' is not meaningful for ordered factors
#> Setting levels: control = 0, case = 1
#> Setting direction: controls < cases
figure.radiomics(model, 'output.pptx')



dt <- dataset.nomogram
nomogram.model <- new('Nomogram')
nomogram.model@uni_p_thresh <- 0.05
nomogram.model <- run.nomogram(nomogram.model, dt)
#> Start:  AIC=125.77
#> Label ~ Age + Zone + PSA + ProstateV + Radscore
#> 
#>             Df Deviance    AIC
#> - Age        1   115.36 125.36
#> <none>           113.77 125.77
#> - PSA        1   118.41 128.41
#> - ProstateV  1   128.62 138.62
#> - Zone       1   137.80 147.80
#> - Radscore   1   187.88 197.88
#> 
#> Step:  AIC=125.36
#> Label ~ Zone + PSA + ProstateV + Radscore
#> 
#>             Df Deviance    AIC
#> <none>           115.36 125.36
#> - PSA        1   119.61 127.61
#> - ProstateV  1   129.45 137.45
#> - Zone       1   140.26 148.26
#> - Radscore   1   201.90 209.90
#> Start:  AIC=125.36
#> Label ~ Zone + PSA + ProstateV + Radscore
#> 
#>             Df Deviance    AIC
#> <none>           115.36 125.36
#> - PSA        1   119.61 127.61
#> - ProstateV  1   129.45 137.45
#> - Zone       1   140.26 148.26
#> - Radscore   1   201.90 209.90
nomo.out.train <- predict.nomogram(nomogram.model, dt)
#>   Variable Units OddsRatio           CI.95    p-value 
#>       Zone            0.14     [0.06;0.34]    < 1e-04 
#>        PSA            1.03     [0.99;1.07]   0.094016 
#>  ProstateV            0.98     [0.96;0.99]   0.005265 
#>   Radscore          295.80 [60.28;1451.59]    < 1e-04

figure.nomogram(nomogram.model, dt, 'output.pptx')
```
