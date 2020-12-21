library(glmnet)
library(caret)
library(mRMRe)
library(pROC)
library(ModelGood)
library(tidyverse)
library(officer)
library(rvg)


setOldClass('preProcess')
setOldClass('lognet')
setOldClass('roc')
setOldClass('cv.glmnet')

#' A radiomics class
#'
#' @slot step_pre0 preProcess class
#' @slot step_pre1 preProcess class
#' @slot names.dt the feature names after nearZeroVar function
#' @slot useful_name the feature names of the final model
#' @slot fit a glmnet output fit model
#' @slot cvfit a cv.glmnet output fit model
#' @slot s the best lambda value
#'
#' @return
#' @export
#'
#' @examples
#' model <- new('Radiomics')
setClass('Radiomics',
         slots = c(step_pre0 = 'preProcess',
                   step_pre1 = 'preProcess',
                   names.dt = 'character',
                   useful_name = 'character',
                   fit = 'lognet',
                   cvfit = 'cv.glmnet',
                   s = 'numeric'
         )
)


#' a generic function to use radiomics model to predict
#'
#' @param object a Radiomics class
#' @param ... other argument
#'
#' @return
#' @export
#'
#' @examples
setGeneric('predict.radiomics',
           function(object, ...){
             standardGeneric('predict.radiomics')
           })


#' a function to predict the radscore.
#'
#' @param object a radiomics model
#' @param dt a data.frame with the same columns with the training set.
#'
#' @return a Label_Score class
#' @export
#'
#' @examples
setMethod('predict.radiomics', signature = 'Radiomics',
          function(object, dt){

            dt$Label <- factor(dt$Label, ordered = T)
            dt <- predict(object@step_pre0, dt)

            dt_radiomics <- select(dt, all_of(object@names.dt))

            dt_radiomics <- predict(object@step_pre1, dt_radiomics) %>%
              select(all_of(object@useful_name))

            radscore  <- predict(object@fit, newx = as.matrix(dt_radiomics),
                                 s = object@s) %>% c()

            out <- new('Label_Score')
            out@Label = dt$Label
            out@Score = radscore

            out
          })

#' Train model generic function
#'
#' @param object a Radiomics model
#' @param ... other arguments
#'
#' @return a updated Radiomics model
#' @export
#'
#' @examples
setGeneric('run.radiomics',
           function(object, ...){
             standardGeneric('run.radiomics')
           })


#' Train model function definition
#'
#' @param object a Radiomics model
#' @param dt the training dataset
#'
#' @return a updated Radiomics class
#' @export
#'
#' @examples
setMethod('run.radiomics', signature = 'Radiomics',
          function(object, dt){
            step_pre0 <- preProcess(dt, method = 'medianImpute')
            dt <- predict(step_pre0, dt)

            idx.nzv <- nearZeroVar(dt)
            if(!is_empty(idx.nzv))
            {
              dt <- dt[, -idx.nzv]
            }

            names.dt <- colnames(dt)

            step_pre1 <- preProcess(dt, method = c('center', 'scale'))
            dt_pre <- predict(step_pre1, dt) %>% as.data.frame


            dt_mrmr <- mRMR.data(dt_pre)
            f_sel <- mRMR.classic(data = dt_mrmr, target_indices = c(1), feature_count = 20)

            # browser()
            useful_name <- featureNames(f_sel)[unlist(solutions(f_sel))]

            dt_pre <- select(dt_pre, c('Label', useful_name))

            x <- as.matrix(dt_pre[, -1])
            y <- dt_pre$Label

            cv.fit <- cv.glmnet(x, y, family = 'binomial')
            fit <- glmnet(x, y, family = 'binomial')

            s <- cv.fit$lambda.min

            object@step_pre0 <- step_pre0
            object@step_pre1 <- step_pre1
            object@names.dt <- names.dt
            object@useful_name <- useful_name
            object@cvfit <- cv.fit
            object@fit <- fit
            object@s <- s

            object
          })

#' a plot generic function
#'
#' @param object a Label_Score class
#' @param ... other arguments
#'
#' @return output to a designated pptx file
#' @export
#'
#' @examples
setGeneric('figure.radiomics',
           def = function(object, ...){
             standardGeneric('figure.radiomics')
           })

#' Plot radiomics figures
#'
#' @param object Radiomics model
#' @param fpath an existing pptx file path
#'
#' @return no return.
#' @export
#'
#' @examples
setMethod('figure.radiomics', signature(object = 'Radiomics'),
          definition = function(object, fpath){
            pptx <- read_pptx()

            pptx <- add_slide(pptx)

            plt.glmnet <- dml(code = {
              oldpar <- par(mfrow = c(1, 2))

              plot(object@cvfit)

              plot(object@fit, s = object@s, xvar = 'lambda')
              abline(v = log(object@s), lty = 2)

              par(oldpar)
            })

            plt.coef <- dml(ggobj = {
              coefs <- coefficients(object@fit, s = object@s)
              useful_feature <- unlist(coefs@Dimnames)[coefs@i + 1]
              useful_feature <- useful_feature[-1]

              dt_coef <- data.frame(Feature = useful_feature, Coef = coefs@x[-1])
              dt_coef <- arrange(dt_coef, desc(Coef))
              dt_coef$Feature <- factor(dt_coef$Feature,
                                        levels = as.character(dt_coef$Feature))

              p_coef <- ggplot(aes(x = Feature, y = Coef), data = dt_coef)
              p_coef <- p_coef + geom_col(fill = 'blue', width = 0.7) + coord_flip() +
                theme_bw() + ylab('Coefficients')

              p_coef
            })

            ph_with(pptx, value = 'LASSO',
                    location = ph_location_type(type = 'title'))
            ph_with(pptx, value = plt.glmnet,
                    location = ph_location_type(type = 'body'))

            pptx <- add_slide(pptx)

            ph_with(pptx, value = 'Coefficients of LASSO',
                    location = ph_location_type(type = 'title'))
            ph_with(pptx, value = plt.coef, location = ph_location_type(type = 'body'))

            print(pptx, target = fpath)
          })

#' a class output by run.radiomics
#'
#' @slot Label the referred label
#' @slot Score the radscore calculated by the model
#' @slot iROC roc curve
#' @slot cmat the metric of the model
#'
#' @return a 'Label_Score' class
#' @export
#'
#' @examples
setClass('Label_Score', slots = c(Label = 'factor',
                                  Score = 'numeric',
                                  iROC = 'roc',
                                  cmat = 'data.frame'))

#' a validate generic function
#'
#' @param object a Label_Score object
#'
#' @return an updated Label_Score object
#' @export
#'
#' @examples
setGeneric('validate.radiomics',
           def = function(object){
             standardGeneric('validate.radiomics')
           })

#' Function to generate the ROC curve and performance metrics
#'
#' @param object a Label_Score object
#'
#' @return an updated Label_Score object
#' @export
#'
#' @examples
setMethod('validate.radiomics', signature(object = 'Label_Score'),
          definition = function(object){
            iROC <- roc(object@Label, object@Score, ci = T)
            object@iROC <- iROC

            imat <- coords(iROC, x = 'best', transpose = F,
                           ret = c('threshold', 'accuracy', 'sensitivity',
                                   'specificity', 'ppv', 'npv'))

            knitr::kable(imat, format = 'simple', digits = 3)
            object@cmat <- imat

            object
          })
