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



#' A function to predict the radscore
#'
#' @param object a Radiomics object
#' @param dt the dataset
#'
#' @return a Radiomics.out object
#' @export
#'
#' @examples
setGeneric('predict.radiomics',
           function(object, dt){
             standardGeneric('predict.radiomics')
           })

setMethod('predict.radiomics', signature = 'Radiomics',
          function(object, dt){

            dt$Label <- factor(dt$Label, ordered = T)
            dt <- predict(object@step_pre0, dt)

            dt_radiomics <- select(dt, all_of(object@names.dt))

            dt_radiomics <- predict(object@step_pre1, dt_radiomics) %>%
              select(all_of(object@useful_name))

            radscore  <- predict(object@fit, newx = as.matrix(dt_radiomics),
                                 s = object@s) %>% c()

            out <- new('Radiomics.out')
            out@Label = dt$Label
            out@Score = radscore

            out
          })


#' Train radiomics model
#'
#' @param object a Radiomics object
#' @param dt the training dataset
#'
#' @return a updated Radiomics object
#' @export
#'
#' @examples
setGeneric('run.radiomics',
           function(object, dt){
             standardGeneric('run.radiomics')
           })
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

#' Plot radiomics figures
#'
#' @param object a Radiomics object
#' @param fpath a pptx file path
#'
#' @return
#' @export
#'
#' @examples
setGeneric('figure.radiomics',
           def = function(object, fpath){
             standardGeneric('figure.radiomics')
           })
setMethod('figure.radiomics', signature(object = 'Radiomics'),
          definition = function(object, fpath){

            if(file.exists(fpath))
            {
              pptx <- read_pptx(path = fpath)
            }
            else
            {
              pptx <- read_pptx()
            }

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
#' @return a 'Radiomics.out' class
#' @export
#'
#' @examples
setClass('Radiomics.out', slots = c(Label = 'factor',
                                    Score = 'numeric',
                                    iROC = 'roc',
                                    cmat = 'data.frame'),
         prototype = prototype(iROC = structure(list(),
                                                class = 'roc')))

#' Function to generate the ROC curve and performance metrics
#'
#' @param object a Radiomics.out object
#'
#' @return an updated Radiomics.out object
#' @export
#'
#' @examples
setGeneric('validate.radiomics',
           def = function(object){
             standardGeneric('validate.radiomics')
           })
setMethod('validate.radiomics', signature(object = 'Radiomics.out'),
          definition = function(object){
            iROC <- roc(object@Label, object@Score, ci = T)
            object@iROC <- iROC

            imat <- coords(iROC, x = 'best', transpose = F,
                           ret = c('threshold', 'accuracy', 'sensitivity',
                                   'specificity', 'ppv', 'npv'))

            object@cmat <- imat

            object
          })


setOldClass('glm')

#' Nomogram class
#'
#' @slot uni_p_thresh the threshold determined for selecting the features
#' @slot fit the model of nomogram
#' @slot formula the formula of the Nomoscore
#'
#' @return a nomogram object
#' @export
#'
#' @examples
setClass('Nomogram',
         slots = c(uni_p_thresh = 'numeric',
                   fit = c('glm'),
                   formula = 'character'
         ), prototype = prototype(fit = structure(list(), class = 'glm')))

#' Train nomogram model
#'
#' @param x a nomogram object
#' @param dt the training dataset
#'
#' @return an updated nomogram object
#' @export
#'
#' @examples
setGeneric('run.nomogram',
           def = function(x, dt){
             standardGeneric('run.nomogram')
           })

setMethod('run.nomogram', signature(x = 'Nomogram', dt = 'data.frame'),
          definition = function(x, dt){

            res_ulogit <- glmSeries(Label~1, data = dt, vars = colnames(dt[, -1]), family = 'binomial')
            cli_name <- res_ulogit$Variable[which(res_ulogit$Pvalue < x@uni_p_thresh)]

            dt_2 <- select(dt, all_of(c('Label', cli_name)))

            pvals_ulogit <- res_ulogit$Pvalue
            names(pvals_ulogit) <- res_ulogit$Variable
            log_fit <- glm(Label~., data = dt_2, family = binomial) %>% step()
            vif_val <- vif(log_fit)
            vif_idx <- which(vif_val > 5)

            dt_i <- model.frame(log_fit, dt_2)
            while(!is_empty(vif_idx))
            {
              vif_name <- names(vif_idx)
              excl_name <- which.max(pvals_ulogit[vif_name]) %>% names()
              dt_i <- select(dt_i, -c(excl_name))
              log_fit <- glm(Label~., data = dt_i, family = binomial)
              vif_val <- vif(log_fit)
              vif_idx <- which(vif_val > 5)
            }
            fit <- step(log_fit)

            coef_nom <- coef(fit)
            nomo_score <- paste('Nomoscore =',
                                paste(names(coef_nom), coef_nom, sep = '*') %>% paste(collapse = '+'))

            x@formula <- nomo_score
            x@fit <- fit

            x
          })


#' A nomogram output class
#'
#' @slot Label the Label
#' @slot Score the nomogram score
#' @slot ulogit the univariate logistic regression output
#' @slot mlogit the multivariate logistic regression output
#'
#' @return
#' @export
#'
#' @examples
setClass('Nomogram.out',
         slots = c(
           ulogit = 'data.frame',
           mlogit = 'data.frame'
         ), contains = 'Radiomics.out')

#' Using the nomogram model to predict the nomogram score
#'
#' @param x a Nomogram object
#' @param dt the dataset
#'
#' @return a 'Nomogram.out' object
#' @export
#'
#' @examples
setGeneric('predict.nomogram',
           def = function(x, dt){
             standardGeneric('predict.nomogram')
           })

setMethod('predict.nomogram', signature(x = 'Nomogram'),
          definition = function(x, dt){

            res_ulogit <- glmSeries(Label~1, data = dt, vars = colnames(dt[, -1]), family = 'binomial')
            dt.final <- model.frame(x@fit, data = dt)

            res_mlogit <- glm(Label~., data = dt.final, family = 'binomial')

            nomo.score <- predict(x@fit, newdata = dt.final, type = 'response')

            nomo.out <- new('Nomogram.out',
                            Label = as.factor(dt.final$Label),
                            Score = nomo.score,
                            ulogit = res_ulogit,
                            mlogit = publish(res_mlogit)$regressionTable)

            nomo.out
          })

#' validate the nomogram output
#'
#' @param object an Nomogram.out object
#' @param ...
#'
#' @return an updated Nomogram.out object
#' @export
#'
#' @examples
setGeneric('validate.nomogram',
           def = function(object, ...)
           {standardGeneric('validate.nomogram')})

setMethod('validate.nomogram', signature = signature(object = 'Nomogram.out'),
          definition = function(object)
          {
            iROC <- roc(object@Label, object@Score, ci = T)
            object@iROC <- iROC

            imat <- coords(iROC, x = 'best', transpose = F,
                           ret = c('threshold', 'accuracy', 'sensitivity',
                                   'specificity', 'ppv', 'npv'))

            object@cmat <- imat

            object
          })

#' The function to plot nomogram
#'
#' @param x a nomogram object
#' @param dt the training dataset
#' @param fpath the output pptx file
#'
#' @export
#'
#' @examples
setGeneric('figure.nomogram',
           def = function(x, dt, fpath){
             standardGeneric('figure.nomogram')
           })

setMethod('figure.nomogram', signature(x = 'Nomogram'),
          definition = function(x, dt, fpath){

            dt.nom <- model.frame(x@fit, data = dt)
            ddist_train_com <<- datadist(dt.nom)
            options(datadist = 'ddist_train_com')
            mod_train <- lrm(Label~.,
                             data = dt.nom, x = TRUE, y = TRUE)

            nom_com <- nomogram(mod_train, lp = F, fun = plogis, fun.at = c(0.1, 0.4, 0.9),
                                funlabel = 'Risk')

            if(file.exists(fpath))
            {
              pptx <- read_pptx(path = fpath)
            }
            else
            {
              pptx <- read_pptx()
            }
            pptx <- add_slide(pptx, layout = 'Title and Content')


            nom.figure <- dml(code = {plot(nom_com)})

            ph_with(pptx, value = 'Nomogram', location = ph_location_type(type = 'title'))
            ph_with(pptx, value = nom.figure, location = ph_location_type(type = 'body'))

            print(pptx, target = fpath)
          })
