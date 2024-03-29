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
#' @slot threshold the model threshold
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
                   s = 'numeric',
                   threshold = 'numeric'
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
            out@threshold = object@threshold

            out
          })


#' Train radiomics model
#'
#' @param object a Radiomics object
#' @param dt the training dataset
#' @param num_feature the number of feature remained after mRMR
#'
#' @return a updated Radiomics object
#' @export
#'
#' @examples
setGeneric('run.radiomics',
           function(object, dt, num_feature){
             standardGeneric('run.radiomics')
           })
setMethod('run.radiomics', signature = 'Radiomics',
          function(object, dt, num_feature){
            dt$Label <- factor(dt$Label, ordered = T)
            step_pre0 <- preProcess(dt, method = 'medianImpute')
            dt <- predict(step_pre0, dt)

            idx.nzv <- nearZeroVar(dt)
            if(!is_empty(idx.nzv))
            {
              dt <- dt[, -idx.nzv]
            }

            u.form <- paste(colnames(dt)[1],
                            paste(colnames(dt)[-1], collapse = '+'),
                            sep = '~') %>% as.formula()
            u.test <- univariateTable(u.form, data = dt, show.totals = F, digits = 3)

            sel.names <- names(which(u.test$p.value < 0.1))

            dt <- select(dt, c('Label', sel.names))

            names.dt <- colnames(dt)

            step_pre1 <- preProcess(dt, method = c('center', 'scale'))
            dt_pre <- predict(step_pre1, dt) %>% as.data.frame


            dt_mrmr <- mRMR.data(dt_pre)
            f_sel <- mRMR.classic(data = dt_mrmr, target_indices = c(1), feature_count = num_feature)

            # browser()
            useful_name <- featureNames(f_sel)[unlist(solutions(f_sel))]
            useful_name <- setdiff(useful_name, 'Label')

            dt_pre <- select(dt_pre, c('Label', useful_name))

            x <- as.matrix(dt_pre[, -1])
            y <- dt_pre$Label

            cv.fit <- cv.glmnet(x, y, family = 'binomial')
            fit <- glmnet(x, y, family = 'binomial')

            s <- cv.fit$lambda.min

            score <- predict(fit, newx = x, s = s) %>% c()
            iroc <- roc(y, score)
            threshold <- coords(iroc, x = 'best', transpose = F,
                                ret = 'threshold')
            threshold <- threshold$threshold[1]

            object@step_pre0 <- step_pre0
            object@step_pre1 <- step_pre1
            object@names.dt <- names.dt
            object@useful_name <- useful_name
            object@cvfit <- cv.fit
            object@fit <- fit
            object@s <- s
            object@threshold <- threshold

            object
          })

#' cross-validation training using 100 LGOCV
#'
#' @param object a Radiomics object
#' @param dt a training dataset
#' @param num_feature  the number of feature remained after mRMR
#'
#' @return
#' @export
#'
#' @examples
setGeneric('cv.run.radiomics',
           function(object, dt, num_feature){
             standardGeneric('cv.run.radiomics')
           })
setMethod('cv.run.radiomics', signature = 'Radiomics',
          function(object, dt, num_feature){
            dt$Label <- factor(dt$Label, ordered = T)
            step_pre0 <- preProcess(dt, method = 'medianImpute')
            dt <- predict(step_pre0, dt)

            idx.nzv <- nearZeroVar(dt)
            if(!is_empty(idx.nzv))
            {
              dt <- dt[, -idx.nzv]
            }

            u.form <- paste(colnames(dt)[1],
                            paste(colnames(dt)[-1], collapse = '+'),
                            sep = '~') %>% as.formula()
            u.test <- univariateTable(u.form, data = dt, show.totals = F, digits = 3)

            sel.names <- names(which(u.test$p.value < 0.05))

            dt <- select(dt, c('Label', sel.names))

            names.dt <- colnames(dt)

            step_pre1 <- preProcess(dt, method = c('center', 'scale'))
            dt_pre <- predict(step_pre1, dt) %>% as.data.frame

            auc.list <- list()
            s.list <- list()
            fit.list <- list()
            cvfit.list <- list()
            dt.list <- list()
            for(i_a in 1:100)
            {
              idx.train <- createDataPartition(dt_pre$Label, p = 0.6, list = F) %>% c()
              dt.train <- dt_pre[idx.train, ]
              dt.test <- dt_pre[-idx.train, ]

              dt_mrmr <- mRMR.data(dt.train)
              f_sel <- mRMR.classic(data = dt_mrmr, target_indices = c(1), feature_count = num_feature)

              # browser()
              useful_name <- featureNames(f_sel)[unlist(solutions(f_sel))]

              dt.train <- select(dt.train, c('Label', useful_name))
              dt.test <- select(dt.test, c('Label', useful_name))

              dt.final <- bind_rows(dt.train, dt.test)

              x <- as.matrix(dt.train[, -1])
              y <- dt.train$Label

              x.test <- as.matrix(dt.test[, -1])
              y.test <- dt.test$Label
              cv.fit <- cv.glmnet(x, y, family = 'binomial')
              fit <- glmnet(x, y, family = 'binomial')

              s <- cv.fit$lambda.min


              score.train <- predict(fit, newx = x, s = s)
              iroc.train <- roc(y, score.train)

              score.test <- predict(fit, newx = x.test, s = s)
              iroc.test <- roc(y.test, score.test)

              if(identical(iroc.train$direction, iroc.test$direction))
              {
                auc.list[[i_a]] <- auc(iroc.test)
              } else
              {
                auc.list[[i_a]] <- 0
              }

              auc.list[[i_a]] <- auc(iroc.test)
              s.list[[i_a]] <- s
              fit.list[[i_a]] <- fit
              cvfit.list[[i_a]] <- cv.fit
              dt.list[[i_a]] <- dt.final
            }

            idx.final <- which.max(unlist(auc.list))
            s <- s.list[[idx.final]]
            fit <- fit.list[[idx.final]]
            cv.fit <- cvfit.list[[idx.final]]
            dt.final <- dt.list[[idx.final]]
            x <- as.matrix(dt.final[, -1])
            y <- dt.final$Label

            score <- predict(fit, newx = x, s = s) %>% c()
            iroc <- roc(y, score)
            threshold <- coords(iroc, x = 'best', transpose = F,
                                ret = 'threshold')
            threshold <- threshold$threshold[1]

            object@step_pre0 <- step_pre0
            object@step_pre1 <- step_pre1
            object@names.dt <- names.dt
            object@useful_name <- useful_name
            object@cvfit <- cv.fit
            object@fit <- fit
            object@s <- s
            object@threshold <- threshold

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

            pptx <- add_slide(pptx)

            ph_with(pptx, value = paste(dt_coef$Coef, dt_coef$Feature, sep = '*') %>% paste(collapse = '+') %>% paste(coefs@x[1], sep = '+'),
                    location = ph_location_type(type = 'body'))

            print(pptx, target = fpath)
          })

#' a class output by run.radiomics
#'
#' @slot Label the referred label
#' @slot Score the radscore calculated by the model
#' @slot iROC roc curve
#' @slot cmat the metric of the model
#' @slot threshold model threshold
#'
#' @return a 'Radiomics.out' class
#' @export
#'
#' @examples
setClass('Radiomics.out', slots = c(Label = 'factor',
                                    Score = 'numeric',
                                    iROC = 'roc',
                                    cmat = 'data.frame',
                                    threshold = 'numeric'),
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

            imat <- coords(iROC, x = object@threshold, transpose = F,
                           input = 'threshold',
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
#' @slot threshold model threshold
#'
#' @return a nomogram object
#' @export
#'
#' @examples
setClass('Nomogram',
         slots = c(uni_p_thresh = 'numeric',
                   fit = c('glm'),
                   formula = 'character',
                   threshold = 'numeric'
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

            score <- predict(fit, type = 'response')

            iroc <- roc(dt$Label, score)
            threshold <- coords(iroc, x = 'best', transpose = F,
                                ret = 'threshold')
            threshold <- threshold$threshold[1]

            coef_nom <- coef(fit)
            nomo_score <- paste('Nomoscore =',
                                paste(names(coef_nom), coef_nom, sep = '*') %>% paste(collapse = '+'))

            x@formula <- nomo_score
            x@fit <- fit
            x@threshold <- threshold

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
                            mlogit = publish(res_mlogit)$regressionTable,
                            threshold= x@threshold)

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

            imat <- coords(iROC, x = object@threshold, transpose = F,
                           input = 'threshold',
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
#' @param fun.at function values to label on axis
#'
#' @export
#'
#' @examples
setGeneric('figure.nomogram',
           def = function(x, dt, fpath, fun.at){
             standardGeneric('figure.nomogram')
           })

setMethod('figure.nomogram', signature(x = 'Nomogram'),
          definition = function(x, dt, fpath, fun.at){

            dt.nom <- model.frame(x@fit, data = dt)
            ddist_train_com <<- datadist(dt.nom)
            options(datadist = 'ddist_train_com')
            mod_train <- lrm(Label~.,
                             data = dt.nom, x = TRUE, y = TRUE)

            nom_com <- nomogram(mod_train, lp = F, fun = plogis, fun.at = fun.at,
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

            pptx <- add_slide(pptx)
            ph_with(pptx, value = publish(x@fit) %>% pluck('regressionTable'),
                    location = ph_location_type(type = 'body'))

            print(pptx, target = fpath)
          })
