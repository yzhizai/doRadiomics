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
           Label = 'numeric',
           Score = 'numeric',
           ulogit = 'data.frame',
           mlogit = 'data.frame'
         ))

#' Using the nomogram model to predict the nomogram score
#'
#' @param a nomogram object
#' @param ... the dataset used to predict
#'
#' @return a 'Nomogram.out' object
#' @export
#'
#' @examples
setGeneric('predict.nomogram',
           def = function(x, ...){
             standardGeneric('predict.nomogram')
           })

setMethod('predict.nomogram', signature(x = 'Nomogram'),
          definition = function(x, dt){

            res_ulogit <- glmSeries(Label~1, data = dt, vars = colnames(dt[, -1]), family = 'binomial')
            dt.final <- model.frame(x@fit, data = dt)

            res_mlogit <- glm(Label~., data = dt.final, family = 'binomial')

            nomo.score <- predict(x@fit, newdata = dt.final, type = 'response')

            nomo.out <- new('Nomogram.out',
                            Label = dt.final$Label,
                            Score = nomo.score,
                            ulogit = res_ulogit,
                            mlogit = publish(res_mlogit)$regressionTable)

            nomo.out
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
setGeneric('plot_nomogram',
           def = function(x, dt, fpath){
             standardGeneric('plot_nomogram')
           })

setMethod('plot_nomogram', signature(x = 'Nomogram'),
          definition = function(x, dt, fpath){

            dt.nom <- model.frame(x@fit, data = dt)
            ddist_train_com <<- datadist(dt.nom)
            options(datadist = 'ddist_train_com')
            mod_train <- lrm(Label~.,
                             data = dt.nom, x = TRUE, y = TRUE)

            nom_com <- nomogram(mod_train, lp = F, fun = plogis, fun.at = c(0.1, 0.4, 0.9),
                                funlabel = 'Risk')

            pptx <- read_pptx()
            pptx <- add_slide(pptx, layout = 'Title and Content')


            nom.figure <- dml(code = {plot(nom_com)})

            ph_with(pptx, value = 'Nomogram', location = ph_location_type(type = 'title'))
            ph_with(pptx, value = nom.figure, location = ph_location_type(type = 'body'))

            print(pptx, target = fpath)
          })
