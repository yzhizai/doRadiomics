#' Compare two model
#'
#' @param obj1 an Radiomics.out or Nomogram.out object
#' @param obj2 an Radiomics.out or Nomogram.out object
#' @param fpath an output pptx file path
#'
#' @return output the results in fpath
#' @export
#'
#' @examples
setGeneric('compare.model',
           def = function(obj1, obj2, fpath){
             standardGeneric('compare.model')
           })

setMethod('compare.model', signature = signature(obj1 = c('Radiomics.out'),
                                                 obj2 = c('Radiomics.out')),
          definition = function(obj1, obj2, fpath){
            if(file.exists(fpath))
            {
              pptx <- read_pptx(path = fpath)
            }
            else
            {
              pptx <- read_pptx()
            }

            p.roc <- dml(code = {
              plot(obj1@iROC, legacy.axes = F, col = 'red',
                   print.auc = T)
              plot(obj2@iROC, add = T, col = 'blue',
                   print.auc = T,
                   print.auc.y = 0.45)
            })

            pred.1 <- ifelse(obj1@iROC$predictor > obj1@cmat$threshold[1], 0.9, 0.1)
            pred.2 <- ifelse(obj2@iROC$predictor > obj2@cmat$threshold[1], 0.9, 0.1)




            dt.idi.nri <- data.frame(Label = as.numeric(obj1@iROC$response)-1,
                                     pred.1 = pred.1,
                                     pred.2 = pred.2)


            idi.nri.out <- capture.output(reclassification(data = dt.idi.nri,
                                         cOutcome = 1,
                                         predrisk1 = pred.1,
                                         predrisk2 = pred.2,
                                         cutoff = c(0, 0.5, 1)))
            cmat <- bind_rows(obj1@cmat, obj2@cmat) %>%
              add_column(Model = c('Obj1', 'Obj2'), .before = 1)

            pptx <- add_slide(pptx, layout = 'Two Content')
            ph_with(pptx, value = p.roc, location = ph_location_left())
            ph_with(pptx, value = idi.nri.out,
                    location = ph_location_right())

            pptx <- add_slide(pptx, layout = 'Title and Content')
            ph_with(pptx, value = cmat,
                    location = ph_location_type(type = 'body'))

            print(pptx, target = fpath)
          })
