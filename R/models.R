#' HC-robust confidence interval
#'
#' Computes confidence intervals using the sandwich package
#' @param object fitted model compatible with the sandwich package
#' @export
confint_robust <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
        pct))
    ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}

#' Robust VarCov
#'
#' @param models a list of models
#' @export
RobustVarCov = function(models, fun=NULL){
    bad = NULL
    vcov = list()
    if(is.null(fun)){
        fun = sandwich::vcovHC
    }
    for(i in seq_along(models)){
        tmp = try(fun(models[[i]]), silent=TRUE)
        if(class(tmp) == 'try-error'){
            tmp = vcov(models[[i]])
            bad = c(bad, i)
        }
        vcov[[i]] = tmp
    }
    if(!is.null(bad)){
        bad = paste(bad, collapse=', ')
        msg = paste('Could not compute robust covariance matrix for model(s):', bad, '\n')
        warning(msg)
    }
    return(vcov)
}

#' Saves (multiple) LaTeX tables to file
#' 
#' @param nmod maximum number of models (columns) per table
#' @param varcov a list of variance covariance matrices of length length(models)
#' @param output txt, html, or tex
#' @export
tables = function(models, 
                  filename = NULL, 
                  label = '', 
                  caption = '',
                  output = 'latex',
                  varcov = NULL, 
                  nmod = 6,  
                  digits = 3, 
                  stars = NULL, 
                  custom.coef.map = NULL,
                  include.loglik = TRUE, 
                  include.aic = FALSE, 
                  include.bic = FALSE,
                  include.deviance = FALSE, 
                  use.packages = FALSE, 
                  warn = TRUE,
                  ...){

    GetP = function(i) lmtest::coeftest(models[[i]], vcov=varcov[[i]])[, 4]
    GetSE = function(i) sqrt(diag(varcov[[i]]))
    idx = split(seq_along(models), ceiling(seq_along(models) / nmod))
    for(i in seq_along(idx)){
        # Uncertainty
        if(is.null(varcov)){
            se = FALSE
            pvalues = FALSE
        }else{
            se = lapply(idx[[i]], GetSE)
            pvalues = lapply(idx[[i]], GetP)
        }
        # Labels and captions
        if(length(idx) > 1){
            caption_tmp = paste0(caption, ' (', i, ' of ', length(idx), ')')
            label_tmp = paste0(label, i)
            file_tmp = paste0(filename, i, '.tex')
        }else{
            caption_tmp = caption
            label_tmp = label
            file_tmp = paste0(filename, '.tex')
        }
        # LaTeX production
        if (output == 'tex') {
            f = texreg
        } else if (output == 'txt') {
            file_tmp = gsub('tex$', 'txt', file_tmp)
            f = screenreg
        } else if (output == 'html') {
            file_tmp = gsub('tex$', 'html', file_tmp)
            f = htmlreg
        }
        if (is.null(filename)) {
            file_tmp = NULL
        }
        tab = f(models[idx[[i]]],
          label = label_tmp,
          caption = caption_tmp,
          custom.coef.map = custom.coef.map,
          override.se = se,
          override.pvalues = pvalues,
          digits = digits, 
          stars = stars, 
          include.loglik = include.loglik,
          include.aic = include.aic,
          include.bic = include.bic,
          include.deviance = include.deviance,
          caption.above=TRUE, 
          ...)
        if (is.null(filename)) {
            cat(tab, '\n')
        } else {
            cat(tab, file = file_tmp) 
        }
        # Warn if coefficients were omitted by custom.coef.map
        if(!is.null(custom.coef.map) & warn){
            coefficients = unique(unlist(sapply(models, function(k) names(coef(k)))))
            coefficients = coefficients[!coefficients %in% names(custom.coef.map)]
            if(length(coefficients) > 0) {
                warning('The following coefficients were omitted from the table: ', paste(coefficients, collapse=', ')) 
            }
        }
    }
}
