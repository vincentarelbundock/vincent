#' Download and clean WDI data
#'
#' @param dictionary (named character vector)
#' @export
#' @examples
#' GetWDI(dictionary=c("NY.GDP.MKTP.CD.XD"='deflator', "NY.GDP.PCAP.KD"='gdppc'))
GetWDI = function(dictionary=c("NY.GDP.PCAP.KD"='gdppc')){
    dat = WDI(indicator=names(dictionary), start=1960, end=2015, extra=TRUE)
    dictionary = dictionary[names(dictionary) %in% colnames(dat)]
    dat = dat[, c('iso3c', 'country', 'income', 'region', 'year', names(dictionary))]
    colnames(dat) = c('iso3c', 'country', 'income', 'region', 'year', dictionary)
    dat = dat[dat$region != 'Aggregates',]
    dat$region = gsub(' \\(.*', '', dat$region)
    dat$iso3n = countrycode(dat$iso3c, 'iso3c', 'iso3n')
    dat = vincent::sort_df(dat, c('iso3c', 'iso3n', 'country', 'region', 'year', 'income'))
    dat = dat[!is.na(dat$country),]
    return(dat)
}

#' Find best fuzzy matches for each element of a vector with itself
#'
#' @param x strings to be matched (character)
#' @param y strings to match to (character)
#' @param matches how many strings to return? (integer)
#' @export
ClosestString = function(x, y, n=1){
    dist = adist(x, y)
    f = function(i) rank(i, ties.method='first')
    g = function(i) y[order(i)]
    dist = t(apply(dist, 1, f))
    dist = t(apply(dist, 1, g))
    out = data.frame(x, dist)
    lb = ifelse(identical(x, y), 2, 1)
    ub = min(lb + n, ncol(out))
    out = out[, lb:ub]
    colnames(out) = c('source', paste('matches', 1:(ncol(out)-1), sep='_'))
    return(out)
}

#' Clean a data.frame
#'
#' Sorts rows, cleans columnames, alphabetizes columns, remove empty rows and
#' columns of a data frame
#' @param data data.frame to sort
#' @param index sorting columns (character vector)
#' @keywords sort data frame
#' @export
#' @examples
#' sort_df(dat, c('iso3c_host', 'iso3c_home'))
clean_df = function(data, index=NULL) {
    out = data %>%
          clean_names %>% # janitor
          remove_empty_cols # janitor
    if(!is.null(index)){
        for(i in index){
            out = out[!is.na(out[, i]),]
        }
        out = out %>%
              arrange_(.dots=index) %>%
              group_by_(.dots=index) %>%
              remove_empty_rows %>% # janitor
              data.frame %>% 
              select_(.dots = c(index, noquote(order(names(.)))))
        dups = apply(out[, index], 1, paste, collapse='')
        dups = sum(duplicated(dups))
        if(dups > 0){
            warning(paste(dups, 'duplicate indices.'))
        }
    }
    return(out)
}

#' Write data frame to CSV file with date stamp
#'
#' Writes to a CSV file with date automatically appended to file name
#' @param data data.frame to write
#' @param file filename to write to
#' @param include row.names (boolean)
#' @export
#' @examples
#' write_df(dat, file="data/dataset")
write_df = function(data, file, row.names=FALSE) {
    today = gsub('-', '', Sys.Date())
    fn = paste(file, '_', today, '.csv', sep='')
    write.csv(data, file=fn, row.names=row.names)
}

#' Studentize a variable
#' @param x numeric vector to studentize
#' @export
studentize = function(x) {
    out = (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
    return(out)
}

#' Recode a variable to 0-1 interval
#' @param x numeric vector to studentize
#' @export
zero_one = function(x) {
    out = x - min(x, na.rm=TRUE)
    out = out / max(out, na.rm=TRUE)
    return(out)
}

#' Read last CSV
#'
#' Globs file names, sorts and reads the last file. Useful when CSV file names
#' have dates appended to them.
#' @param file beginning of file name -- no extension (character)
#' @export
#' @examples
#' read_last_csv('dataset')
read_last_csv = function(file, ...) {
    fn = Sys.glob(paste(file, '*.csv', sep=''))
    fn = fn[length(fn)]
    cat('\nLoading file: ', fn, '\n')
    out = read.csv(fn, stringsAsFactors=FALSE, ...)
    return(out)
}

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
#' @export
TeXTables = function(models, file, label, caption,
					 varcov=NULL, 
                     dict=NULL, 
					 nmod=6,  
                     digits=3, 
                     stars=NULL, 
				     omit.coef='iso3|region|president|year',
					 include.loglik=FALSE, 
                     include.aic=FALSE, 
                     include.bic=FALSE,
					 include.deviance=FALSE, 
                     use.packages=FALSE, 
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
            file_tmp = paste0(file, i, '.tex')
        }else{
            caption_tmp = caption
            label_tmp = label
            file_tmp = paste0(file, '.tex')
        }
        # LaTeX production
        tab = texreg(models[idx[[i]]],
                     label = label_tmp,
                     caption = caption_tmp,
                     override.se = se,
                     override.pvalues = pvalues,
                     digits = digits, 
                     stars = stars, 
                     include.loglik = include.loglik,
                     include.aic = include.aic,
                     include.bic = include.bic,
                     include.deviance = include.deviance,
                     omit.coef = omit.coef,
                     caption.above=TRUE, 
                     ...)
        if(!is.null(dict)){
            for(n in names(dict)){
                tab = gsub(n, dict[n], tab, fixed=FALSE)
            }
        }
        cat(tab, file=file_tmp)
    }
}
