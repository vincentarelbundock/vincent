#' Find best fuzzy matches for each element of a vector with itself
#'
#' @param x strings to be matched (character)
#' @param y strings to match to (character)
#' @param n how many strings to return? (integer)
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
#' @param path stem to read from (without date or .tsv extension)
#' @export
#' @examples
#' read_last_tsv('dataset')
read_last_tsv = function(path) {
    fn = Sys.glob(paste(path, '*.tsv', sep=''))
    fn = fn[length(fn)]
    out = vroom::vroom(fn)
    return(out)
}
