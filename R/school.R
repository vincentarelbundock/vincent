#' Normalize grades by 2nd/3rd quartiles
#'
#' @param data (data.frame with 3 columns: 'id', 'note', 'correcteur')
#' @param plot (boolean plot or return raw data)
#' @export
NormalizeGrades = function(data, plot=FALSE){
    if(ncol(data) != 3){
        stop('Wrong number of columns')
    }
    if(any(colnames(data) != c('id', 'note', 'correcteur'))){
        stop('Bad column names')
    }
    data = data %>% filter(!is.na(note))
    # find larger quartiles
    tar = data %>% na.omit %>% group_by(correcteur) %>% 
          summarise(q25 = quantile(note, probs=.25, na.rm=TRUE),
                    q75 = quantile(note, probs=.75, na.rm=TRUE)) %>%
          select(-correcteur) %>% apply(., 2, max)
    # normalize inter-grader results
    f = function(x){
        src = quantile(x, probs=c(.25, .75), na.rm=TRUE)
        out = tar[1] + ((tar[2] - tar[1]) / (src[2] - src[1])) * (x - src[1])
        return(out)
    }
    out = data %>% group_by(correcteur) %>%
          mutate(note_norm = f(note))
    out$note_norm = out$note
    for(i in unique(out$correcteur)){
        out$note_norm[out$correcteur == i] = f(out$note[out$correcteur == i])
    }
    out = out[, c('id', 'correcteur', 'note', 'note_norm')]
    out$note_norm[out$note_norm < 0] = 0
    out$note_norm[out$note_norm > 100] = 100
    if(!plot){
        return(out)
    }else{
        x = ggplot(out, aes(note, color=correcteur, linetype=correcteur)) + 
            geom_density() + theme_minimal()
        y = ggplot(out, aes(note_norm, color=correcteur, linetype=correcteur)) + 
            geom_density() + theme_minimal()
        z = ggplot(out, aes(note, note_norm, color=correcteur, linetype=correcteur)) + 
            geom_point() + theme_minimal()
        grid.arrange(x, y, z)
    }
}

#' Convert percentage to letter grade
#'
#' @param index (character vector)
#' @param slack (integer)
#' @export
pct2let = function(grades, slack_fail = 2, slack_pass = 1){
    bareme = structure(list(lb = c(90, 85, 80, 77, 73, 70, 65, 60, 57, 54, 
    50, 35, 0.01, 0), ub = c(100, 89.99, 84.99, 79.99, 76.99, 72.99, 
    69.99, 64.99, 59.99, 56.99, 53.99, 49.99, 34.99, 0.01), let = c("A+", 
    "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "E", 
    "F", "F*")), .Names = c("lb", "ub", "let"), class = "data.frame", row.names = c(NA, 
    -14L))
    grades = ifelse(grades < 50, grades + slack_fail, grades + slack_pass)
    out = grades
    for(i in nrow(bareme):1){
        out[grades >= bareme$lb[i]] = bareme$let[i]
    }
    return(out)
}

#' Convert percentage to letter grade
#'
#' @param data (data.frame)
#' @param index (vector of columns names)
#' @export
get_dups = function(data, index){
    idx = data[, index]
    idx = apply(idx, 1, paste, collapse='-')
    idx = idx %in% idx[duplicated(idx)]
    out = data[idx,]
    out = vincent::sort_df(out, index)
    return(out)
}
