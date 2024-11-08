#' @title Convert a dataframe to markdown
#' @description Takes a dataframe and convert it to markdown format
#' @param df a dataframe
#' @return print in the console the markdown format
#' @export
to_md <- function(your_df){
  cn <- as.character(names(your_df))
  headr <- paste0(c("", cn),  sep = "|", collapse='')
  sepr <- paste0(c('|', rep(paste0(c(rep('-',3), "|"), collapse=''),length(cn))), collapse ='')
  st <- "|"
    for (i in 1:nrow(your_df)){
      for(j in 1:ncol(your_df)){
        if (j%%ncol(your_df) == 0) {
          st <- paste0(st, as.character(your_df[i,j]), "|", "\n", "" , "|", collapse = '')
        } else {
        st <- paste0(st, as.character(your_df[i,j]), "|", collapse = '')
        }
      }
    }
  fin <- paste0(c(headr, sepr, substr(st,1,nchar(st)-1)), collapse="\n")
  cat(fin)
}  