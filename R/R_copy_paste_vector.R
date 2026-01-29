#' Print a vector as you would write it manually.
#' 
#' @description
#' Mainly usefull when i need to hard code a set of values that are to be extracted. 
#' Example: I want to use all columns names of a data.frame as a vector
#'
#' @param vector to write manually
#'
#' @export
#'
#' @examples
#' 
#' # To use it extract the vector you want to hard code somewhere
#' 
#' data <- sample(x=c("ceci","est","un","test"), size = 1000, replace = TRUE) %>% 
#'   as.factor() %>% 
#'    summary()
#'    
#' vec_to_hard_code <- which(data > 250) %>% 
#'    names()
#'    
#' print_to_copy(vec_to_hard_code)
#' 
# ready to copy paste !
#' 
print_to_copy <- function(vector){
  cat(paste0("\nc(",paste0("'",vector,"'",collapse = " ,"),")\n"))
}
