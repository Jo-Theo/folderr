

#' Bind_cols to manage map_dfc in package
#' This function as no use
#'
#' @param ... ok
#' @param .name_repair ok 
#'
#' @return bind rows
bind_cols <- function(...,
                      .name_repair = c("unique", "universal", "check_unique", "minimal")){
  dplyr::bind_cols(...,
                   .name_repair = c("unique", "universal", "check_unique", "minimal"))
}
 

#' get_chr_to_quote
#' 
#' This function identify weird names format and correct them with : `` 
#'
#' @param x  character
#'
#' @return a boolean vector wether or not each element need to be strong quoted
#'
get_chr_to_quote <- function(x){
  is_weird <- purrr::map_dfc(c('"',"'",'!',"\\(.*\\)","\\$","\\*","\\|",'&',"~","\\?",",",";"," ","\\+","-",
                               "\\[.*\\]","\\{.*\\}","=","%","/",":"),~data.frame(a = grepl(x = x,pattern = .x)) %>% 
                               `names<-`(.x)) %>% 
    apply(1, any)
  is_number <-  !is.na(as.numeric(x)) %>% 
    suppressWarnings()
  return(is_weird|is_number)
}

#' Get character encoding the vector hard writing
#'
#' @param x character
#'
#' @return a chr
chr_print_vector <- function(x){
  names_vec <- names(x)
  if(!is.null(names_vec)){
    to_quote <- get_chr_to_quote(names_vec) 
    quotes <- ifelse(to_quote,'`','')
    names_vec <- ifelse(names_vec == "","",paste0(quotes,names_vec,quotes," = "))
  }else{
    names_vec <- rep("",length(x))
  }
  
  if(is.factor(x)){
    class_begin <- "as.factor(c(" 
    class_end <- "))"
  }else{
    class_begin <- "c(" 
    class_end <- ")"
  }

  if(suppressWarnings(any(!is.na(x) & is.na(as.numeric(x)))) | is.factor(x)){
    quotes <-  rep(NA,length(x))
    quotes[is.na(x)] <- ""
    quotes[is.na(quotes) & !grepl(x=x,pattern = '"')] <- '"'
    quotes[is.na(quotes) & !grepl(x=x,pattern = "'")] <- "'"
  }else{
    quotes <- "" 
  }
  if(any(is.na(quotes))){
    stop("Some characters contain both ' and",' " ',"quotes inside them can't print them quoted proprely")
  }else{
    paste0(class_begin,paste0(names_vec,quotes,x,quotes,collapse = ", "),class_end)
  }
}

#' Get character encoding the list or data.fraùe hard writing
#'
#' @param x character
#' @param type 'list' or 'description'data.frame', what is the wanted results
#'
#' @return a chr
chr_print_buffed <- function(x,type = "list"){
  names_vec <- names(x)
  if(!is.null(names_vec)){
    to_quote <- get_chr_to_quote(names_vec) 
    quotes <- ifelse(to_quote,'`','')
    names_vec <- ifelse(names_vec == "",paste0("V",1:ncol(x)," = "),paste0(quotes,names_vec,quotes," = "))
  }else{
    names_vec <- paste0("V",1:length(x)," = ")
  }
  if(type == 'list'){
    preambule <-   'list('
    intermed <- ",\n     "
  }else{
    preambule <-   'data.frame('
    intermed <- ",\n           "
  }
  paste0(preambule,paste0(names_vec,purrr::map_chr(x,~chr_print_vector(.x)),
                          collapse = intermed),")\n")
}


#' copy_to_clipboard
#' Safe wrapper for clipr::write_clip. I want to use it but it doesn't pass checks correctly
#'
#' @param x string to copy to clip board
#'
#' @return clipr::write_clip(x) if it can
copy_to_clipboard <- function(x) {
  
  if (!interactive()) {
    message("Clipboard not available in non-interactive session.")
    return(invisible(FALSE))
  }
  
  if (!requireNamespace("clipr", quietly = TRUE)) {
    message("Package 'clipr' not installed.")
    return(invisible(FALSE))
  }
  
  if (!clipr::clipr_available()) {
    message("No clipboard available.")
    return(invisible(FALSE))
  }
  
  clipr::write_clip(x)
  invisible(TRUE)
}



#' Add to clipboard text describing x as you would write it manually in R.
#' 
#' @description
#' Mainly usefull when i need to hard code a set of values that are to be extracted. 
#' Example: I want to use all columns names of a data.frame as a vector
#'
#' @param x a vector/list/data.frame/matrix to write manually
#' @param shape (default : `NULL`) decide what shape to print: only when x is a vector and not a list
#'  - vector -> "vector"
#'  - dyplr::select -> "select"
#'  
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
#' add_to_clipboard(vec_to_hard_code)
#' add_to_clipboard(vec_to_hard_code,shape = 'select')
#' 
#' list_to_hard_code <- list(a = sample(1:5,size = 5),
#'                                 b = sample(1:5,size = 10,
#'                                 replace = TRUE))
#' add_to_clipboard(list_to_hard_code)
#' 
#' data_to_hard_code <- data.frame(a = sample(1:10,size = 10),
#'                                 b = sample(1:5,size = 10,
#'                                 replace = TRUE))
#' add_to_clipboard(data_to_hard_code)
#' 
#' add_to_clipboard(as.matrix(data_to_hard_code))
#' # ready to paste ! Press Ctrl + V 
#' 
add_to_clipboard <- function(x, shape = NULL){
  if('data.frame' %in% class(x)){
    res <- chr_print_buffed(x,type = 'data.frame')
    
  }else if(is.numeric(x) | is.character(x) | is.logical(x) | is.factor(x)){
    if(is.matrix(x)){
      res <- (paste0("matrix(",chr_print_vector(x),", nrow = ",nrow(x),")"))
      
    }else{
      if(is.null(shape)){
        shape <- "vector"
      }
      if(shape == "vector"){
        res <- chr_print_vector(x)
      }else if(shape == 'select'){
        to_quote <- get_chr_to_quote(x)
        quotes <- ifelse(to_quote,'`','')
        res <- paste0("%>%\nselect(",paste0(quotes,x,quotes,collapse = ", "),")")
      }else{
        stop("x is a vector: shape agrument can only be 'vector' or 'select'")
      }
    }
    
  }else if(is.list(x)){
    res <- chr_print_buffed(x,type = 'list')
  }else{
    stop("Uncoded class for x")
  }
  copy_to_clipboard(res)
}

