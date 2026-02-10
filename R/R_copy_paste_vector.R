

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
  if(suppressWarnings(any(is.na(as.numeric(x))))){
    quotes <-  rep(NA,length(x))
    quotes[!grepl(x=x,pattern = '"')] <- '"'
    quotes[is.na(quotes) & !grepl(x=x,pattern = "'")] <- "'"
  }else{
    quotes <- "" 
  }
  if(any(is.na(quotes))){
    stop("Some characters contain both ' and",' " ',"quotes inside them can't print them quoted proprely")
  }else{
    paste0("c(",paste0(names_vec,quotes,x,quotes,collapse = ", "),")")
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





#' Print a vector as you would write it manually.
#' 
#' @description
#' Mainly usefull when i need to hard code a set of values that are to be extracted. 
#' Example: I want to use all columns names of a data.frame as a vector
#'
#' @param x a vector/list/data.frame/matrix to write manually
#' @param shape = `NULL`decide what shape to print: only when x is a vector and not a list
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
#' print_to_copy(vec_to_hard_code)
#' print_to_copy(vec_to_hard_code,shape = 'select')
#' 
#' list_to_hard_code <- list(a = sample(1:5,size = 5),
#'                                 b = sample(1:5,size = 10,
#'                                 replace = TRUE))
#' print_to_copy(list_to_hard_code)
#' 
#' data_to_hard_code <- data.frame(a = sample(1:10,size = 10),
#'                                 b = sample(1:5,size = 10,
#'                                 replace = TRUE))
#' print_to_copy(data_to_hard_code)
#' 
#' print_to_copy(as.matrix(data_to_hard_code))
#' # ready to copy paste !
#' 
print_to_copy <- function(x, shape = NULL){
  if('data.frame' %in% class(x)){
    cat(paste0('\n',chr_print_buffed(x,type = 'data.frame'),'\n'))
    
  }else if(is.numeric(x) | is.character(x) | is.logical(x) | is.factor(x)){
    if(is.matrix(x)){
      cat(paste0("\nmatrix(",chr_print_vector(x),", nrow = ",nrow(x),")\n"))
      
    }else{
      if(is.null(shape)){
        shape <- "vector"
      }
      if(shape == "vector"){
        cat(paste0('\n',chr_print_vector(x),'\n'))
      }else if(shape == 'select'){
        to_quote <- get_chr_to_quote(x)
        quotes <- ifelse(to_quote,'`','')
        cat(paste0("\n%>%\nselect(",paste0(quotes,x,quotes,collapse = ", "),")\n"))
      }else{
        stop("x is a vector: shape agrument can only be 'vector' or 'select'")
      }
    }
    
  }else if(is.list(x)){
    cat(paste0('\n',chr_print_buffed(x,type = 'list'),'\n'))
  }else{
    stop("Uncoded class for x")
  }
}


