# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Gives a character vector of new numerated names for files in path 
#'
#' @param path Location for folders and files to rename
#' @param sep separation character between numeration and actual name
#'
#' @return a character with all new names
#' @export
#'
#' @examples
#' enumerated_files_names()
#' enumerated_files_names(path = ".",sep =' = ')
enumerated_files_names <- function(path = ".",sep ='_'){
  names_folders <- list.files(path)
  if(length(names_folders) >= 100){
    warning("To many files, we didn't change them")
    return(names_folders)
  }
  new_names <- names_folders %>% 
    purrr::map_chr(function(name){
      if(nchar(name)>=1 && substring(name,1,1) %in% 0:9){
        if(nchar(name)>=2 && substring(name,2,2) %in% 0:9){
          if(nchar(name)>=3 && substring(name,3,3) == "_"){
            if(nchar(name)>=4){
              return(substr(name, 4,nchar(name)))
            }else{
              return("")
            }
            
          }else if(nchar(name)>=5 && substring(name,3,5) == " - "){
            if(nchar(name)>=6){
              return(substr(name, 6,nchar(name)))
            }else{
              return("")
            }
          }
        }else if(nchar(name)>=2 && substring(name,2,2) == "_"){
          if(nchar(name)>=3){
            return(substr(name, 3,nchar(name)))
          }else{
            return("")
          }
        }else if(nchar(name)>=4 && substring(name,2,4) == " - "){
          if(nchar(name)>=5){
            return(substr(name, 5, nchar(name)))
          }else{
            return("")
          }
        }
      }
      return(substr(name, 1,nchar(name)))
    })
  
  
  if(any(c("archive","archives","Archive","Archives") %in% new_names)){
    pos <-  which(new_names %in%  c("archive","archives","Archive","Archives"))[1]
    first <- 0
  }else{
    pos <- c()
    first <- 1
  }
  
  
  
  new_num <- first:(length(names_folders) - (first == 0))
  
  
  if(max(new_num) >= 10){
    new_num[1:9] <- paste0("0",new_num[1:9])
  }
  
  if(length(pos) != 0 && pos>=2){
    if(pos!=length(new_num)){
      new_num <- new_num[c(2:pos,1,(pos+1):length(new_num))] 
    }else{
      new_num <- new_num[c(2:length(new_num),1)] 
    }
  }
  
  
  new_names <- paste(new_num,new_names,sep=sep) 
  return(new_names)
}


#' Renumerated files and folders names in path 
#'
#' @param path Location for folders and files to rename
#' @param sep separation character between numeration and actual name
#' @param check  Boolean asks user validation ?
#' @param rename Boolean Actually rename ?
#'
#' @return Can return a character vector of renamed files if rename = FALSE
#' @export
#'
#' @examples
#' renumerate_files(path = ".",sep ='_', check = TRUE, rename = FALSE)
#' \dontrun{
#' renumerate_files(path = "my_files_adress",sep =' - ')
#' }
renumerate_files <- function(path = ".",sep ='_', check = TRUE, rename = TRUE){
  new_names <- enumerated_files_names(path,sep)
  if(rename){
    cat(paste0("\nNew_name : ",paste0("'",new_names,"'",collapse = ", ")))
    if(check){
      answer <- readline("Do you want to renames files y or N: ") 
    }else{
      answer <- 'y'
    }
    if(answer == 'y'){
      file.rename(paste0(path,"/",list.files(path)), paste0(path,"/",new_names))
      cat("\nFiles successfully renamed\n")
      }else{
        cat("\nAction Canceled\n")
      }
    }else{
    return(new_names)
    }
}


