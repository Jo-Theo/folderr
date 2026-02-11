# Some useful keyboard shortcuts for package authoring:
#   Insert Roxygen skeleton :  'Crtl + Shift + Alt + R'
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Gives a character vector of new numerated names for files in path
#'
#' @param names_files file names to rename
#' @param sep separation character between numeration and actual name
#'
#' @return a character with all new names
enumerated_files_names <- function(names_files, sep = "_") {
  if (length(names_files) >= 100) {
    warning("To many files, we didn't change them")
    return(names_files)
  }
  new_names <- names_files %>%
    purrr::map_chr(function(name) {
      if (nchar(name) >= 1 && substring(name, 1, 1) %in% 0:9) {
        if (nchar(name) >= 2 && substring(name, 2, 2) %in% 0:9) {
          if (nchar(name) >= 3 && substring(name, 3, 3) == "_") {
            if (nchar(name) >= 4) {
              return(substr(name, 4, nchar(name)))
            } else {
              return("")
            }
          } else if (nchar(name) >= 5 && substring(name, 3, 5) == " - ") {
            if (nchar(name) >= 6) {
              return(substr(name, 6, nchar(name)))
            } else {
              return("")
            }
          }
        } else if (nchar(name) >= 2 && substring(name, 2, 2) == "_") {
          if (nchar(name) >= 3) {
            return(substr(name, 3, nchar(name)))
          } else {
            return("")
          }
        } else if (nchar(name) >= 4 && substring(name, 2, 4) == " - ") {
          if (nchar(name) >= 5) {
            return(substr(name, 5, nchar(name)))
          } else {
            return("")
          }
        }
      }
      return(substr(name, 1, nchar(name)))
    })


  if (any(c("archive", "archives", "Archive", "Archives") %in% new_names)) {
    pos <- which(new_names %in% c("archive", "archives", "Archive", "Archives"))[1]
    first <- 0
  } else {
    pos <- c()
    first <- 1
  }



  new_num <- first:(length(names_files) - (first == 0))


  if (max(new_num) >= 10) {
    new_num[1:9] <- paste0("0", new_num[1:9])
  }

  if (length(pos) != 0 && pos >= 2) {
    if (pos != length(new_num)) {
      new_num <- new_num[c(2:pos, 1, (pos + 1):length(new_num))]
    } else {
      new_num <- new_num[c(2:length(new_num), 1)]
    }
  }


  new_names <- paste(new_num, new_names, sep = sep)
  return(new_names)
}




#' Renumerated files and/or folders names in path
#'
#' @param path Location for folders and files to rename
#' @param ...  Additionnal parameters : 
#' \describe{
#'     \item{folders}{TRUE, should it rename folders}
#'     \item{files}{FALSE, should it rename files}
#'     \item{sep}{"_", separation character between numeration and actual name}
#'     \item{hidden}{TRUE, Consider hidden files/folders ?}
#'     \item{check}{TRUE, Boolean asks for user validation ?}
#'     \item{rename}{TRUE,  Boolean Actually rename ?}
#'     \item{without}{character vector, grep like pattern to be excluded from enumeration}
#'   }
#'
#' @return Can return a character vector of renamed files if rename = FALSE
#' @export
#'
#' @examples
#' renumerate_folders(path = ".", rename = FALSE)
#' \dontrun{
#' renumerate_folders(path = "my_files_adress", 
#'                    sep = " - ", files = TRUE, 
#'                    folders = FALSE)
#' }
renumerate_folders <- function(path = "." , ...) {
  default_args <- list(folders = TRUE, 
                       files = FALSE,
                       sep = "_",
                       hidden = FALSE,
                       check = TRUE,
                       rename = TRUE,
                       without = character())
  
  args <- list(...)
  empty_names <- names(args)==""
  if(sum(empty_names) != 0){
    args <- args[!empty_names]
    warning("There can't be another unnamed agrument than 'path', arguments ignored")
  }
  
  weird_names <- setdiff(names(args),names(default_args))

  if(length(weird_names) != 0){
    stop("argument",ifelse(length(weird_names)>1,"s "," "),paste0("'",weird_names,"'",collapse = ", "),
         " does not exist.\n Only : 'path', 'folders', 'files', 'sep', 'hidden', 'check' and 'rename' (see with ?folderr::renumerate_folders)")
  }
  default_args[names(args)] <- args
  args <- default_args
  
  if(args$hidden){
    all_names <- list.files(path,all.files = TRUE,no..=TRUE)
  }else{
    all_names <- list.files(path)
  }
  dir_names <- list.dirs(path,recursive = FALSE,
                         full.names = FALSE)
  
  if(args$folders & args$files){
    selection <- all_names
  }else if(args$folders){
    selection <- intersect(all_names,dir_names)
  }else if(args$files){
    selection <- setdiff(all_names,dir_names)
  }else{
    stop("No files nor folders are selected")
  }
  for(pattern_i in args$without){
    selection <- selection %>% 
      .[!grepl(pattern_i,.)]
  }
  if(length(selection)==0){
    stop("No file or folder selected after applying without argument")
  }
  
  new_names <- enumerated_files_names(selection, args$sep)
  if (args$rename) {
    cat(paste0("\nNew_name : ", paste0("'", new_names, "'", collapse = ", ")))
    if (args$check) {
      answer <- readline("Do you want to renames files y or N: ")
    } else {
      answer <- "y"
    }
    if (answer == "y") {
      file.rename(paste0(path, "/", selection), paste0(path, "/", new_names))
      cat("\nFiles successfully renamed\n")
    } else {
      cat("\nAction Canceled\n")
    }
  } else {
    return(new_names)
  }
}
