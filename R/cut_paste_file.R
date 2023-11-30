#' @title Copy and Paste File
#'
#' @description
#' The function is used to copy a file from a specified source folder or a specific file path to a destination folder and possibly
#' change the name of the file.
#' If a file path is provided, the function will use it, otherwise it will use the last modified file in the source folder.
#'
#' @param source_folder character. The path to the source folder where the function will look for the last modified file.
#' @param destination_folder character. The path to the destination folder where the file will be copied to.
#' @param new_name character. The new name for the file. If not provided, the function will use the original file name.
#' @param pattern_file_name regex of the file name to find
#' @param last_modified logical. Indicates if the function should use the last modified file in the source folder.
#' @param file_path character. The path to the specific file that should be copied.
#'
#' @return Nothing
#' @export
#' @examples
#' # copy the last modified file in the default source
#' # folder to the specified destination folder with the original file name
#' # cut_paste_file(destination_folder = "C:/path/to/destination_folder")
#' # copy the last modified file in the folder to the specified destination folder
#' # cut_paste_file(destination_folder = "C:/path/to/destination_folder", new_name = "new_name.csv")
cut_paste_file <- function(source_folder = NULL, destination_folder, pattern_file_name,new_name = NULL,last_modified = TRUE,file_path = NULL){


  if(!is.null(file_path)){
    if(is.null(destination_folder)) stop("destination_folder must be provided")
    if(is.null(new_name)){
      new_name <- basename(file_path)
    }
    destination_file <- file.path(destination_folder, new_name)
    # move the file to the destination folder
    file.copy(file_path, destination_file, overwrite = TRUE)
  } else {
    if(is.null(source_folder)) stop("source_folder must be provided")
    if(is.null(destination_folder)) stop("destination_folder must be provided")
    if(last_modified){
      # get all files in the source folder
      all_files <- list.files(path = source_folder, full.names = TRUE, pattern = pattern_file_name)

      # get the last modified time for each file
      last_modified_time <- file.info(all_files)$mtime

      # get the file with the latest modified time
      last_file_index <- which.max(last_modified_time)
      file_path <- all_files[last_file_index]
      print(file_path)
    }

    if(is.null(new_name)){
      new_name <- basename(file_path)
    }

    # create the destination file path with the new name
    destination_file <- file.path(destination_folder, new_name)

    # move the file to the destination folder
    file.copy(file_path, destination_file, overwrite = TRUE)

  }

  print(destination_file)

}
