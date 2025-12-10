#' @title Create Excel file with a sheet for each dataframe in a list
#' @description Takes a list of dataframes and exports each dataframe to a separate sheet in an Excel file, with the sheet name being the name of the dataframe if it's named in the list, or "Sheet" followed by the index of the dataframe in the list if it's not named.
#' @param df a list of dataframes
#' @param arquivo a character string for the name of the excel file
#' @param caminho a character string for the file path
#' @param open_file logical indicating whether to open the file after creation (default: TRUE)
#' @return opens the Excel file if open_file is TRUE
#' @export
to_excel <- function(df, arquivo = "padrao", caminho = "C:/Users/FLeal/Downloads", open_file = TRUE) {
  
  options("openxlsx.dateFormat" = "dd/mm/yyyy")
  caminho_windows <- stringr::str_replace(caminho, "C:/", "/mnt/c/")

  if(file.exists(paste0(caminho_windows, "/", arquivo,".xlsx"))) {
    i <- 1
    while(file.exists(paste0(caminho_windows, "/", arquivo,"_", i,".xlsx"))) {
      i <- i + 1
    }
    arquivo <- paste0(arquivo,"_", i)
  }

  if (is.data.frame(df)) {

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "dataframe")
    openxlsx::writeDataTable(wb, "dataframe", df)

  } else if (is.list(df)) {

    wb <- openxlsx::createWorkbook()
    for (i in 1:length(df)) {
      
      sheet_name <- if(!is.null(names(df)[[i]])) names(df)[[i]] else paste0("Sheet", i)
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeDataTable(wb, sheet_name, df[[i]])
    }

  } else {
    print("The object should be a list or a dataframe")
  }
  

  openxlsx::saveWorkbook(wb, file = paste0(caminho_windows, "/", arquivo,".xlsx"), overwrite = TRUE)
  caminho_arquivo <- paste0(caminho, "/", arquivo,".xlsx")

  if(open_file) {
    system(glue::glue('cmd.exe /C start "" "{caminho_arquivo}"'))
  }
}