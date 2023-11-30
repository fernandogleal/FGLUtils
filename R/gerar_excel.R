#' @title Create Excel file with a sheet for each dataframe in a list
#' @description Takes a list of dataframes and exports each dataframe to a separate sheet in an Excel file, with the sheet name being the name of the dataframe if it's named in the list, or "Sheet" followed by the index of the dataframe in the list if it's not named.
#' @param df a list of dataframes
#' @param arquivo a character string for the name of the excel file
#' @return opens the Excel file
#' @export
to_excel <- function(df, arquivo = "padrao") {
  # require(openxlsx)
  options("openxlsx.dateFormat" = "dd/mm/yyyy")

  # Create file path
  # Check if file already exists, if so add a number to the name
  caminho <- path.expand("~")

  if(file.exists(paste0(caminho, "/", arquivo,".xlsx"))) {
    i <- 1
    while(file.exists(paste0(caminho, "/", arquivo,"_", i,".xlsx"))) {
      i <- i + 1
    }
    arquivo <- paste0(arquivo,"_", i)
  }

  # Check if input is a dataframe or a list
  if (is.data.frame(df)) {

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "dataframe")
    openxlsx::writeDataTable(wb, "dataframe", df)

  } else if (is.list(df)) {

    wb <- openxlsx::createWorkbook()
    for (i in 1:length(df)) {
      # Get the name of the dataframe in the list or create a name "Sheet" + index
      sheet_name <- if(!is.null(names(df)[[i]])) names(df)[[i]] else paste0("Sheet", i)
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeDataTable(wb, sheet_name, df[[i]])
    }

  } else {
    print("The object should be a list or a dataframe")
  }

  openxlsx::saveWorkbook(wb, file = paste0(caminho, "/", arquivo,".xlsx"), overwrite = TRUE)
  openxlsx::openXL(file = paste0(caminho, "/", arquivo,".xlsx"))
}
