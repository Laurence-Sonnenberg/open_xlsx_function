
#' # Name the sheets
#' nameVec1 <- c("1", "2", "3")
#' nameVec2 <- c("4", "5", "6")
#' nameVec3 <- c("3", "5", "7")
#' 
#' a <- data.frame(1:3, 4:6, 7:9)
#' b <- data.frame(10:12, 13:15, 16:18)
#' tableList1 <- list(a, a, a)
#' tableList2 <- list(b, b, a)
#' 
#' open_xlsx("1", a, "test")
#' open_xlsx(nameVec1, tableList1, "test")
#' open_xlsx(nameVec2, tableList1, "test", loadWorkBook = TRUE)
#' open_xlsx(nameVec3, tableList2, "test", loadWorkBook = TRUE, replaceSheets = TRUE)
#' 
#' file.remove("test.xlsx")

open_xlsx <- function (nameVector, 
                       tableList, 
                       workBookName, 
                       colNames = TRUE,
                       RowNames = FALSE,
                       loadWorkBook = FALSE,
                       replaceSheets = FALSE) { 
  
  # load devtools library
  install.packages("devtools", repos = "http://cran.rstudio.com/")
  
  # check for openxlsx package, if not installed install latest 
  # version from github
  if (require("openxlsx") == FALSE) {
    
    devtools::install_github("awalker89/openxlsx")
    library(openxlsx)
  }
  
  # check if a workbook needs to be loaded or a new workbook created
  if (loadWorkBook == TRUE) {
    
    # load workbook
    wb <- openxlsx::loadWorkbook(paste(workBookName, "xlsx", sep = "."))
    
  } else {
    
    # create workbook
    wb <- openxlsx::createWorkbook()    
  }
  
  # check sheet names
  sheetNames <- names(wb)
  
  if (replaceSheets == FALSE) {
    
    if (sum(nameVector %in% sheetNames) > 0) {
      return("One or more of your worksheet names already exists in this workbook.")
    }
    
    # name sheets
    sapply(nameVector, function(x) openxlsx::addWorksheet(wb, x))
    
  } else {
    
    if (sum(!(nameVector %in% sheetNames)) > 0) {
      
      sapply(nameVector[!(nameVector %in% sheetNames)], function(x) openxlsx::addWorksheet(wb, x))
    }
    
    warning("ReplaceSheets only replaces cell data, not the full sheet.\n  Unaltered cells may still contain previous data.")
  }
  
  # check for dataframe or list
  if (is.data.frame(tableList) == TRUE) {
    
    # check lengths equal
    if (length(nameVector) > 1) {
      return("Number of sheet names not equal to number of dataframes.")
    }
    
    # write dataframes to sheets
    suppressWarnings(
      
      # write dataframe to sheet    
      openxlsx::writeData(wb, nameVector, tableList, colNames = colNames)
    )
    
  } else {
    
    # check lengths equal
    if (length(nameVector) != length(tableList)) {
      return("Number of sheet names not equal to number of dataframes.")
    }
    
    # write dataframes to sheets
    suppressWarnings(
      
      # write data frames to sheets
      for (i in 1:length(nameVector)) {
        openxlsx::writeData(wb, nameVector[i], tableList[[i]], colNames = colNames, rowNames = rowNames)
      }
    )
  }
  
  # save workbook
  openxlsx::saveWorkbook(wb, file = paste(workBookName, "xlsx", sep = "."), 
                         overwrite = TRUE)
}
