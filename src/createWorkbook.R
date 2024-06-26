createWorkbook <- function(type="xlsx")
{
  if (type=="xls") {
    wb <- .jnew("org/apache/poi/hssf/usermodel/HSSFWorkbook")
  } else if (type == "xlsx") {
    wb <- .jnew("org/apache/poi/xssf/usermodel/XSSFWorkbook")
  } else {
    stop(paste("Unknown format", type))
  }
  return(wb)
}