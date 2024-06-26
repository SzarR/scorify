createSheet <- function(wb, sheetName="Sheet1")
{
  sheet <- .jcall(wb, "Lorg/apache/poi/ss/usermodel/Sheet;",
                  "createSheet", sheetName)
  return(sheet)
}