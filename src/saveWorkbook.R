saveWorkbook <- function(wb, file, password=NULL)
{
  jFile <- .jnew("java/io/File", file)
  fh <- .jnew("java/io/FileOutputStream", jFile)
  
  # write the workbook to the file
  wb$write(fh)
  
  # close the filehandle
  .jcall(fh, "V", "close")
  
  if ( !is.null(password) ) {
    fs <- .jnew("org/apache/poi/poifs/filesystem/POIFSFileSystem")
    encMode <- J("org/apache/poi/poifs/crypt/EncryptionMode", "valueOf", "agile")
    info <- .jnew("org/apache/poi/poifs/crypt/EncryptionInfo", fs, encMode)
    
    enc <- info$getEncryptor()
    enc$confirmPassword(password)
    
    access <- J("org.apache.poi.openxml4j.opc.PackageAccess", "valueOf", "READ_WRITE")
    opc <- J("org.apache.poi.openxml4j.opc.OPCPackage", "open", jFile, access)
    outputStream <- enc$getDataStream(fs)
    opc$save(outputStream)
    opc$close()
    
    fos <- .jnew("java/io/FileOutputStream", file)
    fs$writeFilesystem(fos)
    fos$close()
  }
  
  invisible()
}