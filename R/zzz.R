.onAttach<-function(libname, pkgname){
  version <- utils::packageVersion("RedeR")
  msg <- paste("***This is RedeR ",version,"! For a quick start, please type 'vignette('RedeR')'.
   Supporting information is available at Genome Biology 13:R29, 2012,
   (doi:10.1186/gb-2012-13-4-r29). \n",sep="")
  packageStartupMessage(msg,appendLF=FALSE)
}
