
#Plugin parser------------------------------------------------------------------

pluginParser = function(dyname, dyncode, args=FALSE)
{	
  #Prepare sections:
  if(args){
  		dyncode=deparse(dyncode)
  		dyncode=sub("^\\s*", "", dyncode)
  		dyncode=paste(dyncode, collapse = "\n  ", sep="")
  		dyncode=paste(dyname, "<-", dyncode, sep=" ")
  	} else {
  		dyncode=deparse(body(dyncode))
  		dyncode=dyncode[c(-1,-length(dyncode))]
  		dyncode=sub("^\\s*", "", dyncode)
  		dyncode=paste(dyncode, collapse = "\n", sep="")
  		dyncode=paste("<",dyname,">","  \n",dyncode, sep="")
  	}
  return(dyncode)
}

