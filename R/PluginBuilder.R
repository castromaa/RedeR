

#PluginBuilder class constructor------------------------------------------------

PluginBuilder = function(title='plugin', allMethods, allAddons=NULL)
{
  
  .Deprecated(new="addGraph/getGraph related methods",old="PluginBuilder") 
  
    #1st input check
    if(is.null(allMethods) || is.na(allMethods) || !is.list(allMethods)){
      print("invalid 'allMethods' declaration!")
      return(NULL)     
    }
    if(!is.null(allAddons) && !is.list(allAddons)){
      print("invalid 'allAddons' declaration!")
      return(NULL)    
    } 
    #2nd input check
    negtest=FALSE
    note1=NULL 
    note2=NULL  
    for(i in 1:length(allMethods)){
       	if(!is.function(allMethods[[i]])){
          	negtest=TRUE
          	note1="error: all plugin methods must be wrapped as R functions!"
       	} else {
    		checkargs=formals(allMethods[[i]]) 
    		if(length(checkargs)>0){
    			negtest=TRUE
    			note2="error: args must be wrapped in methods! ...use addons for formal args!"
    		}  
    	}   
    }
    note3=NULL  
    if(is.null(allAddons)){
        allAddons =list('')
    } else {
        for(i in 1:length(allAddons)){
           if(!is.function(allAddons[[i]])){
              negtest=TRUE
              note3="error: all plugin addons must be wrapped as R functions!"
           }
        }   
    } 
    #Return
    if(negtest){
      if(!is.null(note1))print(note1)
      if(!is.null(note2))print(note2)
      if(!is.null(note3))print(note3)
      return(NULL)
    } else {
      pb = new ('PluginBuilder', title=title, allMethods=allMethods, allAddons=allAddons)
      return(pb)
    }
}



