
#-------------------------------------------------------------------------------
#RedeR port class--------------------------------------------------------------
setClass ("RedPort", 
          representation = representation (title  = "character",
                                           uri    = "character",
                                           port   = "numeric",
                                           jclass = "character"),
          prototype = prototype (title  = "RedPort",
                                 uri    = "http://127.0.0.1:9091",
                                 port   = 9091,
                                 jclass = "reder/rj/CanvasR"
                                 )
          )
          
#Test the validity of RRede class ---------------------------------------------
setValidity ("RedPort",
  function (object) {
      c1 = length (object@title)    == 1
      c2 = length (object@uri)      == 1 
      c3 = length (object@port)     == 1 
      c4 = length (object@jclass)   == 1      
      if (c1 && c2 && c3 && c4) TRUE
	  else cat("'title', 'uri', 'port', and 'jclass' must all have length 1")
    })
    
#-------------------------------------------------------------------------------    
#PluginBuilder class------------------------------------------------------------
setClass ("PluginBuilder", 
          representation = representation (title        = "character",
                                           allMethods   = "list",
                                           allAddons    = "list")                                          
          )                                           
     
#Test the validity of PluginBuilder class --------------------------------------
setValidity ('PluginBuilder',
  function (object) {
      c1 = length (object@title) == 1
      if (c1) TRUE
      else cat("'title' must have length 1")
    })
    
