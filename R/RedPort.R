

#RedPort class constructor---------------------------------------------------

RedPort = function (title='default', host='127.0.0.1', port=9091)
{ 
  uri = sprintf ('http://%s:%s', host, port)
  dp = new ('RedPort', title=title, uri=uri, port=port)
  return (dp)
}


