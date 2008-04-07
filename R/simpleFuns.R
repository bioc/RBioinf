
 simpleSort = function(x) 
    .Call("simpleSort", x, PACKAGE="RBioinf")

 simpleRand = function(x, y="notused")
    .Call("simpleRand", x, y, PACKAGE="RBioinf")


 Rcal = function(month, year) {
  pD = function(x) pipe(paste("date \"+%", x, "\"", sep=""))

  if(missing(month))
    month = readLines(pD("m"))
  if(missing(year))
    year = readLines(pD("Y"))

  cat(readLines(pipe(paste("cal ", month, year))), sep="\n")
 }

 simplePVect = function(iV) {
   .C("simplePVect", as.double(iV), as.integer(length(iV)), 
        PACKAGE="RBioinf")
   invisible(NULL)
 }
