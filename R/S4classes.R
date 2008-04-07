
 fullyQcName = function(x) {
    pName = attr(x, "package")
    if( is.null(pName) )
        x
    else 
        paste(attr(x, "package"), x, sep=":")
}

 superClasses = function(x) {
   if(!is(x, "classRepresentation") )
      return("must have a class representation object")
   superCs = names(x@contains)
   if(length(superCs) == 0 )
     return(character(0))
   directSCs = sapply(x@contains, function(x) if(length(x@by) > 0 ) FALSE else
            TRUE)
   pkgNames = sapply(x@contains, function(x) x@package)
   clss = superCs[directSCs]
   pkgNames = pkgNames[directSCs]
   ans = vector("list", length=length(clss)) 
   for( i in 1:length(clss)) {
      v = clss[i]
      attr(v, "package") = pkgNames[i]
      ans[[i]] = v
   }
   return(ans)
 }


 class2Graph = function(class, fullNames=TRUE) {
    if(is(class, "character"))
	class = getClass(class)
    if( !is(class, "classRepresentation") )
        stop("need a character or a classRepresentation")

    cname = as.character(class@className)
    superClasses = getAllSuperClasses(class)
    
    ##handle the one node graph separately
    if( length(superClasses) == 0 ) {
        cn = fullyQcName(class@className)
        eL = list(numeric(0)); names(eL) = cn;
        return(new("graphNEL", edgeL=eL, nodes=cn, edgemode="directed"))
    }
    ##otherwise build a simple incidence matrix
    nN = length(superClasses)+1
    rmat = matrix(0, nr=nN, nc=nN)
    dimnames(rmat) = list(c(cname, superClasses),
                          c(cname, superClasses))
    sCn = superClasses(class)
    fNms = rep("", nN)
    if( fullNames ) 
        fNms[1] = fullyQcName(class@className)
    rmat[cname, as.character(sCn)] = 1
    for(i in 1:(nN-1)) {
       tc = getClass(superClasses[i])
       tCn = superClasses(tc)
       rmat[superClasses[i], as.character(tCn)] = 1
       if( fullNames )
          fNms[i+1] = fullyQcName(tc@className)
    } 
    if( fullNames )
       dimnames(rmat) = list(fNms, fNms)
    return(as(rmat, "graphNEL"))
}

classList2Graph = function(class, fullNames=TRUE) {
    g1 = class2Graph(class[1], fullNames=fullNames)
    if( length(class) > 1 ) {
        for(i in 2:length(class))
           g1 = join(g1, class2Graph(class[i], fullNames=fullNames))
    }
    g1
 }

##augment the usual help system with a list of choices
##which include all subclasses and all methods for a generic
##optionally those methods satisfying a particular signature
## and those in the location where 

 S4Help = function(name, signature) {
  if( !is.character(name) || !(length(name) == 1) )
     stop("the name argument must be a length 1 character vector")
  if( isClass(name) ) {
	superC = getAllSuperClasses(getClass(name))
        classList = c(name, superC)
        cL = paste(classList, c("class", rep("superclass", length(superC))))
        if( length(cL) > 1 ) {
            whichone = menu(cL, 
            title="Please Select a Topic\nOr type 0 to cancel\n")
            if( whichone == 0)
               return(NULL)
        } else whichone = 1
        print(do.call("?", list("class", classList[whichone])))
   }
   else if( isGeneric(name) ) {
        gen = getGeneric(name)
        methodT = methods:::.getMethodsTable(gen)
        allMeths = objects(methodT, all.names = TRUE)
        allMeths = gsub("#",",", allMeths)
        methList = c(name, allMeths)
        mL = paste(methList, 
            c("generic", rep("method", length(methList))),sep=", ")
        if( length(mL) > 1 ) {
            whichone = menu(mL,
            title="Please Select a Topic\nOr type 0 to cancel\n")
            if( whichone == 0)
               return(NULL)
        } else whichone = 1
        if( whichone == 1)
           print(do.call("help", list(topic = name)))
        else print(do.call("help", list(topic = paste(name, ",", 
                methList[whichone], "-method", sep=""))))
       } else warning(name, " is neither a known class or generic function")
   return(invisible(NULL))
  }

##two orderings are consistent if for any a and b, in both
##lists either a < b in both or a>b in both 
consistentOrdering = function(Ord1, Ord2) {
  if( !is.character(Ord1) || !is.character(Ord2))
    stop("must have character strings")
  where = match(Ord2, Ord1)
  where = where[!is.na(where)]
  if(any(diff(where) < 0 ))
    FALSE
  else TRUE
}

