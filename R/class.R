
 subClassNames = function(x) {
    if (isClassDef(x)) 
        classDef <- x
    else if(is.character(x) )
        classDef <- getClass(x)
    else
        stop("invalid argument")
    ans = names(classDef@subclasses)
    if( length(ans) == 0 )
      character()
    else
      ans
 }

 superClassNames = function(x) {
    if (isClassDef(x)) 
        classDef <- x
    else if(is.character(x) )
        classDef <- getClass(x)
    else
        stop("invalid argument")

    ans = names(classDef@contains)
    if( length(ans) == 0 )
      character()
    else
      ans
 }

