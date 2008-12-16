asSimpleVector <- function(x, mode = "logical")
{
    if (!(mode %in% c("logical", "integer",
                      "numeric", "double",
                      "complex", "character")))
        stop("invalid mode ", mode)
    Dim <- dim(x)
    nDim <- length(Dim)
    Names <- names(x)
    if (nDim > 0)
        DimNames <- dimnames(x)
    x <- as.vector(x, mode)
    names(x) <- Names
    if (nDim > 0) {
        dim(x) <- Dim
        dimnames(x) <- DimNames
    }
    x
}

subsetAsCharacter <- function(x, i, j)
{
    if (nargs() == 3) {
        if (missing(i)) {
            if (missing(j))
                x <- x[ , ]
            else x <- x[, j]
        } else if (missing(j))
            x <- x[i, ]
        else x <- x[i, j]
    } else if (missing(i)) {
        x <- x[]
    } else {
        x <- x[i]
    }
    asSimpleVector(x, "character")
}

setVNames <- function(x, nm)
{
    names(x) <- nm
    asSimpleVector(x, "numeric")
}

convertMode <- function(from, to)
{
    asSimpleVector(from, mode(to))
}

printWithNumbers = function(f) {
 if( !is.function(f) )
   stop("requires a function argument")

 fform = capture.output(f)
 lnos = as.list(body(f))
 if( lnos[[1]] == '{' )
     lnos[[1]] = "{"
 else
     stop("only set line numbers for functions that use {")
 start = match("{", fform)
 if( is.na(start) )
     stop("problems with {")

 ##set the padding
 nlnos = length(lnos)
 if( nlnos >= 100 ) extras = "    "
 else if( nlnos >= 10 ) extras = "   "
 else extras = "  "

 lni = 1
 for(i in 1:length(fform) ) {
    if(i < start || lni > nlnos ) {
      fform[i] = paste(extras, fform[i], sep="")
      next
    }
    tstr = gsub("^\\s*", "", fform[i])
    if( length(grep(tstr, deparse(lnos[[lni]]), fixed=TRUE)) > 0 ){
      if( nlnos >= 100 ) {
          if( lni < 10 ) spaces = "  "
          else if( lni < 100 ) spaces = " "
          else spaces = ""
      } else if (nlnos >= 10 ) {
          if(lni < 10 ) spaces = " "
          else spaces = ""
      } else
          spaces = ""
      fform[i] = paste(lni, ":", spaces, fform[i], sep="")
      lni = lni + 1
    }
    else
      fform[i] = paste(extras, fform[i], sep="")
 }
 cat(fform, sep="\n")
 invisible(fform)
}

 ##crappy little helper function to parse the value
 ##returned by showMethods
 ##for now, we will drop methods that are "inherited",
 ##
 parseSignatures <- function(input) {
   drop = grep("inherited", input)
   if( length(drop) )
       input = input[-drop]
   ##sometimes we get empty lines
   slens = nchar(input)
   in2 = input[slens > 0 ]
   return(paste("c(", in2, ")"))
 }
   

 traceMethods <- function(generic, traceStrings, tracer) {
   if( is.character(generic) ) gendef = get(generic) else {
     gendef = generic
     generic = deparse(substitute(generic)) }
   if( !isGeneric(generic) ) stop("need a generic function")
   foo = showMethods(generic, printTo=FALSE)
   methSigs = parseSignatures(foo[-1])
   if(missing(traceStrings) )
     traceStrings = paste("in method", methSigs)
   for( i in 1:length(methSigs) ) {
     if( missing(tracer) )
       tracer = substitute(expression(print(foo)), list(foo=traceStrings[i]))
     do.call(trace, list(generic, 
                signature = eval(parse(text=methSigs[i])), tracer = tracer,
                where=.GlobalEnv))
   }
   invisible(methSigs)
 }

 untraceMethods <- function(generic, methodSigs) 
   for( i in methodSigs) untrace(generic, methodSigs[i])
