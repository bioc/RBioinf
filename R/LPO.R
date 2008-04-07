##taken from OOPSLA paper
    ##let's guess that these lists are character vectors
    ##we start with some helper functions
isHead = function(list, value) {
    if(length(list) == 0 )
        FALSE
    else
        value == list[1]
}

inTail = function(Ilist, value) {
    if(length(Ilist) == 0 )
        FALSE
    else
        value %in% Ilist[-1]
}
isEmpty = function(value) is.null(value) || length(value) == 0
##a candidate must be in the head of at least one list
##and not in the tail of any - return c or FALSE
candidate = function(cl, rI) {
    if( any(sapply(rI, isHead, cl)) &&
       !any(sapply(rI, inTail, cl)) )
        cl
    else
        FALSE
}

##return a candidate or FALSE
candidateDirectSC = function(cl, rI) {
    classL = as.character(superClasses(getClass(cl)))
    ans = sapply(classL, function(x, y) candidate(x, y), rI)
    ans = ans[ans!=FALSE]
    if( length(ans > 0) )
        ans[1]
    else
        FALSE
}

removeNext = function(inList, nval) {
    if(length(inList) == 0 )
        inList
    else if (inList[1] == nval) inList[-1]
    else inList
}

mergeLists = function(revPartialResult, remainingInputs, C3=FALSE) {
    if (all(sapply(remainingInputs, isEmpty)) )
        rev(revPartialResult)
    else {
         if( C3 )
            nvC = sapply(remainingInputs, candidateAtHead,
                          remainingInputs)
        else
            nvC = sapply(revPartialResult, candidateDirectSC,
                          remainingInputs)

        if (!all(nvC == FALSE)) {
            nvC = nvC[nvC!=FALSE][1]
            mergeLists(c(nvC, revPartialResult),
                       sapply(remainingInputs, removeNext, nvC), C3)
        }
        else
            stop("Inconsistent precedence graph")
    }
}

##it seems like this needs to be in the right linearization order to start
OLDcplList = function(c)
    getAllSuperClasses(getClass(c))

cplList = function(c, C3)
    computeClassLinearization(c, C3)



candidateC3 = function(cl, rI) {
## returns c if it can go in the result now,
## otherwise false
    if( !any(sapply(rI, inTail, cl)) )
        cl
    else
        FALSE
}

candidateAtHead = function(inList, rI) {
    ##if( inList[1] == "scrolling-mixin") browser()
    if( !isEmpty(inList) )
        candidateC3(inList[1], rI)
    else
        FALSE
}

LPO = computeClassLinearization = function(inClass, C3 = FALSE) {

    cdirectSC = unlist(as.character(superClasses(getClass(inClass))))

    ans = mergeLists(inClass, c(lapply(cdirectSC, cplList, C3),
                           list(cdirectSC)), C3)
    names(ans) = NULL
    ans
}
