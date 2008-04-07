
 intActFilter <- function() {
     numInt <- 0
     currentRecord <- 0
     expectingName <- FALSE

     inIntList <- FALSE

     interactionList <- function(name, attrs) 
         inIntList <<- TRUE

     interactions <- NULL
     intids <- NULL

     interaction <- function(name, attrs) {
         if( inIntList ) {
            intids <<- c(attrs["id"], intids) 
            expectingName <<- TRUE
            numInt <<- numInt + 1
         }
     }

     text <- function(txt) {
        if( expectingName && nchar(txt) > 0 ) {
          interactions <<- c(txt, interactions)
          expectingName <<- FALSE
        }
     }

     return(list(interaction = interaction, text=text, 
              interactionList = interactionList))
 }

