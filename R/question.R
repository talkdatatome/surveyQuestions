
#' @export
question <- function(id, questionText, answersText, data=NULL){
    x <- structure(list(id=id, questionText=questionText, answersText=answersText, data=data), class="question")
    x
}

#' @export
ask <- function(q){ UseMethod("ask") }
#' @export
ask.question <- function(q){ warning("Unknown question. Returning NULL") }
#' @export
ask.questionMCOA <- function(q){ 
    print(q$questionText)
    for(ans in 1:length(q$answersText)){
        print(paste(ans, ") ", q$answersText[[ans]], sep=""))
    }
    user_id <- readline(prompt="Papers please: ")
    user_id <- as.integer(user_id)

    choice <- NULL
    while( is.null(choice) || !(choice %in% 1:length(q$answersText))){
        choice <- readline(prompt="Choose: ") 
        choice <- as.integer(choice)
    }
    
    #add data to MCOA
    q <- addResult(q, user_id, choice)
    q
}


#' @export
addResult <- function(q, user_id, ans){ UseMethod("addResult") }
#' @export
addResult.default <- function(q, user_id, ans){ 
    if(!(ans %in% 1:length(q$answersText))){
        stop("answer position does not seem to be in range of possible answers!")
    }
    q$data <- rbind(q$data, data.frame(id=user_id, ans=ans))
    q
}

#' @export
questionMCOA <- function(id, questionText, answersText, type, answerDirection){
    x <- question(id, questionText, answersText)
    x$type <- type
    if (type == "nominal"){

    }else{ 
        if (type == "ordinal"){
            x$answerDirection <- answerDirection
        }else{
            stop("Bad type")
        }
    }
    class(x) <- c("questionMCOA", class(x))
    x
}



