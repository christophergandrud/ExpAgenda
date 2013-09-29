#' Find the main stems associated with a topic
#' 
#' \code{TopicSummary} uses mus estimated from \code{\link{ExpAgendaVonmon}} to find the main word stems estimated to be associated with a given topic.
#' 
#' @param obj an \code{ExpAgendaOut} class object created by \code{\link{ExpAgendaVonmon}}.
#' @param NStems numeric. The number of top stems in a topic.
#' 
#' @return A data frame with three column: \code{TopicNumber}, \code{Stems}, and \code{Mus}.
#' 
#' @importFrom DataCombine MoveFront
#' 
#' @export

TopicSummary <- function(obj, NStems = 10){
  if (class(obj) != "ExpAgendaOut"){
    stop("obj must be a ExpAgendaOut class object created by ExpAgendaVonmon.")
  }
  
  NStems <- NStems - 1
  Mus <- data.frame(obj$mus)
  TopicCount <- ncol(Mus)
  TopicCount <- TopicCount + 1
  Mus$stems <- row.names(Mus)
  Mus <- MoveFront(Mus, "stems")
  
  OutDF <- data.frame()
  for (i in 2:TopicCount){
    temp <- Mus[, c(1, i)]
    n <- nrow(temp)
    Cut <- sort(temp[, 2], partial = n - NStems)[n - NStems]
    TopStems <- subset(temp, temp[, 2] >= Cut)
    
    TopStems$topic <- i - 1
    TopStems <- MoveFront(TopStems, "topic")
    names(TopStems) <- c("TopicNumber", "Stems", "Mus")
    
    OutDF <- rbind(OutDF, TopStems)
  }
  OutDF
}