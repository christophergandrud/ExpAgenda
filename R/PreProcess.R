#' Pre-process texts to create a corpus suitable for \code{ExpAgendaVonmon}
#' 
#' \code{PreProcess} prepares texts and author information for use with \code{\link{ExpAgendaVonmon}}.
#' 
#' @param textsDF a data frame containing a column with texts and a column with author names. Unnecessary if \code{textsDir} and \code{authorsDF} are set.
#' @param TextsCol character string identifying the column in \code{textsDF} with the texts.
#' @param AuthorCol character string identifying the column in either \code{textsDF} or \code{authorDF} identifying the authors.
#' @param textsPattern character string. Regular expression pattern identifying the texts in \code{textsDF}. nnecessary if \code{textDF} is set.
#' @param authorsDF a data frame with author information for each text in \code{textDF}. They must be in the same order. Unnecessary if \code{textDF} is set.
#' @param removeNumbers logical. Whether or not to remove numbers from the texts.
#' @param StopWords character vector of stop words to remove. If \code{StopWords = NULL} (the default) then \code{tm}'s default English stop word list will be used. See \code{\link{stopwords}}.
#' @param removeAuthors character vector. The names of authors to remove.
#' @param sparse numeric for the maximal allowed sparsity. See \code{\link{removeSparseTerms}}
#'
#' @return Returns an object of class \code{ExpAgendaDTMatrix} that can be used with \code{\link{ExpAgendaVonmon}} to estimated authors' expressed agendas in documents. The object contains two matrices. \code{doc.term} is a document term matrix and \code{authors} locates the authors of the texts in \code{doc.term}. 
#'  
#' 
#' @source Feinerer, K. Hornik, and D. Meyer. Text mining infrastructure in R. Journal of Statistical Software, 25(5):1-54, March 2008. \url{http://www.jstatsoft.org/v25/i05}.
#' 
#' @importFrom tm stopwords
#' @importFrom tm removeNumbers
#' @importFrom tm removePunctuation
#' @importFrom tm removeWords
#' @importFrom tm stripWhitespace
#' @importFrom tm VectorSource
#' @importFrom tm Corpus
#' @importFrom tm tm_map
#' @importFrom tm stemDocument
#' @importFrom tm DocumentTermMatrix
#' @importFrom tm removeSparseTerms
#' @importFrom plyr rename
#' 
#' 
#' @export

PreProcess <- function(textsDF = NULL, TextsCol, AuthorCol, textsPattern = NULL, authorsDF = NULL, removeNumbers = TRUE, StopWords = NULL, removeAuthors = NULL, sparse = 0.4){
  textDF <- VectorSource <- NULL
  # Determine if textsDF or textsPattern/authorsDF is specified
  if (!is.null(textsDF) & !is.null(textsPattern) & !is.null(authorsDF)){
    stop("Only textsDF or textsPattern & authorsDF can be set at once.")
  }
  if (!is.null(textsDF) & !is.null(textsPattern)){
    stop("term.doc cannot be set if textsDF is also specified.")
  }
  if (!is.null(textsDF) & !is.null(authorsDF)){
    stop("authorsDF cannot be set if textsDF is also specified.")
  }
  
  if (!is.null(textsPattern)){ 
    FileList <- list.files(pattern = textsPattern)
    TextsV <- lapply(FileList, readLines)
  }
  else if (!is.null(textsDF)){
    TextsV <- textDF[, TextsCol]
  }
  
  # Basic clean
  message("Converting all text to lower case.")
  TextsV <- tolower(TextsV)
  message("Removing all punctuation.")
  TextsV <- removePunctuation(TextsV)
  if (isTRUE(removeNumbers)){
      message("Removing all numbers.")
      TextsV <- removeNumbers(TextsV)
  }

  message("Removing stop words.")
  if (is.null(StopWords)){
    StopWords <- stopwords(kind = "en")
  }
  else if (!is.null(StopWords)){
    StopWords <- StopWords
  }
  TextsV <- removeWords(TextsV, StopWords)
  message("Removing white space.")
  TextsV <- stripWhitespace(TextsV)
  
  if (!is.null(authorsDF)){
    names <- authorsDF[, AuthorCol]
  }
  else if (!is.null(textsDF)){
    names <- textsDF[, AuthorCol]
  }
  
  # Bind into one data frame
  Full <- cbind(names, TextsV)
  
  # Remove specific authors
  if (!is.null(removeAuthors)){
    message("Dropping unwanted authors.")
    for (i in removeAuthors){
      Full <- subset(Full, names != i)
    }
  }
  #### Create author matrix ####
  # Order by author
  FullOrd <- Full[order(Full[, 1]), ]
  
  # Create author matrix
  AuthorsRaw <- as.data.frame(FullOrd[, 1])
  AuthorsRaw$ID <- row.names(AuthorsRaw)
  
  First <- by(AuthorsRaw, AuthorsRaw[, 1], head, n = 1)
  Last <- by(AuthorsRaw, AuthorsRaw[, 1], tail, n = 1)
  
  First <- do.call("rbind", as.list(First))
  Last <- do.call("rbind", as.list(Last))
  
  authors <- cbind(First[, 1:2], Last[, 2])
  
  names(authors) <- c("names", "first", "last")
  authors$first <- as.numeric(authors$first)
  authors$last <- as.numeric(as.character(authors$last))
  authors <- as.matrix(authors[, 2:3])
  
  #### Create document term matrix ####
  # Convert to corpus
  FullCorp <- Corpus(VectorSource(FullOrd[, 2]))
  
  # Stemming
  message("Creating stems.")
  Stems <- tm_map(FullCorp, stemDocument)
  
  # Create term document matrix
  TermDoc <- DocumentTermMatrix(Stems)
  
  # Remove sparse terms
  message("Removing sparse terms.")
  TermDocS <- removeSparseTerms(TermDoc, sparse)
  
  # Plain matrix
  term.doc <- as.matrix(TermDocS) 
  
  # Summarize the data
  NAuthors <- nrow(authors)
  NStems <- ncol(term.doc)
  message(paste("There are", NAuthors, "authors."))
  message(paste("There are", NStems, "stems."))
  
  # Create ExpAgendaDTMatrix object
  EADTMatrix <- list(authors, term.doc)
  names(EADTMatrix) <- c("authors", "term.doc")
  class(EADTMatrix) <- "ExpAgendaDTMatrix"
  return(EADTMatrix)
}