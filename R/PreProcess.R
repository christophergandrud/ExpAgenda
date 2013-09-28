#' Pre-process texts to create a corpus suitable for \code{ExpAgendaVonmon}
#' 
#' \code{PreProcess} prepares texts and author information for use with \code{\link{ExpAgendaVonmon}}.
#' 
#' @param textsDF a data frame containing a column with texts and a column with author names. Unnecessary if \code{textsDir} and \code{authorsDF} are set.
#' @param TextsCol character string identifying the column in \code{textsDF} with the texts.
#' @param AuthorCol character string identifying the column in either \code{textsDF} or \code{authorDF} identifying the authors.
#' @param textsPattern character string. Regular expression pattern identifying the texts in \code{textsDF}. nnecessary if \code{textDF} is set.
#' @param textsDir character string. A directory containing texts separated into individual text files. Unnecessary if \code{textDF} is set.
#' @param authorsDF a data frame with author information for each text in \code{textDF}. They must be in the same order. Unnecessary if \code{textDF} is set.
#' @param removeNumbers logical. Whether or not to remove numbers from the texts.
#' @param StopWords character vector of stop words to remove. If \code{StopWords = NULL} (the default) then \code{\link{tm}}'s default English stop word list will be used. See \code{\link{stopwords}}.
#' @param removeAuthors character vector. The names of authors to remove.
#' @param sparse numeric for the maximal allowed sparsity. See \code{\link{removeSparseTerms}}
#' 
#' @importFrom tm stopwords
#' @importFrom tm removeNumbers
#' @importFrom tm removePunctuation
#' @importFrom tm removeWords
#' @importFrom tm stripWhitespace
#' @importFrom tm Corpus
#' @importFrom tm tm_map
#' @importFrom tm stemDocument
#' @importFrom tm DocumentTermMatrix
#' @importFrom tm removeSparseTerms
#' 
#' 
#' @export

PreProcess <- function(textsDF = NULL, TextsCol, AuthorCol, textsDir = NULL, textsPattern, authorsDF = NULL, removeNumbers = TRUE, StopWords = NULL, removeAuthors = NULL, sparse = 0.4){
  # Determine if textsDF or textsDir/authorsDF is specified
  if (!is.null(textsDF) & !is.null(textsDir) & !is.null(authorsDF)){
    stop("Only textsDF or textsDir & authorsDF can be set at once.")
  }
  if (!is.null(textsDF) & !is.null(textsDir)){
    stop("term.doc cannot be set if textsDF is also specified.")
  }
  if (!is.null(textsDF) & !is.null(authorsDF)){
    stop("authorsDF cannot be set if textsDF is also specified.")
  }
  
  if (!is.null(textsDir)){ 
    Patt <- paste0(textsDir, textsPattern)
    FileList <- list.files(path = textsDir, pattern = textsPattern)
    Texts <- lapply(FileList, readLines)
    TextsV <- as.vector(Texts)
  }
  else if(!is.null(textsDF)){
    TextsV <- textDF[, TextsCol]
  }
  
  # Basic clean
  if (isTRUE(removeNumbers)){
      TextsV <- removeNumbers(TextsV)
  }
  TextsV <- tolower(TextsV)
  TextsV <- removePunctuation(TextsV)

  if (is.null(StopWords)){
    StopWords <- stopwords(kind = "en")
  }
  else if (!is.null(StopWords)){
    StopWords <- StopWords
  }
  TextsV <- removeWords(TextsV, StopWords)
  TextsV <- stripWhitespace(TextsV)
  
  if (!is.null(authorsDF)){
    MetaSub <- authorsDF[, AuthorCol]
  }
  else if (!is.null(textsDF)){
    MetaSub <- textsDF[, AuthorCol]
  }
  
  # Bind into one data frame
  Full <- cbind(MetaSub, TextsV)
  
  # Remove specific authors
  if (!is.null(removeAuthors)){
    for (i in removeAuthors){
      Full <- subset(Full, name != i)
    }
  }
  #### Create author matrix ####
  # Order by author
  FullOrd <- Full[order(Full$name), ]
  
  # Create author matrix
  AuthorsRaw <- as.data.frame(FullOrd$name)
  AuthorsRaw$ID <- row.names(AuthorsRaw)
  
  First <- by(AuthorsRaw, AuthorsRaw[, 1], head, n = 1)
  Last <- by(AuthorsRaw, AuthorsRaw[, 1], tail, n = 1)
  
  First <- do.call("rbind", as.list(First))
  Last <- do.call("rbind", as.list(Last))
  
  authors <- cbind(First[, 1:2], Last[, 2])
  
  names(authors) <- c("name", "first", "last")
  authors$first <- as.numeric(authors$first)
  authors$last <- as.numeric(as.character(authors$last))
  authors <- as.matrix(authors[, 2:3])
  
  #### Create document term matrix ####
  # Convert to corpus
  FullCorp <- Corpus(VectorSource(FullOrd$TextsV))
  
  # Stemming
  Stems <- tm_map(FullCorp, stemDocument)
  
  # Create term document matrix
  TermDoc <- DocumentTermMatrix(Stems)
  
  # Remove sparse terms
  TermDocS <- removeSparseTerms(TermDoc, sparse)
  
  # Plain matrix
  term.doc <- as.matrix(TermDocS) 
  
  # Create ExpAgendaDTMatrix object
  EADTMatrix <- list(authors, term.doc)
  names(EADTMatrix) <- c("authors", "term.doc")
  class(EADTMatrix) <- "ExpAgendaDTMatrix"
  return(EADTMatrix)
}