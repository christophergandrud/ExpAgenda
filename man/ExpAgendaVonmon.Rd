\name{ExpAgendaVonmon}
\alias{ExpAgendaVonmon}
\title{Bayesian hierarchical expressed agenda model}
\source{
  Grimmer, J. (2010). A Bayesian Hierarchical Topic Model
  for Political Texts: Measuring Expressed Agendas in
  Senate Press Releases. Political Analysis, 18, 1-35.
  \url{http://pan.oxfordjournals.org/content/18/1/1.short}.
}
\usage{
  ExpAgendaVonmon(obj = NULL, term.doc = NULL,
    authors = NULL, n.cats = 10, kappa = 400,
    verbose = TRUE)
}
\arguments{
  \item{obj}{an EADTMatrix class object created by
  \code{\link{PreProcess}} containing the \code{term.doc}
  and \code{authors} matrices. Note \code{term.doc} and
  \code{authors} should not be set independently if
  \code{obj} is specified.}

  \item{term.doc}{matrix. Suppose that there are \eqn{D}
  total documents and \eqn{w} words. \code{term.doc} is a
  term-document matrix with dimensions: \eqn{D * w}.
  \code{term.doc} should be sorted by author. Do not set if
  \code{obj} is specified.}

  \item{authors}{matrix. If there are \eqn{n} total actors
  whose attention to issues you would like to measure.
  \code{author} is an \eqn{n * 2} matrix. The first column
  specifies the first document in the term-document matrix
  that was authored by a given author. The second column
  specifies the last document authored. Do not set if
  \code{obj} is specified.}

  \item{n.cats}{numeric. Sets the number of components
  (topics) in the mixture model. The default is 10.}

  \item{kappa}{numeric. Distribution's dispersion [CHECK].}

  \item{verbose}{logical. Whether or not to print out each
  iteration.}
}
\value{
  A \code{ExpAgendaOut} object. The object contains five
  elements: \code{thetas}, \code{mus}, \code{rs},
  \code{alpha}, and \code{authorID}. \code{thetas} are
  [FILL IN]. \code{mus} are location on the unit hyperspace
  where the vMF distribution reaches its mode for each stem
  [CHECK]. \code{rs} are the probability of document
  \eqn{j} from author \eqn{i} being from topic \eqn{k}.
  \code{alphas} are the prior distributions of the topics.
  \code{authorID} is used for \code{\link{DocTopics}} to
  return the documents their their original order.
}
\description{
  \code{ExpAgendaVonmon} implements bayesian hierarchical
  topic model for texts from Grimmer (2010).
}

