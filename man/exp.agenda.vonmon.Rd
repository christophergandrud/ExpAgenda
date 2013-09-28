\name{exp.agenda.vonmon}
\alias{exp.agenda.vonmon}
\title{Bayesian hierarchical expressed agenda model}
\source{
  Grimmer, J. (2010). A Bayesian Hierarchical Topic Model
  for Political Texts: Measuring Expressed Agendas in
  Senate Press Releases. Political Analysis, 18, 1-35.
  \url{http://pan.oxfordjournals.org/content/18/1/1.short}.
}
\usage{
  exp.agenda.vonmon(obj = NULL, term.doc = NULL,
    authors = NULL, n.cats = 10, verbose = TRUE,
    kappa = 400)
}
\arguments{
  \item{obj}{an EADTMatrix class object created by
  \code{\link{PreProsses}} containing the \code{term.doc}
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

  \item{verbose}{}

  \item{kappa}{}
}
\description{
  \code{exp.agenda.vonmon} implements bayesian hierarchical
  topic model for texts from Grimmer (2010).
}

