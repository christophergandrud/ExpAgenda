\name{exp.agenda.vonmon}
\alias{exp.agenda.vonmon}
\title{Bayesian hierarchical expressed agenda model}
\source{
  Grimmer, J. (2010). A Bayesian Hierarchical Topic Model
  for Political Texts: Measuring Expressed Agendas in
  Senate Press Releases. Political Analysis, 18, 1-35.
  doi:10.1093/pan/mpp034
}
\usage{
  exp.agenda.vonmon(term.doc, authors, n.cats,
    verbose = TRUE, kappa = 400)
}
\arguments{
  \item{term.doc}{matrix. Suppose that there are \eqn{D}
  total documents and \eqn{w} words. \code{term.doc} is a
  term-document matrix with dimensions: \eqn{D * w}.
  \code{term.doc} should be sorted by author.}

  \item{author}{matrix. If there are \eqn{n} total actors
  whose attention to issues you would like to measure.
  \code{author} is an \eqn{n * 2} matrix. The first column
  specifies the first document in the term-document matrix
  that was authored by a given author. The second column
  specifies the last document authored.}

  \item{n.cats}{numeric. Sets the number of components
  (topics) in the mixture model.}
}
\description{
  \code{exp.agenda.vonmon} implements bayesian hierarchical
  topic model for texts from Grimmer (2010).
}

