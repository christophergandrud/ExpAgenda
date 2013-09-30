\name{TopicSummary}
\alias{TopicSummary}
\title{Find the main stems associated with a topic}
\usage{
  TopicSummary(obj, NStems = 10)
}
\arguments{
  \item{obj}{an \code{ExpAgendaOut} class object created by
  \code{\link{ExpAgendaVonmon}}.}

  \item{NStems}{numeric. The number of stems with the most
  mutual information, i.e. the main stems in a topic.}
}
\value{
  A data frame with three columns: \code{TopicNumber},
  \code{Stems}, and \code{Mus}.
}
\description{
  \code{TopicSummary} uses mus estimated from
  \code{\link{ExpAgendaVonmon}} to find the main word stems
  estimated to be associated with a given topic.
}

