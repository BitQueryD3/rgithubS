% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parsers.R
\name{yaml.search.parser}
\alias{yaml.search.parser}
\title{Parse YAML encoded metadata retrieved via \code{search.code}}
\usage{
yaml.search.parser(search.code.results, print_item = FALSE,
  error_to_results = TRUE)
}
\arguments{
\item{search.code.results}{results from \code{search.code}}

\item{print_item}{if true, then print retrieved items}

\item{error_to_results}{if true, then print information on errors}
}
\value{
a list with processed data
}
\description{
Parse YAML encoded metadata retrieved via \code{search.code}
}
\examples{
\dontrun{
	
# search for Quanlets	
q_search = 'Quantlet Published Description Keywords Author filename:"metainfo.txt"'
spec_search_term = "yaml user:Quantlet user:lborke user:b2net"
sr = search.code(paste(spec_search_term, q_search), per_page = 20)	
q_top = yaml.search.parser(sr, print_item = FALSE)

# search for Gitenberg ebooks
r_pack_search = 'gitenberg filename:"metadata.yaml"'
spec_search_term = "king arthur"
sr = search.code(paste(spec_search_term, r_pack_search), per_page = 20)
yaml_top = yaml.search.parser(sr, print_item = FALSE)	
}
}

