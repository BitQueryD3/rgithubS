
#' Get summary statistics for Quantlets available on Github
#' 
#' @param spec_editor an optional character vector with the desired author list for being displayed 
#' 
#' @param json_path an optional character with the url for json file with all validated Quantlets available on QuantNetXploRer 
#' 
#' @examples \dontrun{
#' cran.stats(spec_editor = c("borke", "bykovskaya"))
#' }
#' @export
qnet.stats = function(spec_editor = c("borke", "bykovskaya"), json_path = "http://quantlet.de/data/qlets_github_ia_utf8.json") {
	q_stat_r = list()
	
	q_yaml_search = 'Quantlet Published Description Keywords Author filename:"Metainfo.txt"'

	sr = search.code(q_yaml_search, per_page = 100)
	q_stat_r$full_gh = sr$content$total_count
	
	spec_search_term = "user:quantlet"
	sr = search.code(paste(spec_search_term, q_yaml_search), per_page = 100)
	q_stat_r$quantlet_gh_org = sr$content$total_count
	
	#QuantNetXploRer: validated Qlets
	con = url(json_path, encoding = "UTF-8")
	q_json = fromJSON(con)
	q_stat_r$QuantNetXploRer = dim(q_json$nodes)[1]

	for (a_str in spec_editor) {
		sr = search.code(paste(a_str, q_yaml_search), per_page = 100)
		q_stat_r[[paste(a_str, "as editor")]] = sr$content$total_count
	}
	
	return(q_stat_r)
}

#' Get summary statistics for \code{R} packages available on Github and CRAN
#' 
#' @param spec_author an optional character vector with the desired author list for being displayed 
#' 
#' @examples \dontrun{
#' cran.stats(spec_author = c("hornik", "leisch"))
#' }
#' @export
cran.stats = function(spec_author = c("hornik", "leisch")) {
	cran_stat_r = list()
	
	r_pack_search = 'Package Title Version Description Author filename:"DESCRIPTION" path:"/"'
	
	sr = search.code(r_pack_search, per_page = 100)
	cran_stat_r$full_gh = sr$content$total_count
	
	spec_search_term = "user:cran"
	sr = search.code(paste(spec_search_term, r_pack_search), per_page = 100)
	cran_stat_r$cran_gh_org = sr$content$total_count
	
	ap_s = available.packages()
	cran_stat_r$official_CRAN_mirror = dim(ap_s)[1]
	
	for (a_str in spec_author) {
		sr = search.code(paste(a_str, r_pack_search), per_page = 100)
		cran_stat_r[[paste(a_str, "as author")]] = sr$content$total_count
	}
	
	return(cran_stat_r)
}

npm.stats = function(spec_author = c("Jonathan Ong", "James Halliday")) {
	npm_stat_r = list()
	
	npm_pack_search = 'name version description author node filename:"package.json" path:"/"'
	
	sr = search.code(npm_pack_search, per_page = 100)
	npm_stat_r$full_gh = sr$content$total_count
	
	for (a_str in spec_author) {
		sr = search.code(paste(a_str, npm_pack_search), per_page = 100)
		npm_stat_r[[paste(a_str, "as author")]] = sr$content$total_count
	}
	
	return(npm_stat_r)
}
