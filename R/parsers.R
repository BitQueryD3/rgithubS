#' Parse YAML encoded metadata retrieved via \code{search.code}
#' @template parser
#' @examples \dontrun{
#' 	
#' # search for Quanlets	
#' q_search = 'Quantlet Published Description Keywords Author filename:"metainfo.txt"'
#' spec_search_term = "yaml user:Quantlet user:lborke user:b2net"
#' sr = search.code(paste(spec_search_term, q_search), per_page = 20)	
#' q_top = yaml.parser.light(sr, print_item = FALSE)
#' 
#' # search for Gitenberg ebooks
#' r_pack_search = 'gitenberg filename:"metadata.yaml"'
#' spec_search_term = "king arthur"
#' sr = search.code(paste(spec_search_term, r_pack_search), per_page = 20)
#' yaml_top = yaml.parser.light(sr, print_item = FALSE)	
#' }
#' @export
yaml.parser.light = function(search.code.results, print_item = TRUE, error_to_results = TRUE) {
	yaml_list = list()
	for (item in search.code.results$content$items) {
		path_raw = paste("https://raw.githubusercontent.com", item$repository$full_name, "master", item$path, sep ="/")
		r = GET(path_raw)
		q_str = httr::content(r, "text")
		result = try( meta_parsed_yaml <- yaml.load(q_str), silent = FALSE )
		if (class(result) == "try-error") {
			print(path_raw); flush.console()
			if (error_to_results) {yaml_list = c(yaml_list, list("YAML error"))}
		} else {
			if (print_item) {print(meta_parsed_yaml); flush.console()}
			yaml_list = c(yaml_list, list(meta_parsed_yaml))
		}
	}
	return(yaml_list)
}


#' Parse DCF encoded metadata retrieved via \code{search.code}
#' @template parser
#' @examples \dontrun{
#' 
#' # search for R packages
#' r_pack_search = 'Package Title Version Description Author
#' 				filename:"DESCRIPTION" path:"/"'
#' spec_search_term = "yaml"
#' sr = search.code(paste(spec_search_term, r_pack_search), per_page = 10)
#' dcf_top = dcf.parser.light(sr)
#' }
#' @export
dcf.parser.light = function(search.code.results, print_item = TRUE, error_to_results = TRUE) {
	dcf_list = list()
	for (item in search.code.results$content$items) {
		path_raw_dcf = paste("https://raw.githubusercontent.com", item$repository$full_name, "master/DESCRIPTION", sep ="/")
		con = url(path_raw_dcf)
		result = try( meta_parsed_cran <- read.dcf(con, all = TRUE) )
		close(con)
		if (class(result) == "try-error") {
			print(path_raw_dcf); flush.console()
			if (error_to_results) {dcf_list = c(dcf_list, NULL)}
		} else {
			if (print_item) {print(meta_parsed_cran); flush.console()}
			dcf_list = c(dcf_list, list(meta_parsed_cran))
		}
	}
	return(dcf_list)
}

json.parser.light = function(search.code.results, print_item = TRUE, error_to_results = TRUE) {
	json_list = list()
	for (item in search.code.results$content$items) {
		#path_raw = paste("https://raw.githubusercontent.com", item$repository$full_name, "master", item$path, sep ="/")
		path_raw = gsub("github.com", "raw.githubusercontent.com", item$html_url)
		path_raw = gsub("/blob", "", path_raw)
		r = GET(path_raw)
		q_str = httr::content(r, "text")
		result = try( meta_parsed_json <- fromJSON(q_str), silent = FALSE )
		if (class(result) == "try-error") {
			print(path_raw); flush.console()
			if (error_to_results) {json_list = c(json_list, list("JSON error"))}
		} else {
			if (print_item) {print(meta_parsed_json); flush.console()}
			meta_parsed_json$path = item$repository$full_name
			meta_parsed_json$score = item$score
			json_list = c(json_list, list(meta_parsed_json))
		}
	} 
	return(json_list)
}

get.field = function(f_name, object) {
	field = sapply(object, function(item){ if (is.recursive(item)) {item[f_name]} else {"error"} } ) # if JSON is not valid
	if (f_name=="score") {n_field = round(as.numeric(field), 2)}
	else {
		field = unname(field)
		n_field = sapply(field, function(item){paste(unlist(item), collapse=", ")} ) # if data field does not exist or there is a vector of values
    	n_field = unlist(n_field)
    }
	return(n_field)
}

get.data = function(fields, object) {
	dat = data.frame(matrix(, nrow=length(object), ncol=0))
	for (item in fields) {
		col = get.field(item, object)
		dat = cbind(dat, col)
	}
	colnames(dat) <- paste("package.", fields, sep="")
	return(dat)
}