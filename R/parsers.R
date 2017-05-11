#' Parse YAML encoded metadata retrieved via \code{search.code}
#' @template parser
#' @examples \dontrun{
#' 	
#' # search for Quanlets	
#' q_search = 'Quantlet Published Description Keywords Author filename:"metainfo.txt"'
#' spec_search_term = "yaml user:Quantlet user:lborke user:b2net"
#' sr = search.code(paste(spec_search_term, q_search), per_page = 20)	
#' q_top = yaml.search.parser(sr, print_item = FALSE)
#' 
#' # search for Gitenberg ebooks
#' r_pack_search = 'gitenberg filename:"metadata.yaml"'
#' spec_search_term = "king arthur"
#' sr = search.code(paste(spec_search_term, r_pack_search), per_page = 20)
#' yaml_top = yaml.search.parser(sr, print_item = FALSE)	
#' }
#' @export
yaml.search.parser = function(search.code.results, print_item = FALSE, error_to_results = TRUE) {
	yaml_list = list()
	techinfo = list()
	error_count = 0
	i_counter = 0
	
	if (!is.null(search.code.results$content$items)) {
		items_list = search.code.results$content$items
	} else if (!is.null(search.code.results$full_search)) {
		items_list = search.code.results$full_search
	} else {
		items_list = search.code.results
	}
	
	item_len = length(items_list)
	
	for (item in items_list) {
		path_raw = paste("https://raw.githubusercontent.com", item$repository$full_name, "master", item$path, sep ="/")
		r = GET(path_raw)
		q_str = httr::content(r, "text")
		result = try( meta_parsed_yaml <- yaml.load(q_str), silent = FALSE )
		if ("try-error" %in% class(result)) {
			print(path_raw); flush.console()
			error_count = error_count + 1
			if (error_to_results) {
				parser_res = list()
				parser_res$ok		= FALSE
				parser_res$path		= item$repository$full_name
				parser_res$score	= item$score
				# yaml_list = c(yaml_list, list("YAML error"))
				yaml_list	= c(yaml_list, list(meta_parsed_yaml))
				techinfo	= c(techinfo, list(parser_res))
			}
		} else {
			if (print_item) {print(meta_parsed_yaml); flush.console()}
			parser_res = list()
			parser_res$ok		= TRUE
			parser_res$path		= item$repository$full_name
			parser_res$score	= item$score
			yaml_list	= c(yaml_list, list(meta_parsed_yaml))
			techinfo	= c(techinfo, list(parser_res))
		}
		i_counter = i_counter + 1
		if ((i_counter %% 10) == 0) {
			print(paste("item number:", i_counter))
			print(paste(round(i_counter / item_len, 4) * 100, "% done"))
			flush.console()
		}
	}
	
	ok_vec		= sapply( techinfo, function(info){ info$ok } )
	path_vec	= sapply( techinfo, function(info){ info$path } )
	score_vec	= sapply( techinfo, function(info){ info$score } )
	
	res = list()
	res$parsed_list	= yaml_list
	res$techinfo	= techinfo
	res$error_count = error_count
	res$type		= "yaml"
	res$ok_vec		= ok_vec
	res$path_vec	= path_vec
	res$score_vec	= score_vec
	
	return(res)
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
#' dcf_top = dcf.search.parser(sr)
#' }
#' @export
dcf.search.parser = function(search.code.results, print_item = FALSE, error_to_results = TRUE) {
	dcf_list = list()
	techinfo = list()
	error_count = 0
	i_counter = 0
	
	if (!is.null(search.code.results$content$items)) {
		items_list = search.code.results$content$items
	} else if (!is.null(search.code.results$full_search)) {
		items_list = search.code.results$full_search
	} else {
		items_list = search.code.results
	}
	
	item_len = length(items_list)
	
	for (item in items_list) {
		path_raw_dcf = paste("https://raw.githubusercontent.com", item$repository$full_name, "master/DESCRIPTION", sep ="/")
		con = url(path_raw_dcf)
		result = try( meta_parsed_cran <- read.dcf(con, all = TRUE) )
		close(con)
		if ("try-error" %in% class(result)) {
			print(path_raw_dcf); flush.console()
			error_count = error_count + 1
			if (error_to_results) {
				parser_res = list()
				parser_res$ok		= FALSE
				parser_res$path		= item$repository$full_name
				parser_res$score	= item$score
				# dcf_list = c(dcf_list, list("DCF error"))
				dcf_list	= c(dcf_list, list(meta_parsed_cran))
				techinfo	= c(techinfo, list(parser_res))
			}
		} else {
			if (print_item) {print(meta_parsed_cran); flush.console()}
			parser_res = list()
			parser_res$ok		= TRUE
			parser_res$path		= item$repository$full_name
			parser_res$score	= item$score
			dcf_list	= c(dcf_list, list(meta_parsed_cran))
			techinfo	= c(techinfo, list(parser_res))
		}
		i_counter = i_counter + 1
		if ((i_counter %% 10) == 0) {
			print(paste("item number:", i_counter))
			print(paste(round(i_counter / item_len, 4) * 100, "% done"))
			flush.console()
		}
	}
	
	ok_vec		= sapply( techinfo, function(info){ info$ok } )
	path_vec	= sapply( techinfo, function(info){ info$path } )
	score_vec	= sapply( techinfo, function(info){ info$score } )
		
	res = list()
	res$parsed_list	= dcf_list
	res$techinfo	= techinfo
	res$error_count = error_count
	res$type		= "CRAN/dcf"
	res$ok_vec		= ok_vec
	res$path_vec	= path_vec
	res$score_vec	= score_vec
	
	return(res)
}



json.search.parser = function(search.code.results, print_item = FALSE, error_to_results = TRUE) {
	json_list = list()
	techinfo = list()
	error_count = 0
	i_counter = 0

	if (!is.null(search.code.results$content$items)) {
		items_list = search.code.results$content$items
	} else if (!is.null(search.code.results$full_search)) {
		items_list = search.code.results$full_search
	} else {
		items_list = search.code.results
	}
	
	item_len = length(items_list)
	
	for (item in items_list) {
		#path_raw = paste("https://raw.githubusercontent.com", item$repository$full_name, "master", item$path, sep ="/")
		path_raw = gsub("github.com", "raw.githubusercontent.com", item$html_url)
		path_raw = gsub("/blob", "", path_raw)
		r = GET(path_raw)
		q_str = httr::content(r, "text")
		result = try( meta_parsed_json <- fromJSON(q_str), silent = FALSE )
		# if (class(result) == "try-error") {
		if ("try-error" %in% class(result)) {
			print(path_raw); flush.console()
			error_count = error_count + 1
			if (error_to_results) {
				parser_res = list()
				parser_res$ok		= FALSE
				parser_res$path		= item$repository$full_name
				parser_res$score	= item$score
				# res$error = "JSON error"
				json_list	= c(json_list, list(meta_parsed_json))
				techinfo	= c(techinfo, list(parser_res))
			}
		} else {
			if (print_item) {print(meta_parsed_json); flush.console()}
			
			parser_res = list()
			parser_res$ok		= TRUE
			parser_res$path		= item$repository$full_name
			parser_res$score	= item$score
			
			json_list	= c(json_list, list(meta_parsed_json))
			techinfo	= c(techinfo, list(parser_res))
		}
		i_counter = i_counter + 1
		if ((i_counter %% 10) == 0) {
			print(paste("item number:", i_counter))
			print(paste(round(i_counter / item_len, 4) * 100, "% done"))
			flush.console()
		}
	}
	
	ok_vec		= sapply( techinfo, function(info){ info$ok } )
	path_vec	= sapply( techinfo, function(info){ info$path } )
	score_vec	= sapply( techinfo, function(info){ info$score } )
	
	res = list()
	res$parsed_list	= json_list
	res$techinfo	= techinfo
	res$error_count = error_count
	res$type		= "json"
	res$ok_vec		= ok_vec
	res$path_vec	= path_vec
	res$score_vec	= score_vec
	
	return(res)
}



flat.search.parser = function(search.code.results, print_item = TRUE, error_to_results = TRUE) {
	flat.text_list = list()
	techinfo = list()
	i_counter = 0
	
	if (!is.null(search.code.results$content$items)) {
		items_list = search.code.results$content$items
	} else if (!is.null(search.code.results$full_search)) {
		items_list = search.code.results$full_search
	} else {
		items_list = search.code.results
	}
	
	item_len = length(items_list)	
	
	for (item in items_list) {
		path_raw = gsub("github.com", "raw.githubusercontent.com", item$html_url)
		path_raw = gsub("/blob", "", path_raw)
		r = GET(path_raw)
		q_str = httr::content(r, "text")
		
		if (print_item) {print(q_str); flush.console()}
		
		parser_res = list()
		parser_res$ok		= TRUE
		parser_res$path		= item$repository$full_name
		parser_res$score	= item$score
		flat.text_list	= c(flat.text_list, list(q_str))
		techinfo		= c(techinfo, list(parser_res))
		
		i_counter = i_counter + 1
		if ((i_counter %% 10) == 0) {
			print(paste("item number:", i_counter))
			print(paste(round(i_counter / item_len, 4) * 100, "% done"))
			flush.console()
		}
	}
	
	ok_vec		= sapply( techinfo, function(info){ info$ok } )
	path_vec	= sapply( techinfo, function(info){ info$path } )
	score_vec	= sapply( techinfo, function(info){ info$score } )
	
	res = list()
	res$parsed_list	= flat.text_list
	res$techinfo	= techinfo
	res$type		= "plain text"
	res$ok_vec		= ok_vec
	res$path_vec	= path_vec
	res$score_vec	= score_vec
	
	return(res)
}



