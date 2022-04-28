library(dplyr)
library(ggplot2)

#' Recursive Multi-factorial Outlier Detection 
#' 
#' @param questionnaire responses to a survey, with column "rt" (response times)!
#' @param cutoff_factor_long factor (x times mad deviation from median) for determining the upper cutoff points, i.e. for long outliers
#' @param cutoff_factor_short factor (y times mad deviation from median) for determining the lower cutoff points, i.e. for short outliers
#' @param item_function_colname name of the column of questionnaire specifying the item functions
#' @param participant_id_colname name of the column of questionnaire specifiying the ids of different participants
#' @param group which criteria are to be considered for determining the cutoff points, 
#' "item&participant" for proper remfod, 
#' "item" (outliers are only determined w.r.t. all rts of the same item function) or 
#' "participant" (outliers are only determined w.r.t. all rts of the same participants 
#' also available (i.e. for remfod_plot!)
#' @return the questionnaire with additional columns:
#' "cutoff_upper_item" & "cutoff_lower_item",
#' "cutoff_upper_worker" & "cutoff_lower_worker",
#' "cutoff_lower" & "cutoff_upper",
#' "direction" long or short outlier,
#' "degree" iteration number, i.e. degree 2 means that the rt is only considered outlying if outliers of degree 1 are removed from the data   
remfod = function(questionnaire, cutoff_factor_long = 2.5, cutoff_factor_short = 1.5, item_function_colname = "ITEM_FUNCTION", participant_id_colname = "workerId", group = "item&participant"){
	n = 1  # outlier degree
	outliers = data.frame()
	non_outliers = questionnaire 
	while(TRUE){
		item_cutoffs = non_outliers %>%
			dplyr::group_by_at(item_function_colname) %>%
			dplyr::summarise(
				cutoff_upper_item = median(rt) + cutoff_factor_long*mad(rt),
				cutoff_lower_item = median(rt) - cutoff_factor_short*mad(rt))
		worker_cutoffs = non_outliers %>% 
			dplyr::group_by_at(participant_id_colname) %>% 
			dplyr::summarise(
				cutoff_upper_worker = median(rt) + cutoff_factor_long*mad(rt),
				cutoff_lower_worker = median(rt) - cutoff_factor_short*mad(rt))
		cutoffs = merge(item_cutoffs, worker_cutoffs) %>% 
			rowwise() %>%
			mutate(
				cutoff_lower = ifelse(group == "participant", cutoff_lower_worker,
					ifelse(group == "item", cutoff_lower_item, min(cutoff_lower_item, cutoff_lower_worker))),
				cutoff_upper = ifelse(group == "participant", cutoff_upper_worker, 
					ifelse(group == "item", cutoff_upper_item, max(cutoff_upper_item, cutoff_upper_worker))))
		non_outliers_marked = non_outliers %>% 
			merge(cutoffs) %>%
			mutate(direction = ifelse(rt <= cutoff_lower, "short", ifelse(rt >= cutoff_upper, "long", NA))) 
		outlier_n = non_outliers_marked %>% filter(!is.na(direction))
		if(nrow(outlier_n) > 0){
			outlier_n$degree = n 
			non_outliers = non_outliers_marked %>% 
				filter(is.na(direction)) %>%
				select(colnames(questionnaire))
			outliers = plyr::rbind.fill(outliers, outlier_n)
			n = n + 1
		}else{
			break;
		}
	}
	outliers = plyr::rbind.fill(outliers, non_outliers)
	return(outliers)
}


#' Plots for demonstrating ReMFOD (Recursive Multi-factorial Outlier Detection) 
#' by comparison to other outlier types 
#' return unstructured plot,  filter or facet_wrap according to your own needs
#' 
#' @param questionnaire responses to a survey, with column "rt" (response times)!
#' @param cutoff_factor_long factor (x times mad deviation from median) for determining the upper cutoff points, i.e. for long outliers
#' @param cutoff_factor_short factor (y times mad deviation from median) for determining the lower cutoff points, i.e. for short outliers
#' @param item_function_colname name of the column of questionnaire specifying the item functions
#' @param participant_id_colname name of the column of questionnaire specifiying the ids of different participants
#' @return plot
outlier_plots_remfod = function(questionnaire, cutoff_factor_long = 2.5, cutoff_factor_short = 1.5, item_function_colname = "ITEM_FUNCTION", participant_id_colname = "workerId"){
	overall_upper = median(questionnaire$rt) + cutoff_factor_long * mad(questionnaire$rt)
	overall_lower = median(questionnaire$rt) - cutoff_factor_short * mad(questionnaire$rt)
	
	out_genuine = remfod(questionnaire, cutoff_factor_long, cutoff_factor_short) %>% 
		mutate(method = "item&participant")
	out_worker = remfod(questionnaire, cutoff_factor_long, cutoff_factor_short, group = "participant") %>% 
		mutate(method = "participant")
	out_item = remfod(questionnaire, cutoff_factor_long, cutoff_factor_short, group = "item") %>% 
		mutate(method = "item")
	out_all = out_genuine %>%
		rbind(out_worker) %>%
		rbind(out_item) %>%
	  mutate(out_type = ifelse(is.na(degree),NA,method)) %>%
	  mutate_at(.vars = c("direction","degree", "method"), .funs = as_factor) %>%
	  select(-method) %>% distinct()
 
	plot = out_all %>% 
		mutate(
			out_type = ifelse(is.na(out_type),"--",out_type), 
			degree = ifelse(is.na(degree),0,degree)
		) %>% 
		ggplot(aes(x = trial_index, y = log(rt), group = out_type)) + 
			geom_point(aes(shape = out_type, color = out_type, size = out_type)) + 
			scale_shape_manual(values = c("--" = 16, "item&participant" = 0, "participant" = 3, "item" = 4)) +
			scale_color_manual(values = c("--" = "#dcdcdc", "item&participant" = "#000000", "participant" = "#000000", "item" = "#000000")) + 
			scale_size_manual(values = c("--" = 1, "item&participant" = 5, "participant" = 3, "item" = 3)) +
			theme_classic(base_size = 20) + 
			theme(legend.position =  "bottom", axis.text.x = element_blank(),axis.ticks.x=element_blank()) + 
			geom_hline(yintercept = log(overall_lower), linetype = "dashed", color = "red") +
			geom_hline(yintercept = log(overall_upper), linetype = "dashed", color = "red")
	return(plot)  
}