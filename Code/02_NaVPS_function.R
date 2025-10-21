
tidy.logistf <- function(x, conf.int = TRUE, conf.level = 0.95, exponentiate = TRUE, ...) {
    ret <- as.data.frame(x$coefficients)
    ret$term <- rownames(ret)
    rownames(ret) <- NULL
    names(ret)[1] <- "estimate"
    

    
    if (conf.int) {
        ci <- confint(x, level = conf.level)
        ret$conf.low <- ci[,1]
        ret$conf.high <- ci[,2]
    }
    if(exponentiate){
        ret$estimate <- exp(ret$estimate )
        ret$conf.low <- exp(ret$conf.low )
        ret$conf.high <- exp(ret$conf.high )
    }
    
    ret$estimate <- round(ret$estimate, 3)
    ret$conf.low <- round(ret$conf.low, 3)
    ret$conf.high <- round(ret$conf.high, 3)
    
    ret$ci <- paste0(ret$conf.low, " - ",  ret$conf.high)
    
    ret$p.value <- nice_pvalues(x$prob)
    ret <- ret[, c("term", "estimate", "ci", "p.value")]
    return(ret)
}









#' Summarize Categorical Data with Optional Statistical Tests
#'
#' @description Generates a summary table for one or two categorical variables within a dataset, summarizing the frequency and percentage of each combination of categories. Additionally, this function can perform statistical tests between groups and include missing categories in the summary, providing a comprehensive view of categorical distributions.
#'
#' @param df A data frame containing the variables to summarize.
#' @param x The primary categorical variable to summarize, provided as an unquoted column name using tidy evaluation.
#' @param group An optional categorical grouping variable, also provided as an unquoted column name. Default is NULL.
#' @param summary_title An optional title for the summary table. Default is NULL.
#' @param digits Integer specifying the number of decimal places to use for percentages. Default is 2.
#' @param include_zero_categories Logical. If TRUE, includes categories with zero counts in the summary. Default is FALSE.
#' @param nice_p Logical. If TRUE, formats p-values in a 'nice' manner for readability. Default is TRUE.
#' @param test_type Character string specifying the statistical test to apply if `group` is provided. Options are "none", "fisher" for Fisher's Exact Test, and "chi-square" for the Chi-Square Test. Default is "none".
#' @param ... Additional arguments passed to underlying functions, if applicable.
#'
#' @details This function calculates the frequency and percentage for each level of `x`, optionally within levels of `group`. If `include_zero_categories` is TRUE, categories with zero observations are included. If `test_type` is "fisher" or "chi-square" and `group` is specified, the function performs the respective test and appends the p-value to the summary. If `summary_title` is provided, it is added as a header in the output table.
#'
#' @return A data frame summarizing the counts and percentages of `x`, optionally within each `group`. When `group` is specified, the table is pivoted to wide format, with counts and percentages as separate columns for each level of `group`. If a statistical test is applied, a p-value is included.
#'
#' @examples
#' # Summarize the 'gear' variable in the mtcars dataset
#' summarize_categorical(mtcars, gear, digits = 2)
#' 
#' # Summarize 'gear' by 'carb' group with Chi-Square test
#' summarize_categorical(mtcars, gear, carb, test_type = "chi-square", digits = 2)
#'
#' # Summarize with zero categories included and a custom summary title
#' summarize_categorical(mtcars, gear, carb, include_zero_categories = TRUE, summary_title = "Gear by Carb Summary")
#'
#' @import dplyr
#' @import tidyr
#' @import rlang
#' @export
summarize_categorical <- function(df, x, group = NULL, summary_title = NULL, digits = 2, 
                                      include_zero_categories = FALSE, include_N_total = c("no", "N", "all"),
                                      short = TRUE,
                                      nice_p = TRUE, test_type = c("none", "fisher", "chi-square"), ...) {
    # Check if required packages are loaded
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Required package 'dplyr' is not loaded. Please load it before running this function.")
    }
    if (!requireNamespace("tidyr", quietly = TRUE)) {
        stop("Required package 'tidyr' is not loaded. Please load it before running this function.")
    }
    if (!requireNamespace("rlang", quietly = TRUE)) {
        stop("Required package 'rlang' is not loaded. Please load it before running this function.")
    }
    
    test_type       <- match.arg(test_type)
    include_N_total <- match.arg(include_N_total)
    
    # Convert x and group to symbols to allow for quoted and unquoted input
    x_sym <- rlang::ensym(x)
    
    total_count <- nrow(df)
    N_total <-  df %>% dplyr::group_by(!!x_sym) %>%
        summarise(N_total = sum(!is.na(!!x_sym)),
                  Percent_total = round(N_total / total_count * 100, digits))
    N_total <- N_total[-1]
    
    # If group is not provided, summarize the data by x only
    if (rlang::quo_is_null(rlang::enquo(group))) {
        
        
        if(include_zero_categories){
            cat_summary <- df %>%
                dplyr::group_by(!!x_sym) %>%
                dplyr::summarise(N = n(), .groups = 'drop') %>%
                complete(!!x_sym, fill = list(N = 0)) %>%  # Ensure all levels of the factor are included
                mutate(Percent = round(N / total_count * 100, digits = digits)) %>%
                dplyr::arrange(!!x_sym)
            
        } else {
            cat_summary <- df %>%
                dplyr::group_by(!!x_sym) %>%
                dplyr::summarise(N = dplyr::n(),
                                 Percent = round(N / total_count * 100, digits)) %>%
                dplyr::arrange(!!x_sym)
        }
    } else {
        group_sym <- rlang::ensym(group)
        
        if(include_zero_categories){
            cat_summary <- df %>%
                dplyr::group_by(!!x_sym, !!group_sym) %>%
                dplyr::summarise(N = dplyr::n(), .groups = 'drop') %>%
                complete(!!x_sym, !!group_sym, fill = list(N = 0)) %>%  # Ensure all combinations are present
                dplyr::group_by(!!group_sym) %>%
                dplyr::mutate(Percent = round(100 * N / sum(N), digits)) %>%
                dplyr::ungroup()
            
            
            N_total <-  df %>% dplyr::group_by(!!x_sym) %>%
                summarise(N_total =  dplyr::n() , .groups = 'drop') %>%
                complete(!!x_sym, fill = list(N_total = 0))  %>%
                dplyr::mutate(Percent_total = round(N_total / total_count * 100, digits)) 
            N_total <- N_total[-1]
            
        } else {
            # Count the frequency of each combination of x and group
            cat_summary <- df %>%
                dplyr::group_by(!!x_sym, !!group_sym) %>%
                dplyr::summarise(N = dplyr::n(), .groups = 'drop') %>%
                dplyr::group_by(!!group_sym) %>%
                dplyr::mutate(Percent = round(100 * N / sum(N), digits)) %>%
                dplyr::ungroup()
            
        }
        # Pivot table to wider format
        cat_summary <- cat_summary %>%
            tidyr::pivot_wider(names_from = !!group_sym, values_from = c(N, Percent), names_vary = "slowest")
        
        
        
        if(include_N_total == "N"){
            
            cat_summary <- cbind(cat_summary[1], N_total[1], cat_summary[-1])
        } else if(include_N_total == "all"){
            
            cat_summary <- cbind(cat_summary[1], N_total, cat_summary[-1])
        }
        
        if (short) {
            n_groups <- (ncol(cat_summary) - 1) / 2
            cat_temp <- NULL
            for (i in 1:n_groups) {
                cat_temp <- cbind(cat_temp,
                                  mapply(paste0, cat_summary[, i * 2], " (", cat_summary[, i *
                                                                                             2 + 1], ")"))
                
            }
            
            colnames(cat_temp) <- colnames(cat_summary)[seq(2,n_groups*2,2)]
            
            cat_summary <- cbind(cat_summary[1], cat_temp)
            
        }
        
        
        
        # Perform statistical test if group is provided and test_type is not "none"
        if (test_type != "none") {
            pvalue <- if (test_type == "chi-square") {
                message("Chi-square test has been performed")
                stats::chisq.test(df %>% dplyr::pull(!!x_sym), df %>% dplyr::pull(!!group_sym))$p.value
            } else if (test_type == "fisher") {
                message("Fisher's Exact test has been performed")
                stats::fisher.test(table(df %>% dplyr::pull(!!x_sym), df %>% dplyr::pull(!!group_sym)), simulate.p.value = TRUE)$p.value
            }
            cat_summary$pvalue <- c(pvalue, rep("", nrow(cat_summary) - 1))
        }
    }
    
    cat_summary[[1]] <- as.character(cat_summary[[1]])
    
    # If summary_title is provided, add it as the first row
    if (!is.null(summary_title)) {
        
        # Add spaces before levels
        cat_summary[[1]] <- paste("    ", cat_summary[[1]], sep = "")
        
        if (test_type != "none") {
            cat_summary$pvalue <- NA 
            # Add p-value to the summary
            cat_summary <- rbind(c(summary_title, rep("", ncol(cat_summary) - 2), pvalue),
                                 cat_summary[])   
            
        } else {
            cat_summary <- rbind(c(summary_title, rep("", ncol(cat_summary) - 1)),
                                 cat_summary)
        }
    }
    
    # Format p-values nicely if required
    if (nice_p && "pvalue" %in% colnames(cat_summary)) {
        cat_summary$pvalue <- nice_pvalues(as.numeric(cat_summary$pvalue))
    }
    
    # Rename the first column to "Characteristic"
    colnames(cat_summary)[1] <- "Characteristic"
    
    # Return the summary table
    return(cat_summary)
}













#' Summarize Continuous Data by Group with Statistical Tests
#'
#' @description This function calculates summary statistics for a continuous variable,
#' optionally grouped by another variable. Summary types include median with interquartile range (IQR),
#' median with range, and mean with standard deviation. Additionally, statistical tests such as Wilcoxon,
#' Kruskal-Wallis, t-test, and ANOVA can be performed to assess differences between groups.
#'
#' @param df A data frame containing the variables of interest.
#' @param x The name of the continuous variable to summarize, provided as a string.
#' @param group Optional. The name of the grouping variable, provided as a string. Default is NULL, 
#' in which case the function computes overall summaries without grouping.
#' @param summary_title A descriptive title for the summary table, added as a column. Default is an empty string.
#' @param digit Number of decimal places to round the summary statistics to. Default is 2.
#' @param nice_p Logical. If TRUE, formats p-values for readability. Default is TRUE.
#' @param summary_type A character string specifying the type of summary statistics to compute.
#' Valid options are "median_iqr" (median and IQR), "median_range" (median and range),
#' and "mean_sd" (mean and standard deviation). Default is "median_iqr".
#' @param test_type A character string specifying the type of statistical test to use. Valid options include:
#' "none" (no test), "default" (automatic selection based on group and data characteristics),
#' "wilcox", "kruskal", "t-test", and "anova". Default is "default".
#' @param ... Additional arguments passed to the statistical test functions.
#'
#' @return A data frame with summary statistics for each group (if grouping is specified) and 
#' the p-value from the statistical test, if performed. Columns include group names, computed summary
#' measures, and optional p-values.
#'
#' @details The function selects the appropriate test based on the number of groups and the type of 
#' summary chosen:
#' - For non-parametric data with two groups, the Wilcoxon test is used.
#' - For non-parametric data with more than two groups, the Kruskal-Wallis test is used.
#' - For parametric data with two groups, a t-test is performed.
#' - For parametric data with more than two groups, an ANOVA is performed.
#'
#' If grouping is not provided, the function calculates overall summary statistics for the continuous variable.
#'
#' @examples
#' # Calculate median and IQR for Sepal.Length grouped by Species in the iris dataset
#' summarize_continuous(iris, "Sepal.Length", "Species", summary_type = "median_iqr", summary_title = "Sepal Length")
#'
#' # Perform ANOVA for Sepal.Width grouped by Species with mean and standard deviation summaries
#' summarize_continuous(iris, "Sepal.Width", "Species", summary_type = "mean_sd", test_type = "anova")
#'
#' # Calculate median and range for Petal.Length without grouping
#' summarize_continuous(iris, "Petal.Length", summary_type = "median_range")
#'
#' @import dplyr
#' @import tidyr
#' @import coin
#' @export
summarize_continuous <- function(df, x, group = NULL, 
                                 summary_title = "", digit = 2,  nice_p = TRUE, 
                                 include_total = FALSE, 
                                 include_N = TRUE,
                                 summary_type = c("median_iqr", "median_range", "mean_sd"),
                                 test_type = c("none", "default", "wilcox", "kruskal", "t-test", "anova"),
                                 ...){
    # Check if required packages are loaded
    if (!requireNamespace("dplyr", quietly = TRUE)) {
        stop("Required package 'dplyr' is not loaded. Please load it before running this function.")
    }
    if (!requireNamespace("tidyr", quietly = TRUE)) {
        stop("Required package 'tidyr' is not loaded. Please load it before running this function.")
    }
    if (!requireNamespace("coin", quietly = TRUE)) {
        stop("Required package 'coin' is not loaded. Please load it before running this function.")
    }
    
    
    
    summary_type <- match.arg(summary_type)
    
    
    # Convert x and group to symbols to allow for quoted and unquoted input
    x_sym <- rlang::ensym(x)
    
    # If group is not provided, summarize the data by x only
    
        
        # Compute summary statistics
        tab_cont_total <- df %>%
            summarise(
                N          = sum(!is.na(!!x_sym)),
                median_IQR = paste0(round(median(!!x_sym, na.rm = TRUE), digit), " (", 
                                    round(quantile(!!x_sym, 0.25, na.rm = TRUE), digit), " to ", 
                                    round(quantile(!!x_sym, 0.75, na.rm = TRUE), digit),")"),
                median_Range = paste0(round(median(!!x_sym, na.rm = TRUE), digit), " (", 
                                      round(min(!!x_sym, na.rm = TRUE), digit), " to ", 
                                      round(max(!!x_sym, na.rm = TRUE), digit),")"),
                mean_SD = paste0(round(mean(!!x_sym, na.rm = TRUE), digit), " (", 
                                 round(sd(!!x_sym, na.rm = TRUE), digit),")")
            )
        
        if (summary_type == "median_iqr"){
            tab_cont_total <- tab_cont_total %>% dplyr::select(-mean_SD,-median_Range)
        } else if (summary_type == "median_range"){
            tab_cont_total <- tab_cont_total %>% dplyr::select(-median_IQR,-mean_SD, )
        } else if (summary_type == "mean_sd"){
            tab_cont_total <- tab_cont_total %>% dplyr::select(-median_IQR,-median_Range)
        }
        
        
if (rlang::quo_is_null(rlang::enquo(group))) {}

    else{
        group_sym <- rlang::ensym(group)    
        
        # Compute summary statistics
        tab_cont <- df %>%
            group_by(!!group_sym) %>%
            summarise(
                N          = sum(!is.na(!!x_sym)),
                median_IQR = paste0(round(median(!!x_sym, na.rm = TRUE), digit), " (", 
                                    round(quantile(!!x_sym, 0.25, na.rm = TRUE), digit), " to ", 
                                    round(quantile(!!x_sym, 0.75, na.rm = TRUE), digit),")"),
                median_Range = paste0(round(median(!!x_sym, na.rm = TRUE), digit), " (", 
                                      round(min(!!x_sym, na.rm = TRUE), digit), " to ", 
                                      round(max(!!x_sym, na.rm = TRUE), digit),")"),
                mean_SD = paste0(round(mean(!!x_sym, na.rm = TRUE), digit), " (", 
                                 round(sd(!!x_sym, na.rm = TRUE), digit),")")
            )
        
        
        if (summary_type == "median_iqr"){
            tab_cont <- tab_cont %>% dplyr::select(-mean_SD,-median_Range)
        } else if (summary_type == "median_range"){
            tab_cont <- tab_cont %>% dplyr::select(-median_IQR,-mean_SD, )
        } else if (summary_type == "mean_sd"){
            tab_cont <- tab_cont %>% dplyr::select(-median_IQR,-median_Range)
        }
        
        
        
        # Rename the summary measure column
        colnames(tab_cont)[3] <- "summary_measure"
        
        if(include_N == FALSE){
            tab_cont <- tab_cont %>% dplyr::select(-N)
            spec <- build_wider_spec(tab_cont, names_from = !!group_sym, values_from = c(summary_measure))
        } else{
            
            spec <- build_wider_spec(tab_cont, names_from = !!group_sym, values_from = c(N, summary_measure))
        }
        
        # Widen the data frame

        spec <- arrange(spec, !!group_sym)
        tab_cont <- pivot_wider_spec(tab_cont, spec)
        
        
        
        if(include_total == TRUE){
            # print(tab_cont_total)    
            tab_cont <- cbind(tab_cont_total[2], tab_cont[])
        } 
        
        
        
        test_type    <- match.arg(test_type)
        # Determine the type of test to use
        n_groups  <- length(unique(df[[rlang::as_string(group_sym)]]))
        if(test_type == "default"){
            
            
            
            type_test <- switch(summary_type,
                                "median_iqr"  = "non-parametric",
                                "median_range"= "non-parametric",
                                "mean_sd"     = "parametric",
                                "non-parametric")
            test_type <-
                if(n_groups == 2 & type_test == "non-parametric") "wilcox"  else
                    if(n_groups > 2  & type_test == "non-parametric") "kruskal" else
                        if(n_groups == 2 & type_test == "parametric")     "t-test"  else
                            if(n_groups > 2  & type_test == "parametric")     "anova"
            
        }
        
        if((test_type == "wilcox" | test_type ==  "t-test") & n_groups>2) { warning("More than two groups - Other test recommanded")}
        if((test_type == "kruskal" | test_type ==  "anova") & n_groups==2) { warning("Only two groups - Other test recommanded")} 
        
        # Compute p-value
        pvalue <-         
            if(test_type == "wilcox") {
                message("Wilcoxon Rank Sum test has been performed")
                # wilcox.test(df[[rlang::as_string(x_sym)]] ~ df[[!!group_sym]])$p.value
                as.numeric(coin::pvalue((coin::wilcox_test(df[[rlang::as_string(x_sym)]] ~ as.factor(df[[rlang::as_string(group_sym)]]), distribution = "exact"))))
            } else if(test_type == "kruskal") {
                message("Kruskal-Wallis Rank Sum test has been performed")
                kruskal.test(df[[rlang::as_string(x_sym)]] ~ df[[rlang::as_string(group_sym)]])$p.value
            } else if(test_type == "t-test") {
                message("T-test has been performed")
                t.test(df[[rlang::as_string(x_sym)]] ~ df[[rlang::as_string(group_sym)]])$p.value  
            } else if(test_type == "anova") {
                message("Anova has been performed")
                summary(aov(df[[rlang::as_string(x_sym)]] ~ df[[rlang::as_string(group_sym)]]))[[1]][["Pr(>F)"]][1]
            }
        
        if(!is.null(pvalue)){
            tab_cont <- cbind(tab_cont, pvalue)
            
            
            
            if(nice_p){
                tab_cont$pvalue <- nice_pvalues(tab_cont$pvalue)
            }
        }
    }
    
        
        
        
    tab_cont <- cbind(summary_title, tab_cont)
    
    
    return(tab_cont)
}








#-----------------------------
# Boxplot with included wilcox rank sum test
#-----------------------------
#' Boxplot with Wilcoxon Rank Sum Test Annotation
#'
#' @description Creates a boxplot for a numeric variable grouped by a factor, with an option to add points and 
#' include a subtitle indicating the result of a Wilcoxon rank sum test.
#'
#' @param df Data frame containing the data.
#' @param x Numeric variable for the y-axis.
#' @param group Grouping variable for the x-axis.
#' @param x_lab x-axis label. Optional.
#' @param y_lab y-axis label. Optional.
#' @param include_points Logical; if TRUE, overlays points on the boxplot.
#' @param include_pvalue Logical; if TRUE, includes a subtitle with the Wilcoxon rank sum test p-value.
#'
#' @return A ggplot object with the boxplot and optional annotations.
#'
#' @examples
#' box_wilcox(mtcars, "mpg", "cyl", "Cylinders", "MPG", include_points = TRUE)
#'
#' @import ggplot2
#' @import dplyr
#' @import coin
#' @export
box_wilcox <- function(df, x , group, 
                       x_lab = "", y_lab = "", 
                       include_points = FALSE,
                       include_pvalue = TRUE,
                       breaks = NULL,
                       ...){
    
    ## DF = data frame
    ## x   = var 1 - type numeric
    ## group  = var 2 - type factor
    ## x_lab     = Label of the x-axis
    ## y_lab     = Label of the y-axis
    
    params <- list(...)
    
    # Clean up variable names to allow for names without quotes
    x_name <- deparse(substitute(x))
    x_name <- gsub("`", "", x_name)
    group_name <- deparse(substitute(group))
    group_name <- gsub("`", "", group_name)
    
    ## Colour of bins
    colour_groups <- if (hasArg(colour_box)) params$colour_box else c("gray", "gray")
    
    ## Wilcox rank sum test 
    pvalue_temp <- coin::pvalue((coin::wilcox_test(df[[x_name]] ~ as.factor(df[[group_name]]), distribution = "exact")))
    pvalue_temp <- nice_pvalues(pvalue_temp)
    
    
    p <- df %>%
        ggplot(aes(x = .data[[group_name]], y = .data[[x_name]], fill = .data[[group_name]])) +
        stat_boxplot(geom = "errorbar", width=0.15)  
    
    p <- p + list(
        if(include_points == TRUE ){ list(geom_boxplot(outlier.shape = NA),
                                          geom_point(size = 3, shape = 20) ) },
        if(include_points == FALSE){ geom_boxplot() },
        if(include_pvalue == TRUE ){ labs(subtitle = paste0("Wilcoxon rank sum test - p-value: ", pvalue_temp)) }
    )
    
    
    p <- p + scale_fill_manual(values = colour_groups) +
        theme_classic() +
        theme(legend.position = "none") +
        labs(x = x_lab, 
             y = y_lab)
    
    p <- p + list(if(!is.null(breaks )) {  scale_y_continuous(breaks = breaks, limits = c(min(breaks), max(breaks)))})
    
    p 
}




# -------------------------------------------
# Creates presentable p-values 
# -------------------------------------------
#' Format P-Values for Presentation
#'
#' @description Formats p-values for presentation, rounding to three decimal places or marking
#' as "<0.001" for very small values.
#'
#' @param p_value Numeric vector of p-values to format.
#'
#' @return A vector of formatted p-values.
#'
#' @examples
#' nice_pvalues(c(0.000234, 0.02, 0.3456))
#'
#' @export
nice_pvalues <- function(p_value){
    
    # Check if the value is NA
    is_na <- is.na(p_value)
    
    # Handle non-NA values
    p_value[!is_na & p_value >= 0.001] <- round(p_value[!is_na & p_value >= 0.001], 3)
    p_value[!is_na & p_value <  0.001] <- "<0.001"
    
    # Return the formatted p-values
    p_value
}

    	
		
		
		
		
		
		
		
		
		
		
		
library(checkmate)
library(stringr)

#' Generate Formatted Regression Tables
#'
#' @description Creates a formatted regression table appropriate for the type of regression model provided.
#' This is a generic function with methods for different types of regression models.
#'
#' @param model A regression model object.
#' @param ... Additional arguments passed to specific methods.
#'
#' @return A formatted regression table, the format of which depends on the method used.
#' @export
table_regression <- function(model, ...){
    UseMethod("table_regression", model)
}



# Extracts coefficients and their corresponding p-values from a model summary.
# Optionally uses a likelihood ratio test for p-value extraction.
#
# Args:
#   model: The model object from which to extract coefficients.
#   summary_func: The function to use for summarizing the model. Default is `summary`.
#   coef_index: The index of the coefficients in the summary output. Default is 1.
#   p_value_index: The index of the p-values in the summary output. Default is 4.
#   lrt_p: Logical indicating whether to use likelihood ratio test for p-values. Default is FALSE.
#   digits_sign: Number of significant digits for the coefficients. Default is 4.
#
# Returns:
#   A list containing two elements: 'coefs' (the significant coefficients) and 'p_value' (the p-values).
extract_coefs <- function(model, summary_func = summary, coef_index = 1, p_value_index = 4, lrt_p = FALSE, digits_sign = 4) {
    model_summary <- summary_func(model)
    coefs <- coef(model_summary)[, coef_index]
    coefs <- signif(coefs, digits_sign)
    
    if(lrt_p) {
        p_value <- extract_p_value_lrt(model)  # Assumes extract_p_value_lrt function is defined elsewhere
    } else {
        p_value <- coef(model_summary)[, p_value_index]
    }
    
    list(coefs = coefs, p_value = p_value)
}




# Calculates numeric confidence intervals for the model coefficients.
# Allows for adjustment in the calculation (e.g., for multiple comparisons) and retains numeric format for further processing.
#
# Args:
#   model: The model object from which to calculate confidence intervals.
#   adjust: Optional adjustment method for confidence intervals (e.g., "tukey").
#   digits_sign: Number of significant digits for the confidence intervals. Default is 4.
#
# Returns:
#   A matrix of numeric confidence intervals, each row corresponding to a coefficient.
calculate_cis <- function(model, adjust = NULL, digits_sign = 4) {
    if(is.null(adjust)) {
        conints <- confint(model)
    } else {
        conints <- confint(model, adjust = adjust)  # Adjust parameter is used for emmeans, etc.
    }
    
    conints <- signif(conints, digits_sign)
    return(conints)  # Return numeric confidence intervals
}









# Creates a formatted regression table from coefficients, confidence intervals, and p-values.
# Allows for nice formatting of p-values, exclusion of intercept, and renaming of coefficients.
#
# Args:
#   coefs: Numeric vector of coefficients.
#   cis: Character vector of confidence intervals, formatted as "lower - upper".
#   p_values: Numeric vector of p-values.
#   col_names: Character vector specifying the column names for the table.
#   nice_p: Logical indicating whether to format p-values nicely (e.g., <0.001). Default is TRUE.
#   include_intercept: Logical indicating whether to include the intercept in the table. Default is TRUE.
#   coef_names: Optional named vector for renaming coefficients. Names should match existing coefficient names.
#
# Returns:
#   A data.frame representing the formatted regression table.
create_regression_table <- function(names_coefs = NULL,
                                    n_info = NULL,
                                    coefs,
                                    cis,
                                    p_values,
                                    col_names,
                                    lrt_p,
                                    nice_p = TRUE,
                                    include_intercept = TRUE,
                                    coef_names = NULL,
                                    model = NULL,
                                    title_category =  "none") {
    
    if(nice_p) {
        p_values <- nice_pvalues(p_values)  # Assumes nice_pvalues function is defined elsewhere
    }
    
    if(is.null(names_coefs))
    {
        names_coefs <- names(coefs)
    }
    
    tab <- data.frame(Coefficients = names_coefs, Estimates = as.character(coefs), CI = cis, P.value = p_values)
    
    # Categorical variables with multiple levels > 2 
    # Add a title row above Levels
    if(title_category == "none"){
        print("Without a title line for the categories N cannot be added")
    }
    if(!is.null(model) & title_category != "none" ){
        
        # Initialize a new data frame with the same column name as temp
        new_tab <- tab[FALSE, ]
        
        # Add Intercept
        new_tab <- rbind(new_tab, tab[1,])     
        
        # Starting and endpoint of rows in to extract from tab
        n_start <- 1
        n_end <- n_start
        
        # Get the levels of factors in the model frame
        # list_levels <- sapply(model.frame(model), levels)
        list_levels <- lapply(model.frame(model), levels)[-1]
        
        # Iterate over each factor or variable in list_levels
        for(i in 1:length(list_levels)){
            
            # Get the number of levels for the current factor or variable
            n_levels <- length(unlist(list_levels[i]))
            n_lines <- max(n_levels, 1)  # Handle both factors with levels and continuous variables
            
            n_start <- n_end + 1
            
        
            if(n_lines > 1 & title_category == "always"){
                # If the variable is categorical (i.e., it has more than one level)
                n_end <- n_start + (n_lines - 2)
                
                # Add a row for the variable name, then add rows for its levels
                new_tab <- rbind(new_tab,
                                 c(names(list_levels)[i], rep("", ncol(new_tab)-1) ),
                                 # data.frame(a = paste0("Variable: ", names(list_levels)[i])),  # Variable name
                                 tab[n_start:n_end, , drop = FALSE])                   # Corresponding levels
                
            }  else if(n_lines > 2 & title_category == "multi_category"){
                    # If the variable is categorical (i.e., it has more than one level)
                    n_end <- n_start + (n_lines - 2)
                    
                    # Add a row for the variable name, then add rows for its levels
                    new_tab <- rbind(new_tab,
                                     c(names(list_levels)[i], rep("", ncol(new_tab)-1) ),
                                     tab[n_start:n_end, , drop = FALSE])                   
                
            } else {
                # If the variable is continuous or has only one level
                n_end <- n_start
                
                # Directly add the row from 
                new_tab <- rbind(new_tab,
                                 tab[n_start:n_end, , drop = FALSE])
                
            } # end else
        } # end for
        
        # Final output
        tab <- new_tab
    }
    
    if(!is.null(n_info) & title_category == "always") {
        tab <- cbind(tab[,"Coefficients"], "N" = n_info, tab[,-1])
    }
    
    colnames(tab) <- col_names
    
    if(!include_intercept) {
        tab <- tab[-1, ]  # Exclude the first row if it corresponds to the intercept
    }
    
    
    if(lrt_p){
        
        if("glm" %in% class(model)){
            var_factor <- attr(attr(model[["terms"]], "factors"), "dimnames")[[2]]    
        } else if("logistf" %in% class(model)){
            var_factor <- attr(attr(attr(model[["model"]], "terms"), "dataClasses")[-1], "names")
        } else if("lm" %in% class(model)){
            term_info <- attr(terms(model), "dataClasses")
            var_factor <- names(term_info[term_info %in% c("factor", "character")])
        }
        
        
        for (i in var_factor) {
            
            if (is.factor(model[["model"]][[i]])) {
                row_factor <- which(tab[,"Coefficients"] == i)
                tab[row_factor, "P-value"] <- tab[row_factor+1, "P-value"]
                tab[row_factor+1, "P-value"] <- ""
            }
            
        }
        
    }
    
    
    if(!is.null(coef_names)) {
        
        coef_names           <- as.data.frame(coef_names)
        colnames(coef_names) <- "new"
        coef_names["old"]    <- attributes(coef_names)$row.names
        
        for(i in 1:nrow(tab)){
            
            if(tab[i,"Coefficients"] %in% coef_names[["old"]] ){
                
                tab[i,"Coefficients"] <- coef_names[coef_names[["old"]] == tab[i,"Coefficients"], "new"]
                
            }
        }
    }
    
    rownames(tab) <- NULL
    tab
}





# Generates a regression table for linear models (lm).
# Allows customization of p-value presentation, inclusion of intercept, coefficient renaming,
# and output format (flextable vs. data frame). Now includes a digits_sign parameter.
#
# Args:
#   model: A linear model object created with lm().
#   nice_p: Logical, if TRUE, formats p-values nicely (default TRUE).
#   include_intercept: Logical, if TRUE, includes the intercept in the output table (default TRUE).
#   coef_names: Optional named vector for renaming coefficients. Names should match existing coefficient names.
#   lrt_p: Logical, if TRUE, uses likelihood ratio test for p-values (default FALSE).
#   flextable: Logical, if TRUE, returns a flextable object instead of a data frame (default FALSE).
#   digits_sign: Integer, number of significant digits for rounding (default 4).
#   ...: Additional arguments passed to other methods or functions.
#
# Returns:
#   A formatted regression table as a data.frame or a flextable, depending on the flextable argument.


#' @describeIn table_regression Generates a regression table for linear models.
#' @param model A linear model object created with \code{lm()}.
#' @param nice_p Format p-values nicely.
#' @param include_intercept Include the intercept in the output table.
#' @param coef_names Optional named vector for renaming coefficients.
#' @param lrt_p Use likelihood ratio test for p-values.
#' @param flextable Return a flextable object instead of a data frame.
#' @param digits_sign Number of significant digits for rounding.
#' @param ... Additional arguments passed to other methods or functions.
#' @export
table_regression.lm <- function(model,
                                nice_p = TRUE,
                                include_intercept = TRUE,
                                coef_names = NULL,
                                lrt_p = FALSE,
                                title_category =  c("none", "always", "multi_category"),
                                flextable = FALSE,
                                digits_sign = 4,
                                ...) {
    
    # Check arguments with checkmate
    assertClass(model, "lm")
    assertLogical(nice_p, len = 1)
    assertLogical(include_intercept, len = 1)
    assertLogical(lrt_p, len = 1)
    assertLogical(flextable, len = 1)
    assertIntegerish(digits_sign, lower = 1)
    if (!is.null(coef_names)) {
        assertNamed(coef_names)
    }
    
    title_category <- match.arg(title_category)
    
    # Extract coefficients and p-values using the helper function
    coefs_info <- extract_coefs(model, coef_index = 1, p_value_index = 4, lrt_p = lrt_p, digits_sign = digits_sign)
    
    # Calculate confidence intervals using the helper function
    numeric_cis <- calculate_cis(model, digits_sign = digits_sign)
    
    # Format the numeric confidence intervals into strings
    formatted_cis <- apply(numeric_cis, 1, function(x) paste(signif(x[1], digits_sign), "-", signif(x[2], digits_sign)))
    
    # Define column names for the table
    col_names <- c("Coefficients", "Estimates", "CI", "P-value")
    
    # Create the regression table using the helper function
    tab <- create_regression_table(coefs = coefs_info$coefs, 
                                   cis = formatted_cis,
                                   p_values = coefs_info$p_value, 
                                   col_names = col_names, 
                                   nice_p = nice_p, 
                                   lrt_p = lrt_p,
                                   title_category = title_category,
                                   include_intercept = include_intercept,
                                   coef_names =  coef_names,
                                   model = model)
    
    # Return the table as either a data frame or a flextable
    if(flextable) {
        return(flextab.standard(data.set = tab, ...))  # Assumes flextab.standard is defined elsewhere
    } else {
        return(tab)
    }
}





# Generates a regression table for generalized linear models (GLM).
# This function handles the presentation of p-values, inclusion of the intercept,
# renaming of coefficients, and output format customization (flextable vs. data frame).
# For logistic regression models, it converts estimates to odds ratios and applies rounding.
#
# Args:
#   model: A GLM model object created with glm().
#   nice_p: Logical, indicating whether to format p-values nicely (default TRUE).
#   include_intercept: Logical, indicating whether to include the intercept in the output table (default TRUE).
#   coef_names: Optional named vector for renaming coefficients. Names should match existing coefficient names.
#   flextable: Logical, indicating whether to return a flextable object instead of a data frame (default FALSE).
#   digits_sign: Integer, number of significant digits for rounding (default 4).
#   ...: Additional arguments passed to other methods or functions.
#
# Returns:
#   A formatted regression table as a data.frame or a flextable, depending on the flextable argument.


#' @describeIn table_regression Generates a regression table for generalized linear models.
#' @param model A GLM model object created with \code{glm()}.
#' @inheritParams table_regression.lm
#' @export
table_regression.glm <- function(model,
                                 n = TRUE,
                                 nice_p = TRUE,
                                 include_intercept = TRUE,
                                 coef_names = NULL,
                                 lrt_p = TRUE,
                                 title_category =  c("none", "always", "multi_category"),
                                 flextable = FALSE,
                                 digits_sign = 4,
                                 ...) {
    
    # Check arguments with checkmate
    assertClass(model, c("glm", "lm"))
    assertLogical(nice_p, len = 1)
    assertLogical(include_intercept, len = 1)
    assertLogical(flextable, len = 1)
    assertIntegerish(digits_sign, lower = 1)
    if (!is.null(coef_names)) {
        assertNamed(coef_names)
    }
    
    title_category <- match.arg(title_category)
    
    # Extract coefficients and p-values using the helper function
    coefs_info <- extract_coefs(model, coef_index = 1, p_value_index = 4, lrt_p = lrt_p, digits_sign = digits_sign)
    
    # Calculate confidence intervals using the helper function
    numeric_cis <- calculate_cis(model, digits_sign = digits_sign)
    
    # Extract N 
    col_names_n <- NULL
    n_info <- NULL
    if(n & title_category == "always"){
        n_info <- extract_n(model)
        col_names_n <- c("N")
    }
    
    
    # Adjust for logistic regression models by converting estimates and CIs to odds ratios
    if(model$family$family == "binomial" && model$family$link == "logit") {
        coefs_info$coefs <- exp(coefs_info$coefs) # Convert coefficients to odds ratios
        numeric_cis <- exp(numeric_cis) # Convert CIs to odds ratios
        col_names_add <- c("Odds Ratios", "CI", "P-value")
    } else {
        col_names_add <- c("Estimates", "CI", "P-value")
    }
    
    col_names <- c("Coefficients", col_names_n, col_names_add)
    
    
    # Round coefficients and CIs again if required
    coefs_info$coefs <- signif(coefs_info$coefs, digits_sign)
    
    # Format the numeric confidence intervals into strings
    formatted_cis <- apply(numeric_cis, 1, function(x) paste(signif(x[1], digits_sign), "-", signif(x[2], digits_sign)))
    
    # Extract coefficient names
    # This is to remove the variable name from levels of categorical variable
    names_coefs <- extract_coef_names(model)
    
    # Create the regression table using the helper function
    tab <- create_regression_table(names_coefs = names_coefs,
                                   n_info = n_info,
                                   coefs = coefs_info$coefs,
                                   cis = formatted_cis,
                                   p_values = coefs_info$p_value,
                                   col_names = col_names,
                                   nice_p = nice_p,
                                   lrt_p = lrt_p,
                                   title_category = title_category,
                                   include_intercept = include_intercept,
                                   coef_names =  coef_names,
                                   model = model
    )
    
    # Return the table as either a data frame or a flextable
    if(flextable) {
        return(flextab.standard(data.set = tab, ...)) # Assumes flextab.standard is defined elsewhere
    } else {
        rownames(tab) <- NULL
        return(tab)
    }
}





# This whole function is mess and should not extist like this 
# But here we are and is a quick fix, but for sure not beautiful
table_regression.logistf <- function(model,
                                     n = TRUE,
                                     nice_p = TRUE,
                                     include_intercept = TRUE,
                                     coef_names = NULL,
                                     lrt_p = TRUE,
                                     title_category =  c("none", "always", "multi_category"),
                                     flextable = FALSE,
                                     digits_sign = 4,
                                     ...) {
    # Check arguments with checkmate
    assertClass(model, c("logistf"))
    assertLogical(nice_p, len = 1)
    assertLogical(include_intercept, len = 1)
    assertLogical(flextable, len = 1)
    assertIntegerish(digits_sign, lower = 1)
    if (!is.null(coef_names)) {
        assertNamed(coef_names)
    }
    
    title_category <- match.arg(title_category)
    
    # Extract coefficients and p-values using the helper function
    
    # function(model, summary_func = summary, coef_index = 1, p_value_index = 4, lrt_p = FALSE, digits_sign = 4) {
    capture.output({coefs <- summary(model)[["coefficients"]]})
    coefs <- signif(coefs, digits_sign)
    
    if (lrt_p) {
        capture.output({p_value <- extract_p_value_firth(model) }) # Assumes extract_p_value_lrt function is defined elsewhere
    } else {
        capture.output({ p_value <- summary(model)$prob})
    }
    coefs_info <- list(coefs = coefs, p_value = p_value)
    
    # coefs_info <- extract_coefs(model, coef_index = 1, p_value_index = 4, lrt_p = lrt_p, digits_sign = digits_sign)
    
    # Calculate confidence intervals using the helper function
    numeric_cis <- calculate_cis(model, digits_sign = digits_sign)
    
    # Extract N 
    col_names_n <- NULL
    n_info <- NULL
    if(n & title_category == "always"){
        n_info <- extract_n(model)
        col_names_n <- c("N")
    }
    
    
    # Adjust for logistic regression models by converting estimates and CIs to odds ratios
    
    coefs_info$coefs <- exp(coefs_info$coefs) # Convert coefficients to odds ratios
    numeric_cis <- exp(numeric_cis) # Convert CIs to odds ratios
    col_names_add <- c("Odds Ratios", "CI", "P-value")
    
    
    col_names <- c("Coefficients", col_names_n, col_names_add)
    
    
    # Round coefficients and CIs again if required
    coefs_info$coefs <- signif(coefs_info$coefs, digits_sign)
    
    # Format the numeric confidence intervals into strings
    formatted_cis <- apply(numeric_cis, 1, function(x) paste(signif(x[1], digits_sign), "-", signif(x[2], digits_sign)))
    
    # Extract coefficient names
    # This is to remove the variable name from levels of categorical variable
    names_coefs <- extract_coef_names(model)
    
    # Create the regression table using the helper function
    tab <- create_regression_table(names_coefs = names_coefs,
                                   n_info = n_info,
                                   coefs = coefs_info$coefs,
                                   cis = formatted_cis,
                                   p_values = coefs_info$p_value,
                                   col_names = col_names,
                                   nice_p = nice_p,
                                   lrt_p = lrt_p,
                                   title_category = title_category,
                                   include_intercept = include_intercept,
                                   coef_names =  coef_names,
                                   model = model
    )
    
    
    # This is a work around because if we hand over the model to correct the level names
    # Firth gets one more line with the outcome variable name
    # tab <- tab[-1,]    
    
    
    # Return the table as either a data frame or a flextable
    if(flextable) {
        return(flextab.standard(data.set = tab, ...)) # Assumes flextab.standard is defined elsewhere
    } else {
        rownames(tab) <- NULL
        return(tab)
    }
}









# Generates a regression table for meta-regression models fitted with the 'rma' function from the 'metafor' package.
# This function presents meta-analytic estimates, confidence intervals, and p-values, with options for nicely formatting
# p-values and customizing the output format (flextable vs. data frame).
#
# Args:
#   model: A model object created with the 'rma' function from the 'metafor' package, used for meta-analysis.
#   nice_p: Logical, indicating whether to format p-values nicely (default TRUE).
#   flextable: Logical, indicating whether to return a flextable object instead of a data frame (default FALSE).
#   digits_sign: Integer, number of significant digits for rounding estimates and confidence intervals (default 4).
#   ...: Additional arguments passed to other methods or functions.
#
# Returns:
#   A formatted regression table as a data.frame or a flextable, depending on the flextable argument.


#' @describeIn table_regression Generates a regression table for meta-regression models analyzed with 'rma'.
#' @param model A meta-regression model object from 'metafor'.
#' @inheritParams table_regression.lm
#' @export
table_regression.metareg <- function(model, nice_p = TRUE, flextable = FALSE, include_intercept = FALSE, 
                                     I_sqaure = TRUE, R_square = TRUE,
                                     digits_sign = 4, 
                                     coef_names = NULL, 
                                     ...) {
    assertClass(model, "metareg")
    assertLogical(nice_p, len = 1)
    assertLogical(flextable, len = 1)
    assertIntegerish(digits_sign, lower = 1)
    
    # Extract coefficients, p-values, and confidence intervals
    coefs_info    <- extract_coefs(model, coef_index = 1, p_value_index = 4, digits_sign = digits_sign)
    conint_lower  <- signif(coef(summary(model))[,5], digits_sign)
    conint_upper  <- signif(coef(summary(model))[,6], digits_sign)
    formatted_cis <- paste(conint_lower, "-", conint_upper)
    
    # Define column names
    col_names <- c("Coefficients", "Estimates", "CI", "P-value")
    
    # Create the regression table
    tab <- create_regression_table(names_coefs = names(coef(model)),
                                   coefs = coefs_info$coefs, 
                                   cis = formatted_cis, 
                                   p_values = coefs_info$p_value, 
                                   col_names = col_names, 
                                   nice_p = nice_p,
                                   lrt_p = NULL,
                                   include_intercept = include_intercept, 
                                   coef_names = coef_names)
    
    if(I_sqaure){ tab$I2 <- c(round(model[["I2"]],2) , rep("", nrow(tab) - 1)) }
    if(R_square){ tab$R2 <- c(round(model[["R2"]],2) , rep("", nrow(tab) - 1)) }
    
    # Returning the table
    if(flextable) {
        
        if(I_sqaure & !R_square){
            tab <- flextab.standard(data.set = tab,  
                                    header = c("Coefficients", "Estimates", "CI",  "P-value", "I2"), ...)
            tab <- compose(tab,
                           i = 1, j = 5, part = "header",
                           value = as_paragraph( "I", as_sup("2"))
            )
        }
        if(!I_sqaure & R_square){
            tab <- flextab.standard(data.set = tab,  
                                    header = c("Coefficients", "Estimates", "CI",  "P-value", "R2"), ...)
            tab <- compose(tab,
                           i = 1, j = 5, part = "header",
                           value = as_paragraph( "R", as_sup("2"))
            )
        }
        if(I_sqaure & R_square){
            tab <- flextab.standard(data.set = tab,  
                                    header = c("Coefficients", "Estimates", "CI",  "P-value", "I2", "R2"), ...)
            tab <- compose(tab,
                           i = 1, j = 5, part = "header",
                           value = as_paragraph( "I", as_sup("2"))
            )
            tab <- compose(tab,
                           i = 1, j = 6, part = "header",
                           value = as_paragraph( "R", as_sup("2"))
            )
            
            
        }
        if(!I_sqaure & !R_square){
            tab <- flextab.standard(data.set = tab,  
                                    header = c("Coefficients", "Estimates", "CI",  "P-value"), ...)
        }
        
        
        return(tab)
        
        
    } else {
        return(tab)
    }
}


		
		
#-----------------------------
# Helper functions 
#-----------------------------
#-----------------------------
# Likelihood ratio test for GLM
#-----------------------------
# Function needs package lmtest
# Function dosen't work with interactions will only remove 1 var at a time

lrt_glm <- function(model, ...){
    UseMethod("lrt_glm", model)
}

lrt_glm.glm <- function(model){
    
    drop1(model, test = "LRT")
}

lrt_glm.logistf <- function(model){
    drop1(model)
}

lrt_glm.lm <- function(model){
    
    drop1(model, test = "Chisq")    
}


# Helper function to extract P-Values when lrt_p is TRUE
extract_p_value_lrt <- function(model) {
    p_value_lrt <- lrt_glm(model)
    p_value_lrt$`P-value` <- as.numeric(p_value_lrt[,5])
    p_value_f <- as.data.frame(coef(summary(model))[,4])
    
    for(i in 1:nrow(p_value_lrt)){
        p_value_f[startsWith(rownames(p_value_f), rownames(p_value_lrt)[i]), 1] <- p_value_lrt$`P-value`[i]
    }
    
    # Drop double LRT P-Values
    for(i in 1:nrow(p_value_lrt)){
        temp_names <- startsWith(rownames(p_value_f), rownames(p_value_lrt)[i])
        if(sum(temp_names) > 1){
            temp_names[which(temp_names)[1]] <- FALSE
            p_value_f[temp_names, 1] <- NA
        }
    } 
    return(p_value_f)
}



extract_p_value_firth <-function(model) {
    
    p_value_lrt <- as.data.frame(lrt_glm(model))
    p_value_lrt$`P-value` <- as.numeric(p_value_lrt[,3])
    p_value_f <- as.data.frame(summary(model)$prob)
    
    for(i in 1:nrow(p_value_lrt)){
        
        p_value_f[startsWith(rownames(p_value_f), rownames(p_value_lrt)[i]), 1] <- p_value_lrt$`P-value`[i]
    }
    
    for(i in 1:nrow(p_value_lrt)){
        temp_names <- startsWith(rownames(p_value_f), rownames(p_value_lrt)[i])
        if(sum(temp_names) > 1){
            temp_names[which(temp_names)[1]] <- FALSE
            p_value_f[temp_names, 1] <- NA
        }
    } 
    return(p_value_f)
}



#-----------------------------
# Combine vectors
#-----------------------------
create_table <- function(columns, col_names = NULL) {
    
    ## Transform table into a dataframe
    ## important if you want to use flextable as it assumes a dataframe.
    tab <- as.data.frame(columns)
    ## Delete rownames
    rownames(tab) <- NULL
    
    # Set colnames if provided
    if (!is.null(col_names)){
        colnames(tab) <- col_names
    }
    
    return(tab)
}

#-----------------------------
# Rename coefficients
#-----------------------------s
rename_coefficients <- function(tab, coef_names) {
    
    coef_names           <- as.data.frame(coef_names)
    colnames(coef_names) <- "new"
    coef_names["old"]    <- attributes(coef_names)$row.names
    
    for(i in 1:nrow(tab)){
        if(tab[i,"Coefficients"] %in% coef_names[["old"]] ){
            tab[i,"Coefficients"] <- coef_names[coef_names[["old"]] == tab[i,"Coefficients"], "new"]
        }
    }
    
    return(tab)
}


#-----------------------------
# Extract N from levels
#-----------------------------
extract_n <- function(model) {
    var_names <- attr(attr(model[["terms"]], "factors"), "dimnames")[[2]]
    
    n_group <- "" 
    for (i in var_names) {
        if (is.factor(model[["model"]][[i]])) {
            n_level <- as.integer(table(model[["model"]][[i]]))
            # n_level <- n_level[-1]  # Drop reference value
            n_group <- c(n_group, n_level)
        } else {
            n_level <- length(model[["model"]][[i]])
            n_group <- c(n_group, n_level)
        }
    }
    return(n_group)
    
}


extract_n_firth <- function(model) {
    var_names <- attr(attr(attr(model[["model"]], "terms"), "dataClasses")[-1], "names")
    
    n_group <- "" 
    for (i in var_names) {
        if (is.factor(model[["model"]][[i]])) {
            n_level <- as.integer(table(model[["model"]][[i]]))
            # n_level <- n_level[-1]  # Drop reference value
            n_group <- c(n_group, n_level)
        } else {
            n_level <- length(model[["model"]][[i]])
            n_group <- c(n_group, n_level)
        }
    }
    return(n_group)
    
}




#-----------------------------
# Extract coefficient names
#-----------------------------s
extract_coef_names <- function(model){
    
    
    # Step 1: Get the model coefficients
    coeff_names <- names(coef(model))
    
    # Step 2: Get the term labels
    term_labels <- attr(model[["terms"]], "term.labels")
    
    # Step 3: Get the model frame to identify categorical variables
    model_frame <- model.frame(model)
    
    # Step 4: Identify which variables are categorical (factors)
    categorical_vars <- sapply(model_frame, is.factor)
    
    # Step 5: Filter to get only the categorical variables that are also strings
    cat_string_vars <- names(categorical_vars[categorical_vars])
    
    # Step 6: Remove only the coefficients related to categorical string variables
    # (assuming you want to remove the labels of these terms from the coefficients)
    for (cat_var in cat_string_vars) {
        coeff_names <- str_remove(coeff_names, fixed(cat_var))
    }
    
    # Print the modified coefficient names
    coeff_names
    
}









