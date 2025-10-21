# -----------------------------------------------------------------------------
# Script Name: 06_NaVPS_analysis_figures.R
# Purpose: Forestplot
# Author: Florian Halbeisen
# Date Created: 2025-01-22
# -----------------------------------------------------------------------------
# Description:
# 1, Creating forestplot figure
# -----------------------------------------------------------------------------


create_ggforestplot <- function(x, include_N = FALSE, vertical_line = 1, title_estimate = "Odds Ratios", x_axis_title = "", size_study_label = 2, 
                                log_scale = FALSE, x_ticks = NULL){
    
    # Set variables
    size_x_axis_title <- 12
    postion_x_axis_title <- 0

    
    # Redefine Dataframe 
    # Deframe coming from table_regression()
    # Coefficent, N, Odds Ratios, 95%CI, P-value
    tab_forest <- x
    if(is.null(tab_forest$N)){ tab_forest$N <- 0}
    if(is.null(tab_forest$`Odds Ratios`) && !is.null( tab_forest$Estimates) ){ tab_forest$`Odds Ratios` <- tab_forest$Estimates}
    if(is.null(tab_forest$`Odds Ratios`) && !is.null( tab_forest$`Rate Ratios`) ){ tab_forest$`Odds Ratios` <- tab_forest$`Rate Ratios`}
    
    tab_forest$conf_low <- as.numeric(str_split(tab_forest$CI, " - ", n = Inf, simplify = TRUE)[, 1])
    tab_forest$conf_low <- round(tab_forest$conf_low, 3)
    tab_forest$conf_high <- as.numeric(str_split(tab_forest$CI, " - ", n = Inf, simplify = TRUE)[, 2])
    tab_forest$conf_high <- round(tab_forest$conf_high, 3)
    tab_forest$`Odds Ratios` <- as.numeric(tab_forest$`Odds Ratios`)
    tab_forest$OR <- round(tab_forest$`Odds Ratios`, 2)
    
    tab_forest$pvalue <- tab_forest$`P-value`
    
    tab_forest$CI[tab_forest$CI != ""] <- paste(round(tab_forest$conf_low[tab_forest$CI  != ""], 2),
                                                "-",
                                                round(tab_forest$conf_high[tab_forest$CI  != ""], 2))
  
    
    # Titles will be bold. 
    # Titles are defined to be the lines without OR
    tab_forest$Coefficients[is.na(tab_forest$`Odds Ratios`)] <- paste0("**", tab_forest$Coefficients[is.na(tab_forest$`Odds Ratios`)], "**")
       
    # Add empty line before title
    if(is.null(tab_forest$is_title)){ tab_forest$is_title <- is.na(tab_forest$conf_low)}
    tab_forest <- insert_empty_rows(tab_forest, "is_title")
    
    # No empty line as first line
    tab_forest <- tab_forest %>% filter(!(row_number() == 1 & Coefficients == ""))
    
    tab_forest <- tab_forest %>%
        mutate(Coefficients = ifelse(is.na(Coefficients), " ", Coefficients))  # Replace NA with " "
    
    # Give sequential number for order of plot and text
    tab_forest$order <- seq(nrow(tab_forest), 1)  # Assign a sequential index
    
    p <- ggplot(tab_forest, aes(x = OR, y = as.factor(order), xmin = conf_low, xmax = conf_high)) +
        
        geom_pointrange(shape = 16, fill = "black") +
        xlab(x_axis_title) +
        theme_classic() +
        theme(axis.text.y = element_blank(), axis.title.y = element_blank()) +
        ggtitle(" ") +
        theme(
            plot.title = element_text(size = 12),
            axis.title.x = element_text(size = size_x_axis_title, hjust = postion_x_axis_title),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.line.y  = element_blank()
        ) +
        
        geom_vline(xintercept = vertical_line, linetype = "dashed", color = "red") 
    
    if(log_scale | !is.null(x_ticks)){
        p <- p + scale_x_continuous(trans='log2',
            breaks = if (!is.null(x_ticks)) x_ticks else waiver(),
            labels = if (!is.null(x_ticks)) x_ticks else waiver()
        )
    }
    
    
    tab_base <- ggplot(tab_forest, aes(y = as.factor(order))) +
        ylab(NULL) + xlab("  ") + 
        theme(
            axis.text.x=element_text(color="white"), ## need text to be printed so it stays aligned with figure but white so it's invisible
            axis.line=element_blank(),
            axis.title.x = element_text(size = size_x_axis_title),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.y=element_blank(),legend.position="none",
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank()
        ) 
    
    # Base table left aligned
    tab_base_left <- tab_base + 
        theme(plot.title = element_text( size=12))
    
    # Base table center aligned
    tab_base_center <- tab_base + 
        theme(plot.title = element_text(hjust = 0.5, size=12))  
    
    # Base table right aligned
    tab_base_right <- tab_base + 
        theme(plot.title = element_text(hjust = 1, size=12))
    
    data_table_Coefficients <- tab_base_left +
        geom_richtext(aes(x =1, label = Coefficients), hjust = 0, fill = NA, label.color = NA) +
        ggtitle("Coefficents") + 
        coord_cartesian(xlim = c(1.05, 2), clip = "off")
    
    data_table_OR <- tab_base_center +
        geom_text(aes(x = 1, label = OR)) +
        ggtitle(title_estimate) 
       
    data_table_CI <- tab_base_center +
        geom_text(aes(x = 1, label = CI)) +
        ggtitle("95% CI") 
      
    data_table_N <- tab_base_center +
        geom_text(aes(x = 1, label = N)) +
        ggtitle("N") 
    
    data_table_pvalue <- tab_base_center +
        geom_text(aes(x = 1, label = pvalue)) +
        ggtitle("P-value") 
     
    
    if(include_N){
        
        n_left  <- 2
        n_right <- 3
        size_study_label = 2
        
        lay <- matrix(c(rep(1, size_study_label),
                        rep(seq(2, n_left)),
                        rep(n_left + 1, 6),
                        seq(n_left + 2, n_left + 1 + n_right)),
                      nrow = 1)
        
        
        plot_list <- vector("list", length = n_left + n_right + 1)
        names(plot_list) <- c(c("Coefficients", "N"), "p", c("OR", "CI", "pvalue"))
        
        
    } 
    else {
        
        
        n_left  <- 1
        n_right <- 3
        size_study_label = size_study_label
        
        lay <- matrix(
            c(rep(1, size_study_label),   # Study label = 1
              rep(n_left + 1, 6),         # Size of Plot 
              seq(n_left + 2, n_left + 1 + n_right)  # Size right side - Start
            ),
            nrow = 1)
        
        
        plot_list <- vector("list", length = n_left + n_right + 1)
        names(plot_list) <- c(c("Coefficients"), "p", c("OR", "CI", "pvalue"))
        
        
        
    }

    for (item in names(plot_list)) {
        if (item == "p") {
            plot_list[[item]] <- p
        } else {
            plot_list[[item]] <- get(paste0("data_table_", item))
        }
    }
    
    gridExtra::grid.arrange(grobs = plot_list,
                            top = "",
                            layout_matrix = lay)

    
}


insert_empty_rows <- function(df, title_flag = "is_title") {
    if (!title_flag %in% names(df)) stop("Need a logical title_flag column")
    
    empty_row <- df[0, ]; empty_row[1, ] <- NA
    
    expanded_df <- list()
    for (i in seq_len(nrow(df))) {
        if (isTRUE(df[[title_flag]][i])) {
            expanded_df[[length(expanded_df) + 1]] <- empty_row
        }
        expanded_df[[length(expanded_df) + 1]] <- df[i, ]
    }
    result_df <- do.call(rbind, expanded_df)
    rownames(result_df) <- NULL
    result_df
}
