# ++++++++++++++++++++++++
# Load the packages
# ++++++++++++++++++++++++

library(flextable)    # Version 0.9.4
library(stringr)      # Version 1.5.1
library(dplyr)        # Version 1.1.4
library(tidyr)        # Version 1.1.4
library(ggplot2)      # Version 3.4.4
library(ggtext)       # Version 0.1.2
library(gridExtra)    # Version 2.3
library(forcats)      # Version 1.0.0
library(DHARMa)       # Version 0.4.7
library(MASS)         # Version 7.3-60
library(sandwich)     # Version 3.1-0
library(lmtest)       # Version 0.9-40
library(glmmTMB)      # Version 1.1.10
library(performance)  # Version 0.13.0
library(logistf)      # Version 1.26.0
library(broom)        # Version 1.0.5
library(ggtext)       # Version 0.1.2

# Flowchart packages
library(DiagrammeR)     # Version 1.0.11
library(DiagrammeRsvg)  # Version 0.1
library(rsvg)           # Version 2.6.1



# ++++++++++++++++++++++++
# Load Data
# ++++++++++++++++++++++++

# Set working directory
path_study       <- here::here()   # Version 1.0.1
path_stats       <- paste0(path_study, "/")
path_data        <- paste0(path_study, "/RawData/")
  
# Read data
dta <- read.csv(paste0(path_data, "..."))
dta <- janitor::clean_names(dta)


# ++++++++++++++++++++++++
# Load functions
# ++++++++++++++++++++++++

# Custom function
source(paste0(path_stats, "02_NaVPS_function.R"))


# ++++++++++++++++++++++++
# Set Variables
# ++++++++++++++++++++++++

# Variables 
# Today's date
today <- Sys.Date()
today <- format(today, format="%Y%m%d")

# Figure width
width_forest_preop_abs    <- 16.25
width_forest_contra_absNM <- 13.54


# ++++++++++++++++++++++++
# Run analysis
# ++++++++++++++++++++++++
# Run Codes
source(paste0(path_stats, "03_NaVPS_data_preparation.R"))
source(paste0(path_stats, "04_NaVPS_flowchart.R"))
source(paste0(path_stats, "05_NaVPS_characteristics.R"))
source(paste0(path_stats, "06_NaVPS_figures_function.R"))
source(paste0(path_stats, "07_NaVPS_analysis_times.R"))
source(paste0(path_stats, "08_NaVPS_analysis_shunts.R"))
source(paste0(path_stats, "09_NaVPS_analysis_volumetry.R"))
source(paste0(path_stats, "10_NaVPS_analysis_evans.R"))
source(paste0(path_stats, "11_NaVPS_analysis_complications.R"))
source(paste0(path_stats, "12_NaVPS_analysis_revisions.R"))
source(paste0(path_stats, "13_NaVPS_analysis_mrs.R"))
