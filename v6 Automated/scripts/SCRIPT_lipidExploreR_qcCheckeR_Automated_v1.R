# . ------------------------------------------------------------------------------------------------------------------------------  
# PROJECT SETUP: --------------------------------------
start_time <- Sys.time()
## 1. load packages: --------------------------------------
#clean skylineR/previous session
rm(master_list)
#load packages
package_list <- c('statTarget', 'svDialogs', 'ggpubr', 'janitor', 'plotly', 'knitr', 'viridisLite', 'mzR', 'httr', 'cowplot', 'matrixStats', 'tidyverse')

for(idx_package in package_list){
  if(!require(package = idx_package, character.only = TRUE)){
    install.packages(idx_package, dependencies = TRUE)
    suppressMessages(require(package = idx_package, character.only = TRUE))
  }
}

## 2. project setup / load skylineR --------------------------------------
skylineR_directory <- paste0("C:/Users/Hszem/OneDrive - Murdoch University/Desktop/Dummy Cohort") # Change to entering from notebook later##############################

#selct what plates to qc
tempQCflag <- "all"

#rda
if(tempQCflag == "all"){
  dir.create(paste0(skylineR_directory,"/", tempQCflag))
  }

#does plate subfolder exist
if(!dir.exists(paste0(skylineR_directory, "/", tempQCflag))){
  stop(print(paste0(skylineR_directory, "/", tempQCflag, " does not exist. check and re-run script")))
}

#add plateTag
if(dir.exists(paste0(skylineR_directory, "/", tempQCflag))){
  #take back up directory for all plates (want parent directory)
  skylineR_directory_all <- skylineR_directory
  skylineR_directory <- paste0(skylineR_directory, "/", tempQCflag)
  if(tempQCflag == "all"){skylineR_directory <- skylineR_directory_all}
}

#list .rda files
rda_fileList <- list.files(skylineR_directory, pattern = "_skylineR", recursive = TRUE)
#ensure only .rda files are in filelist
rda_fileList <- rda_fileList[grepl(".rda", rda_fileList, ignore.case = T)]

#remove any tagged with archive
rda_fileList <- rda_fileList[!grepl("archive", rda_fileList)]

#display rda for assessment
#message/print process!!!!######
cat(paste0("The following .rda are currently selected for QC:\n   ~/", 
            paste0(rda_fileList, collapse = " \n   ~/"),
            "\n To remove any .rda from procesing workflow move them to a sub-folder tagged `archive`."))

if(length(rda_fileList)>0){
#load 1st rda 
 load(paste(skylineR_directory, rda_fileList[1], sep ="/"))
 master_list<-master_list2
 rm(master_list2)
} else {
  stop(print(paste0("no .rda present in ", skylineR_directory)))
}

#display rda for assessment
#message/print process!!!!######
master_list$project_details$script_log$messages$rda_assesment <- paste0("The following .rda are currently selected for QC:\n   ~/", 
                                                                        paste0(rda_fileList, collapse = " \n   ~/"),
                                                                        "\n To remove any .rda from procesing workflow move them to a sub-folder tagged `archive`.")
#set user
master_list$project_details$user_name <- "ANPC"
#set project name
master_list$project_details$project_name <- master_list$project_details$project_name
#reset parent directory
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "local directory")] <- skylineR_directory
master_list$project_details$project_dir <- skylineR_directory
#set github directory to v4
master_list$project_details$github_master_dir <- "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR/refs/heads/main/v4/"
#set lipid method version list for later use
master_list$project_details$plate_method_versions[[master_list$project_details$plateID]] <- master_list$project_details$is_ver

#reassign master_list[1] and remove original master_list object
masterListBatch <- master_list; rm(master_list)

#load and combine plate RDA files from skylineR
if(length(rda_fileList) > 1){
  #run loop to combine .rda files
  for(idx_rda in rda_fileList[2:length(rda_fileList)]){
    
    #load loop .rda
    load(paste(skylineR_directory, idx_rda, sep ="/"))
    master_list <- master_list2; rm(master_list2)
    
    #combine mzml
    for(idx_mzml in names(master_list$data$mzR)){masterListBatch$data$mzR[[idx_mzml]] <- master_list$data$mzR[[idx_mzml]]}
    
    #combine lipid version list
    masterListBatch$project_details$plate_method_versions[[idx_mzml]] <- c(
      masterListBatch$project_details$plate_method_versions[[idx_mzml]],
      master_list$project_details$is_ver)
    
    #combine plate list
    masterListBatch$project_details$mzml_plate_list <- c(
      masterListBatch$project_details$mzml_plate_list,
      master_list$project_details$mzml_plate_list)
    
    #combine mzml_sample_list
    masterListBatch$project_details$mzml_sample_list <- c(
      masterListBatch$project_details$mzml_sample_list,
      master_list$project_details$mzml_plate_list)
    
    #combine skylineR data
    masterListBatch$data$skyline_report <- bind_rows(
      masterListBatch$data$skyline_report,
      master_list$data$skyline_report)
    
    #remove master_list for next iteration of loop
    rm(master_list)
  } #close for idx_ra
}#close if length(rda_filelist)>1 statement

#reset to master_list for remainder of script and removal of masterlistBatchObject
master_list <- masterListBatch; rm(masterListBatch)

#set skyline directory if all plates
if(tempQCflag == "all"){
skylineR_directory <- paste0(skylineR_directory, "/", tempQCflag)
master_list$project_details$plateID <- "all"
}

master_list$project_details$project_dir <- skylineR_directory

#set version of lipidExploreR used
master_list$project_details$lipidExploreR_version <- "6.0"
master_list$project_details$qcCheckR_version <- "6.0"
master_list$project_details$script_log$timestamps$start_time <- start_time;rm(start_time)
master_list$summary_tables$project_summary$value[which(master_list$summary_tables$project_summary$`Project detail` == "lipidExploreR version")] <- "4.0"
master_list$summary_tables$area_project_summary$value[which(master_list$summary_tables$area_project_summary$`Project detail` == "lipidExploreR version")] <- "4.0"

## 3. Load templates/guides --------------------------------------
#prepare template/guides for concentration for calculation
master_list$templates <- list()
#if using sciex lipidizer internal standards
  master_list$templates$v1$SIL_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_lipid_mrm_template_v1.csv"),
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$v1$conc_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir,
                  "templates/LGW_SIL_batch_103.csv"), 
    show_col_types = FALSE) %>% 
    clean_names()

#if using ultra splash mix (ANPC method v2)
  master_list$templates$v2$SIL_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_lipid_mrm_template_v2.csv"),
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$v2$conc_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"), 
    show_col_types = FALSE) %>% 
    clean_names()

#if using ultra splash mix (ANPC method v3)
  master_list$templates$v3$SIL_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_lipid_mrm_template_v3.csv"),
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$v3$conc_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"), 
    show_col_types = FALSE) %>% 
    clean_names()

#ultra splash mix (ANPC method v4)

  master_list$templates$v4$SIL_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_lipid_mrm_template_v4.csv"),
    show_col_types = FALSE) %>%
    clean_names()
  master_list$templates$v4$conc_guide <- read_csv(
    file = paste0(master_list$project_details$github_master_dir, 
                  "templates/LGW_SIL_batch_Ultimate_2023-03-06.csv"), 
    show_col_types = FALSE) %>% 
    clean_names()

  ##4. source packages----
  #update_script_log
  ###Change to github repo!!!!! ----
  master_list$environment$user_functions$update_script_log <- source(paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/functions/FUNC_lipidExploreR_Update_script_log.R"))
  
  #update script log
  master_list <- master_list$environment$user_functions$update_script_log$value(master_list, "project_setup", "start_time", "data_preparation")
  
# . ------------------------------------------------------------------------------------------------------------------------------  

# PHASE 1: DATA PREPARATION  -----------------
master_list$data$peakArea <- list()
## 1.1. move skyline data and nest under peakArea ------
master_list$data$peakArea$skylineReport <- master_list$data$skyline_report

#set batches (plates)
if(tempQCflag != "all"){
master_list$project_details$mzml_plate_list <- tempQCflag
}

if(tempQCflag == "all"){
  master_list$project_details$mzml_plate_list <- list.dirs(sub("/all","",skylineR_directory), full.names = FALSE, recursive = FALSE)
  master_list$project_details$mzml_plate_list <- master_list$project_details$mzml_plate_list[grepl("p", master_list$project_details$mzml_plate_list)]
  master_list$project_details$mzml_plate_list <- master_list$project_details$mzml_plate_list[!grepl("archive", master_list$project_details$mzml_plate_list)]
  master_list$project_details$mzml_plate_list <- master_list$project_details$mzml_plate_list[!grepl("all", master_list$project_details$mzml_plate_list)]
}

cat(paste0("subfolders with plates for QC are: | ", paste0(master_list$project_details$mzml_plate_list, collapse = " | "), " | . . . . . if any are incorrect check and re-run script"))

## 1.2. transpose data to standard metabolomics structure (features in columns, samples in rows) ---------------------------------------
master_list$data$peakArea$transposed <- list()
#run loop for each data plate/batch
for(idx_batch in master_list$project_details$mzml_plate_list){
  master_list$data$peakArea$transposed[[idx_batch]] <- pivot_wider(
    data = master_list$data$peakArea$skylineReport %>%
      filter(file_name %in% names(master_list$data$mzR[[idx_batch]])),
    id_cols = file_name,
    names_from = molecule_name,
    values_from = area
  ) %>%
    dplyr::rename(sample_name = file_name) 
  
  #remove file extenstion (.mzML from sample_name)
  master_list$data$peakArea$transposed[[idx_batch]]$sample_name <- sub(".mzML", "", master_list$data$peakArea$transposed[[idx_batch]]$sample_name)
  
  
  #make numeric 
  master_list$data$peakArea$transposed[[idx_batch]][,-1] <-
    sapply(master_list$data$peakArea$transposed[[idx_batch]][,-1], 
           as.numeric) %>% 
    as_tibble()
}
## 1.3. Sort by run order and add annotation data ------------------------------- 
#list for storing concentration data area_sorted by run order
master_list$project_details$run_orders <- list()
#set up sorted area list
master_list$data$peakArea$sorted <- list()

#run loop
for (idx_batch in names(master_list$data$peakArea$transposed)){
  master_list$project_details$run_orders[[idx_batch]] <- sub(".mzML", "", names(master_list$data$mzR[[idx_batch]])) %>% as_tibble() %>% dplyr::rename(sample_name = value)
  temp_timestamp <- NULL
  for(idx_file in names(master_list$data$mzR[[idx_batch]])){
    temp_timestamp <- c(temp_timestamp, master_list$data$mzR[[idx_batch]][[idx_file]]$mzR_timestamp)
  }
  master_list$project_details$run_orders[[idx_batch]] <-  master_list$project_details$run_orders[[idx_batch]] %>%
    add_column(sample_timestamp = temp_timestamp) %>%
    arrange(sample_timestamp)
  
  #create metadata columns
  master_list$project_details$run_orders[[idx_batch]]$sample_batch <- sub("(_SER|_PLA).*", "", master_list$project_details$run_orders[[idx_batch]]$sample_name)# cohort name
  master_list$project_details$run_orders[[idx_batch]]$sample_plate_id <- idx_batch #plate_id
  master_list$project_details$run_orders[[idx_batch]]$sample_plate_order <- c(1:nrow(master_list$project_details$run_orders[[idx_batch]])) #sample_plate_order
  master_list$project_details$run_orders[[idx_batch]]$sample_type <- NA
  master_list$project_details$run_orders[[idx_batch]]$sample_matrix <- ifelse(grepl("(?i)SER", master_list$project_details$run_orders[[idx_batch]]$sample_name), "SER",
                                                                              ifelse(grepl("(?i)PLA", master_list$project_details$run_orders[[idx_batch]]$sample_name), "PLA", NA))
  
  # set sample_type using mutate
  master_list$project_details$run_orders[[idx_batch]] <- master_list$project_details$run_orders[[idx_batch]]  %>%
    mutate(
      sample_type = ifelse(str_detect(string = sample_name, pattern = "(?i)pqc"), "pqc",
                           ifelse(str_detect(string = sample_name, pattern = "(?i)vltr"), "vltr",
                                  ifelse(str_detect(string = sample_name, pattern = "(?i)sltr"), "sltr",
                                         ifelse(str_detect(string = sample_name, pattern = "(?i)ltr"), "ltr",
                                                ifelse(str_detect(string = sample_name, pattern = "(?i)blank"), "blank",
                                                       ifelse(str_detect(string = sample_name, pattern = "(?i)istds"), "istds",
                                                              ifelse(str_detect(string = sample_name, pattern = "(?i)cond"), "conditioning",
                                                                     "sample")
                                                       )))))))
  
  #sort area_transposed data by run order and then remove conditioning runs
  master_list$data$peakArea$sorted[[idx_batch]] <- master_list$project_details$run_orders[[idx_batch]] %>%
    left_join(master_list$data$peakArea$transposed[[idx_batch]], by = "sample_name") %>% 
    filter(sample_type == "pqc" | sample_type == "sample" | sample_type == "ltr" | sample_type == "sltr" | sample_type == "vltr") %>%
    arrange(sample_timestamp) %>%
    add_column(sample_run_index = c(1:nrow(.)), .before = 1)
}
  
  # Dynamically set qc_type
#Check logic with luke-----
  # Flag low number of QCs using ratio of QC to total samples for qc types
  master_list$project_details$qc_passed <- list()
  for (idxPlate in names(master_list$data$peakArea$sorted)) {
    for (qc in c("vltr", "ltr", "pqc")) {
      plateData <- master_list$data$peakArea$sorted[[idxPlate]]
      totalSamples <- length(plateData$sample_name)
      requiredQCs <- 6 / 96 # Minimum of 6 QC for 96 plate
      qcCount <- length(which(tolower(plateData$sample_type) == tolower(qc)))
      qcCountRatio <- qcCount / totalSamples
      if (qcCountRatio < requiredQCs || qcCount < 2) {
        master_list$project_details$qc_passed[[idxPlate]][[qc]] <- "fail"
      } else {
        master_list$project_details$qc_passed[[idxPlate]][[qc]] <- "pass"
      }
    }
  }
  
  
  # Summarise qc assessment pass/fail for all plates in project for qc types in master
  master_list$project_details$global_qc_pass <- list(vltr = "pass", ltr = "pass", pqc = "pass")
  for (idxPlate in names(master_list$data$peakArea$sorted)) {
    for (qc in c("vltr", "ltr", "pqc")) {
      if (master_list$project_details$qc_passed[[idxPlate]][[tolower(qc)]] == "fail") {
        master_list$project_details$global_qc_pass[[qc]] <- "fail"
      }
    }
  }

  # Set project qc_type based on qc assesment 
  master_list$project_details$qc_type <- if (master_list$project_details$global_qc_pass$ltr == "pass") {
    "ltr"
  } else if (master_list$project_details$global_qc_pass$vltr == "pass") {
    "vltr"
  }  else if (master_list$project_details$global_qc_pass$pqc == "pass") {
    "pqc"
  } else {
    "unknown"
  }
  
  #Stop script and print error if there are no viable QC for QC checker
  if (master_list$project_details$qc_type == "unknown") {
    # Capture the structure of the lists as strings
    global_qc_pass_str <- capture.output(str(master_list$project_details$global_qc_pass))
    plate_qc_passed_str <- capture.output(str(master_list$project_details$qc_passed))
    
    # Create a formatted message
    error_message <- paste0(
      "STOPPING SCRIPT \n",
      "There are no viable ltr, pqc, or vltr to qc in project:", master_list$project_details$project_name, ".\n",
      "Please refer to the details below:\n",
      "Global QC Assessment: ", paste(global_qc_pass_str, collapse = "\n"), "\n",
      "Plate QC Assessment: ", paste(plate_qc_passed_str, collapse = "\n")
    )
    
    # Stop execution and print the error message
    stop(error_message)
  } else{
    message <- paste("qcCheckeR has set QC to:", master_list$project_details$qc_type)
  cat(message)
  }
  
  #Resume formatting of master_list$data$peakArea$sorted
  for (idx_batch in names(master_list$data$peakArea$transposed)){
  #add_factor column for plotting
  master_list$data$peakArea$sorted[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    add_column(sample_type_factor = master_list$data$peakArea$sorted[[idx_batch]]$sample_type %>% 
                 factor(
                   levels = c("sample", master_list$project_details$qc_type, "ltr", "pqc", "vltr", "sltr")[!duplicated(c("sample", master_list$project_details$qc_type, "ltr", "pqc", "vltr", "sltr"))], 
                   ordered = TRUE),
               .after = "sample_type") %>% 
    add_column(
      sample_type_factor_rev = factor(
        .$sample_type_factor,
        levels = rev(levels(.$sample_type_factor)), 
        ordered = TRUE),
      .after = "sample_type_factor")
  
  #convert sample type to "qc" for selected qc type and "sample" for everything else
  master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]][-which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]]))] <- "sample"
  master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]][which(tolower(master_list$project_details$qc_type) == tolower(master_list$data$peakArea$sorted[[idx_batch]][["sample_type"]]))] <- "qc"
  
  #add sample_data_source column
  master_list$data$peakArea$sorted[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    add_column(sample_data_source = ".peakArea",
               .after = "sample_type_factor_rev")
  
}


#add a global runorder based on mzML sample timestamp
tempAllSamples <- master_list$data$peakArea$sorted %>%
  bind_rows() %>%
  arrange(sample_timestamp)
#set project master run index
tempAllSamples$sample_run_index <- c(1:nrow(tempAllSamples))
#reset sorted list
master_list$data$peakArea$sorted <-list()
for(idx_batch in unique(tempAllSamples$sample_plate_id)){
  master_list$data$peakArea$sorted[[idx_batch]] <- tempAllSamples %>%
    filter(sample_plate_id == idx_batch)%>%
    select(where(~any(!is.na(.))))
}

## 1.4. impute missing values [min/2 imputation (missing assumed < LOD)] -----------------------------------------------------
#Imputation of the all zero value and missing data 
#Imputation is completed using x/2, where x is minimum intensity of that feature in the batch

#impute function
#add LGW impute function
lgw_impute <- function(x){
  map(.x = x, .f = ~ (min(.x[.x > 0], na.rm = TRUE))/2) %>%
    #use replace_na to replace NAs with min/2 value
    replace_na(
      data = x %>% mutate_all(~ replace(., . == 0, NA)), #note - replace zeros with NA to make compatible with replace_na()
      replace = .) #note - replace with list of min/2 values generated from map function in pipe (.)
}

#set data list
master_list$data$peakArea$imputed <- list()
#run loop in all plates
for(idx_batch in names(master_list$data$peakArea$sorted)){
  #set all 0, NaN, is.infinate to a NA value for consistency
  #create matrix 
  master_list$data$peakArea$imputed[[idx_batch]]  <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample")) %>%
    as.matrix()
  
  #replace 0, NaN and Inf with NA for imputation (min/2)
  master_list$data$peakArea$imputed[[idx_batch]][master_list$data$peakArea$imputed[[idx_batch]] ==0] <- NA
  master_list$data$peakArea$imputed[[idx_batch]][is.infinite(master_list$data$peakArea$imputed[[idx_batch]])] <- NA
  master_list$data$peakArea$imputed[[idx_batch]][is.nan(master_list$data$peakArea$imputed[[idx_batch]])] <- NA
  
  #run lgw_impute function
  master_list$data$peakArea$imputed[[idx_batch]] <- master_list$data$peakArea$imputed[[idx_batch]] %>% 
    as.data.frame() %>%
    lgw_impute() %>%
    rownames_to_column("sample_name") %>%
    as_tibble() %>%
    #samples that are 100% missing (zero/na) have imputation error as min/2(x) is inf. So this step replaces inf with zeros (will be flagged for filter later anyway).
    mutate_all(function(x) ifelse(is.infinite(x), 1, x)) %>%
    left_join(
      select(master_list$data$peakArea$sorted[[idx_batch]], contains("sample")),
      .,
      by = "sample_name"
    )
  
  #tag data type
  master_list$data$peakArea$imputed[[idx_batch]]$sample_data_source <- ".peakAreaImputed"

}



## 1.5. calculate peakResponse and concentration values -------------

# Initialise lists
master_list$data$response <- list()
master_list$data$concentration <- list()

# Store batches (plates)
batches <- unique(names(master_list$data$peakArea$imputed))

# For loop for SIL template version control by plate
for(idx_batch in batches){
  
  template_version <- master_list$project_details$plate_method_versions[[idx_batch]]
  
  master_list$project_details$is_ver <- template_version
  master_list[["templates"]][["Plate SIL version"]][[idx_batch]] <- template_version
  
  # Complete for both sorted and imputed lists
  for (idx_dataType in c("sorted", "imputed")){
    
    # Find common cols between data and template
    SIL_filter <- intersect(
      colnames(master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>% select(contains("SIL"))),
      master_list$templates[[template_version]]$SIL_guide$note
    )
    
    # Select columns containing "SIL" that are in the SIL_filter
    sil_cols <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
      select(all_of(SIL_filter))
    
    # Select columns that do not contain "SIL"
    non_sil_cols <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
      select(-contains("SIL"))
    
    # Combine the two sets of columns
    master_list$data$peakArea[[idx_dataType]][[idx_batch]] <- bind_cols(non_sil_cols, sil_cols)
    
    # Set empty list to store output data for response
    master_list$data$response[[idx_dataType]][[idx_batch]] <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
      select(contains("sample"))
    
    # Set empty list to store output data for concentration
    master_list$data$concentration[[idx_dataType]][[idx_batch]] <- master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
      select(contains("sample"))
    
    # Run loop for each SIL IS
    for(idx_SIL in master_list$data$peakArea[[idx_dataType]][[idx_batch]] %>%
        select(contains("SIL")) %>%
        names()){
      if(length(which(master_list$templates[[template_version]]$SIL_guide$note == idx_SIL)) > 0){
        # Find which SIL is used from the template
        target_lipids <- select(master_list$data$peakArea[[idx_dataType]][[idx_batch]], 
                                sample_name,
                                any_of(master_list$templates[[template_version]]$SIL_guide$precursor_name[which(master_list$templates[[template_version]]$SIL_guide$note == idx_SIL)])) %>%
          column_to_rownames("sample_name")
        
        if(ncol(target_lipids) > 0){
          # Calculate response ratio
          target_lipids_response <- as.matrix(target_lipids / master_list$data$peakArea[[idx_dataType]][[idx_batch]][[idx_SIL]])
          
          # Standardise data set to remove infinities and NAs
          target_lipids_response[is.na(target_lipids_response)] <- 0
          target_lipids_response[is.infinite(target_lipids_response)] <- 0
          
          # Round and improve decimal places (2dp for >1; 3 sf for < 1)
          #target_lipids_response[target_lipids_response < 1] <- signif(target_lipids_response[target_lipids_response < 1], 2)
          #target_lipids_response[target_lipids_response > 1] <- round(target_lipids_response[target_lipids_response > 1], 2)
          
          # Rejoin and make master
          master_list$data$response[[idx_dataType]][[idx_batch]] <- left_join(
            by = "sample_name",
            master_list$data$response[[idx_dataType]][[idx_batch]], 
            as.data.frame(target_lipids_response) %>%
              rownames_to_column("sample_name")
          )
        } # if ncol()
        
        ## 1.6.b. Convert response to single point concentration factor
        # Find the concentration factor of SIL
        sil_conc_factor <- master_list$templates[[template_version]]$conc_guide$concentration_factor[which(master_list$templates[[template_version]]$conc_guide$sil_name == idx_SIL)]
        
        if(length(sil_conc_factor) == 1){
          # Select response data
          target_lipids <- master_list$data$response[[idx_dataType]][[idx_batch]] %>%
            select(sample_name,
                   any_of(master_list$templates[[template_version]]$SIL_guide$precursor_name[which(master_list$templates[[template_version]]$SIL_guide$note == idx_SIL)])) %>%
            column_to_rownames("sample_name")
          
          # Calculate concentration
          target_lipids_concentration <- as.matrix(target_lipids * sil_conc_factor)
          
          # Standardise data set to remove infinities and NAs
          target_lipids_concentration[is.na(target_lipids_concentration)] <- 0
          target_lipids_concentration[is.infinite(target_lipids_concentration)] <- 0
          
          # Round and improve decimal places (2dp for >1; 3 sf for < 1)
          #target_lipids_concentration[target_lipids_concentration < 1] <- signif(target_lipids_concentration[target_lipids_concentration < 1], 2)
          #target_lipids_concentration[target_lipids_concentration > 1] <- round(target_lipids_concentration[target_lipids_concentration > 1], 2)
          
          # Rejoin and make master
          master_list$data$concentration[[idx_dataType]][[idx_batch]] <- left_join(
            by = "sample_name",
            master_list$data$concentration[[idx_dataType]][[idx_batch]], 
            as.data.frame(target_lipids_concentration) %>%
              rownames_to_column("sample_name")
          )
        } # close if(length(sil_conc_factor) == 1)
      } # if length()
    } # idx_sil
    
    master_list$data$response[[idx_dataType]][[idx_batch]]$sample_data_source <- paste0(".response.", idx_dataType)
    master_list$data$concentration[[idx_dataType]][[idx_batch]]$sample_data_source <- paste0("concentration.", idx_dataType)
  } # idx_dataType
} # SIL version control by plate

# Find common lipids across all data frames
# Complete for both sorted and imputed lists
for (idx_dataType in c("response", "concentration")) {
  for (idx_dataTypeII in names(master_list$data[[idx_dataType]])) {
    # Find common lipids
    common_lipids <- Reduce(intersect, lapply(master_list$data[[idx_dataType]][[idx_dataTypeII]], colnames))
    
    # Select common lipids in each data frame
    for (idx_batch in names(master_list$data[[idx_dataType]][[idx_dataTypeII]])) {
      master_list$data[[idx_dataType]][[idx_dataTypeII]][[idx_batch]] <- master_list$data[[idx_dataType]][[idx_dataTypeII]][[idx_batch]] %>%
        select(all_of(common_lipids))
    }
  }
}

## 1.6. statTarget signalDrift | batch correction ----------------------------

#create batch correction directory
#data
if(!dir.exists(paste0(skylineR_directory, "/data"))){
  dir.create(paste0(skylineR_directory, "/data"))
}

if(!dir.exists(paste0(skylineR_directory, "/data/batch_correction"))){
  dir.create(paste0(skylineR_directory, "/data/batch_correction"))
}

if(!dir.exists(paste0(skylineR_directory, "/data/rda/"))){
  dir.create(paste0(skylineR_directory, "/data/rda/"))
}

if(!dir.exists(paste0(skylineR_directory, "/data/batch_correction"))){
  dir.create(paste0(skylineR_directory, "/data/batch_correction"))
}

if(!dir.exists(paste0(skylineR_directory, "/xlsx_report"))){
  dir.create(paste0(skylineR_directory, "/xlsx_report"))
}

if(!dir.exists(paste0(skylineR_directory, "/html_report"))){
  dir.create(paste0(skylineR_directory, "/html_report"))
}


#check logic with luke ----
#set qc for statTarget
master_list$project_details$statTarget_qc_type <- master_list$project_details$qc_type

# #dynamic set qc-type for stattarget
# master_list$project_details$statTarget_qc_type <- if (master_list$project_details$global_qc_pass$ltr == "pass") {
#   "ltr"
# } else if (master_list$project_details$global_qc_pass$vltr == "pass") {
#   "vltr"
# }  else if (master_list$project_details$global_qc_pass$pqc == "pass") {
#   "pqc"
# } else {
#   "unknown"
# }
# 
# #Stop script and print error if there are no viable QC
# if (master_list$project_details$statTarget_qc_type == "unknown") {
#   # Capture the structure of the lists as strings
#   global_qc_pass_str <- capture.output(str(master_list$project_details$global_qc_pass))
#   plate_qc_passed_str <- capture.output(str(master_list$project_details$qc_passed))
#   
#   # Create a formatted message
#   error_message <- paste0(
#     "STOPPING SCRIPT \n",
#     "There are no viable ltr, pqc, or vltr for statTarget:", master_list$project_details$project_name, ".\n",
#     "Please refer to the details below:\n",
#     "Global QC Assessment: ", paste(global_qc_pass_str, collapse = "\n"), "\n",
#     "Plate QC Assessment: ", paste(plate_qc_passed_str, collapse = "\n")
#   )
#   
#   # Stop execution and print the error message
#   stop(error_message)
# } else{
#   message <- paste("qcCheckeR has set stattarget QC to:", master_list$project_details$statTarget_qc_type)
#   cat(message)
# }


#create data list 
FUNC_list <- list()
FUNC_list$project_dir <- paste0(skylineR_directory,
                                "/data/batch_correction")

#set up project folders for batch correct
#create directories 
if(!dir.exists(paste0(FUNC_list$project_dir,"/", Sys.Date(), "_signal_correction_results"))){
  dir.create(paste0(FUNC_list$project_dir,"/", Sys.Date(), "_signal_correction_results"))
}

setwd(paste0(FUNC_list$project_dir,"/", Sys.Date(), "_signal_correction_results")) 

#set master data for function
FUNC_list$master_data <- bind_rows(master_list$data$concentration$imputed)#apply on concentration data (post-impute)
#set_qc type used for signal drift correction
FUNC_list$master_data[["sample_type"]] <- "sample"
FUNC_list$master_data[["sample_type"]][which(tolower(FUNC_list$master_data[["sample_type_factor"]]) == tolower(master_list$project_details$statTarget_qc_type))] <- "qc"

#flag QCs that have failed because of mis-injection (very low signal (<10 % of median))
temp.rowSums <- FUNC_list$master_data %>%
  filter(sample_type == "qc") %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))%>%
  rowSums()
#find file names
temp.qcFail <- (FUNC_list$master_data  %>%
                  filter(sample_type == "qc") %>%
                  .$sample_name)[which(temp.rowSums < median(temp.rowSums*0.1))]

#reset failed QC injections to "sample" so is not included in statTarget algorithm
FUNC_list$master_data[["sample_type"]][which(FUNC_list$master_data[["sample_name"]] %in% temp.qcFail)] <- "sample"

#exclude failed qcs (see chunk above to id failed QCs)
FUNC_list$master_data[["sample_type"]]

#set metabolite list
FUNC_list$metabolite_list <- master_list$data$concentration$imputed %>%
  bind_rows() %>%
  select(-contains("sample")) %>%
  names()

#### 1.6.a. create the required metadata file (PhenoFile) for statTarget::shiftCor---------------------------------------------------------------
FUNC_list$PhenoFile <- list()
# build PhenoFile file template
FUNC_list$PhenoFile$template <- FUNC_list$master_data %>% 
  select(all_of("sample_name")) %>%
  dplyr::rename(sample = all_of("sample_name")) %>%
  add_column(FUNC_list$master_data %>% 
               select(all_of("sample_name"))) %>%
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_plate_id"))) %>%
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_type"))) %>%
  dplyr::rename(class = all_of("sample_type")) %>% 
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_type"))) %>%
  add_column(FUNC_list$master_data %>%
               select(all_of("sample_run_index")))

#### arrange by run order
FUNC_list$PhenoFile$template <- FUNC_list$PhenoFile$template %>%
  arrange_at("sample_run_index")

#### QC placement
#stat target needs a qc at postion 1 and last position in the run. Default run order takes this into account, but some datasets this is not the case
#in this instance this section of code artificially moves the first and last qc into position. It is completed for each batch.

FUNC_list$PhenoFile$template_qc_order <- NULL
qc_idx <- NULL
for(idx_batch in FUNC_list$PhenoFile$template %>% 
    select(all_of("sample_plate_id")) %>%
    unique() %>%
    as.matrix() %>%
    c()
){
  
  #create a temp tibble batch specific
  loop_temp_data <- FUNC_list$PhenoFile$template %>%
    filter(!!as.symbol("sample_plate_id") == idx_batch)
  
  #ensure a sample_type "qc" is "first" and "last" in the worklist order. Required for statTarget::shiftCor
  loop_qc_idx <- which(loop_temp_data %>% 
                         select(all_of("sample_type")) 
                       == "qc")
  # browser()
  #if qc is not run before the samples - artificially move first qc to run order position 1. This is required for statTarget
  if(loop_qc_idx[1] > 1){
    loop_temp_data <- loop_temp_data %>%
      slice(loop_qc_idx[1],1:nrow(loop_temp_data)) %>%
      slice(-(loop_qc_idx[1]+1))
  }
  
  #create last qc
  if(loop_qc_idx[length(loop_qc_idx)] < nrow(loop_temp_data)){
    loop_temp_data <- loop_temp_data %>%
      slice(1:nrow(loop_temp_data), loop_qc_idx[length(loop_qc_idx)]) %>%
      slice(-loop_qc_idx[length(loop_qc_idx)])
  }
  
  #create total qc_idx for use later
  qc_idx <- c(qc_idx,
              loop_qc_idx)
  
  FUNC_list$PhenoFile$template_qc_order <- bind_rows(FUNC_list$PhenoFile$template_qc_order,
                                                     loop_temp_data)
  
}

#set sample column for statTarget requires "QC" in QC rows, and sample name in sample rows
FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                     select(all_of("sample_type")) == "qc")] <- paste0("QC", rep(1:length(qc_idx)))
FUNC_list$PhenoFile$template_qc_order$sample[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                     select(all_of("sample_type")) == "sample")] <- paste0("sample", 
                                                                                                           rep(1:(nrow(FUNC_list$PhenoFile$template_qc_order)-length(qc_idx))))
#set NA for class column in rows that are NA
FUNC_list$PhenoFile$template_qc_order$class[which(FUNC_list$PhenoFile$template_qc_order %>% 
                                                    select(all_of("sample_type")) == "qc")] <- NA

#rename column header for statTarget template
FUNC_list$PhenoFile$template_sample_id <- FUNC_list$PhenoFile$template_qc_order %>% 
  dplyr::rename(sample_id = all_of("sample_name"),
         batch = all_of("sample_plate_id"),
         order = all_of("sample_run_index")) %>%
  select(sample, batch, class, order, sample_id)

#confirm order columnn is continuous
FUNC_list$PhenoFile$template_sample_id$order <- c(1:nrow(FUNC_list$PhenoFile$template_sample_id))

#set batch/plate - numeric value starting at 1 - max number of plates/batch
temp_batch <- 1
for(idx_batch_set in unique(FUNC_list$PhenoFile$template_sample_id$batch)){
  FUNC_list$PhenoFile$template_sample_id$batch[which(FUNC_list$PhenoFile$template_sample_id$batch == idx_batch_set)] <- temp_batch
  temp_batch <- temp_batch + 1
}


FUNC_list$PhenoFile$template_sample_id$batch <- FUNC_list$PhenoFile$template_sample_id$batch %>%
  as.numeric()

#final Phenofile
FUNC_list$PhenoFile$PhenoFileOut <- FUNC_list$PhenoFile$template_sample_id %>%
  select(-sample_id)

# write out as csv (requirement for statTarget::shiftCor)
write_csv(x = FUNC_list$PhenoFile$PhenoFileOut,
          file = paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep="")
)

#### 1.6.b. create data for statTarget::shiftCor  -----------------------------------
FUNC_list$ProfileFile <- list()

#must have samples in columns and metabolites in rows
FUNC_list$ProfileFile$template  <- FUNC_list$master_data %>%
  select(all_of("sample_name"),
         all_of(FUNC_list$metabolite_list)) %>%
  dplyr::rename(sample_id = !!"sample_name")

#match run order to PhenoFile
FUNC_list$ProfileFile$template_qc_order <- FUNC_list$PhenoFile$template_sample_id %>%
  select(sample, sample_id) %>%
  left_join(FUNC_list$ProfileFile$template, by = "sample_id") %>%
  select(-sample_id)

#transpose tibble for statTarget
FUNC_list$ProfileFile$ProfileFile <- as_tibble(
  cbind(nms = names(FUNC_list$ProfileFile$template_qc_order), 
        t(FUNC_list$ProfileFile$template_qc_order))
) %>%
  setNames(.[1,]) %>%
  dplyr::rename(name = sample) %>%
  filter(name != "sample") %>%
  mutate(across(!contains("name", ignore.case = FALSE), as.numeric))

#create a metabolite list and create metabolite code
FUNC_list$ProfileFile$metabolite_list <- FUNC_list$ProfileFile$ProfileFile %>%
  select(name) %>%
  select(!contains("SIL"))%>%
  add_column(metabolite_code = paste0("M", rep(1:nrow(FUNC_list$ProfileFile$ProfileFile))))
  

#add metabolite code to data
FUNC_list$ProfileFile$ProfileFile <- left_join(
  FUNC_list$ProfileFile$metabolite_list,
  FUNC_list$ProfileFile$ProfileFile,
  by = "name") %>%
  select(-name) %>%
  dplyr::rename(name = metabolite_code)

# write out as csv (requirement for statTarget::shiftCor)
write_csv(x = FUNC_list$ProfileFile$ProfileFile, 
          file = paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results/ProfileFile.csv")
)

#script files
samPeno <- paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/PhenoFile.csv", sep="")
samFile <- paste(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results",  "/ProfileFile.csv", sep="")

#run statTarget
statTarget::shiftCor(samPeno = samPeno,
                     samFile =  samFile,
                     Frule = 0,
                     ntree = 500,
                     MLmethod = 'QCRFSC',
                     imputeM = "minHalf",
                     plot = FALSE,
                     coCV = 10000
)

#read back in csv

FUNC_list$corrected_data$data <- read_csv(
  paste0(FUNC_list$project_dir, "/", Sys.Date(), "_signal_correction_results", "/statTarget/shiftCor/After_shiftCor/shift_all_cor.csv"),
  show_col_types = FALSE)

#different versions of r/statTarget appear to produce shift_all_cor in different orientations. Some have metabolites in columns some in rows. Unsure as to why, I did not find out which package inside statTarget was producing this outcome.
#the next phase fixes this by performing a check as to the orientation of the csv

#older packages (metabolites in rows)
if("sample1" %in% colnames(FUNC_list$corrected_data$data)){
  FUNC_list$corrected_data$data  <- FUNC_list$corrected_data$data  %>%
    filter(sample != "class") %>%
    dplyr::rename(name = sample) %>%
    mutate(across(!contains("name", ignore.case = FALSE), as.numeric))
}

#updated packages (metabolites in columns)
if("M1" %in% colnames(FUNC_list$corrected_data$data)){
  FUNC_list$corrected_data$data  <- FUNC_list$corrected_data$data  %>%
    t() %>% data.frame() %>%
    rownames_to_column() %>% 
    as_tibble() %>%
    setNames(.[1,]) %>%
    filter(sample != "class" & sample != "sample" ) %>%
    dplyr::rename(name = sample) %>%
    mutate(across(!contains("name", ignore.case = FALSE), as.numeric))
}

#recombine with sample filenames and lipid names
FUNC_list$corrected_data$data_transposed <- right_join(
  FUNC_list$ProfileFile$metabolite_list %>% dplyr::rename(lipid = metabolite_code),
  FUNC_list$corrected_data$data %>% dplyr::rename(lipid = name),
  by = "lipid"
) %>%
  select(-lipid) %>%
  as.matrix() %>%
  t() %>% data.frame() %>%
  rownames_to_column() %>% 
  as_tibble() %>%
  setNames(.[1,]) %>%
  filter(name!="name") %>%
  mutate(across(!contains("name", ignore.case = FALSE), as.numeric)) %>%
  dplyr::rename(sample = name) %>%
  left_join(x = FUNC_list$PhenoFile$template_sample_id,
            y = .,
            by = "sample") %>%
  dplyr::rename(!!"sample_name" := sample_id) %>%
  left_join(
    x = FUNC_list$master_data %>%
      select(contains("sample")),
    y = .,
    by = "sample_name"
  ) %>%
  select(-all_of(c("sample", "batch", "class", "order")))

#### 1.6.c. post-statTarget peak area mean adjustment ------------------------------------
#because the statTarget correction changes the output signal area of the lipids, this next section re-scales the values based on the change (ratio) between pre and post corrected signal mean in the QCs
#step one - get mean value for each metabolite in the QC samples - pre-single drift corrected data 
FUNC_list$corrected_data$qc_means <- FUNC_list$master_data %>%
  filter(!!as.symbol("sample_type") == "qc") %>%
  select(-contains("sample")) %>%
  colMeans() %>%
  as_tibble() %>%
  dplyr::rename(original_mean = value) %>%
  add_column(metabolite = FUNC_list$master_data %>%
               select(-contains("sample")) %>%
               names(), 
             .before = "original_mean") %>%
  #step two - get mean value for each metabolite in the QC samples - post-single drift corrected data 
  left_join(.,
            FUNC_list$corrected_data$data_transposed %>%
              filter(!!as.symbol("sample_type") == "qc") %>%
              select(-contains("sample")) %>%
              colMeans() %>%
              data.frame %>%
              rownames_to_column() %>%
              as_tibble() %>%
              setNames(c("metabolite", "corrected_mean")),
            by = "metabolite") %>%
  #step three - create ratio factor for concentration adjustment
  add_column(correction_ratio = .$corrected_mean/.$original_mean)

#step 4 - adjust data concentrations
FUNC_list$corrected_data$data_qc_mean_adjusted <- FUNC_list$corrected_data$data_transposed
#run loop
for(idx_metabolite in FUNC_list$corrected_data$qc_means$metabolite){
  FUNC_list$corrected_data$data_qc_mean_adjusted[[idx_metabolite]] <- FUNC_list$corrected_data$data_qc_mean_adjusted[[idx_metabolite]]/
    FUNC_list$corrected_data$qc_means[["correction_ratio"]][which(FUNC_list$corrected_data$qc_means[["metabolite"]]==idx_metabolite)]
}

#return QC type to ltr data
FUNC_DATA_qc_type <- unique(bind_rows(master_list$data$concentration$imputed)$sample_type_factor[which(bind_rows(master_list$data$concentration$imputed)$sample_type =="qc")]) %>% as.character()
FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type<- "sample"
FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type[which(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_type_factor == FUNC_DATA_qc_type)] <- "qc"

#list for storing signal drift corrected data (per project)
master_list$data$peakArea$statTargetProcessed <- list()
master_list$data$concentration$statTargetProcessed <- list()
#loop to back into plate elements of list
for(idx_batch in unique(FUNC_list$corrected_data$data_qc_mean_adjusted$sample_plate_id)){
  master_list$data$peakArea$statTargetProcessed[[idx_batch]] <- FUNC_list$corrected_data$data_qc_mean_adjusted %>%
    filter(sample_plate_id == idx_batch) 
  master_list$data$concentration$statTargetProcessed[[idx_batch]] <- FUNC_list$corrected_data$data_qc_mean_adjusted %>%
    filter(sample_plate_id == idx_batch) 
  #set dataSource
  master_list$data$peakArea$statTargetProcessed[[idx_batch]]$sample_data_source <- "concentration.statTarget"
}

#reset wd to project dir
setwd(master_list$project_details$project_dir)

#remove extra files
rm(list = c(ls()[which(ls() != "master_list")]))

# Update script_log
master_list$environment$user_functions$update_script_log$value(master_list, "data_preparation", "project_setup", "data_filtering")

#.-----------

# PHASE 2: DATA FILTERING ------------------
# mising value data filtering is performed on peakArea data from the skyLine export.
# First the filtering is performed per sample to identify failed samples (e.g. injection, extraction, preparation errors, or if sample is missing from well) it will return a high % of missing values
# Once failed samples have been identified - the filtering then identifies lipids that have >50% missing values
# note: missing also refers to <limit of detection [<LOD]. This refers to instances of peak areas that are <5000 counts, as skyline will sometimes integrate noise giving a small value.

#Dynamically set QC for filtering 
#Check logic with luke ----
#dynamic set qc-type for filtering
master_list$project_details$qc_type  <- if (master_list$project_details$global_qc_pass$pqc == "pass") {
  "pqc"
} else if (master_list$project_details$global_qc_pass$ltr == "pass") {
  "ltr"
} else {
  "unknown"
}

#Stop script and print error if there are no viable QC
if (master_list$project_details$qc_type  == "unknown") {
  # Capture the structure of the lists as strings
  global_qc_pass_str <- capture.output(str(master_list$project_details$global_qc_pass))
  plate_qc_passed_str <- capture.output(str(master_list$project_details$qc_passed))

  # Create a formatted message
  error_message <- paste0(
    "STOPPING SCRIPT \n",
    "There are no viable ltr, pqc, or vltr for RSD filtering:", master_list$project_details$project_name, ".\n",
    "Please refer to the details below:\n",
    "Global QC Assessment: ", paste(global_qc_pass_str, collapse = "\n"), "\n",
    "Plate QC Assessment: ", paste(plate_qc_passed_str, collapse = "\n")
  )

  # Stop execution and print the error message
  stop(error_message)
} else{
  message <- paste("qcCheckeR has set filtering QC to:", master_list$project_details$qc_type)
  cat(message)
}

master_list$filters <- list()

## 2.1. sample filter flags [missing value and summed signal] -------
master_list$project_details$mv_sample_threshold <- "50" %>% as.numeric()
#For loop generates sample filter flags by plate
for (idx_batch in names(master_list$data$peakArea$sorted)){
master_list$filters$samples.missingValues[[idx_batch]] <- list()
# complete on raw uncorrected peakArea data
master_list$filters$samples.missingValues[[idx_batch]] <- master_list$data$peakArea$sorted[[idx_batch]] %>%
  bind_rows() %>%
  select(
    sample_run_index,
    sample_name,
    sample_batch,
    sample_plate_id,
    sample_type_factor
  )

# Check the version of the plate
template_version <- master_list$templates$`Plate SIL version`[[idx_batch]]
#generate summed target lipid intensity
master_list$filters$samples.missingValues[[idx_batch]][["summed.lipid.signal"]] <- bind_rows(master_list$data$peakArea$sorted[[idx_batch]]) %>%
  select(!contains("sample")) %>%
  select(!contains("SIL")) %>%
  rowSums(na.rm = TRUE)

#generate summed SIL lipid intensity
master_list$filters$samples.missingValues[[idx_batch]][["summed.SIL.Int.Std.signal"]] <- bind_rows(master_list$data$peakArea$sorted[[idx_batch]]) %>%
  select(contains("SIL")) %>%
  rowSums(na.rm = TRUE)

#zero values [samples.target.lipids] (values with a peakArea of <5000 are considered <LOD and are noise)
master_list$filters$samples.missingValues[[idx_batch]][["missing.lipid[<LOD.peakArea<5000]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix()) <5000, na.rm =T)

#zero values [samples.SIL.Int.Stds]
master_list$filters$samples.missingValues[[idx_batch]][["missing.SIL.Int.Std[<LOD.peakArea<5000]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix()) <5000, na.rm =T)

#na values [samples.target.lipids]
master_list$filters$samples.missingValues[[idx_batch]][["naValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.na()), na.rm =T)

#na values [samples.SIL.Int.Stds]
master_list$filters$samples.missingValues[[idx_batch]][["naValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.na()), na.rm =T)

#nan values [samples.target.lipids]
master_list$filters$samples.missingValues[[idx_batch]][["nanValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.nan()), na.rm =T)

#nan values [samples.SIL.Int.Stds]
master_list$filters$samples.missingValues[[idx_batch]][["nanValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.nan()), na.rm =T)

#inf values [samples.target.lipids]
master_list$filters$samples.missingValues[[idx_batch]][["infValues[lipidTarget]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(!contains("SIL")) %>%
         as.matrix() %>%
         is.infinite()), na.rm =T)

#inf values [samples.SIL.Int.Stds]
master_list$filters$samples.missingValues[[idx_batch]][["infValues[SIL.Int.Stds]"]] <- rowSums(
  x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
         bind_rows() %>%
         select(!contains("sample")) %>%
         select(contains("SIL")) %>%
         as.matrix() %>% 
         is.infinite()), na.rm =T)

#total missing values [samples.target.lipids]
master_list$filters$samples.missingValues[[idx_batch]][["totalMissingValues[lipidTarget]"]] <- master_list$filters$samples.missingValues[[idx_batch]] %>%
  select(-contains("sample_")) %>%
  select(-contains("SIL")) %>%
  select(-contains("summed")) %>%
  as.matrix() %>%
  rowSums()

#total missing values[samples.SIL.Int.stds]
master_list$filters$samples.missingValues[[idx_batch]][["totalMissingValues[SIL.Int.Stds]"]] <- master_list$filters$samples.missingValues[[idx_batch]] %>%
  select(-contains("sample_")) %>%
  select(contains("SIL")) %>%
  select(-contains("summed")) %>%
  as.matrix() %>%
  rowSums()


#sample flag?
master_list$filters$samples.missingValues[[idx_batch]]$sample.lipid.intensity.flag <- 0
#flag samples with a summed target intensity of <20% of median indicating an outlier low signal from prep or sample. e.g. sample has not been added to well correctly.
master_list$filters$samples.missingValues[[idx_batch]]$sample.lipid.intensity.flag[
which(master_list$filters$samples.missingValues[[idx_batch]]$summed.lipid.signal < (median(master_list$filters$samples.missingValues[[idx_batch]]$summed.lipid.signal)*0.20))
] <- 1

#low SIL.Int.Std flag indicating incorrect addition of internal standards
master_list$filters$samples.missingValues[[idx_batch]]$sample.SIL.Int.Std.intensity.flag <- 0
#flag samples with a summed SIL.Int.Std intensity of <33% of median indicating an outlier low signal from prep or sample. e.g. SIL have not been added correctly.
#more stringent for IS as IS should be more consistent across the analysis
master_list$filters$samples.missingValues[[idx_batch]]$sample.SIL.Int.Std.intensity.flag[
  which(master_list$filters$samples.missingValues[[idx_batch]]$summed.SIL.Int.Std.signal < (median(master_list$filters$samples.missingValues[[idx_batch]]$summed.SIL.Int.Std.signal)*0.33))
] <- 1

#missing value flag
master_list$filters$samples.missingValues[[idx_batch]]$sample.missing.value.flag <- 0
#remove where missing data >mv_sample_threshold [default = 50%] total lipidTargets
master_list$filters$samples.missingValues[[idx_batch]]$sample.missing.value.flag[which(
  master_list$filters$samples.missingValues[[idx_batch]]$`totalMissingValues[lipidTarget]` > ((
    bind_rows(master_list$data$peakArea$sorted[[idx_batch]]) %>%
      select(-contains("sample")) %>%
      select(-contains("SIL")) %>%
      ncol())*(master_list$project_details$mv_sample_threshold/100))
)] <- 1

#remove where missing (<LOD) data > 33% total SIL.int.Stds (all IS should always be present so more stringent filter)
master_list$filters$samples.missingValues[[idx_batch]]$sample.missing.value.flag[which(
  master_list$filters$samples.missingValues[[idx_batch]]$`totalMissingValues[SIL.Int.Stds]` > ((bind_rows(
    master_list$data$peakArea$sorted[[idx_batch]]) %>%
      select(-contains("sample")) %>%
      select(contains("SIL")) %>%
      ncol())*(0.33))
)] <- 1

#create a overall sample flag
master_list$filters$samples.missingValues[[idx_batch]]$sample.flag <- 0
master_list$filters$samples.missingValues[[idx_batch]]$sample.flag[
  which(
    (master_list$filters$samples.missingValues[[idx_batch]] %>% 
       select(contains("flag")) %>% rowSums(na.rm = T)) > 0
  )
] <- 1

#create failed sample list
master_list$filters$failed_samples[[idx_batch]] <- filter(master_list$filters$samples.missingValues[[idx_batch]], sample.flag==1)[["sample_name"]]
}

#Combine samples.missing values from plates 
combined_missing_values <- do.call(rbind, master_list$filters$samples.missingValues)
master_list$filters$samples.missingValues <- combined_missing_values
#Combine failed samples
combined_failed_samples <- do.call(rbind, master_list$filters$failed_samples)
master_list$filters$failed_samples <- combined_failed_samples
rm(combined_missing_values,combined_failed_samples)

## 2.2. sil.IntStd filter [missing value] -----
master_list$filters$sil.intStd.missingValues <- list()
# Initialise allplates as an empty data frame with the desired column names
master_list$filters$sil.intStd.missingValues$allPlates.totalMissingValues <- data.frame(
  lipid = character(),
  template_version = character(),
  plateID = character(),
  peakArea_5000_LOD = numeric(),
  naValues = numeric(),
  nanValues = numeric(),
  infValues = numeric(),
  totalMissingValues = numeric(),
  flag_SIL_intStd_Plate = numeric(),
  stringsAsFactors = FALSE
)

# Loop for every batch
for(idx_batch in names(master_list$data$peakArea$sorted)){
  # Create SIL list
  master_list$filters$sil.intStd.missingValues[[idx_batch]] <- tibble(
    lipid = master_list$data$peakArea$sorted[[idx_batch]] %>%
      bind_rows() %>%
      select(contains("SIL")) %>%
      names()
  )
  
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["peakArea<5000[LOD]"]] <- colSums(
    x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
           filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
           select(contains("SIL")) %>%
           as.matrix()) < 5000, 
    na.rm = TRUE
  )
  
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["naValues"]] <- colSums(
    x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
           filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
           select(contains("SIL")) %>%
           as.matrix()) %>%
      is.na(), na.rm = TRUE
  )
  
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["nanValues"]] <- colSums(
    x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
           filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
           select(contains("SIL")) %>%
           as.matrix()) %>%
      is.nan(), na.rm = TRUE
  )
  
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["infValues"]] <- colSums(
    x = (master_list$data$peakArea$sorted[[idx_batch]] %>%
           filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
           select(contains("SIL")) %>%
           as.matrix()) %>%
      is.infinite(), na.rm = TRUE
  )
  
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["totalMissingValues"]] <- rowSums(
    x = (master_list$filters$sil.intStd.missingValues[[idx_batch]] %>%
           select(contains(idx_batch)) %>%
           as.matrix()),
    na.rm = TRUE
  )
  
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["flag.SIL.intStd[Plate]"]] <- 0
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["flag.SIL.intStd[Plate]"]][
    which(master_list$filters$sil.intStd.missingValues[[idx_batch]][["totalMissingValues"]] > 
            ((master_list$data$peakArea$sorted[[idx_batch]] %>%
                filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
                nrow) * 0.05)
    )] <- 1
  
  
  # Check the version of the plate and add to SIL list
  template_version <- master_list$templates$`Plate SIL version`[[idx_batch]]
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["template_version"]] <- template_version
  
  #Add plateID 
  master_list$filters$sil.intStd.missingValues[[idx_batch]][["plateID"]] <- idx_batch
  
  # Bind results
  master_list$filters$sil.intStd.missingValues$allPlates.totalMissingValues <- rbind(
    master_list$filters$sil.intStd.missingValues$allPlates.totalMissingValues, 
    master_list$filters$sil.intStd.missingValues[[idx_batch]]
  )
}

# Does SIL.int.std fail across all plates?
# Initialise an empty list to store results
results_list <- list()

# For loop to handle each lipid method version
for (version in unique(master_list$filters$sil.intStd.missingValues$allPlates.totalMissingValues$template_version)) {
  
  # Filter data for the current version
  data <- master_list$filters$sil.intStd.missingValues$allPlates.totalMissingValues %>% 
    filter(template_version == version)
  
  # Gather index of plates from the current version
  plate_idx <- data %>%
    distinct(plateID)
  
  # Calculate the number of samples that passed the mv filter
  samples_data <- master_list$data$peakArea$sorted %>%
    bind_rows() %>%  # Ensure the data is in a data frame format
    filter(sample_plate_id %in% plate_idx$plateID) %>%
    filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
    nrow()
  
  # Add a flag if SIL IS is missing in > 5% of samples
  flag <- ifelse(data$totalMissingValues > (samples_data * 0.05), 1, 0)
  
  # Store the results in the list
  results_list[[version]] <- list(
    version = version,
    plate_idx = plate_idx,
    samples_data = samples_data,
    flag = flag
  )
}

# Create a failed internal standard list
failed_sil_intStds <- lapply(results_list, function(res) {
  if (any(res$flag == 1)) {
    return(master_list$filters$sil.intStd.missingValues$allPlates.totalMissingValues %>%
             filter(template_version == res$version) %>%
             select(lipid))
  }
  return(NULL)
}) %>% bind_rows()


# convert to tibble object
master_list[["filters"]][["sil.intStd.missingValues"]] <- lapply(master_list[["filters"]][["sil.intStd.missingValues"]], as_tibble)


# Assign the failed internal standard list to master_list
master_list$filters$failed_sil.intStds <- failed_sil_intStds

## 2.3. lipid filter [missing value] --------
lipid_results <- list()

# Store batches (plates)
batches <- unique(names(master_list$data$peakArea$sorted))

for (idx_batch in batches) {
  # Create lipid list for the current batch
  lipid_list <- tibble(
    lipid = master_list$data$peakArea$sorted[[idx_batch]] %>%
      select(!contains("sample") & !contains("SIL")) %>%
      names()
  )
  
  # Initialize the flag
  lipid_list[["silFilter.flag.Lipid"]] <- 0
  
  # Retrieve the SIL version for the current batch
  SIL_version <- master_list[["templates"]][["Plate SIL version"]][[idx_batch]]
  
  # Find matching indices
  matching_indices <- which(lipid_list$lipid %in% 
                              filter(master_list$templates[[SIL_version]]$SIL_guide, 
                                     note %in% master_list$filters$failed_sil.intStds)[["precursor_name"]])
  
  # Assign the value 1 to the silFilter.flag.Lipid column at the matching indices
  lipid_list[["silFilter.flag.Lipid"]][matching_indices] <- 1
  
  # Filter and select data
  filtered_data <- master_list$data$peakArea$sorted[[idx_batch]] %>%
    filter(sample_name %in% filter(master_list$filters$samples.missingValues, sample.flag == 0)[["sample_name"]]) %>%
    select(!contains("sample") & !contains("SIL")) %>%
    as.matrix()
  
  # Peak area < 5000 (LOD)
  lipid_list[["peakArea<5000[LOD]"]] <- colSums(filtered_data < 5000, na.rm = TRUE)
  
  # NA values
  lipid_list[["naValues"]] <- colSums(is.na(filtered_data), na.rm = TRUE)
  
  # NaN values
  lipid_list[["nanValues"]] <- colSums(is.nan(filtered_data), na.rm = TRUE)
  
  # Infinite values
  lipid_list[["infValues"]] <- colSums(is.infinite(filtered_data), na.rm = TRUE)
  
  # Total missing values for plate
  lipid_list[["totalMissingValues"]] <- rowSums(
    lipid_list %>%
      select(contains(idx_batch)) %>%
      as.matrix(),
    na.rm = TRUE
  )
  
  # Only keep lipids with <50% missing values
  lipid_list[["flag.Lipid[Plate]"]] <- 0
  lipid_list[["flag.Lipid[Plate]"]][which(
    lipid_list[["totalMissingValues"]] > 
      (nrow(filtered_data) * 0.5)
  )] <- 1
  
  # Store the results for the current batch
  lipid_results[[idx_batch]] <- lipid_list
}

# Combine the results for all batches
master_list$filters$lipid.missingValues <- bind_rows(lipid_results, .id = "batch")

# Does lipid fail across all plates?
# Add fail.filter column if lipid failed for a plate or for all plates
master_list$filters$lipid.missingValues[["PROJECT.flag.Lipid"]] <- 0
# Loop through each unique lipid
for (lipid in unique(master_list$filters$lipid.missingValues$lipid)) {
  # Subset the data for the current lipid
  lipid_data <- master_list$filters$lipid.missingValues %>%
    filter(lipid == !!lipid) %>%
    select(contains("flag.Lipid")) %>%
    as.matrix()
  
  # Check if the lipid fails in any plate
  if (all(rowSums(lipid_data, na.rm = TRUE) >= 1)) {
    master_list$filters$lipid.missingValues[["PROJECT.flag.Lipid"]][master_list$filters$lipid.missingValues$lipid == lipid] <- 1
  }
}


# Create a failed lipid list
master_list$filters$failed_lipids <- filter(master_list$filters$lipid.missingValues, PROJECT.flag.Lipid == 1)[["lipid"]]


## 2.4. RSD filter ------------
# following mising value filtering, replicate QC samples are evaluated to see the precision of measurement (represented by % relative standard deviation)
# features per plate with a %RSD >30% are flagged for removal

### 2.4.a. peakArea ---------
master_list$filters$rsd <- tibble()

#per batch/plate
for(idx_batch in names(master_list$data$peakArea$imputed)){
  loopData <- master_list$data$peakArea$imputed[[idx_batch]] %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    filter(sample_type %in% c("qc")) %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
  
#loopMAD <- (apply(X = loopData, MARGIN = 2, FUN = mad) / apply(X = loopData, MARGIN = 2, FUN = median)) * 100 #Median absolute deviation

  
  #export tibble
  master_list$filters$rsd <- bind_rows(
    master_list$filters$rsd,
    rbind(c("peakArea",idx_batch, loopRSD)) %>% 
      as_tibble() %>%
      mutate(across(names(loopRSD), as.numeric))
  )
}

#per project
loopData <- master_list$data$peakArea$imputed %>%
  bind_rows() %>%
  filter(!sample_name %in% master_list$filters$failed_samples) %>%
  filter(sample_type %in% c("qc")) %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))
loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

#export tibble
master_list$filters$rsd <- bind_rows(
  master_list$filters$rsd,
  rbind(c("peakArea", "allBatches", loopRSD)) %>% 
    as_tibble() %>%
    mutate(across(names(loopRSD), as.numeric))
)

### 2.4.b. concentration ---------
#per batch/plate
for(idx_batch in names(master_list$data$concentration$imputed)){
  loopData <- master_list$data$concentration$imputed[[idx_batch]] %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    filter(sample_type %in% c("qc")) %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
  
  #export tibble
  master_list$filters$rsd <- bind_rows(
    master_list$filters$rsd,
    rbind(c("concentration",idx_batch, loopRSD)) %>% 
      as_tibble() %>%
      mutate(across(names(loopRSD), as.numeric))
  )
}

#per project
loopData <- master_list$data$concentration$imputed %>%
  bind_rows() %>%
  filter(!sample_name %in% master_list$filters$failed_samples) %>%
  filter(sample_type %in% c("qc")) %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))
loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

#export tibble
master_list$filters$rsd <- bind_rows(
  master_list$filters$rsd,
  rbind(c("concentration","allBatches", loopRSD)) %>% 
    as_tibble() %>%
    mutate(across(names(loopRSD), as.numeric))
)

### 2.4.c. statTarget concentration ---------
#per batch/plate
for(idx_batch in names(master_list$data$concentration$statTargetProcessed)){
  loopData <- master_list$data$concentration$statTargetProcessed[[idx_batch]] %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    filter(sample_type %in% c("qc")) %>%
    select(!contains("sample")) %>%
    select(!contains("SIL"))
  loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100
  
  #export tibble
  master_list$filters$rsd <- bind_rows(
    master_list$filters$rsd,
    rbind(c("concentration[statTarget]",idx_batch, loopRSD)) %>% 
      as_tibble() %>%
      mutate(across(names(loopRSD), as.numeric))
  )
}

#per project
loopData <- master_list$data$concentration$statTargetProcessed %>%
  bind_rows() %>%
  filter(!sample_name %in% master_list$filters$failed_samples) %>%
  filter(sample_type %in% c("qc")) %>%
  select(!contains("sample")) %>%
  select(!contains("SIL"))
loopRSD <- (apply(X = loopData, MARGIN = 2, FUN = sd)/apply(X = loopData, MARGIN = 2, FUN = mean))*100

#export tibble
master_list$filters$rsd <- bind_rows(
  master_list$filters$rsd,
  rbind(c("concentration[statTarget]","allBatches", loopRSD)) %>% 
    as_tibble() %>%
    mutate(across(names(loopRSD), as.numeric))
)
#tidy rsd table
master_list$filters$rsd <- master_list$filters$rsd %>%
  dplyr::rename(dataSource = V1,
                dataBatch = V2) %>%
  mutate(across(!contains("data"), as.numeric)) %>%
  mutate(across(!contains("data"), round, 2))

# Update script_log
master_list$environment$user_functions$update_script_log$value(master_list, "data_filtering", "data_preparation", "summary_report")

#. ------------------------------------------------------------------------------------------------------------------------------------------------
# PHASE 3. SUMMARY REPORT ------------------------ 
## 3.1. dataSetSummary -----
metric =  c("Cohorts","MatrixType", "totalSamples", "studySamples", "ltrSamples", "vltrSamples", "sltrSamples", "pqcSamples", 
            "lipidTargets", "SIL.version","SIL.IntStds", 
            "missingValueFilterFlags[samples]", "missingValueFilterFlags[SIL.IS]", "missingValueFilterFlags[lipidTargets]", 
            "rsd<30%[peakArea]", "rsd<20%[peakArea]", "rsd<10%[peakArea]",
            "rsd<30%[concentration]", "rsd<20%[concentration]", "rsd<10%[concentration]",
            "rsd<30%[concentration.statTarget]", "rsd<20%[concentration.statTarget]", "rsd<10%[concentration.statTarget]")

# create tibble
master_list$summary_tables <- NULL
master_list$summary_tables$projectOverview <- tibble(metric = metric)

#fill info for each plate/batch
for(idx_batch in names(master_list$data$peakArea$sorted)){
  
  master_list$summary_tables$projectOverview <- left_join(
    master_list$summary_tables$projectOverview,
    rbind(
      c("Cohorts", paste0(unique(master_list$data$peakArea$sorted[[idx_batch]]$sample_batch), collapse = ",")),
      c("MatrixType", unique(master_list$data$peakArea$sorted[[idx_batch]]$sample_matrix)),
      c("totalSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]])),
      c("studySamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "sample"))),
      c("ltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "ltr"))),
      c("vltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "vltr"))),
      c("sltrSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "sltr"))),
      c("pqcSamples", nrow(master_list$data$peakArea$sorted[[idx_batch]] %>% filter(sample_type_factor == "pqc"))),
      c("lipidTargets", ncol(master_list$data$peakArea$sorted[[idx_batch]] %>% select(-contains("sample"), - contains("SIL")))),
      c("SIL.version", paste0(unique(master_list$templates$`Plate SIL version`[[idx_batch]]), collapse = ",")),
      c("SIL.IntStds", ncol(master_list$data$peakArea$sorted[[idx_batch]] %>% select(contains("SIL")))),
      c("missingValueFilterFlags[samples]", master_list$filters$samples.missingValues %>% filter(sample_plate_id == idx_batch & sample.flag == 1) %>% nrow()),
      c("missingValueFilterFlags[SIL.IS]", which(as.matrix(master_list[["filters"]][["sil.intStd.missingValues"]][[idx_batch]][["flag.SIL.intStd[Plate]"]])[,1] == 1) %>% length()), # Broken
      c("missingValueFilterFlags[lipidTargets]", which(as.matrix(master_list$filters$lipid.missingValues %>% filter(batch == idx_batch) %>% select(contains("flag")))[,1] == 1) %>% length()),#Broken
      c("rsd<30%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
      c("rsd<20%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
      c("rsd<10%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
      c("rsd<30%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
      c("rsd<20%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data"))%>% select(!any_of(master_list$filters$failed_lipids))  <20) %>% length()),
      c("rsd<10%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
      c("rsd<30%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids))  <30) %>% length()),
      c("rsd<20%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
      c("rsd<10%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == idx_batch & dataSource == "concentration[statTarget]") %>% select(!contains("data"))%>% select(!any_of(master_list$filters$failed_lipids))  <10) %>% length())
    ) %>%
      as_tibble() %>%
      dplyr::rename(metric = V1,
             !! paste0(idx_batch) := V2),
    by = "metric"
  )
  #change column to numeric
  #master_list$summary_tables$projectOverview[[paste0(idx_batch)]] <- as.numeric(master_list$summary_tables$projectOverview[[paste0(idx_batch)]])
}
  
 ## 3.2. interPlate ----
master_list$summary_tables$projectOverview <- left_join(
  master_list$summary_tables$projectOverview,
  rbind(
    c("Cohorts", length(unique(master_list$filters$samples.missingValues$sample_batch))),
    c("MatrixType", paste0(bind_rows(master_list$data$peakArea$sorted)%>% select(contains("sample_matrix")) %>% unique(), collapse = ',')),
    c("totalSamples", nrow(bind_rows(master_list$data$peakArea$sorted))),
    c("studySamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sample"))),
    c("ltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "ltr"))),
    c("vltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "vltr"))),
    c("sltrSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sltr"))),
    c("pqcSamples", nrow(bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "pqc"))),
    c("lipidTargets", ncol(bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("sample"), - contains("SIL")))),
    c("SIL.version", paste0((unique(master_list$templates$`Plate SIL version`)),collapse = ",")),
    c("SIL.IntStds", ncol(bind_rows(master_list$data$peakArea$sorted) %>% select(contains("SIL")))),
    c("missingValueFilterFlags[samples]", master_list$filters$samples.missingValues %>% filter(sample.flag == 1) %>% nrow()),
    c("missingValueFilterFlags[SIL.IS]", length(master_list$filters$failed_sil.intStds)),
    c("missingValueFilterFlags[lipidTargets]", which(as.matrix(master_list$filters$lipid.missingValues$PROJECT.flag.Lipid)[,1] == 1) %>% length()),
    c("rsd<30%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
    c("rsd<20%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
    c("rsd<10%[peakArea]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "peakArea") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
    c("rsd<30%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
    c("rsd<20%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20) %>% length()),
    c("rsd<10%[concentration]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length()),
    c("rsd<30%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <30) %>% length()),
    c("rsd<20%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <20)  %>% length()),
    c("rsd<10%[concentration.statTarget]", which(master_list$filters$rsd %>% filter(dataBatch == "allBatches" & dataSource == "concentration[statTarget]") %>% select(!contains("data")) %>% select(!any_of(master_list$filters$failed_lipids)) <10) %>% length())
  ) %>%
    as_tibble() %>%
    dplyr::rename(metric = V1,
           !! paste0("allBatches") := V2),
  by = "metric"
)

#master_list$summary_tables$projectOverview$allBatches <- master_list$summary_tables$projectOverview$allBatches %>% as.numeric()

# Update script_log
master_list$environment$user_functions$update_script_log$value(master_list, "summary_report", "data_filtering", "plot_generation")

# . ------------------------------------------------------------------------------------------------------------------------------  
# PHASE 4: PLOTS ---------------------

## 4.1. Set plot color/fill/shape/size --------------------------------------
#set plots colours
master_list$project_details$plot_fill <-  c("sample" = "white",
                                            "ltr" = "steelblue2",
                                            "pqc" = "darkorange",
                                            "sltr" = "purple1",
                                            "vltr" = "seagreen")
#set plots colours
master_list$project_details$plot_colour <-  c("sample" = "black",
                                              "ltr" = "black",
                                              "pqc" = "black",
                                              "sltr" = "black",
                                              "vltr" = "black")
#set preferred LTR type for subsequent QC filters
master_list$project_details$plot_colour[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_colour)))] <- "red"

#set plot shapes
master_list$project_details$plot_shape <- c("sample" = 21,
                                            "ltr" = 21,
                                            "sltr" = 21,
                                            "vltr" = 21,
                                            "pqc" = 21)
#set preferred LTR type for subsequent QC filters
master_list$project_details$plot_shape[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_shape)))] <- 23
#set plot size
master_list$project_details$plot_size <- c("sample" = 2,
                                           "ltr" = 2,
                                           "sltr" = 2,
                                           "vltr" = 2,
                                           "pqc" = 2)
#set preferred LTR type for subsequent QC filters
master_list$project_details$plot_size[which(tolower(master_list$project_details$qc_type) == tolower(names(master_list$project_details$plot_size)))] <- 3

## 4.2. PCA  -----------
### 4.2.a. create models and extract scores ---------
#setLists
master_list$pca <- list()
master_list$pca$models <- list()
master_list$pca$scores <- list()
for(idx_pca in c("peakArea", "concentration")){
  pca_x <- bind_rows(master_list$data[[idx_pca]]$imputed) %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample"), -contains("SIL")) %>%
    as.matrix()
  
  #build PCA
  master_list$pca$models[[idx_pca]] <- ropls::opls(
    x = pca_x,
    y=NULL,
    crossvalI = 1,
    predI = 3,
    algoC = "nipals",
    log10L = FALSE,
    scale = "pareto",
    plotSubC = NA, 
    fig.pdfC = "none",
    subset = NULL)
  
  #extract scores data
  master_list$pca$scores[[idx_pca]] <- master_list$pca$models[[idx_pca]]@scoreMN %>% 
    as_tibble() %>%
    dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
    add_column(.before = 1, sample_name = rownames(pca_x)) %>%
    left_join(by = "sample_name",
              .,
              bind_rows(master_list$data[[idx_pca]]$imputed) %>%
                select(contains("sample"))
    )
  
  master_list$pca$scores[[idx_pca]]$sample_data_source <- paste0(unique(master_list$pca$scores[[idx_pca]]$sample_data_source),
                                                                 ": s=",
                                                                 dim(pca_x)[1],
                                                                 ", l=",
                                                                 dim(pca_x)[2]
  )
  
}

#statTarget model
pca_x <- bind_rows(master_list$data$concentration$statTargetProcessed) %>%
  column_to_rownames("sample_name") %>%
  select(-contains("sample"), -contains("SIL")) %>%
  as.matrix()

#build PCA
master_list$pca$models[["concentration.statTarget"]] <- ropls::opls(
  x = pca_x,
  y=NULL,
  crossvalI = 1,
  predI = 3,
  algoC = "nipals",
  log10L = FALSE,
  scale = "pareto",
  plotSubC = NA, 
  fig.pdfC = "none",
  subset = NULL)

#extract scores data
master_list$pca$scores[["concentration.statTarget"]] <- master_list$pca$models[["concentration.statTarget"]]@scoreMN %>% 
  as_tibble() %>%
  dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
  add_column(.before = 1, sample_name = rownames(pca_x)) %>%
  left_join(by = "sample_name",
            .,
            bind_rows(master_list$data$concentration$statTargetProcessed) %>%
              select(contains("sample"))
  )


master_list$pca$scores$concentration.statTarget$sample_data_source <- paste0(
  "concentration.statTarget",
  ": s=",
  dim(pca_x)[1],
  ", l=",
  dim(pca_x)[2]
)


#processed and filtered models
for(idx_pca in c("peakArea", "concentration")){
  loopLipidFilter <- c(
    master_list$filters$failed_lipids,
    master_list$filters$rsd %>% 
      filter(dataBatch == "allBatches" & dataSource == idx_pca) %>%
      select(-dataBatch) %>%
      t() %>%
      as.data.frame() %>%
      rownames_to_column() %>%
      setNames(c("lipid", "value")) %>%
      .[-1,] %>%
      as_tibble() %>% 
      mutate_at("value", as.numeric) %>%
      filter(value >30) %>%
      .$lipid
  ) %>% 
    unique()
  
  pca_x <- bind_rows(master_list$data[[idx_pca]]$imputed) %>%
    filter(!sample_name %in% master_list$filters$failed_samples) %>%
    column_to_rownames("sample_name") %>%
    select(-contains("sample"), -contains("SIL"), 
           -any_of(loopLipidFilter)) %>%
    as.matrix()
  
  #build PCA
  master_list$pca$models[[paste0(idx_pca, ".preProcessed")]] <- ropls::opls(
    x = pca_x,
    y=NULL,
    crossvalI = 1,
    predI = 3,
    algoC = "nipals",
    log10L = FALSE,
    scale = "pareto",
    plotSubC = NA, 
    fig.pdfC = "none",
    subset = NULL)
  
  #extract scores data
  master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]] <- master_list$pca$models[[paste0(idx_pca, ".preProcessed")]]@scoreMN %>% 
    as_tibble() %>%
    dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
    add_column(.before = 1, sample_name = rownames(pca_x)) %>%
    left_join(by = "sample_name",
              .,
              bind_rows(master_list$data[[idx_pca]]$imputed) %>%
                select(contains("sample"))
    )
  
  master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]]$sample_data_source <- paste0(
    gsub(":.*", "", unique(master_list$pca$scores[[idx_pca]]$sample_data_source)), 
    ".preProcessed: ",
    "s=",
    dim(pca_x)[1],
    ", l=",
    dim(pca_x)[2])
  
  # if(idx_pca =="peakArea"){
  #   master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]]$sample_data_source <- paste0(".", master_list$pca$scores[[paste0(idx_pca, ".preProcessed")]]$sample_data_source)
  # }
}

#statTarget
pca_x <- bind_rows(master_list$data$concentration$statTargetProcessed) %>%
  filter(!sample_name %in% master_list$filters$failed_samples) %>%
  select(!any_of(master_list$filters$failed_lipids)) %>%
  select(!any_of(
    (master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
  )) %>%
  column_to_rownames("sample_name") %>%
  select(-contains("sample"), -contains("SIL")) %>%
  as.matrix()

#build PCA
master_list$pca$models[["concentration.statTarget.preProcessed"]] <- ropls::opls(
  x = pca_x,
  y=NULL,
  crossvalI = 1,
  predI = 3,
  algoC = "nipals",
  log10L = FALSE,
  scale = "pareto",
  plotSubC = NA, 
  fig.pdfC = "none",
  subset = NULL)

#extract scores data
master_list$pca$scores[["concentration.statTarget.preProcessed"]] <- master_list$pca$models[["concentration.statTarget.preProcessed"]]@scoreMN %>% 
  as_tibble() %>%
  dplyr::rename(PC1 = p1, PC2 = p2, PC3 = p3) %>%
  add_column(.before = 1, sample_name = rownames(pca_x)) %>%
  left_join(by = "sample_name",
            .,
            bind_rows(master_list$data$concentration$statTargetProcessed) %>%
              select(contains("sample"))
  )

master_list$pca$scores[["concentration.statTarget.preProcessed"]]$sample_data_source <- paste0(
  gsub(":.*","",unique(master_list$pca$scores$concentration.statTarget$sample_data_source)), 
  ".preProcessed: ",
  "s=",
  dim(pca_x)[1],
  ", l=",
  dim(pca_x)[2])

### 4.2.b. create plotly scores ----------------------------
#make plotly scores
master_list$pca$plot <- list()
for(idx_fill in c("sample_type_factor", "sample_plate_id")){
  #ggplotly
  master_list$pca$plot[[idx_fill]] <- ggplotly(
    ggplot(
      data = bind_rows(master_list$pca$scores),
      aes(x = PC1, y = PC2,
          group  = sample_name,
          fill = get(idx_fill),
          color = sample_type_factor,
          shape = sample_type_factor,
          size = sample_type_factor)) +
      geom_vline(xintercept = 0, colour = "darkgrey") +
      geom_hline(yintercept = 0, color = "darkgrey")+
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = master_list$project_details$plot_shape) +
      (if(idx_fill == "sample_type_factor"){scale_fill_manual(values = master_list$project_details$plot_fill)}) +
      scale_color_manual(values = master_list$project_details$plot_colour) +
      scale_size_manual(values = master_list$project_details$plot_size) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0(idx_fill))) +
      facet_wrap(facets = "sample_data_source", scales = "free", ncol = 2, nrow = 3)
  ) %>% layout(
    title = list(
      text = paste0("PCA scores; coloured by ", idx_fill, "; ", master_list$project_details$project_name),
      y = 1.1, 
      x=0.05),
    margin = list(
      l = 10, r = 10, b=65, t=85),
    legend = list(
      orientation = "v",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      y = 0.5,
      x= 1.075)           # put legend in center of x-axis
  )}

### runOrder plots ------
#### find plateBoundary ------
#find plate boundaries for vline
plate_boundary = 0.5
annotate_coordinate = NULL
boundary_finder <- bind_rows(master_list$pca$scores) %>%
  select(sample_run_index, sample_plate_id, sample_batch) %>%
  distinct()

for(idx_plate in unique(boundary_finder$sample_plate_id)){
  plate_boundary <- c(
    plate_boundary,
    c(min((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]]) - 0.5,
      max((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]]) + 0.5)
  )
  
  annotate_coordinate <- c(
    annotate_coordinate,
    median((boundary_finder %>% filter(sample_plate_id == idx_plate))[["sample_run_index"]])
  )
}
#remove duplicates
plate_boundary <- unique(plate_boundary)
#finalise annotation co-ordinate
boundary_finder$annotation <- paste0(boundary_finder$sample_batch,":","\n",boundary_finder$sample_plate_id)
annotate_coordinate <- setNames(annotate_coordinate, paste0(unique(boundary_finder$annotation)))

#make data.frame
annotate_label <- tibble(
  sample_data_source = rep("x.plateID", length(names(annotate_coordinate))),
  sample_plate_id = names(annotate_coordinate),
  sample_run_index = annotate_coordinate,
  PC1=0,PC2 =0, PC3=0, value =0
)

#### pca score vs run order -----
#plot PCA scores vs run order on x
#make plotly
master_list$pca$scoresRunOrder <- list()
for(idx_pca in c("PC1", "PC2", "PC3")){
  master_list$pca$scoresRunOrder[[idx_pca]] <- ggplotly(
    ggplot(
      data = bind_rows(master_list$pca$scores),
      aes(x = sample_run_index, y = get(idx_pca),
          group = sample_name,
          fill = sample_type_factor,
          color = sample_type_factor,
          shape = sample_type_factor,
          size = sample_type_factor,
      )) +
      geom_vline(xintercept = plate_boundary, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = master_list$project_details$plot_shape) +
      scale_fill_manual(values = master_list$project_details$plot_fill) +
      scale_color_manual(values = master_list$project_details$plot_colour) +
      scale_size_manual(values = master_list$project_details$plot_size) +
      ylab(paste0(idx_pca)) +
      guides(shape = "none", size = "none", color = "none", fill=guide_legend(title=paste0(idx_fill))) +
      geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = sample_run_index,  y = get(idx_pca), label = sample_plate_id), col = "black") +
      facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y") 
  ) %>% layout(
    title = list(
      text = paste0(idx_pca, "; run order (x) vs PCA scores (y) ; ", master_list$project_details$project_name),
      y = 1.1, 
      x=0.05),
    margin = list(
      l = 10, r = 10, b=65, t=85),
    legend = list(
      orientation = "v",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      x = 1.1,
      y= 0.5
    )           # put legend in center of x-axis
  )
}

#### targetControlCharts ------------------
#setLists
master_list$control_charts <- list()
precursor_lists <- list()
Control_SIL_List <- list()
Control_SIL_List_Common <- list()

# Create guide for SILS in common through versions
SIL_versions <- unique(unlist(master_list[["templates"]][["Plate SIL version"]]))

for (SIL_ver in SIL_versions) {
  SIL_Temp <- master_list$templates[[SIL_ver]]$SIL_guide[grepl("TRUE", master_list$templates[[SIL_ver]]$SIL_guide$control_chart),]
  precursor_lists[[SIL_ver]] <- SIL_Temp$precursor_name
  Control_SIL_List[[SIL_ver]] <- SIL_Temp
}

common_control_charts <- Reduce(intersect, precursor_lists)
Control_SIL_List_Common <- bind_rows(Control_SIL_List)
Control_SIL_List_Common <- Control_SIL_List_Common %>% filter(Control_SIL_List_Common$precursor_name %in% common_control_charts)

for (idx_metabolite in common_control_charts) {
  # Extract SIL int.std
  idx_sil <- Control_SIL_List_Common %>% filter(Control_SIL_List_Common$precursor_name == idx_metabolite)

  # Plot generation
  master_list$control_charts[[idx_metabolite]] <- ggplotly(
    ggplot(
      data = bind_rows(
        bind_rows(master_list$data$peakArea$imputed) %>% select(contains("sample"), any_of(idx_metabolite)) %>% 
          mutate(sample_data_source = ".peakArea"),
        
        # Add SIL data from various versions
        bind_rows(master_list$data$peakArea$imputed) %>%
          select(contains("sample"), any_of(idx_sil$note)) %>%
          rowwise() %>% 
          mutate(SIL = paste(na.omit(c_across(contains("SIL"))), collapse = " ")) %>% 
          ungroup() %>%
          mutate(SIL = as.numeric(SIL)) %>%
          dplyr::rename(!!paste0(idx_metabolite) := SIL) %>%
          select(-contains("SIL")) %>%
          mutate(sample_data_source = ".SIL.peakArea"),
        
        # Add concentration data
        bind_rows(master_list$data$concentration$imputed) %>%
          select(contains("sample"), any_of(idx_metabolite)) %>%
          mutate(sample_data_source = "concentration"), 
        
        # Add statTarget concentration data
        bind_rows(master_list$data$concentration$statTargetProcessed) %>%
          select(contains("sample"), any_of(idx_metabolite)) %>%
          mutate(sample_data_source = "statTargetConcentration"),

        ) 
      #rename metabolite
      %>% dplyr::rename(!!paste0("value") := !!paste0(idx_metabolite)),
      
      #Structure ggplot
      aes(x = sample_run_index, y = value,
          group = sample_name,
          fill = sample_type_factor,
          color = sample_type_factor,
          shape = sample_type_factor,
          size = sample_type_factor)) +
      geom_vline(xintercept = plate_boundary, linetype = "dashed") +
      geom_point() +
      theme_bw() +
      scale_shape_manual(values = master_list$project_details$plot_shape) +
      scale_fill_manual(values = master_list$project_details$plot_fill) +
      scale_color_manual(values = master_list$project_details$plot_colour) +
      scale_size_manual(values = master_list$project_details$plot_size) +
      ylab(paste0(idx_metabolite)) +
      guides(shape = "none", size = "none", color = "none", fill = guide_legend(title = paste0("sample_type"))) +
      geom_text(inherit.aes = FALSE, data = annotate_label, aes(x = sample_run_index, y = value, label = sample_plate_id), col = "black") +
      facet_wrap(facets = "sample_data_source", ncol = 1, scales = "free_y")
  ) %>% layout(
    title = list(
      text = paste0(idx_metabolite, "; control chart; ", master_list$project_details$project_name),
      y = 1.1, 
      x = 0.05),
    margin = list(
      l = 10, r = 10, b = 65, t = 85),
    legend = list(
      orientation = "v",   # show entries horizontally
      xanchor = "center",  # use center of legend as anchor
      x = 1.1,
      y = 0.5
    )           # put legend in center of x-axis
  )
}

# Update script_log
master_list$environment$user_functions$update_script_log$value(master_list, "plot_generation", "summary_report", "data_exports")

# . ------------------------------------------------------------------------------------------------------------------------------  
#PHASE 5: EXPORTS ------------------------------------
## 5.1. xlsx file -------------------------------
### 5.1.a. create peakArea user guide ----
master_list$summary_tables$odsAreaOverview <- rbind(
  c("projectName", master_list$project_details$project_name),
  c("user", master_list$project_details$user_name),
  c("lipidExploreR_version", master_list$project_details$lipidExploreR_version),
  c("qcType_preProcessing|filtering", master_list$project_details$qc_type),
  c("SIL_Int.Std_version", paste(unique(master_list[["templates"]][["Plate SIL version"]]), collapse = ',', sep = ',')),
  c("total.StudyPlates", bind_rows(master_list$data$peakArea$sorted)[["sample_plate_id"]] %>% unique() %>% length()),
  c("total.Samples", bind_rows(master_list$data$peakArea$sorted) %>% nrow()),
  c("studySamples", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sample") %>% nrow()),
  c("ltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "ltr") %>% nrow()),
  c("vltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "vltr") %>% nrow()),
  c("sltr", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "sltr") %>% nrow()),
  c("pqc", bind_rows(master_list$data$peakArea$sorted) %>% filter(sample_type_factor == "pqc") %>% nrow()),
  c("total.LipidFeatures", bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("sample")) %>% select(-contains("SIL")) %>% ncol()),
  c("total.SIL.Int.Stds", bind_rows(master_list$data$peakArea$sorted) %>% select(contains("SIL")) %>% ncol()),
  c("", ""),
  c("TAB_DESCRIPTION:", ""),
  c("QC.platePerformance", "overview of project quality performance (per plate)"),
  c("QC.samplesMV", "detailed overview of sample quality (missing values)"),
  c("QC.lipidsMV", "detailed overview of lipid quality (missing values)"),
  c("QC.lipidQcRsd", "detailed overview of lipid quality (% RSD in QC samples)"),
  c("DATA.lipidPeakArea", "lipid target peak area integrals (skylineMS)"),
  c("DATA.silPeakArea", "stable isotope labelled internal standard peak area integrals (skylineMS)"),
  c("DATA.all.concentration", "peakArea >> missing value imputed [min/2] >> ratio with SIL IS >> single point concentration factor adjusted"),
  c("DATA.all.concentration.S.T.", "peakArea >> missing value imputed [min/2]  >> ratio with SIL IS >> signalDrift|batch correction using the statTarget r package >> single point concentration factor adjusted"),
  c("DATA.preProcessed.conc.S.T.", "peakArea >> missing value imputed [min/2]  >> ratio with SIL IS >> signalDrift|batch correction using the statTarget r package >> single point concentration factor adjusted >> filtered")
) %>%
  as_tibble() 

### 5.1.b. write .xlsx tabbed file ----

openxlsx::write.xlsx(
  file = paste0(master_list$project_details$project_dir, 
                "/xlsx_report/",
                Sys.Date(), "_", master_list$project_details$user_name, "_",
                master_list$project_details$project_name,
                "_",
                master_list$project_details$plateID,
                "_lipidData_qcCheckeR_v4.xlsx"),
  overwrite = TRUE,
  x = list(
    "userGuide" = master_list$summary_tables$odsAreaOverview,
    #summary
    "QC.platePerformance" = master_list$summary_tables$projectOverview,
    "QC.sampleMV" = master_list$filters$samples.missingValues,
    "QC.lipidsMV" = master_list$filters$lipid.missingValues %>% relocate(PROJECT.flag.Lipid, .after = 1),
    "QC.lipidQcRsd" = master_list$filters$rsd %>% mutate(across(!contains("data"), round, 2)) %>%
      add_column(data = paste0(.$dataSource, ".", .$dataBatch), .before = 1) %>%
      select(-dataSource, - dataBatch) %>% t() %>% as.data.frame() %>% rownames_to_column() %>% setNames(.[1,]) %>% filter(data != "data")%>% as_tibble() %>% mutate(across(!contains("data"), as.numeric)),
    #data
    "DATA.peakArea" = bind_rows(master_list$data$peakArea$sorted) %>% select(-contains("SIL")),
    "DATA.silPeakArea" = bind_rows(master_list$data$peakArea$sorted) %>% select(contains("sample") | contains("SIL")),
    "DATA.all.concentration" = bind_rows(master_list$data$concentration$sorted),
    "DATA.preProcessed.concentration" = bind_rows(master_list$data$concentration$imputed) %>% 
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      select(!any_of(master_list$filters$failed_lipids)) %>%
      select(!any_of(
        (master_list$filters$rsd %>% filter(dataSource == "concentration" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
      )),
    "DATA.all.concentration.S.T." = bind_rows(master_list$data$concentration$statTargetProcessed),
    "DATA.preProcessed.conc.S.T." = bind_rows(master_list$data$concentration$statTargetProcessed) %>%
      filter(!sample_name %in% master_list$filters$failed_samples) %>%
      select(!any_of(master_list$filters$failed_lipids)) %>%
      select(!any_of(
        (master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data")))[which(master_list$filters$rsd %>% filter(dataSource == "concentration[statTarget]" & dataBatch == "allBatches") %>% select(!contains("data"))>30)] %>% names()
      ))
  )
)

## 5.2. html report -------------------------

# ### 5.2.a. download template ----
# fileConn<-file(paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v5 Developmental/templates/lipid_exploreR_report_template_v5.R"))
# writeLines(httr::GET(url = paste0(master_list$project_details$github_master_dir, "templates/lipid_exploreR_report_template_v4.R")) %>%
#              httr::content(as = "text"), fileConn)
# close(fileConn)


### 2. render template ----
rmarkdown::render(input ="C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/lipid_exploreR_report_template_v5.R",
                  output_format = "html_document",
                  output_dir = paste0(master_list$project_details$project_dir, "/html_report"),
                  output_file = paste0(master_list$project_details$project_dir, 
                                       "/html_report/",
                                       Sys.Date(), "_", master_list$project_details$user_name, "_",
                                       master_list$project_details$project_name,"_",master_list$project_details$plateID,
                                       "_lipidExploreR_qcCheckeR_report_v4.html")
                  )

### 3. browse template ----
browseURL(url = paste0(master_list$project_details$project_dir, 
                       "/html_report/",
                       Sys.Date(), "_", master_list$project_details$user_name, "_",
                       master_list$project_details$project_name,"_",master_list$project_details$plateID,
                       "_lipidExploreR_qcCheckeR_report_v4.html")
)

## 5.3. rda file of maser list -------

#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))
# Update script_log
invisible(capture.output(master_list$environment$user_functions$update_script_log$value(master_list, "data_exports", "plot_generation", "Script Complete \n\n\n Thank you for choosing Lipid Explore R")))

### 5.3.a. export rda of master_list ----

save(master_list,
     file = paste0(
       master_list$project_details$project_dir,
       "/data/rda/", Sys.Date(), "_", master_list$project_details$user_name, "_",
       master_list$project_details$project_name,
       "_",
       master_list$project_details$plateID,
       "_qcCheckeR_v4.rda"))

master_list$environment$user_functions$update_script_log$value(master_list, "data_exports", "plot_generation", "Script Complete \n\n\n Thank you for choosing Lipid Explore R")
