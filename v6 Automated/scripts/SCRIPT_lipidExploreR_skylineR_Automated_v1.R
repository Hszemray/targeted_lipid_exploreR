#LipidExploreR setup-----
#### load packages
package_list <- c('statTarget', 'svDialogs', 'ggpubr', 'janitor', 'plotly', 'knitr', 'viridisLite', 'mzR', 'httr', 'cowplot', 'matrixStats', 'tidyverse')

for(idx_package in package_list){
  if(!require(package = idx_package, character.only = TRUE)){
    install.packages(idx_package, dependencies = TRUE)
    suppressMessages(require(package = idx_package, character.only = TRUE))
  }
}

##set up project master list
master_list <- list(); master_list$environment <- list(); master_list$environment$user_functions <- list(); master_list$templates <- list(); master_list$templates$mrm_guides <- list(); master_list$project_details <- list();    master_list$data <- list(); master_list$data$mzR <- list(); master_list$summary_tables <- list(); master_list$process_lists <- list()

##store environment details
master_list$environment$r_version <- sessionInfo()$R.version$version.string
master_list$environment$base_packages <- sessionInfo()$basePkgs
master_list$environment$user_packages <- paste0(names(sessionInfo()$otherPkgs), ": ", paste0(installed.packages()[names(sessionInfo()$otherPkgs), "Version"]))

##set project details----
#set project directory
master_list$project_details$project_dir <- paste0("C:/Users/Hszem/OneDrive - Murdoch University/Desktop/Dummy Cohort") # Change to entering from notebook later##############################
setwd(master_list$project_details$project_dir)
#set lipidExploreR version
master_list$project_details$lipidExploreR_version <- "Automated V1"
#set user
master_list$project_details$user_name <- "ANPC"
#set project name
master_list$project_details$project_name <- stringr::str_extract(master_list$project_details$project_dir, "(\\w*\\s)?\\w*$")
#github master directory
master_list$project_details$github_master_dir <- "https://raw.githubusercontent.com/lukewhiley/targeted_lipid_exploreR/main/v4"
#set wiff file paths dynamically from project file wiff
master_list$project_details$wiff_file_paths <- list.files(path = paste0(master_list$project_details$project_dir,"/wiff"), pattern = ".wiff$", all.files = FALSE,
                                                  full.names = TRUE, recursive = FALSE,
                                                  ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
#set plateIDs 
master_list$project_details$plateID <- stringr::str_extract(master_list$project_details$wiff_file_paths,"COVp\\d+") %>% str_extract(., "p\\d+")

##read in mrm_guide from github ----
versions <- c("v1", "v2", "v3", "v4")
for (version in versions) {
  master_list$templates$mrm_guides[[version]]$mrm_guide <- read_csv(
    paste0(master_list$project_details$github_master_dir, "/templates/LGW_lipid_mrm_template_", version, ".csv"),
    show_col_types = FALSE
  )
}

#source functions----
#RT finder
master_list$environment$user_functions$mrm_RT_findeR_mzR <- source(paste0(
  master_list$project_details$github_master_dir , 
  "/functions/FUNC_lipidExploreR_MRM_findeR_pwiz3019_mzR_v4.R"))
#Skyline_Commander
#Change to github repo!!!!! ----
master_list$environment$user_functions$skyline_commandR <- source(paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/functions/FUNC_lipidExploreR_Skyline_CommandR.R"))


##setup project directories-----
for (plate_ID in master_list$project_details$plateID){ 
  #plate
  if(!dir.exists(paste0(master_list$project_details$project_dir, "/",plate_ID))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID))}
  #data
  if(!dir.exists(paste0(master_list$project_details$project_dir, "/",plate_ID,"/data"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data"))}
  #mzml
  if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/mzml"))){dir.create(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/mzml"))}
  #rda
  if(!dir.exists(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/rda"))){dir.create(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/rda"))}
  #skyline
  if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/skyline"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/skyline"))}
  #sciex
  if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/sciex_raw"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/sciex_raw"))}
  #batch_correct
  if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/batch_correction"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/data/batch_correction"))}
  #html_reports
  if(!dir.exists(paste0(master_list$project_details$project_dir,"/",plate_ID, "/html_report"))){dir.create(paste0(master_list$project_details$project_dir, "/",plate_ID, "/html_report"))}
}

##generate mzml from wiff files----
  # replace all "/" with "\\"
  file_paths <- gsub("/", "\\\\", master_list$project_details$wiff_file_paths)
  # define the base command
  base_command <- "\"C:\\Program Files\\ProteoWizard\\ProteoWizard 3.0.25015.b6222f2\\msconvert.exe\" --zlib --filter \"titleMaker <RunId>.<ScanNumber>.<ScanNumber>.<ChargeState> File:\\\"<SourcePath>\\\", NativeID:\\\"<Id>\\\"\""
  # define the output directory
  output_dir <- gsub("/", "\\\\", master_list$project_details$project_dir)%>% paste0(.,"\\msConvert_mzml_output")
  # quote each file path
  quoted_file_paths <- sapply(file_paths, shQuote)
  # construct the full command
  full_command <- paste(
    base_command,
    paste(quoted_file_paths, collapse = " "),
    "--outdir", shQuote(output_dir)
  )
  # execute the command
  system(full_command)

##restructure directory----
# move wiff files into correct location of file structure
temp_path_1 <- list.files(path = file.path(master_list$project_details$project_dir,"/wiff"), pattern = ".wiff$", all.files = FALSE,
                                                                        full.names = TRUE, recursive = FALSE,
                                                                        ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
temp_path_2 <- list.files(path = file.path(master_list$project_details$project_dir,"/wiff"), pattern = ".wiff.scan$", all.files = FALSE,
                                                                         full.names = TRUE, recursive = FALSE,
                                                                         ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  for(plate_ID in master_list$project_details$plateID){
    #Move .wiff
    path1 <- str_subset(string = temp_path_1, pattern = paste0(plate_ID))
    file.copy(from = paste0(path1), to = paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/sciex_raw" ))
    #Move .wiff.scan
    path2 <- str_subset(string = temp_path_2, pattern = paste0(plate_ID))
    file.copy(from = paste0(path2), to = paste0(master_list$project_details$project_dir,"/",plate_ID, "/data/sciex_raw" ))
  }

# Move mzml files to correct locations
master_list$project_details$mzml_file_paths <- list.files(path = file.path(master_list$project_details$project_dir, "msConvert_mzml_output"),pattern = ".mzML$",
                                                          all.files = FALSE,full.names = TRUE,recursive = FALSE,ignore.case = FALSE,include.dirs = FALSE,
                                                          no.. = FALSE)
# Exclude files containing "COND" or "Blank" or "ISTDs"
master_list$project_details$mzml_file_paths <- master_list$project_details$mzml_file_paths[!grepl("COND|Blank|ISTDs", master_list$project_details$mzml_file_paths)]

temp_paths <- master_list$project_details$mzml_file_paths
project_dir <- master_list$project_details$project_dir
  
for (plate_ID in master_list$project_details$plateID) {
    path <- str_subset(string = temp_paths, pattern = plate_ID)
    destination <- file.path(project_dir, plate_ID, "data", "mzml")
    file.copy(from = path, to = destination)
}

#move wiff and output mzml directory
  ##Function for moving folder
  move_folder <- function(source_dir, dest_dir) {
    # Create the destination directory if it doesn't exist
    if (!dir.exists(dest_dir)) {
      dir.create(dest_dir)
    }
    
    # Copy all files from the source to the destination
    file.copy(list.files(source_dir, full.names = TRUE), dest_dir, recursive = TRUE)
    
    # Delete the original directory and its contents
    unlink(source_dir, recursive = TRUE)
  }
  
  #Alter directory
  ##wiff files
  move_folder(file.path(master_list$project_details$project_dir,"wiff"), file.path(master_list$project_details$project_dir,"archive"))
  ##mzml files
  move_folder(file.path(master_list$project_details$project_dir,"msConvert_mzml_output"), file.path(master_list$project_details$project_dir,"archive"))

# import mzml files using mzR ------------------------------------
#initialise mzml_filelist
mzml_filelist <- list()
#Initialise loop to go over each plate
for(idx_plate in master_list$project_details$plateID){
  #Generate list of mzml files for the plate
  mzml_filelist[[idx_plate]] <- list.files(file.path(master_list$project_details$project_dir,
                                                  "/",
                                                  idx_plate,
                                                  "/data/mzml/"),
                                           pattern = ".mzML",
                                           full.names = FALSE)

    # Exclude files containing "COND" or "Blank" or "ISTDs"
    mzml_filelist[[idx_plate]] <- mzml_filelist[[idx_plate]][!grepl("COND|Blank|ISTDs", mzml_filelist[[idx_plate]])]

      master_list$data[[idx_plate]]$mzR <- list()
      #read in mzML files using mzR
      for(idx_mzML in mzml_filelist[[idx_plate]]){
        master_list$data[[idx_plate]]$mzR[[idx_mzML]] <- list()
        master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object <- mzR::openMSfile(
          filename = paste0(master_list$project_details$project_dir,
                            "/",
                            idx_plate,
                            "/data/mzml/", idx_mzML))
        master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_header <- mzR::chromatogramHeader(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object)
        master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_chromatogram <- mzR::chromatograms(master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object)
        master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp <- master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_object@backend$getRunStartTimeStamp()
      }
      # Extract single timestamp and add to global_timestamp
      timestamp <- master_list$data[[idx_plate]]$mzR[[idx_mzML]]$mzR_timestamp
      master_list$data$global_timestamp[[idx_plate]] <- timestamp %>%
        str_sub(., 1, 4) %>%
        as.numeric()
      
      #Store sample lists in project details
      if (is.null(master_list$project_details$mzml_sample_list[[idx_plate]])) {
        master_list$project_details$mzml_sample_list[[idx_plate]] <- character()
      }
      
      master_list$project_details$mzml_sample_list[[idx_plate]] <- c(master_list$project_details$mzml_sample_list[[idx_plate]], names(master_list$data[[idx_plate]]$mzR))
}

#mzml file processing----
  #initialise loop per plate 
  for (plate_idx in master_list$project_details$plateID){
  print(paste("Processing plate:",plate_idx))  
  ## parameters for lipid method version control -----
  # Check and set qc_type
  if (any(grepl("LTR", master_list$project_details$mzml_sample_list[[plate_idx]]))) {
    master_list$project_details$qc_type <- "LTR"
  } else if (any(grepl("PQC", master_list$project_details$mzml_sample_list[[plate_idx]]))) {
    master_list$project_details$qc_type <- "PQC"
  } else {
    print("mzml files contain neither LTR or PQC. SkylineR is unable to process your data.")
  }
  
  #set SIL internal standard version
  # Logic for template selection
  plate_timestamp <- master_list$data$global_timestamp[[plate_idx]]
  versions_to_try  <- if(plate_timestamp < 2023){ 
    c("v1","v2","v3","v4")
  } else if(plate_timestamp >= 2023 & plate_timestamp < 2025){
    c("v2","v1","v3","v4")
  } else if(plate_timestamp >= 2025){
    c("v4","v3","v2","v1")
  }
  
  #Set sil_found bool
  sil_found <- FALSE
  
  for (is_ver_current in versions_to_try) {
    if (sil_found) break
    
      print(paste("Trying SIL version:", is_ver_current))
      
      # Set current template version
      master_list$project_details$is_ver <- is_ver_current
    
  
      #create summary table for report
      Temp_list <- master_list$project_details[c(-5,-6,-8,-9)]
      Temp_list$plateID <- paste(plate_idx)
      master_list$summary_tables$project_summary <- tibble(unlist(Temp_list)) %>%
        add_column("Project detail" = c(
          "local directory", "lipidExploreR version", "user initials", "project name", "plateID", "project qc type", "int. std. version"),
          .before = 1)
      
      master_list$summary_tables$project_summary <- setNames(master_list$summary_tables$project_summary, c("Project detail", "value"))
      
    # retention time optimiser----
    #run function
      master_list$templates$mrm_guides$by_plate[[plate_idx]] <- list()
      for (idx in plate_idx) {
        result <- master_list$environment$user_functions$mrm_RT_findeR_mzR$value(
          FUNC_mzR = master_list$data[[idx]],
          FUNC_mrm_guide = master_list$templates$mrm_guides[[master_list$project_details$is_ver]]$mrm_guide %>% clean_names(),
          FUNC_OPTION_qc_type = master_list$project_details$qc_type
        ) %>% append(master_list$templates$mrm_guides[[master_list$project_details$is_ver]])
    
        master_list$templates$mrm_guides$by_plate[[plate_idx]] <- result
      }
    
    
    #set names from original template so that skyline recognises columns
    master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated <- setNames(master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated,
                                                                   names(master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide))
    
    
    #export updated optimised RT times
    write_csv(x = master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated,
              file = paste0(master_list$project_details$project_dir,"/", plate_idx, "/data/skyline/", Sys.Date(), "_RT_update_",
                            master_list$project_details$project_name,"_",plate_idx, ".csv"))
    
    #export peak boundary output
    write_csv(x = master_list$templates$mrm_guides$by_plate[[plate_idx]]$peak_boundary_update,
              file = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_peak_boundary_update_",
                            master_list$project_details$project_name,"_",plate_idx, ".csv"))
    
    ##Run skyline interface----
    #Function to interact with cmd to run skylinerunner.exe
    #Import blank files into structure
    #Replace with template from github once running------
      ##Blank .sky file
      file.copy(from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/default_skyline_file.sky"),
                to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
        file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_skyline_file.sky"),
                    to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_",master_list$project_details$project_name,"_",plate_idx,".sky"))
      ##Blank .csv file for report
      file.copy(from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/default_csv.csv"),
                to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
        file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_csv.csv"),
                    to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_xskylineR_1_",master_list$project_details$project_name,"_",plate_idx,".csv"))
      ##Blank .tsv for chromatograms
      file.copy(from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/default_tsv.tsv"),
                to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
        file.rename(from = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/default_tsv.tsv"),
                    to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline/",Sys.Date(),"_",master_list$project_details$project_name,"_",plate_idx,"_chromatograms.tsv"))
      ##Report template
      file.copy(from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/YYYY-MM-DD_xskylineR_1_project_name.skyr"), to = paste0(master_list$project_details$project_dir,"/",plate_idx, "/data/skyline" ))
    
    # Construct system command dynamically
    ## Define the paths and arguments separately
      exe_path <- shQuote("C:/Program Files/Skyline/SkylineCmd.exe")
      in_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
      import_transition_list <- shQuote(paste0("skyline/", Sys.Date(), "_RT_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
      import_peak_boundaries <- shQuote(paste0("skyline/", Sys.Date(), "_peak_boundary_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
      out_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
      report_file <- shQuote(paste0("skyline/", Sys.Date(), "_xskylineR_1_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
      chromatogram_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, "_chromatograms.tsv"))
      report_template <- shQuote("skyline/YYYY-MM-DD_xskylineR_1_project_name.skyr")
      mzml_path <- shQuote("mzml")
    
    #set working directory
      setwd(paste0(master_list$project_details$project_dir,"/",plate_idx,"/data"))
    ## Combine the arguments into a single command with a space separator
    cmd <- paste(
      exe_path,
      paste0('--in=', in_file),
      paste0('--import-transition-list=', import_transition_list),
      paste0('--import-all=', mzml_path),
      paste0('--import-peak-boundaries=', import_peak_boundaries),
      paste0('--save-settings'),
      paste0('--overwrite'),
      paste0('--out=', out_file),
      paste0('--report-conflict-resolution=overwrite'),
      paste0('--report-name=YYYY-MM-DD_xskylineR_1_project_name'),
      paste0('--report-file=', report_file),
      paste0('--report-add=', report_template),
      paste0('--report-format=csv'),
      paste0('--report-invariant'),
      paste0('--chromatogram-file=', chromatogram_file),
      paste0('--chromatogram-precursors'),
      paste0('--chromatogram-products'),
      paste0('--chromatogram-base-peaks'),
      paste0('--chromatogram-tics'),
      sep = " "
    )
    
    # execute the command
    system(cmd)
    
    # re-import skyline file
    skyline_data <- read.csv(
      file = list.files(
        paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline"),
        pattern = paste0("xskylineR_1_", master_list$project_details$project_name), 
        full.names = TRUE))
    cols_to_convert <- c("PrecursorMz", "ProductMz", "RetentionTime", "StartTime", "EndTime", "Area", "Height")
    suppressWarnings(skyline_data[cols_to_convert] <- lapply(skyline_data[cols_to_convert], as.numeric))
    skyline_data <- janitor::clean_names(skyline_data)
    master_list$data$skyline_report[[plate_idx]] <- skyline_data

    # Check for SIL internal standards
    sil_found <- any(grepl("SIL", master_list$data$skyline_report[[plate_idx]]$molecule_name, ignore.case = TRUE))
      
  if (sil_found) {
    print(paste("Successfully found SIL standards using version:", is_ver_current))
    #Subset master_list for the plate_idx
    master_list2 <- master_list
    master_list2$templates$mrm_guides <- NULL
    master_list2$templates$mrm_guides <-list()
    master_list2$templates$mrm_guides <- master_list$templates$mrm_guides$by_plate[[plate_idx]]
    master_list2$project_details$wiff_file_paths <- NULL
    master_list2$project_details$mzml_file_paths <- NULL
    master_list2$project_details$plateID <- plate_idx
    master_list2$project_details$mzml_sample_list <- master_list2$project_details$mzml_sample_list[[plate_idx]]
    master_list2$data <- NULL
    master_list2$data$mzR <- list()
    master_list2$data$mzR[[plate_idx]]<- master_list$data[[plate_idx]]$mzR
    master_list2$data$skyline_report <- data.frame()
    master_list2$data$skyline_report <- master_list$data$skyline_report[[plate_idx]]
    
    #export .rda for individual plate
    save(master_list2, file = paste0(
      master_list$project_details$project_dir, "/", plate_idx,"/data/rda/",
      Sys.Date(), "_", master_list$project_details$user_name, "_", master_list$project_details$project_name, "_", plate_idx,
      "_skylineR.rda"))
    
  } else {
    print(paste("No SIL standards detected with version", is_ver_current, "- trying next version"))
  }
    }
    
    if (!sil_found) {
      print(paste("No SIL internal standards detected in plate", plate_idx, "after trying all method versions. Moving to the next plate."))
    }
} # loop per plate
  
#clean environment
rm(list = c(ls()[which(ls() != "master_list")]))  

#reset working directory
setwd(master_list$project_details$project_dir)
  
#export .rda Master RDA containing data for all plates  
save(master_list, file = paste0(
    master_list$project_details$project_dir,"/",
    Sys.Date(), "_", master_list$project_details$user_name, "_", master_list$project_details$project_name, "_all_plates",
    "_skylineR.rda"))

print("SkylineR is now complete, please run qcCheckR now. :)")
