#Skyline_CommandR

skyline_commandR <- function(plate_idx) {
  
  #Access master_list from global environment
  master_list <<- master_list
  # Initialise list for the given plate index
  master_list$templates$mrm_guides$by_plate[[plate_idx]] <- list()
  
  # Loop through each index in plate_idx
  for (idx in plate_idx) {
    result <- master_list$environment$user_functions$mrm_RT_findeR_mzR$value(
      FUNC_mzR = master_list$data[[idx]], 
      FUNC_mrm_guide = master_list$templates$mrm_guides[[master_list$project_details$is_ver]]$mrm_guide %>% clean_names(),
      FUNC_OPTION_qc_type = master_list$project_details$qc_type
    ) %>% append(master_list$templates$mrm_guides[[master_list$project_details$is_ver]])
    
    master_list$templates$mrm_guides$by_plate[[plate_idx]] <- result
  }
  
  # Set names from original template so that Skyline recognizes columns
  master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated <- setNames(
    master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated,
    names(master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide)
  )
  
  # Export updated optimized RT times
  write_csv(
    x = master_list$templates$mrm_guides$by_plate[[plate_idx]]$mrm_guide_updated,
    file = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_RT_update_", 
                  master_list$project_details$project_name, "_", plate_idx, ".csv")
  )
  
  # Export peak boundary output
  write_csv(
    x = master_list$templates$mrm_guides$by_plate[[plate_idx]]$peak_boundary_update,
    file = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_peak_boundary_update_", 
                  master_list$project_details$project_name, "_", plate_idx, ".csv")
  )
  
  # Run Skyline interface
  # Copy and rename blank files 
  #Change to github repo------
  file.copy(
    from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/default_skyline_file.sky"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline")
  )
  file.rename(
    from = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/default_skyline_file.sky"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky")
  )
  file.copy(
    from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/default_csv.csv"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline")
  )
  file.rename(
    from = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/default_csv.csv"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_xskylineR_1_", master_list$project_details$project_name, "_", plate_idx, ".csv")
  )
  file.copy(
    from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/default_tsv.tsv"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline")
  )
  file.rename(
    from = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/default_tsv.tsv"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, "_chromatograms.tsv")
  )
  file.copy(
    from = paste0("C:/Users/Hszem/OneDrive - Murdoch University/Masters THESIS/Documents/GitHub/targeted_lipid_exploreR/v6 Automated/templates/YYYY-MM-DD_xskylineR_1_project_name.skyr"), 
    to = paste0(master_list$project_details$project_dir, "/", plate_idx, "/data/skyline")
  )
  
  # Construct system command dynamically
  exe_path <- shQuote("C:/Program Files/Skyline/SkylineCmd.exe")
  in_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
  import_transition_list <- shQuote(paste0("skyline/", Sys.Date(), "_RT_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
  import_peak_boundaries <- shQuote(paste0("skyline/", Sys.Date(), "_peak_boundary_update_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
  out_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, ".sky"))
  report_file <- shQuote(paste0("skyline/", Sys.Date(), "_xskylineR_1_", master_list$project_details$project_name, "_", plate_idx, ".csv"))
  chromatogram_file <- shQuote(paste0("skyline/", Sys.Date(), "_", master_list$project_details$project_name, "_", plate_idx, "_chromatograms.tsv"))
  report_template <- shQuote("skyline/YYYY-MM-DD_xskylineR_1_project_name.skyr")
  mzml_path <- shQuote("mzml")
  
  # Set working directory
  setwd(paste0(master_list$project_details$project_dir, "/", plate_idx, "/data"))
  
  # Combine the arguments into a single command
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
  
  # Execute the command
  suppressWarnings(system(cmd))
  
  #re_import skyline file
  master_list$data$skyline_report[[plate_idx]] <- read_csv(file = paste0(list.files(
    paste0(master_list$project_details$project_dir,"/", plate_idx,  "/data/skyline"),
    pattern = paste0("xskylineR_1_",master_list$project_details$project_name), full.names = TRUE)), show_col_types = FALSE) %>% 
    mutate(across(any_of(c("PrecursorMz", "ProductMz", "RetentionTime", "StartTime", "EndTime", "Area", "Height"),as.numeric)))%>%
    clean_names() 
  
  # Update the global master_list
  master_list <<- master_list
  
}
  
  


