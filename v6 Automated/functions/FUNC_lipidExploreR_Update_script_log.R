# Define the function to update script_log
update_script_log <- function(master_list, section_name, previous_section_name, next_section_name) {
  
  # Capture the current time 
  master_list$project_details$script_log$timestamps[[section_name]] <- Sys.time()
  
  # Calculate the runtime 
  master_list$project_details$script_log$runtimes[[section_name]] <- difftime(
    master_list$project_details$script_log$timestamps[[section_name]],
    master_list$project_details$script_log$timestamps[[previous_section_name]],
    units = "mins"  # Set units to minutes
  )
  
  # Calculate the total runtime from the start
  master_list$project_details$script_log$runtimes$total_runtime <- difftime(
    master_list$project_details$script_log$timestamps[[section_name]],
    master_list$project_details$script_log$timestamps$start_time,
    units = "mins"  # Set units to minutes
  )
  
  # Create the message
  master_list$project_details$script_log$messages[[section_name]] <- paste0(
    "\n",toupper(gsub("_", " ", section_name))," complete!",
    "\n\n Section runtime: ", signif(as.numeric(master_list$project_details$script_log$runtimes[[section_name]]), digits = 3), " minutes",
    "\n\n Total runtime: ", signif(as.numeric(master_list$project_details$script_log$runtimes$total_runtime), digits = 3), " minutes",
    "\n",
    "\nInitialising: ", toupper(gsub("_", " ", next_section_name)), "...."
  )
  
  # Print the message 
  cat(master_list$project_details$script_log$messages[[section_name]])
  
  
  # Assign the updated master_list to the global environment
  assign("master_list", master_list, envir = .GlobalEnv)
  
}
