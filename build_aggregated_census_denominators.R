# build_aggregated_census_denominators.R
# Generates an aggregated population denominator dataset from one or more wide-format SAS census files,
# where population counts are stored in separate columns for sex–race/ethnicity combinations and each 
# row corresponds to a unique state, year, and age. Performs file discovery, header consistency checks, 
# file-level processing, row binding, and optional CSV output.

#
# Dependencies: haven, dplyr, tidyr, stringr, gtools

build_aggregated_census_denominators <- function(
  folder_path,
  additional_files = character(0),
  output_csv = NULL,
  file_pattern = "\\.sas7bdat$",
  filter_pattern = "20"
) {

  if (missing(folder_path) || is.null(folder_path) || !nzchar(folder_path)) {
    stop("folder_path must be provided as a non-empty character string.")
  }

  # 1. Load files from folder
  file_paths <- list.files(folder_path, pattern = file_pattern, full.names = TRUE)

  # Add any explicitly provided files
  all_files <- c(file_paths, additional_files)

  # Filter files with filter_pattern in their names (default: "20")
  filtered_files <- all_files[grepl(filter_pattern, basename(all_files))]

  if (length(filtered_files) == 0) {
    stop("No files matched the filter criteria. Check folder_path, file_pattern, and filter_pattern.")
  }

  # Check to make sure headers are all the same (based on filtered_files)
  headers_list <- lapply(filtered_files, get_headers)
  names(headers_list) <- basename(filtered_files)

  all_same <- all(sapply(headers_list[-1], identical, headers_list[[1]]))

  if (all_same) {
    message("✅ All files have identical headers.")
  } else {
    message("⚠️ Not all files have the same headers.")
    for (i in seq_along(headers_list)) {
      cat("\nFile:", names(headers_list)[i], "\n")
      print(headers_list[[i]])
    }
  }

  # 2. Process and combine all files (NOTE: uses all_files, not filtered_files, per your original)
  processed_list <- lapply(all_files, process_file)
  processed_list2 <- lapply(processed_list, as.data.frame)
  aggregated_data <- do.call(gtools::smartbind, processed_list2)

  if (!is.null(output_csv)) {
    write.csv(aggregated_data, file = output_csv)
  }

  return(aggregated_data)
}
