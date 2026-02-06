# get_headers.R
# Read the column names from a SAS dataset by reading only the first row.
# Dependency: haven

get_headers <- function(path) {
  names(haven::read_sas(path, n_max = 1))
}
