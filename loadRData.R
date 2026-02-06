# loadRData.R
# Utility: load an .RData file and return the object created by load()
# Notes:
# - This relies on load() creating objects in the current environment.
# - If the .RData contains multiple objects, the returned object depends on ls() ordering.
# - If the .RData contains no objects, get() will error.

loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
