
# 1. Check if working directory is ../BayesDLR
working_directory <- getwd()
working_subfolder <-
  tail(strsplit(x = working_directory, split = "/")[[1]], 1)

if(working_subfolder != "BayesDLR"){
  stop("Load BayesDLR.Rproj before proceeding.")
}