## Parameters

begin <- Sys.time()
datestamp <- Sys.Date()

dir.out <- "output"


## Create Dir

dir.create(dir.out, showWarnings = FALSE)


## Run full pipeline

rmarkdown::render("PVP-FCG_Source_DataSetCodebookCreation.R",
                  output_file = file.path(dir.out,
                                          paste0("PVP-FCG",
                                                 "_",
                                                 datestamp,
                                                 "_Codebook.pdf")))


## Runtime

end <- Sys.time()
print(end-begin)
