




############ HEAD ############


library(lubridate)

nightly_path <- "\"/Users/kevin/Documents/R Projects/ideology/nightly/"
# nightly_path <- "\"/Volumes/kevin/Documents/R Projects/ideology/nightly/" #testing from macbook pro

filename <- paste0("/Users/kevin/Documents/R Projects/ideology/nightly/logs/_master-log-", month(now()), day(now()), year(now()), ".txt")
file.create(filename)
sink(filename)
start <- Sys.time()
print(paste0("STARTING AT: ", start))

setwd("/Users/kevin/Documents/R Projects/ideology/nightly")


############ BODY ############


# RUN BAFUMI MODELS ----

  print("RUNNING BAFUMI")
  print(lubridate::now())
  system(paste0("/usr/local/bin/Rscript ", nightly_path, "nightly-bafumi.R\""))

# FINISH WRITING LOG ----

  end <- Sys.time()
  print(paste0("ENDING AT: ", end))
  print(paste0("TOTAL TIME: ", format(end - start)))
  sink()

# SEND LOG EMAIL ----

  # This is taken from the PP api and would be good to implement here eventually.
# system(paste0("/usr/local/bin/Rscript ", nightly_path, "email-updates/", "email-logs.R\""))




