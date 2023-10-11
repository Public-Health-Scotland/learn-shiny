################################################
# code to create the credential files
# Make sure you have set working directory to shiny app folder
dir.create("admin")

credentials_df <- data.frame(
  user = c("trainingapp"),
  password = c("trainingapp"),
  stringsAsFactors = FALSE
)

saveRDS(credentials_df, "admin/credentials.rds")

################################################