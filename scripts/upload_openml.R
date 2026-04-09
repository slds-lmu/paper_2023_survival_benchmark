# Upload datasets to OpenML and update dataset_table.csv with new IDs
#
# Reads the dataset table, finds datasets in ./datasets/ that are missing
# from the table or have no OpenML ID, uploads them via mlr3oml::publish_data(),
# and updates the table (both .csv and .rds).
#
# Prerequisites:
# - mlr3oml must be installed
# - OpenML API key must be configured, e.g. via:
#     options(mlr3oml.api_key = "your_key")
#   or set the environment variable OPENMLAPIKEY
# - Datasets must exist as .rds files in ./datasets/

library(mlr3oml)

# -- Configuration -------------------------------------------------------------
# Set to TRUE to upload to the OpenML test server instead of production
test_server <- FALSE

# -- Paths ---------------------------------------------------------------------
dataset_dir <- here::here("datasets")
table_path_csv <- here::here("tables", "dataset_table.csv")
table_path_rds <- here::here("tables", "dataset_table.rds")

# -- Read or initialize table --------------------------------------------------
if (file.exists(table_path_rds)) {
  dataset_tbl <- readRDS(table_path_rds)
} else {
  cli::cli_alert_info("No existing dataset table found at {.file {table_path_rds}}, creating from scratch.")
  dataset_tbl <- data.frame(
    package = character(),
    license = character(),
    dataset = character(),
    dataset_id = integer(),
    stringsAsFactors = FALSE
  )
}

# -- Find .rds files in datasets/ ---------------------------------------------
rds_files <- list.files(dataset_dir, pattern = "\\.rds$", full.names = TRUE)
dataset_names <- tools::file_path_sans_ext(basename(rds_files))

# -- Identify datasets missing from table or missing an OpenML ID -------------
missing_from_table <- setdiff(dataset_names, dataset_tbl$dataset)
missing_id <- dataset_tbl$dataset[is.na(dataset_tbl$dataset_id) | dataset_tbl$dataset_id == ""]
to_upload <- union(missing_from_table, missing_id)

if (length(to_upload) == 0) {
  cli::cli_alert_success("All datasets are already in the table with OpenML IDs. Nothing to do.")
} else {
  cli::cli_alert_info("Datasets to upload: {.val {to_upload}}")
}

# -- Source package/license lookup from import_datasets.R context --------------
# Known source packages and licenses for datasets that may be missing
# Add entries here for any new datasets
source_info <- list(
  cat_adoption = list(package = "modeldata", license = "MIT"),
  check_times  = list(package = "modeldata", license = "MIT"),
  wa_churn     = list(package = "modeldata", license = "MIT")
)

# -- Upload and update ---------------------------------------------------------
for (ds_name in to_upload) {
  rds_path <- file.path(dataset_dir, paste0(ds_name, ".rds"))

  if (!file.exists(rds_path)) {
    cli::cli_alert_warning("Dataset {.val {ds_name}} has no .rds file at {.file {rds_path}}, skipping.")
    next
  }

  cli::cli_alert_info("Reading {.val {ds_name}} from {.file {rds_path}}")
  dat <- readRDS(rds_path)

  # Determine description
  desc <- paste0("Survival dataset '", ds_name, "' used in the survival benchmark study (Burk et al., 2026).")

  # Determine license from source_info or existing table
  info <- source_info[[ds_name]]
  license <- if (!is.null(info)) info$license else "Unknown"
  pkg     <- if (!is.null(info)) info$package else "Unknown"

  cli::cli_alert_info("Uploading {.val {ds_name}} to OpenML (license: {license})...")

  new_id <- tryCatch(
    {
      publish_data(
        data = dat,
        name = ds_name,
        desc = desc,
        license = license,
        default_target = "time",
        test_server = test_server
      )
    },
    error = function(e) {
      cli::cli_alert_danger("Failed to upload {.val {ds_name}}: {conditionMessage(e)}")
      return(NA)
    }
  )

  if (is.na(new_id)) next

  cli::cli_alert_success("Uploaded {.val {ds_name}} with OpenML ID {.val {new_id}}")

  # Update or add row in table

  if (ds_name %in% dataset_tbl$dataset) {
    dataset_tbl$dataset_id[dataset_tbl$dataset == ds_name] <- new_id
  } else {
    new_row <- data.frame(
      package = pkg,
      license = license,
      dataset = ds_name,
      dataset_id = new_id,
      stringsAsFactors = FALSE
    )
    dataset_tbl <- rbind(dataset_tbl, new_row)
  }

  Sys.sleep(1) # Be polite to the OpenML server
}

# -- Save updated table --------------------------------------------------------
saveRDS(dataset_tbl, file = table_path_rds)
write.csv(dataset_tbl, file = table_path_csv, row.names = FALSE)

cli::cli_alert_success("Saved {.file {table_path_rds}} (and derived {.file {table_path_csv}})")

# -- Summary -------------------------------------------------------------------
cli::cli_h2("Current dataset table")
print(dataset_tbl[, c("dataset", "dataset_id")])

missing_after <- dataset_tbl$dataset[is.na(dataset_tbl$dataset_id) | dataset_tbl$dataset_id == ""]
if (length(missing_after) > 0) {
  cli::cli_alert_warning("Datasets still missing OpenML IDs: {.val {missing_after}}")
} else {
  cli::cli_alert_success("All datasets have OpenML IDs.")
}
