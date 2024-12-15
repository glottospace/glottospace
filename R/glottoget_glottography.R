#' Download and process Glottography data
#'
#' This function is designed to download and process Glottography data hosted on GitHub.
#' It allows users to specify a dataset and a polygon set (features, languages, or families) to
#' retrieve spatial information about languages and language families. The function downloads
#' the data, processes the GeoJSON files locally, and returns the relevant data in a structured format.
#'
#' Purpose:
#'
#' 1. **Download Glottography data**:
#'    - The function downloads the specified dataset and polygon set from the GitHub repository.
#'    - Users confirm the download process via a prompt.
#'
#' 2. **Process and Load Glottography Data**:
#'    - Reads and processes the downloaded GeoJSON file.
#'    - Extracts the relevant attributes and retains geometry for spatial analysis and visualization.
#'
#' 3. **Augment Data**:
#'    - Ensures the resulting dataset is well-structured and ready for analysis.
#'
#' Result:
#' - The returned dataset contains the attributes specified in the chosen polygon set, such as language IDs
#'   or family-level polygons, depending on the selection.
#'
#' Files Processed:
#' - `features.geojson`: The raw polygons representing linguistic data.
#' - `languages.geojson`: Polygons aggregated at the language level.
#' - `families.geojson`: Polygons aggregated at the top-level family level.
#'
#' Key Outputs:
#' - A structured dataframe containing the attributes from the selected polygon set, including geometry.
#'
#' @param dirpath Path to the directory where Glottography data will be stored and processed.
#' @param dataset The dataset (or project) name, e.g., "walker2011bayesian".
#' @param polygon_set The polygon set to process: one of "features", "languages", or "families".
#' @return A dataframe containing the processed data for the specified polygon set.
#' @importFrom sf st_read
#' @importFrom utils download.file
#' @importFrom dplyr select
#' @export
glottoget_glottography <- function(dirpath = NULL, dataset = "walker2011bayesian", polygon_set = c("features", "languages", "families")) {
  # Match the polygon set argument
  polygon_set <- match.arg(polygon_set)

  # Prompt user confirmation
  invisible(readline(prompt = sprintf("Are you sure you want to download Glottography data for '%s'? \nPress [enter] to continue", dataset)))

  # Default directory path
  if (is.null(dirpath)) {
    dirpath <- tempfile("glottoget_glottography")
  }
  if (!dir.exists(dirpath)) {
    dir.create(dirpath, recursive = TRUE)
  }

  # GitHub base URL for Glottography data
  base_url <- sprintf("https://raw.githubusercontent.com/Glottography/%s/main/cldf/", dataset)

  # Define the file name and its full URL
  file <- sprintf("%s.geojson", polygon_set)
  file_url <- paste0(base_url, file)
  file_path <- file.path(dirpath, file)

  # Download the specified file
  download.file(file_url, file_path, mode = "wb")

  # Process the GeoJSON file
  data <- sf::st_read(file_path, quiet = TRUE)

  # Return processed data
  data
}

