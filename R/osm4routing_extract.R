#' Extract Osmium Files Using osm4routing2
#'
#' @param osm_file osm.pbf file to be used
#' @param edges_file File name for the edges file.
#' @param nodes_file File name for the nodes file
#' @param overwrite Overwrite existing files (TRUE by default)
#'
#' @returns Node and edge files
#' @export
#'
#' @examples
#' \dontrun{
#' # Without Compression
#' iow = osmextract::oe_get_network("isle of wight", mode = "driving")
#' ?osmextract::oe_download
#' ?osmextract::oe_match
#' region_name = "isle of wight"
#' pbf_info = osmextract::oe_match(region_name)
#' pbf_url = pbf_info$url
#' pbf_file = osmextract::oe_download(pbf_info$url)
#' nr_osm4routing(
#'   osm_file = pbf_file,
#'   edges_file = "edges.csv",
#'   nodes_file = "nodes.csv"
#' )
#'
#' # With Compression
#' nr_osm4routing(
#'   osm_file = pbf_file,
#'   edges_file = "edges.csv.gz",
#'   nodes_file = "nodes.csv.gz"
#' )
#' }
nr_osm4routing = function(
  osm_file = NULL,
  edges_file = "edges.csv",
  nodes_file = "nodes.csv",
  overwrite = TRUE
) {
  # Check if osm4routing is installed
  osm4routing_bin = "osm4routing"
  version_check = tryCatch({
    processx::run(
      command = osm4routing_bin,
      args = "--version",
      stdout = NULL,
      stderr = NULL,
      error_on_status = TRUE
    )
    0
  }, error = function(e) {
    1
  })

  if (version_check != 0) {
    # Try the cargo path if the default fails
    osm4routing_bin_cargo = file.path(Sys.getenv("HOME"), ".cargo", "bin", "osm4routing")
    if (file.exists(osm4routing_bin_cargo)) {
        osm4routing_bin = osm4routing_bin_cargo
    } else {
        stop(
            "osm4routing not found in PATH or ~/.cargo/bin/. Please install it."
        )
    }
  }

  # Check Input
  stopifnot("osm_file must be provided" = !is.null(osm_file))
  exist = file.exists(nodes_file) && file.exists(edges_file)
  stopifnot(
    "Nodes/Edges files already exist. Overwrite is set to FALSE!" = !all(
      exist,
      !overwrite
    )
  )

  # Check if output should be compressed
  gz_nodes = grepl("\\.gz$", nodes_file)
  gz_edges = grepl("\\.gz$", edges_file)

  stopifnot(
    "Nodes/Edges files must both be either .csv or .csv.gz" = (gz_nodes &&
      gz_edges) ||
      (!gz_nodes && !gz_edges)
  )

  compress = gz_nodes && gz_edges

  if (compress) {
    nodes_gz_file = nodes_file
    edges_gz_file = edges_file
    nodes_file = gsub("\\.gz$", "", nodes_file)
    edges_file = gsub("\\.gz$", "", edges_file)
  }

  # Build osm4routing call
  call = paste(
    osm4routing_bin,
    osm_file,
    "--nodes-file",
    nodes_file,
    "--edges-file",
    edges_file
  )

  system(call)

  # Compress files if necessary
  if (compress) {
    data.table::fread(nodes_file) |>
      data.table::fwrite(nodes_gz_file)

    data.table::fread(edges_file) |>
      data.table::fwrite(edges_gz_file)

    file.remove(nodes_file)
    file.remove(edges_file)

    return(c(
      nodes_file = nodes_gz_file,
      edges_file = edges_gz_file
    ))
  }

  return(c(
    nodes_file = nodes_file,
    edges_file = edges_file
  ))
}
