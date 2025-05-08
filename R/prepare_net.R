#' Prepare Network from Edges/Nodes Files
#'
#' @param nodes_file File path of nodes file (.csv or .csv.gz)
#' @param edges_file File path of edges file (.csv or .csv.gz)
#' @param mode Mode of transportation. Options are "car", "bike", "foot", "train".
#'
#' @returns A list containing the edges and nodes data tables.
#' @export
#'
#' @examples
#' \dontrun{
#' # Get osm.pbf File
#' osmextract::oe_get("Isle of Wight",
#'                    provider = "geofabrik",
#'                    download_directory = tempdir())
#'
#' # Get nodes and edges files
#' osm4routing_extract(
#'   osm_file = file.path(tempdir(), "geofabrik_isle-of-wight-latest.osm.pbf"),
#'   nodes_file = file.path(tempdir(), "nodes.csv.gz"),
#'   edges_file = file.path(tempdir(), "edges.csv.gz"),
#' )
#'
#' # Prepare network
#' net <- prepare_net(
#'   nodes_file = file.path(tempdir(), "nodes.csv.gz"),
#'   edges_file = file.path(tempdir(), "edges.csv.gz"),
#'   mode = "car"
#' )
#'}
prepare_net = function(nodes_file,
                        edges_file,
                        mode = c("car", "bike", "foot", "train")) {
  mode = match.arg(mode)
  nodes = data.table::fread(nodes_file)
  edges = data.table::fread(edges_file)
  selection = c("id", "osm_id", "source", "target", "length")

  if (mode %in% c("car", "bike")) {
    varnames = paste0(mode, c("_forward", "_backward"))
    # Select relevant variables
    edges = edges[, c(selection, varnames), with = FALSE]

    # Invert edges for backward direction
    fwd = edges[, c(selection, varnames[1]), with = FALSE]
    bwd = edges[, c(selection, varnames[2]), with = FALSE]
    data.table::setnames(fwd, old = varnames[1], new = mode)
    data.table::setnames(bwd, old = c(varnames[2], "source", "target"), new = c(mode, "target", "source"))
    edges = rbind(fwd, bwd)
  } else {
    varnames = mode
    # Select relevant variables
    edges = edges[, c(selection, varnames), with = FALSE]
  }
  # Eliminate edges tagged as forbidden
  #FIXME: When built as a package, following line fails as 'object 'car' is not found.
  edges = edges[get(mode) != "Forbidden",]

  # Filter nodes
  nodes = nodes[id %in% unique(c(edges$source, edges$target)),]

  # Return filtered list with edges and nodes
  list(edges = edges,
       nodes = nodes)
}


