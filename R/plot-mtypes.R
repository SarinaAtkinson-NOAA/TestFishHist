#' Percent of regulations by management type and sector
#'
#' @param data default=fishhist
#' @param FMP Optional filter to plot only a single FMP of interest. If Null, the function will return a list containing a plot for each FMP.
#' \subsection{FMP:} {
#'    \describe{
#'      \item{ATLANTIC COAST RED DRUM FISHERY}
#'      \item{COASTAL MIGRATORY PELAGIC RESOURCES}
#'      \item{DOLPHIN AND WAHOO FISHERY OFF THE ATLANTIC STATES}
#'      \item{FMP FOR THE EEZ AROUND PUERTO RICO}
#'      \item{FMP FOR THE EEZ AROUND ST. CROIX}
#'      \item{FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN}
#'      \item{GOLDEN CRAB FISHERY OF THE SOUTH ATLANTIC}
#'      \item{QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS}
#'      \item{RED DRUM FISHERY OF THE GULF OF MEXICO}
#'      \item{REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS}
#'      \item{REEF FISH RESOURCES OF THE GULF OF MEXICO}
#'      \item{SHRIMP FISHERY OF THE GULF OF MEXICO}
#'      \item{SHRIMP FISHERY OF THE SOUTH ATLANTIC}
#'      \item{SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION}
#'      \item{SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS}
#'      \item{SPINY LOBSTER FISHERY OF THE GULF OF MEXICO AND SOUTH ATLANTIC}
#'    }
#' }
#'
#' @return A plot or list of plots (e.g. use plot$plot[[1]] to view the first plot)
#'
#' @examples plot_mtypes(fishhist, FMP_filter="REEF FISH RESOURCES OF THE GULF OF MEXICO")
plot_mtypes <- function(data=fishhist, FMP_filter=NULL){

  if(missing(FMP_filter)) {
    dat2 <- fishhist |>
      dplyr::group_by(FMP, SECTOR_USE, MANAGEMENT_CATEGORY) |>
      dplyr::summarise(nrecs = n_distinct(REGULATION_ID)) |>
      dplyr::group_by(FMP, SECTOR_USE) |>
      dplyr::mutate(tot = sum(nrecs), perc = nrecs/tot, labels = paste0(round(perc * 100,1), "%"))

    plot <- dat2 %>% group_by(FMP) %>%
      do(plot=ggplot2::ggplot(.,
                            ggplot2::aes(x = "" , y = perc, fill = MANAGEMENT_CATEGORY)) +
      ggplot2::geom_col(width = 1, color = 1) +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::facet_wrap(~SECTOR_USE) +
      ggplot2::geom_text(ggplot2::aes(x = 1.7, label = labels), size=4.5,
                         position = ggplot2::position_stack(vjust = 0.5))+
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Management Category")) +
      ggplot2::theme_void() +
        # still need to fix ggtitle
      ggplot2::ggtitle(.$FMP))

  } else {

    dat2 <- fishhist |>
      dplyr::filter(FMP == FMP_filter) |>
      dplyr::group_by(FMP, SECTOR_USE, MANAGEMENT_CATEGORY) |>
      dplyr::summarise(nrecs = n_distinct(REGULATION_ID)) |>
      dplyr::group_by(FMP, SECTOR_USE) |>
      dplyr::mutate(tot = sum(nrecs), perc = nrecs/tot, labels = paste0(round(perc * 100,1), "%"))

    plot <- ggplot2::ggplot(dat2,
                            ggplot2::aes(x = "" , y = perc, fill = MANAGEMENT_CATEGORY)) +
           ggplot2::geom_col(width = 1, color = 1) +
           ggplot2::coord_polar(theta = "y") +
           ggplot2::facet_wrap(~SECTOR_USE) +
           ggplot2::geom_text(ggplot2::aes(x = 1.7, label = labels), size=4.5,
                              position = ggplot2::position_stack(vjust = 0.5))+
           ggplot2::guides(fill = ggplot2::guide_legend(title = "Management Category")) +
           ggplot2::theme_void() +
           ggplot2::ggtitle(dat2$FMP)
  }

  return(plot)
}
