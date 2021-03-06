% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.R
\name{find_facet_peaks_rough}
\alias{find_facet_peaks_rough}
\title{Rough clustering towards Facet Peaks}
\usage{
find_facet_peaks_rough(
  df,
  local_height_threshold = 2.5,
  clust_melt_rad,
  iterations = 1,
  cores = 1
)
}
\arguments{
\item{df}{A tibble containing triangle center coordinates of vertices that
lie above threshold in columns \verb{x, y, z} and local height in \code{local_height}.
Typically, this is the resulting tibble of the \code{get_local_height()} function.}

\item{local_height_threshold}{Threshold to filter out vertices: Vertices with
\code{local_height >= local_height_threshold * mean(local_height)} will be kept.
Default: \code{2.5}.}

\item{clust_melt_rad}{A numerical value of search radius for agglomerative
clustering.}

\item{iterations}{A numerical value of how many clustering iterations should
be run. Default: \code{1}.}

\item{cores}{A numerical value of how many cores to use. Default: \code{1}.}
}
\value{
Tibble \code{df} with ideally one vertex per facet, located on the facet
peak.
}
\description{
Very roughly find clusters around facet peaks.
}
\examples{
# xxx: add example

}
