% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rotations.R
\name{align_to_global_axis}
\alias{align_to_global_axis}
\title{Align 3D Point Cloud to Global Axis}
\usage{
align_to_global_axis(df, line_points, axis)
}
\arguments{
\item{df}{A tibble containing coordinates in columns \verb{x, y, z}.}

\item{line_points}{A 2x3 tibble containing coordinates of line to align the
point cloud to. Must contain one row per points and columns \verb{x, y, z}.}

\item{axis}{A character string defining the global axis to align to. Must be
\code{x}, \code{y}, or \code{z}.}
}
\value{
Returns a tibble with the aligned coordinates in columns \verb{x, y, z}.
}
\description{
Rotates 3D point cloud according to one defined vector so that this vector
is aligned to one of the global coordinate system axes.
}
\examples{
# xxx: add example

}
