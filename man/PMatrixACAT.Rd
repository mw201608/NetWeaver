\name{PMatrixACAT}
\alias{PMatrixACAT}
\title{Combine Rows of a P-Value Matrix with ACAT}
\description{
Applies \code{ACAT.2} to every row of a p-value matrix.
}
\usage{
PMatrixACAT(Pmatrix, na.action = c("na.omit", "na.to1"))
}
\arguments{
\item{Pmatrix}{A matrix or object coercible to a matrix, with p-values in
rows and tests in columns.}
\item{na.action}{How to handle missing values; passed to \code{ACAT.2}.}
}
\value{A numeric vector containing one combined p-value per row of
\code{Pmatrix}.}
\examples{
PMatrixACAT(matrix(c(0.01, 0.05, 0.20, 0.40), nrow = 2))
}
