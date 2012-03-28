\name{RISAtab}

\alias{RISAtab}

\title{Example of RISA output table from the prepRISA package}

\description{
Example of RISA output table from the prepRISA package.
}

\usage{
data(RISAtab)
}

\format{
\code{RISAtab} is a data frame with 32 rows and 256 columns. The 32 rows are
32 RISA profiles, comprising 20 standard samples and 12 real soil samples. The 256
columns correspond to the RISA bands detected in the RISA raw profiles by the
prepRISA package. The corresponding RISA raw data are in the \code{RISAdata.txt}
file (3424 rows, 4 columns) located in the prepRISA package directory.\cr
\cr
Use \code{system.file("RISAdata.txt", package="prepRISA")} to get the exact
location of this file on your system.
}

\references{
Ranjard L., Nazaret S., Gourbiere F., Thioulouse J., Linet P. & Richaume A. (2000)
A soil microscale study to reveal the heterogeneity of Hg(II) impact on indigenous
bacteria by quantification of adapted phenotypes and analysis of community DNA fingerprints.
\emph{FEMS Microbiology Ecology}, \bold{31}, 107-115.\cr
\cr
Ranjard L., Poly F., Combrisson J., Richaume A., Gourbiere F., Thioulouse J. & Nazaret S. (2000)
Heterogeneous Cell Density and Genetic Structure of Bacterial Pools Associated with
Various Soil Microenvironments as Determined by Enumeration and DNA Fingerprinting Approach (RISA).
\emph{Microbial Ecology}, \bold{39}, 263-272.\cr
\cr
Ranjard L., Poly F., Lata J.C., Mougel C., Thioulouse J. & Nazaret S. (2001)
Characterisation of bacterial and fungal soil communities by automated-RISA (Ribosomal Intergenic
Spacer Analysis) fingerprints: biological and methodological variability.
\emph{Applied and Environmental Microbiology}, \bold{67}, 4479-4487.
}

\examples{
\dontrun{
data(RISAtab)
library(ade4)
RISApca <- dudi.pca(RISAtab)
s.label(RISApca$li)
}
}
\keyword{multivariate}
