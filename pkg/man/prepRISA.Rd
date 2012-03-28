\name{prepRISA}

\alias{prepRISA}
\alias{prepRISA.CLI}
\alias{prepRISA.GUI}
\alias{prepRISAfun}
\alias{repClass}

\title{Preparation of RISA statistical data anlysis}

\description{
prepRISA is a package to read RISA data coming from a sequencer
and compile them into a dataframe that is suitable for multivariate
data analysis.
}

\usage{
prepRISA.CLI()
prepRISA.GUI()
}

\details{
This function will interactively ask for a variable number of
parameters. It reads a RISA file that comes from a sequencer
(text file with 4 columns: 1=sample name, 2=peak number, 3=peak
position (number of base pairs), 4=peak height). The data are treated
according to the parameters given by the user, and a \code{samples x peaks}
table is computed. This table can then be analysed with the \code{ade4}
package. Two text files are written to disk : one with all columns and
one where columns of zeroes are removed.\cr
\cr
\code{prepRISA.CLI} uses the CLI (command line interface) to dialog
with the user, while \code{prepRISA.GUI} uses a Tk GUI (graphical user
interface). Both interfaces call the same function \code{prepRISAfun}.\cr
\cr
For efficiency reasons, computations are done in a C function called
\code{repClass}.
}

\value{
The \code{prepRISA.CLI} function returns invisibly the \code{samples x peaks} table as a dataframe,
so that it can be used subsequently in ade4 like this:\cr
\cr
tab <- prepRISA.CLI()\cr
...\cr
pca1 <- dudi.pca(tab)\cr
\cr
Note that these tables can be quite large (hundreds of columns).\cr\cr

The \code{prepRISA.GUI} returns nothing, as the output dataframe is directly affected in the function:\cr
\cr
prepRISA.GUI()\cr
...\cr
pca1 <- dudi.pca(tab)\cr
\cr
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

\seealso{
\code{\link{RISAtab}} for an example of RISA data.
}

\author{
Jean Thioulouse \email{jean.thioulouse at univ-lyon1.fr} \cr
}

\examples{
\dontrun{
tab <- prepRISA()
}
}
\keyword{multivariate}
