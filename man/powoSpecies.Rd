% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/powoSpecies.R
\name{powoSpecies}
\alias{powoSpecies}
\title{Extract list of species for any plant genus and family}
\usage{
powoSpecies(family,
            genus = NULL,
            hybrid = FALSE,
            synonyms = FALSE,
            country = NULL,
            verbose = TRUE,
            rerun = FALSE,
            save = FALSE,
            dir = "results_powoSpecies",
            filename = "output")
}
\arguments{
\item{family}{Either one family name or a vector of multiple families
that is present in POWO.}

\item{genus}{Either one genus name or a vector of multiple genera
that is present in POWO. If any genus name is not provided, then the
function will search any species from all accepted genera known for the
target family.}

\item{hybrid}{Logical, if \code{TRUE}, the search results will include
hybrid species.}

\item{synonyms}{Logical, if \code{TRUE}, the search results will include
synonyms.}

\item{country}{Either one country name or a vector of multiple countries.
If country names are provided, then the function will return only the species
that are native to such countries, according to POWO.}

\item{verbose}{Logical, if \code{FALSE}, a message showing each step during
the POWO search will not be printed in the console in full.}

\item{rerun}{Logical, if \code{TRUE}, a previously stopped search will continue
from where it left off, starting with the last retrieved taxon. Please ensure
that the 'filename' argument exactly matches the name of the CSV file saved
from the previous search, and that the previously saved CSV file is located
within a subfolder named after the current date. If it is not, please rename
the date subfolder accordingly."}

\item{save}{Logical, if \code{TRUE}, the search results will be saved on disk.}

\item{dir}{Pathway to the computer's directory, where the file will be saved
provided that the argument \code{save} is set up in \code{TRUE}. The default
is to create a directory named \strong{results_powoSpecies} and the search results
will be saved within a subfolder named after the current date.}

\item{filename}{Name of the output file to be saved. The default is to create
a file entitled \strong{output}.}
}
\value{
A table with the following fields: family, genus, species, taxon_name,
authors, scientific_name, publication (information about the protologue of
the species), native_to_country (original distribution according to
political country), native_to_botanical_countries (original distribution
according to botanical country), introduced_to_country (exotic distribution
according to political country), introduced_to_botanical_countries (exotic
distribution according to botanical country), kew_id (each species code
within Kew's databases), powo_uri (the URI to access each species in POWO).
}
\description{
Produces a CSV file listing all accepted species and
associated geographical distribution from any target genus or family of
vascular plants at
\href{https://powo.science.kew.org/}{Plants of the World Online (POWO)}.
}
\examples{
\dontrun{
library(expowo)

powoSpecies(family = "Martyniaceae",
            synonyms = TRUE,
            country = c("Argentina", "Brazil", "French Guiana"),
            save = FALSE,
            dir = "Martyniaceae_results_powoSpecies",
            filename = "Martyniaceae_spp")
}

}
\seealso{
\code{\link{megaGen}}

\code{\link{topGen}}

\code{\link{powoFam}}

\code{\link{powoGenera}}

\code{\link{POWOcodes}}
}
\author{
Debora Zuanny & Domingos Cardoso
}
