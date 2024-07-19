##' List of countries in the UN WPP data 2024
##'
##' A list of the countries and country codes available in the UNWPP
##' population size data by single age in .csv format, useful for
##' initialising `popim_population` objects. The .csv files can be
##' downloaded at
##' <https://population.un.org/wpp/Download/Standard/CSV/>.
##' 
##' @format `unwpp_countries`
##' A data frame with 237 rows and 4 columns.
##' \describe{
##'   \item{Location}{Name of the country}
##'   \item{ISO3_code}{3-letter ISO country code}
##'   \item{ISO2_code}{2-letter ISO country code}
##'   \item{SDMX_code}{SDMX country code}
##' }
##' @source <https://population.un.org/wpp/Download/Standard/CSV/>
"unwpp_countries"
