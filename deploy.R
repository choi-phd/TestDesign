library(devtools)

token = "f90e43b10bc761e4a4b3aa76085604fd7e32bc6e"
host = "https://github.austin.utexas.edu/api/v3"

install_github("chois1/IRTclass", auth_token = token, host = host)
remotes::install_github("chois1/Shadow", auth_token = token, host = host)

debug(class)


rsconnect::setAccountInfo(name='shadowtest',
                          token='C5FE867DC65C12297F7490D4C71BF22C',
                          secret='BelQWB3eMKvNKbziAPtk4pTo7mOaN5eHWuxuMvLY')
library(rsconnect)
deployApp()
