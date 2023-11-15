###############################################.
## Connecting to the shiny.io account.
#Enter the token and secret provided by one of the account managers
rsconnect::setAccountInfo(name='scotland',
                          token='',
                          secret='')

rsconnect::deployApp("/PHI_conf/", # working directory of app
                     appName= "phs-?????-app") # name of app

###########################################################