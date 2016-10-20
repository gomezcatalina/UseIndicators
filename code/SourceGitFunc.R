SourceGitFunc <- function(url)
{
  ## URL is the raw format link from Github online 
  ## e.g. "https://raw.githubusercontent.com/rystanley/RAD_R_Functions/master/GenoPopConvert.R"
  require(RCurl)
  script <- getURL(url, ssl.verifypeer = FALSE)
  eval(parse(text = script),envir=.GlobalEnv)
}

#Open GitHub - go to desired function
#copy http site
#SourceGitFunc("https://raw.githubusercontent.com/gomezcatalina/ExtractIndicators/master/R/LandByGroup.R")