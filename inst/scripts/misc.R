#!/usr/bin/env Rscript
# Automatic Package Reinstallation
# Author:  Jason A. French, Northwestern University
# GPL v2.0

# Replace the 3.1 with your old version
versions <- system('ls /Library/Frameworks/R.framework/Versions/', intern = TRUE)
previous.version <- sort(versions[-which(versions=='Current')])[length(sort(versions[-which(versions=='Current')])) - 1]
full.dir <- paste('/Library/Frameworks/R.framework/Versions/',
                  previous.version,
                  '/Resources/library/',
                  sep = '')
packages <- system(paste('ls', full.dir), intern = TRUE)

lapply(X = packages, function(x){install.packages(x, type = 'source')})

update.packages(ask = FALSE)


addTaskCallback(function(expr, value, ok, visible) {
  cat('\n', as.character(Sys.time()), '\n')
  TRUE
})
removeTaskCallback(getTaskCallbackNames())