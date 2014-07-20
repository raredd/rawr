## sas helpers
# rmacro, get_margs, sas.mget, source_sas, r2sas
##

#' Call SAS macros
#' 
#' \code{rmacro} runs \code{SAS} macros in \code{*.sas} files from \code{R}; 
#' currently only for windows platforms.
#' 
#' @usage
#' rmacro(mpath, mname, args, saspath, show.args, log.file = TRUE, moreArgs)
#' 
#' @param mpath character string of path to \code{macro.sas} file
#' @param mname macro name; if missing, \code{\link{get_margs}} will search
#' \code{mpath} for macro names; if missing and \code{get_margs} finds more
#' than one macro in \code{mpath}, will throw an error
#' @param args arguments passed to the macro; if unsure of arguments, simply 
#' run \code{rmacro} with \code{show.args = TRUE}
#' @param saspath character string of directory of \code{sas.exe}; usually for
#' windows, the path \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number, is the correct directory,
#' and \code{mpath} defaults to this (the most recent version of \code{SAS})
#' @param show.args logical; return macro names and arguments in \code{mpath}
#' @param log.file logical; if \code{TRUE} (default), keeps a \code{SAS} log
#' file (\code{_temp_.log}) in the \code{mpath} directory
#' @param moreArgs character string of additional \code{SAS} (legal) commands 
#' to use separated by semicolons; for example, \code{'options <OPTIONS> ;'} or
#' \code{"x 'cd c:/path/to/new/directory ;'"}
#' 
#' @export

rmacro <- function(mpath, mname, args, saspath, show.args = FALSE, 
                   log.file = TRUE, moreArgs) {
  
  margs <- get_margs(mpath, mname)
  if (missing(mname))
    mname <- names(margs)
  if (show.args)
    return(margs)
  
  ## define sas path
  if (missing(saspath)) {
    sashome <- 'c:/program files/sashome/sasfoundation/'
    saspath <- sprintf('%s%s/sas.exe',
                       sashome,
                       max(as.numeric(list.files(sashome)), na.rm = TRUE))
  }
  
  ## create log file in mpath directory
  logpath <- sprintf('%s/_temp_.log', mpath)
  
  if (length(mname) > 1)
    stop('run one macro per rmacro call')
  
  ## create .sas script to call macro
  if (!missing(moreArgs))
    moreArgs <- gsub(';', ' ; \n', moreArgs)
  else moreArgs <- '\n'
  sass <- c(paste0('%include ', mpath, ' ;'),
            paste0('%', sprintf("%s(%s) ;", mname, args)))
  
  sasin <- paste0(mpath, '/tmp.sas')
  on.exit(unlink(sasin))
  
  cat(c(moreArgs, sass), sep = '\n', file = sasin, append = TRUE)
  sys_args <- paste(sasin, '-log', log.path)
  status <- system2(saspath, sys_args)
  
  if (status != 0)
    warning(sprintf('error in rmacro; see log, %s\n', log.path))
  else cat(sprintf('\n%s macro is complete\n', mname))
  if (!log.file && status == 0)
    on.exit(unlink(log.path))
  else cat(sprintf('see log, %s\n', log.file))
  return(invisible())
}

#' Get arguments from SAS macros
#' 
#' Reads a text file (usually \code{.sas}) and extracts \code{SAS} macro names
#' and parameter names with any default values; see tests in examples below.
#' 
#' @usage
#' get_margs(mpath, mname)
#' 
#' @param mpath character string of path to \code{.sas} file
#' @param mname macro name in \code{mpath} of interest; if missing, returns
#' all macros found
#' 
#' @return
#' A list with macro names in \code{mpath} and their respective arguments.
#' 
#' @examples
#' \dontrun{
#' ## tests
#' 
#' get_margs('./tests/testfiles/macros.sas')
#' 
#' ## $macro1
#' ## [1] "arg1, arg2"
#' ## 
#' ## $macro2
#' ## [1] "arg1 = 1, arg2 = 2, arg3 = 3"
#' ## 
#' ## $macro3
#' ## [1] "arg1=,arg2="
#' ## 
#' ## $macro4
#' ## [1] "this=, macro=, has=, many=, params=, on=, multiple=, lines="
#' 
#' get_margs('./tests/testfiles/macros.sas', 'macro1')
#' 
#' ## $macro1
#' ## [1] "arg1, arg2"
#' 
#' get_margs('./tests/testfiles/macros.sas', 'no_macro_with_this_name')
#' 
#' ## Error in get_margs("./tests/testfiles/macros.sas", "macro") : 
#' ##   check macro name and .sas file for consistency
#' 
#' get_margs('./tests/testfiles/nomacro.sas')
#' 
#' ## Error in get_margs("./tests/testfiles/nomacro.sas") : 
#' ##   no valid macros found in ./tests/testfiles/nomacro.sas
#' 
#' get_margs('./tests/testfiles/onemacro.sas')
#' 
#' ## $macro1
#' ## [1] "arg1, arg2"
#' }
#' 
#' @export

get_margs <- function(mpath, mname) {
  
  macro <- readLines(con <- file(mpath), warn = FALSE)
  close(con)
  
  ## extract lines with '%macro' and ignore after ';'
  mcall <- macro[grep('%macro', macro, ignore.case = TRUE)]
  mcall <- gsub(';.*','', mcall)
  if (length(mcall) < 1L)
    stop(sprintf('no valid macros found in %s\n', mpath))

  ## added for macros with params defined on multiple lines
  ## ugly but works
  idx <- grep('%macro', macro, ignore.case = TRUE)
  idx2 <- grep(';', macro)
  sidx <- sort(c(idx, idx2))
  tmp <- NULL
  sapply(idx, function(x) {
    if (!grepl(';', macro[x]))
      tmp <<- paste0(macro[x:(min(sidx[sidx > x]))], collapse = '')
  })
  mcall <- c(mcall, tmp)
  
  ## extract parentheses data and ignore %macro 
  args <- regmatches(mcall, gregexpr("(?<=\\().*?(?=\\))", mcall, perl = TRUE))
  mcall <- mcall[which(sapply(args, nchar) > 0)]
  args <- args[which(sapply(args, nchar) > 0)]
  
  mnames <- sapply(1:length(mcall), function(x)
    gsub(sprintf('\\(|\\)|%s|;.*', args[[x]]), '', 
         mcall[x], ignore.case = TRUE))
  
  args <- lapply(args, function(x) gsub('\\s+', ' ', x))
  args <- setNames(args, mnames <- gsub('%macro| ', '', 
                                        mnames, ignore.case = TRUE))
  idx <- which(mnames %ni% make.names(mnames))
  
  if (length(idx) > 0)
    args <- args[-idx]
  
  if (!missing(mname)) {
    if (mname %ni% names(args))
      stop(sprintf('%s macro not found in %s\n', mname, mpath))
    else 
      args <- args[mname]
  }
  return(args)
}

#' Convert multiple SAS data sets to R data frame
#' 
#' User-friendly wrapper of \code{\link[Hmisc]{sas.get}} to convert one or more
#' \code{SAS} data sets (\code{.sas7bdat} files) into a list of \code{R} data 
#' frames.
#' 
#' @usage
#' sas.mget(libpath, dsn, saspath, fmtpath, log.file, ..., force = FALSE)
#' 
#' @param libpath character string of directory of data set(s)
#' @param dsn data set name(s); either \code{data1} or \code{data1.sas7bdat} 
#' will work; if missing or \code{NULL}, will get all \code{.sas7bdat} files
#' @param saspath character string of directory of \code{sas.exe}; usually for
#' windows, the path \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number, is the correct directory,
#' and \code{sas.mget} defaults to this (the most recent version of \code{SAS})
#' @param fmtpath character string of path to a format \code{.sas} file; 
#' \code{sas} throws an error if the host used to make the format catalog does
#' not use the same platform; this creates a new format catalog either by
#' running a \code{proc format} macro or by making a copy of the non native
#' \code{.sas7bcat} file
#' @param log.file name of \code{SAS} log file; default value will create 
#' \code{_temp_.log} in the \code{libpath} directory
#' @param ... additional arguments passed to \code{\link[Hmisc]{sas.get}}
#' @param force logical; by default, user must interactively allow 
#' \code{sas.mget} to continue reading all data sets; set to \code{TRUE} to
#' ignore this or for non-interactive \code{R}
#' 
#' @return
#' A list of data frames resembling the \code{SAS} data sets.
#' 
#' @examples
#' \dontrun{
#' sas.mget('./tests/testfiles/')
#' 
#' ## !!! Two data set(s) will be read !!!
#' ## 
#' ##  Size in Kb:
#' ##   
#' ##       size (Kb)
#' ## data1         0
#' ## data2         0
#' ## 
#' ## 
#' ## 
#' ## 
#' ## 
#' ## do you want to continue (y/n)? 
#' }
#' 
#' @export

sas.mget <- function(libpath, dsn, saspath, fmtpath, log.file, ..., 
                     force = FALSE) {
  
  suppressPackageStartupMessages(require(Hmisc))
  oo <- options(stringsAsFactors = FALSE)
  on.exit(options(oo))
  
  ## error checks
  if (missing(log.file))
    log.file <- '_temp_.log'
  if (missing(libpath))
    libpath <- '.'
  if (missing(saspath)) {
    sashome <- 'c:/program files/sashome/sasfoundation/'
    saspath <- sprintf('%s%s/sas.exe',
                       sashome,
                       max(as.numeric(9.2, list.files(sashome)), na.rm = TRUE))
  }
  
  dsf <- list.files(libpath, pattern = '.sas7bdat')
  dsi <- `colnames<-`(round(file.info(list.files(libpath, 
                                                 full.names = TRUE))['size'] / 
                              1000), 'size (Kb)')
  p <- function(x) gsub('.sas7bdat', '', x)
  
  ## check if data exists
  if (missing(dsn) || is.null(dsn)) {
    dsn <- dsf
    if (length(dsn) == 0)
      stop(sprintf('no sas data sets found in %s\n', libpath))
  } else
    if (!all(p(dsn) %in% c(dsf, p(dsf))))
      stop(sprintf('data set(s) not found in %s: %s\n', libpath, 
                   paste0(p(dsn)[p(dsn) %ni% p(dsf)], collapse = ', ')))
  
  ## final warning for reading all dsn
  if (interactive() && !force) {
    cat(sprintf('\n!!! %s data set(s) will be read !!!\n\n Size in Kb:\n\n', 
                num2char(length(dsn))))
    print(`rownames<-`(dsi[which(p(rownames(dsi)) %in% 
                                   paste0(libpath, '/', p(dsn))), , drop = FALSE],
                       p(dsn)))
    cat('\n\n\n\n\n')
    check <- readline('Do you want to continue? (y/n): ')
    if (tolower(substr(check, 1L, 1L)) != 'y')
      return(invisible())
  }
  
  ## create formats native to host
  ## sas doesn't seem to like using unix format catalogs
  ## so we have to make a copy using windows
  ## to do so, user must specify the .sas macro (INFORM)
  ## or a proc format .sas file
  if (!missing(fmtpath)) {
    log.fmt <- sprintf('%s/_temp_fmt_.log', libpath)
    sass <- c(sprintf('x \'cd %s\' ;', libpath),
              sprintf('libname tmp \'%s\' ;', libpath),
              paste0('%include \'', fmtpath, '\' ;'),
              'proc catalog catalog =  work.formats ;',
              'copy out = tmp.formats ;',
              'quit ;')
    sasin <- paste0(libpath, '/tmp.sas')
    on.exit(unlink(sasin))
    cat(sass, sep = '\n', file = sasin, append = TRUE)
    sys_args <- paste(sasin, '-log', log.fmt)
    status <- system2(saspath, sys_args)
    
    if (status != 0) {
      warning('error in getting formats; formatting ignored\n')
      cat('see log, %s\n', log.fmt)
    }
  }
  
  ## sas.get wrapper
  if (force || !interactive() || tolower(substr(check, 1L, 1L)) == 'y') {
    dsn <- p(dsn)
    zzz <- setNames(lapply(dsn, function(x) 
      Hmisc::sas.get(libraryName = libpath, member = x, sasprog = saspath,
                     log.file = log.file, ...)), dsn)
    
    ## print dims for user
    cat('\nread summary:\n\n')
    dims <- sapply(zzz, dim)
    print(`rownames<-`(dims, c('rows','columns')))
    
    cat('\nsee log, %s\n', log.file)
    
    return(zzz)
  } else return(invisible())
}

#' Source SAS code
#' 
#' \code{\link{source}}-esque function for \code{.sas} files
#' 
#' @usage
#' source_sas(path, ...)
#' 
#' @param path character string path to \code{.sas} file
#' @param ... additional parameters passed to \code{\link{r2sas}}
#' 
#' @examples
#' \dontrun{
#' source_sas('./tests/testfiles/onemacro.sas')
#' 
#' ## %macro macro1(arg1, arg2);
#' ## data data;
#' ##   set data;
#' ## run;
#' ## %mend macro1;
#' ##
#' ##
#' ##
#' ## ... will be run. Continue? (y/n): 
#' }
#' 
#' @export

source_sas <- function(path, ...) {
  sas <- readLines(con <- file(path), warn = FALSE)
  close(con)
  r2sas(code = cat(sas, sep = '\n'), ...)
}

#' r2sas
#' 
#' Write and run \code{SAS} code in \code{R}. To run an existing \code{.sas}
#' file, see \code{\link{source_sas}}.
#' 
#' @usage
#' r2sas(code, saspath, log.file = TRUE, force = FALSE)
#' 
#' @param code character string of valid \code{SAS} code
#' @param saspath character string of directory of \code{sas.exe}; usually for
#' windows, the path \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number, is the correct directory,
#' and \code{sas.mget} defaults to this (the most recent version of \code{SAS})
#' @param log.file logical; if \code{TRUE} (default), keeps a \code{SAS} log
#' file (\code{_temp_.log}) in the current working directory
#' @param force logical; by default, user must interactively allow 
#' \code{r2sas} to continue running \code{code}; set to \code{TRUE} to ignore 
#' this or for non-interactive \code{R}
#' 
#' @examples
#' \dontrun{
#' code <- "
#' * this is a sas program file : ;
#' 
#' options nodate nocenter nonumber ;
#' 
#' x 'cd ./newfolder' ;
#' 
#' libname lib './newfolder' ;
#' 
#' data data ;
#'   set data ;
#' if x = 1 then delete ;
#' run ;
#' "
#' 
#' r2sas(code)
#' 
#' ## * this is a sas program file : ;
#' ## 
#' ## options nodate nocenter nonumber ;
#' ## 
#' ## x 'cd ./newfolder' ;
#' ## 
#' ## libname lib './newfolder' ;
#' ## 
#' ## data data ;
#' ##   set data ;
#' ## if x = 1 then delete ;
#' ## run ;
#' ## 
#' ## 
#' ## 
#' ## ... will be run. Continue? (y/n): 
#' }
#' @export

r2sas <- function(code, saspath, log.file = TRUE, force = FALSE) {
  
  if (interactive() && !force) {
    cat(code, '\n\n\n')
    check <- readline('... will be run. Continue? (y/n): ')
    if (tolower(substr(check, 1L, 1L)) != 'y')
      return(invisible())
  }
  
  ## define sas path
  if (missing(saspath)) {
    sashome <- 'c:/program files/sashome/sasfoundation/'
    saspath <- sprintf('%s%s/sas.exe',
                       sashome,
                       max(as.numeric(list.files(sashome)), na.rm = TRUE))
  }
  
  ## create log file in current working directory
  logpath <- sprintf('%s/_temp_.log', getwd())
  
  sasin <- tempfile('_r2sas_', fileext = '.sas')
  on.exit(unlink(sasin))
  
  if (force || !interactive() || tolower(substr(check, 1L, 1L)) == 'y') {
    cat(code, file = sasin, append = TRUE)
    sys_args <- paste(sasin, '-log', log.file)
    status <- system2(saspath, sys_args)
  } else return(invisible())
  
  if (status != 0)
    warning(sprintf('error in r2sas; see %s', log.path))
  else cat('\nr2sas is complete\n')
  if (!log.file && status == 0)
    on.exit(unlink(log.path))
  else cat('see log, %s\n', log.path)
  return(invisible())
}
