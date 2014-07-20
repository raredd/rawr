## sas helpers
# rmacro, get_margs, sas.mget
##

#' Call SAS macros
#' 
#' \code{rmacro} runs \code{SAS} macros in \code{*.sas} files from \code{R}; 
#' currently only for windows platforms.
#' 
#' @usage
#' rmacro(mpath, mname, args, saspath, show.args, log.file, moreArgs)
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
#' @param log.file character string of directory to create \code{_temp_.log},
#' the log file for the \code{SAS} call; defaults to the \code{mpath} directory
#' @param moreArgs character string of additional \code{SAS} (legal) commands 
#' to use separated by semicolons; for example, \code{'options <OPTIONS> ;'} or
#' \code{"x 'cd c:/path/to/new/directory ;'"}
#' 
#' @export

rmacro <- function(mpath, mname, args, saspath, show.args = FALSE, log.file,
                   moreArgs) {
  
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
  if (missing(log.file))
    log.file <- sprintf('%s/_temp_.log', mpath)
  
  ## create .sas script to call macro
  if (length(mname) > 1)
    stop('run one macro per rmacro call')
  
  if (!missing(moreArgs))
    moreArgs <- gsub(';', ' ; \n', moreArgs)
  else moreArgs <- '\n'
  sass <- c(paste0('%include ', mpath, ' ;'),
            paste0('%', sprintf("%s(%s) ;", mname, args)))
  
  sasin <- paste0(mpath, '/tmp.sas')
  on.exit(unlink(sasin))
  
  cat(c(moreArgs, sass), sep = '\n', file = sasin, append = TRUE)
  sys_args <- paste(sasin, '-log', log.file)
  status <- system2(saspath, sys_args)
  
  if (status != 0L)
    warning('error in getting formats; formatting ignored')
  else cat('\n%s macro is complete\n', mname)
  return()
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
    stop(sprintf('no valid macros found in %s', mpath))

  ## test
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
  ## //test
  
  ## extract parentheses data and ignore %macro 
  args <- regmatches(mcall, gregexpr("(?<=\\().*?(?=\\))", mcall, perl = TRUE))
  mcall <- mcall[which(sapply(args, nchar) > 0L)]
  args <- args[which(sapply(args, nchar) > 0L)]
  
  mnames <- sapply(1:length(mcall), function(x)
    gsub(sprintf('\\(|\\)|%s|;.*', args[[x]]), '', 
         mcall[x], ignore.case = TRUE))
  
  args <- lapply(args, function(x) gsub('\\s+', ' ', x))
  args <- setNames(args, mnames <- gsub('%macro| ', '', 
                                        mnames, ignore.case = TRUE))
  idx <- which(mnames %ni% make.names(mnames))
  
  if (length(idx) > 0L)
    args <- args[-idx]
  
  if (!missing(mname)) {
    if (mname %ni% names(args))
      stop('check macro name and .sas file for consistency')
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
#' @param log.file character string of directory to create \code{_temp_.log},
#' the log file for the \code{SAS} call; defaults to \code{libpath} directory
#' @param ... additional arguments passed to \code{\link[Hmisc]{sas.get}}
#' @param force logical; by default, user must interactively allow 
#' \code{sas.mget} to continue reading all data sets; set to \code{TRUE} to
#' ignore this or for non-interactive use
#' 
#' @return
#' A list of data frames resembling the \code{SAS} data sets.
#' 
#' @export

sas.mget <- function(libpath, dsn, saspath, fmtpath, log.file, ..., 
                     force = FALSE) {
  
  suppressPackageStartupMessages(require(Hmisc))
  oo <- options(stringsAsFactors = FALSE)
  on.exit(options(oo))
  
  ## error checks
  if (missing(libpath))
    libpath <- '.'
  if (missing(saspath)) {
    sashome <- 'c:/program files/sashome/sasfoundation/'
    saspath <- sprintf('%s%s/sas.exe',
                       sashome,
                       max(as.numeric(list.files(sashome)), na.rm = TRUE))
  }
  
  dsf <- list.files(libpath, pattern = '.sas7bdat')
  dsi <- `colnames<-`(round(file.info(list.files(libpath, 
                                                 full.names = TRUE))['size'] / 
                              1000L), 'size (Kb)')
  p <- function(x) gsub('.sas7bdat', '', x)
  
  ## check if data exists
  if (missing(dsn) || is.null(dsn)) {
    dsn <- dsf
    if (length(dsn) == 0L)
      stop(sprintf('no sas data sets found in %s', libpath))
  } else
    if (!all(p(dsn) %in% c(dsf, p(dsf))))
      stop(sprintf('data set(s) not found in %s: %s', libpath, 
                   paste0(p(dsn)[p(dsn) %ni% p(dsf)], collapse = ', ')))
  
  ## create log file in libpath directory
  if (missing(log.file))
    log.file <- sprintf('%s/_temp_.log', libpath)
  
  ## final warning for reading all dsn
  if (!force) {
    cat(sprintf('\n!!! %s data set(s) will be read !!!\n\n Size in Kb:\n\n', 
                num2char(length(dsn))))
    print(`rownames<-`(dsi[which(p(rownames(dsi)) %in% 
                                   paste0(libpath, '/', p(dsn))), , drop = FALSE],
                       p(dsn)))
    cat('\n\n\n\n\n')
    check <- readline('do you want to continue (y/n)? ')
  }
  
  ## create formats native to host
  ## sas doesn't seem to like using unix format catalogs
  ## so we have to make a copy using windows
  ## to do so, user must specify the .sas macro (INFORM)
  ## or a proc format .sas file
  if (!missing(fmtpath)) {
    sass <- c(sprintf('x \'cd %s\' ;', libpath),
              sprintf('libname tmp \'%s\' ;', libpath),
              paste0('%include \'', fmtpath, '\' ;'),
              'proc catalog catalog =  work.formats ;',
              'copy out = tmp.formats ;',
              'quit ;')
    sasin <- paste0(libpath, '/tmp.sas')
    on.exit(unlink(sasin))
    cat(sass, sep = '\n', file = sasin, append = TRUE)
    args <- paste(sasin, '-log', log.file)
    status <- system2(saspath, args)
    
    if (status != 0L)
      warning('error in getting formats; formatting ignored')
  }
  
  ## sas.get wrapper
  if (tolower(substr(check, 1L, 1L)) == 'y') {
    dsn <- p(dsn)
    zzz <- setNames(lapply(dsn, function(x) 
      Hmisc::sas.get(libraryName = libpath, member = x, sasprog = saspath,
                     log.file = log.file, ...)), dsn)
    
    ## print dims for user
    cat('\nread summary:\n\n')
    dims <- sapply(zzz, dim)
    print(`rownames<-`(dims, c('rows','columns')))
    
    return(zzz)
  } else
    return()
}
