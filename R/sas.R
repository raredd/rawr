## sas helpers
# r2sas, rmacro, get_margs, sas.mget, source_sas
##

#' r2sas
#' 
#' @description
#' Write and run \code{SAS} code in \code{R}.
#' 
#' This is meant as a utility function for other \code{SAS} functions in this 
#' package, and users are \emph{not} encouraged to use \code{r2sas} to 
#' interface with \code{SAS} (while possible with this function, see examples).
#' 
#' To source an existing \code{.sas} program, see \code{\link{source_sas}}.
#' 
#' For macros, see \code{\link{get_margs}} and \code{\link{rmacro}}.
#' 
#' To convert \code{.sas7bdat} files, \code{\link{sas.mget}}.
#' 
#' @usage
#' r2sas(code, saspath, force = FALSE, out = getwd())
#' 
#' @param code character string of valid \code{SAS} code
#' @param saspath character string of directory of \code{sas.exe}; usually for
#' windows, the path \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number, is the correct directory,
#' and \code{sas.mget} defaults to this (the most recent version of \code{SAS})
#' @param force logical; by default, user must interactively allow 
#' \code{r2sas} to continue running \code{code}; set to \code{TRUE} to ignore 
#' this or for non-interactive \code{R}
#' @param out either \code{FALSE}, directory (as character string) to dump 
#' \code{.sas} program file, \code{.log} file, and any \code{SAS} output, i.e.,
#' \code{.lst} file(s)
#' 
#' @seealso
#' \code{\link{rmacro}}, \code{\link{get_margs}}, \code{\link{sas.mget}},
#' \code{\link{source_sas}}
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

r2sas <- function(code, saspath, force = FALSE, out = getwd()) {
  
  if (interactive() && !force) {
    cat(code, '\n\n\n', sep = '\n')
    check <- readline('... will be run. Continue? (y/n): ')
    if (tolower(substr(check, 1, 1)) != 'y')
      return(invisible())
  }
  
  ## define sas path
  if (missing(saspath)) {
    sashome <- 'c:/program files/sashome/sasfoundation/'
    saspath <- sprintf('%s%s/sas.exe',
                       sashome,
                       max(as.numeric(9.2, list.files(sashome)), na.rm = TRUE))
  }
  
  ## temporary .sas, .lst, .log files
  sasin   <- tempfile('_r2sas_', fileext = '.sas')
  lstpath <- tempfile('_r2sas_', fileext = '.lst')
  logpath <- tempfile('_r2sas_', fileext = '.log')
  
  ## run sas
  if (force || !interactive() || tolower(substr(check, 1, 1)) == 'y') {
    cat(code, sep = '\n', file = sasin, append = TRUE)
    sys_args <- paste(sasin, '-log', logpath, '-print', lstpath)
    status <- system2(saspath, sys_args)
  } else return(invisible())
  
  ## for annoying warning in file.copy/file.remove 
  ## if no sas output is generated from .sas program
  if (!exists(lstpath))
    lstpath <- NULL
  
  ## determine out-paths and spit error(s)
  if (out == FALSE) {
    if (status == 0) {
      on.exit(file.remove(sasin, lstpath, logpath))
      cat('\nr2sas is complete\n')
    } else stop(sprintf('error in r2sas, see log:\n%s\n', logpath))
  } else {
    if (status == 0) {
      on.exit(file.copy(c(sasin, lstpath, logpath), out))
      on.exit(file.remove(sasin, lstpath, logpath), add = TRUE)
      cat('\nr2sas is complete\n')
    } else stop(sprintf('error in r2sas, see log:\n%s/_r2sas_', out,
                        regmatches(logpath, 
                                   gregexpr('(?<=_r2sas_).*', 
                                            logpath, perl = TRUE))[[1]]))
  }
  return(invisible())
}

#' Call SAS macros
#' 
#' \code{rmacro} runs \code{SAS} macros in \code{.sas} files from \code{R}.
#' 
#' @usage
#' rmacro(mpath, mname, args, saspath, show.args = FALSE,
#'        force = FALSE, firstArgs, lastArgs, out = getwd())
#' 
#' @param mpath path to macro (\code{.sas}) file as character string
#' @param mname macro name; if missing, \code{\link{get_margs}} will search
#' \code{mpath} for macro names; if missing and \code{get_margs} finds more
#' than one macro in \code{mpath}, will throw an error
#' @param args arguments passed to the macro, separated by commas (do not 
#' include semicolons---\code{args} is passed directly as 
#' \code{\%macro(args);} ); if unsure of the macro parameters, run 
#' \code{rmacro} with \code{show.args = TRUE}
#' @param saspath character string of directory of \code{sas.exe}; usually for
#' windows, the path \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number, is the correct directory;
#' \code{mpath} defaults to the most recent version of \code{SAS}
#' @param show.args logical; if \code{TRUE}, \code{rmacro} will only return the
#' macro names and arguments found in \code{mpath}
#' @param force logical; by default, user must interactively allow 
#' \code{r2sas} to continue running \code{code}; set to \code{TRUE} to ignore 
#' this or for non-interactive \code{R}
#' @param firstArgs optional character string of (valid) \code{SAS} commands 
#' separated by semicolons to be excuted \emph{before} the macro; for example,
#' \code{'options <OPTIONS> ;'} or \code{"x 'cd c:/path/to/directory\';"}
#' @param lastArgs optional commands to be executed \emph{after} the macro; see
#' \code{firstArgs}
#' @param out either \code{FALSE}, directory (as character string) to dump 
#' \code{.sas} program file, \code{.log} file, and any \code{SAS} output, i.e.,
#' \code{.lst} file(s)
#' 
#' @seealso 
#' \code{\link{get_margs}}, \code{\link{r2sas}}, \code{\link{sas.mget}}
#' \code{\link{source_sas}}
#' 
#' @examples
#' \dontrun{
#' rmacro('./tests/testfiles/onemacro.sas',
#'        args = 'arg1 = 1, arg2 = 2',
#'        firstArgs = 'options nodate no center ; x \'cd ~/desktop\';',
#'        lastArgs = 'endsas;')
#'        
#' ## options nodate no center  ; 
#' ## x 'cd ~/desktop' ; 
#' ## 
#' ## %include "./tests/testfiles/onemacro.sas" ;
#' ## %macro1(arg1 = 1, arg2 = 2) ;
#' ## endsas ; 
#' ## 
#' ## 
#' ## 
#' ## 
#' ## 
#' ## 
#' ## ... will be run. Continue? (y/n): 
#' }
#' 
#' @export

rmacro <- function(mpath, mname, args, saspath, show.args = FALSE,
                   force = FALSE, firstArgs, lastArgs, out = getwd()) {
  
  margs <- get_margs(mpath, mname)
  if (missing(mname))
    mname <- names(margs)
  if (show.args)
    return(margs)

  if (length(mname) > 1)
    stop('run one macro per rmacro call\n')
  
  ## create .sas script to call macro
  if (!missing(firstArgs))
    firstArgs <- gsub(';', ' ; \n', firstArgs)
  else firstArgs <- '\n'
  if (!missing(lastArgs))
    lastArgs <- gsub(';', ' ; \n', lastArgs)
  else lastArgs <- '\n'
  sass <- c(sprintf('%%include \"%s\" ;', mpath),
            sprintf('%%%s(%s) ;', mname, args))
  
  r2sas(code = paste(c(firstArgs, sass, lastArgs), sep = '\n'),
        saspath = saspath, force = force, out = out)
}

#' Get arguments from SAS macros
#' 
#' Reads a text file (usually \code{.sas}) and extracts \code{SAS} macro names
#' and parameters with any default values; see tests in examples below.
#' 
#' @usage
#' get_margs(mpath, mname)
#' 
#' @param mpath character string of path to \code{.sas} file
#' @param mname macro name in \code{mpath} of interest; if missing, returns
#' all macros found
#' 
#' @return
#' A list with macro names in \code{mpath} and their respective parameters.
#' 
#' @seealso
#' \code{\link{rmacro}}, \code{\link{r2sas}}, \code{\link{sas.mget}},
#' \code{\link{source_sas}}
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
#' ## [1] "arg1=1, arg2=2, arg3=3"
#' ##
#' ## $macro3
#' ## [1] "arg1=, arg2="
#' ##
#' ## $macro4
#' ## [1] "this=, macro=, has=, many=, params=, on=, multiple=, lines="
#' ##
#' ## $macro5
#' ## [1] ""this=, macro=, has=, comments=, between=, each=, parameter="
#' 
#' get_margs('./tests/testfiles/macros.sas', 'macro1')
#' 
#' ## $macro1
#' ## [1] "arg1, arg2"
#' 
#' get_margs('./tests/testfiles/macros.sas', 'no_macro_with_this_name')
#' 
#' Error in get_margs("./tests/testfiles/macros.sas", "no_macro_with_this_name") : 
#'  no_macro_with_this_name macro not found in ./tests/testfiles/macros.sas
#' 
#' get_margs('./tests/testfiles/nomacro.sas')
#' 
#' ## Error in get_margs("./tests/testfiles/nomacro.sas") : 
#' ##   no valid macros found in ./tests/testfiles/nomacro.sas
#' 
#' get_margs('./tests/testfiles/onemacro.sas', c('macro2', 'macro5'))
#' 
#' ## Error in get_margs("./tests/testfiles/onemacro.sas", c("macro2", "macro5")) : 
#' ## macro2, macro5 not found in ./tests/testfiles/onemacro.sas
#' }
#' 
#' @export

get_margs <- function(mpath, mname) {
  
  macro <- readLines(con <- file(mpath), warn = FALSE)
  close(con)
  
  ## ignore everything between /* */, collapse, 
  ## then split lines by semicolons
  macro <- paste(macro, collapse = ' ')
  macro <- gsub('/\\*[^/\\*]*?\\*/', '', macro, perl = TRUE)
  macro <- gsub('\\s+', ' ', macro)
  macro <- gsub(';', ';$$$;', macro)
  macro <- strsplit(macro, split = '\\$\\$\\$;')
  
  ## match the macro syntax: " %macro name( ) ; "
  ## and trim whitespace
  mcall <- unlist(lapply(macro, function(x)
    regmatches(x, gregexpr('\\s*%macro\\s+(\\w+)\\((.*)\\)\\s*;{1}', x, 
                           perl = TRUE))))
  mnames <- gsub('%macro|;|(?<=\\().*?(?=\\))|\\(|\\)|\\s*', '',
                 mcall, perl = TRUE)
  args <- gsub(' ','', regmatches(mcall, gregexpr('(?<=\\().*?(?=\\))', 
                                                  mcall, perl = TRUE)))
  
  if (length(mnames) < 1)
    stop(sprintf('no valid macros found in %s\n', mpath))
  if (!missing(mname) && any(mname %ni% mnames))
    stop(sprintf('%s not found in %s', 
                 paste(mname[mname %ni% mnames], collapse = ', '), mpath))
  margs <- setNames(gsub(',',', ', args), mnames)
  return(as.list(margs)[mname])
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
#' @param libpath directory to data set(s) as character string; if missing, 
#' searches the current working directory
#' @param dsn data set name(s); either \code{data1} or \code{data1.sas7bdat} 
#' will work; if missing or \code{NULL}, all \code{.sas7bdat} files found in
#' \code{libpath} will be read
#' @param saspath directory to \code{sas.exe} as character string; usually for
#' windows, the path \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number, is the correct directory,
#' and \code{sas.mget} defaults to this (the most recent version of \code{SAS})
#' @param fmtpath path to a format \code{.sas} file as character string; 
#' \code{SAS} throws an error if the host used to make the format catalog was
#' not on the same platform, so we create a new format catalog either by
#' running a \code{proc format} macro or by making a copy of the non-native
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
#' @seealso
#' \code{\link{rmacro}}, \code{\link{get_margs}}, \code{\link{r2sas}},
#' \code{\link{source_sas}}
#' 
#' @examples
#' \dontrun{
#' data.list <- sas.mget('./tests/testfiles/')
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

sas.mget <- function(libpath, dsn, saspath, fmtpath, catalog = FALSE, 
                     log.file, ..., force = FALSE) {
  
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
  dcf <- list.files(libpath, pattern = '.sas7bcat')
  if (length(dcf) > 1)
    stop('only one format catalog is allowed per SAS directory\n')
  if (catalog && length(dcf) == 0) {
    warning(sprintf('no format catalog found in %s\n\nignoring formats', 
                    libpath))
    no.formats <- TRUE
  }
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
  ## // initial error checks
  
  ## create formats native to host
  ## sas doesn't seem to like using unix format catalogs
  ## so we have to make a copy using windows
  ## to do so, user must specify the .sas macro (INFORM)
  ## or a proc format .sas file
  if (!catalog) {
    if (!missing(fmtpath)) {
      log.fmt <- sprintf('%s/_temp_fmt_.log', libpath)
      sass <- c(sprintf('x \"cd %s\" ;', libpath),
                sprintf('libname tmp \"%s\" ;', libpath),
                sprintf('%%include \"%s\" ;', fmtpath),
                'proc catalog catalog =  work.formats ;',
                'copy out = tmp.formats ;',
                'quit ;')
      sasin <- paste0(libpath, '/tmp.sas')
      on.exit(unlink(sasin), add = TRUE)
      cat(sass, sep = '\n', file = sasin, append = TRUE)
      sys_args <- paste(sasin, '-log', log.fmt)
      status <- system2(saspath, sys_args)
      
      if (status != 0) {
        warning('error in getting formats; formatting ignored\n')
        cat('see log, %s\n', log.fmt)
      } else no.formats <- FALSE
    } else warning('no formats specified, ignoring formats\n')
  } else {
    warning(sprintf('%s is being used for formats\n', dcf),
            'note that sas.get will throw erros if catalog is non native')
    no.format <- FALSE
  }
  
  ## sas.get wrapper
  if (force || !interactive() || tolower(substr(check, 1L, 1L)) == 'y') {
    dsn <- p(dsn)
    zzz <- setNames(lapply(dsn, function(x) 
      Hmisc::sas.get(libraryName = libpath, member = x, sasprog = saspath,
                     log.file = log.file, formats = no.format, ...)), dsn)
    
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
#' @param path path to \code{.sas} file as character string
#' @param ... additional parameters passed to \code{\link{r2sas}}
#' 
#' @seealso
#' \code{\link{r2sas}}, \code{\link{rmacro}}, \code{\link{get_margs}},
#' \code{\link{sas.mget}}
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
  r2sas(code = paste(sas, sep = '\n'), ...)
}
