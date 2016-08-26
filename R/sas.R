## sas helpers
# sas_path, r2sas, rmacro, get_margs, sas_mget, source_sas, parse_formats,
# parse_formats2, apply_formats, sas_catalog, move_formats
##


#' Get \code{SAS} path
#' 
#' @description
#' Find versions of \code{SAS} installed and return path to latest executable
#' version by default.
#' 
#' For windows usually \code{c:/program files/sashome/sasfoundation/x.x/sas.exe},
#' where \code{x.x} is the \code{SAS} version number is the correct directory.
#' For v9.2, the default \code{c:/program files/sasfoundation/9.2/sas.exe} is
#' used. For linux/unix platforms, \code{sas} must be in the \code{$PATH}, or
#' it will be necessary to give \code{saspath}.
#' 
#' For previous versions or if \code{SAS} is not installed in the default
#' directory, it will be necessary to give a full \code{saspath} to the
#' executable.
#' 
#' @param saspath (optional) full path to sas executable; if given, all other
#' arguments are ignored, and \code{saspath} is only checked for existence
#' @param sashome (optional) full path to directory of version directories
#' @param version (optional) version to use
#' 
#' @seealso
#' \code{\link{r2sas}}, \code{\link{rmacro}}, \code{\link{get_margs}},
#' \code{\link{sas_mget}}, \code{\link{source_sas}},
#' \code{\link{parse_formats}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' \dontrun{
#' sas_path()
#' sas_path('/usr/local/SAS_9.2/sas')
#' }
#' 
#' @export

sas_path <- function(saspath, sashome, version) {
  sashome <- file.path('c:', 'program files', 'sashome', 'sasfoundation')
  version <- if (missing(version))
    c(9.3, list.files(sashome)) else as.character(version)
  version <- numeric_version(version)
  if (missing(saspath)) {
    saspath <- if (.Platform$OS.type %in% 'windows') {
      saspath <- file.path(sashome, max(version, na.rm = TRUE), 'sas.exe')
      ## try 9.2 default location if the above does not work
      if (!file.exists(saspath))
        file.path('c:', 'program files', 'sas', 'sasfoundation',
                  '9.2', 'sas.exe') else saspath
    } else suppressWarnings(system2('which', 'sas', stdout = TRUE))
  }
  if (!length(saspath) || !file.exists(saspath))
    stop('\'saspath\' is invalid -- give full path to the sas executable',
         domain = NA)
  saspath
}

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
#' To convert \code{.sas7bdat} files, \code{\link{sas_mget}}.
#' 
#' @param code character string of valid \code{SAS} code
#' @param saspath file path to sas executable as character string, passed to
#' \code{\link{sas_path}}
#' @param force logical; by default, user must interactively allow
#' \code{r2sas} to continue running \code{code}; set to \code{TRUE} to ignore
#' this or for non-interactive \code{R}
#' @param out either \code{FALSE}, directory (as character string) to dump
#' \code{.sas} program file, \code{.log} file, and any \code{SAS} output, i.e.,
#' \code{.lst} file(s)
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{rmacro}}, \code{\link{get_margs}},
#' \code{\link{sas_mget}}, \code{\link{source_sas}},
#' \code{\link{parse_formats}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' \dontrun{
#' code <- "
#' * this is a sas program file :;
#' 
#' options nodate nocenter nonumber;
#' 
#' x 'cd ./newfolder';
#' 
#' libname lib './newfolder';
#' 
#' data data;
#'   input id x y;
#'   datalines;
#'   1 1 1
#'   2 2 2
#'   3 1 3
#'   ;
#' run;
#' 
#' data data;
#'   set data;
#'   if x = 1 then delete;
#' run;
#' "
#' 
#' r2sas(code, sas_path(), FALSE, getwd())
#' 
#' ## * this is a sas program file :;
#' ## 
#' ## options nodate nocenter nonumber;
#' ## 
#' ## x 'cd ./newfolder';
#' ## 
#' ## libname lib './newfolder';
#' ##
#' ## data data;
#' ##   input id x y;
#' ##   datalines;
#' ##   1 1 1
#' ##   2 2 2
#' ##   3 1 3
#' ##   ;
#' ## run;
#' ## 
#' ## data data;
#' ##   set data;
#' ##   if x = 1 then delete;
#' ## run;
#' ## 
#' ## 
#' ## 
#' ## ... will be run. Continue? (y/n): 
#' }
#' 
#' @export

r2sas <- function(code, saspath, force, out) {
  if (interactive() && !force) {
    cat(code, '\n\n\n', sep = '\n')
    check <- readline('... will be run. Continue? (y/n): ')
    if (tolower(substr(check, 1, 1)) != 'y')
      return(invisible())
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
  
  ## determine out-paths and spit error(s) for annoying warning in
  ## file.copy/file.remove if no output is generated from .sas program
  if (!file.exists(lstpath)) {
    message('NOTE: .lst file not created\n',
            'check .sas program if output was expected\n', domain = NA)
    lstpath <- NULL
  }
  if (!file.exists(logpath)) {
    message('NOTE: .log file not created\n', 'check sas program for errors\n',
            domain = NA)
    logpath <- NULL
  }
  
  if (identical(out, FALSE)) {
    if (status == 0L) {
      on.exit(file.remove(sasin, lstpath, logpath))
      cat('\nr2sas is complete\n')
    } else stop(sprintf('error in r2sas, see log:\n%s\n', logpath),
                domain = NA)
  } else {
    if (status == 0L) {
      on.exit(file.copy(c(sasin, lstpath, logpath), out))
      on.exit(file.remove(sasin, lstpath, logpath), add = TRUE)
      cat('\nr2sas is complete\n')
    } else stop(paste('error in r2sas, see log:',
                      shQuote(file.path(out, basename(logpath)))),
                domain = NA)
  }
  invisible()
}

#' Call \code{SAS} macros
#' 
#' \code{rmacro} runs \code{SAS} macros in \code{.sas} files from \code{R}.
#' 
#' @param path path to macro (\code{.sas}) file as character string
#' @param name macro name; if missing, \code{\link{get_margs}} will search
#' \code{path} for macro names; if missing and \code{get_margs} finds more
#' than one macro in \code{path}, will throw an error
#' @param args arguments passed to the macro, separated by commas (do not
#' include semicolons---\code{args} is passed directly as
#' \code{\%macro(args);} ); if unsure of the macro parameters, run
#' \code{rmacro} with \code{show.args = TRUE}
#' @param saspath file path to sas executable as character string, passed to
#' \code{\link{sas_path}}
#' @param show.args logical; if \code{TRUE}, \code{rmacro} will only return
#' the macro names and arguments found in \code{path}
#' @param force logical; by default, user must interactively allow
#' \code{r2sas} to continue running \code{code}; set to \code{TRUE} to ignore
#' this or for non-interactive \code{R}
#' @param firstArgs (optional) character string of (valid) \code{SAS} commands
#' separated by semicolons to be excuted \emph{before} the macro; for example,
#' \code{options <OPTIONS>;} or \code{x "cd ..";}
#' @param lastArgs (optional) commands to be executed \emph{after} the macro;
#' see \code{firstArgs}
#' @param out either \code{FALSE}, directory (as character string) to dump
#' \code{.sas} program file, \code{.log} file, and any \code{SAS} output,
#' i.e., \code{.lst} file(s)
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{get_margs}}, \code{\link{r2sas}},
#' \code{\link{sas_mget}}, \code{\link{source_sas}},
#' \code{\link{parse_formats}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' \dontrun{
#' rmacro(system.file('testfiles', 'onemacro.sas', package = 'rawr'),
#'        args = 'arg1 = 1, arg2 = 2',
#'        firstArgs = 'options nodate nocenter; x \'cd ~/desktop\';',
#'        lastArgs = 'endsas;')
#'        
#' ## options nodate nocenter; 
#' ## x 'cd ~/desktop'; 
#' ## 
#' ## %include "./.../onemacro.sas";
#' ## %macro1(arg1 = 1, arg2 = 2);
#' ## endsas; 
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

rmacro <- function(path, name, args, saspath, show.args = FALSE,
                   force = FALSE, firstArgs, lastArgs, out = getwd()) {
  margs <- get_margs(path, name)
  if (missing(name))
    name <- names(margs)
  if (show.args)
    return(margs)
  if (length(name) > 1L)
    stop('run one macro per \'rmacro\' call\n')
  
  ## create .sas script to call macro
  if (!missing(firstArgs))
    firstArgs <- gsub(';', '; \n', firstArgs)
  else firstArgs <- '\n'
  if (!missing(lastArgs))
    lastArgs <- gsub(';', '; \n', lastArgs) else lastArgs <- '\n'
  sass <- c(sprintf('%%include \"%s\";', path),
            sprintf('%%%s(%s);', name, gsub(';','', args)))
  saspath <- sas_path(saspath)
  
  r2sas(paste(c(firstArgs, sass, lastArgs), sep = '\n'), saspath, force, out)
}

#' Get arguments from \code{SAS} macros
#' 
#' Reads a text file (usually \code{.sas}) and extracts \code{SAS} macro names
#' and parameters with any default values; see tests in examples below.
#' 
#' @param path character string of path to \code{.sas} file
#' @param name macro name in \code{path} of interest; if missing, returns
#' all macros found
#' @param text (optional) character string(s) of macros given instead of
#' \code{path}
#' 
#' @return
#' A list with macro names in \code{path} and their respective parameters.
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{rmacro}}, \code{\link{r2sas}},
#' \code{\link{sas_mget}}, \code{\link{source_sas}},
#' \code{\link{parse_formats}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' get_margs(text = '%macro macro(a = 1, b = 2); %mend;')
#' 
#' path <- system.file('testfiles', package = 'rawr')
#' get_margs(file.path(path, 'macros.sas'))
#' 
#' get_margs(file.path(path, 'macros.sas'), 'macro1')
#' 
#' \dontrun{
#' get_margs(file.path(path, 'nomacro.sas'))
#' 
#' ## Error in get_margs(file.path(path, 'nomacro.sas')) :
#' ##   no valid macros found in './.../nomacro.sas'
#' 
#' 
#' get_margs(file.path(path, 'onemacro.sas'), c('macro2', 'macro5'))
#' 
#' ## Error in get_margs(file.path(path, 'onemacro.sas'), c("macro2", "macro5")) :
#' ##   'macro2', 'macro5' not found in './.../onemacro.sas'
#' }
#' 
#' @export

get_margs <- function(path, name, text) {
  macro <- if (missing(path) && !missing(text)) {
    path <- shQuote('text')
    readLines(con <- textConnection(text, encoding = 'UTF-8'))
  } else if (is.character(path)) {
    readLines(con <- file(path), warn = FALSE)
  }
  on.exit(close(con))
  
  ## ignore everything between /* */, collapse,
  ## then split lines by semicolons
  macro <- paste(macro, collapse = ' ')
  macro <- gsub('/\\*[^/\\*]*?\\*/', '', macro, perl = TRUE)
  macro <- gsub('\\s+', ' ', macro)
  macro <- gsub(';', ';$$$;', macro)
  macro <- strsplit(macro, split = '\\$\\$\\$;')
  
  ## match the macro syntax: " %macro name( ); "
  ## and trim whitespace
  mcall <- unlist(lapply(macro, function(x)
    regmatches(x, gregexpr('\\s*%macro\\s+(\\w+)\\((.*)\\)\\s*;{1}', x,
                           perl = TRUE))))
  mnames <- gsub('%macro|;|(?<=\\().*?(?=\\))|\\(|\\)|\\s*', '',
                 mcall, perl = TRUE)
  args <- gsub(' ', '', regmatches(mcall, gregexpr('(?<=\\().*?(?=\\))',
                                                   mcall, perl = TRUE)))
  
  if (length(mnames) < 1L)
    stop(sprintf('no valid macros found in %s\n', path))
  if (!missing(name) && any(name %ni% mnames))
    stop(sprintf('%s not found in %s',
                 paste(shQuote(name[name %ni% mnames]), collapse = ', '),
                 shQuote(path)),
         domain = NA)
  margs <- setNames(gsub(',', ', ', args), mnames)
  as.list(margs)[name]
}

#' Convert multiple \code{SAS} data sets to \code{R} data frame
#' 
#' User-friendly wrapper of \code{\link[Hmisc]{sas.get}} to convert one or
#' more \code{SAS} data sets (\code{.sas7bdat} files) into a list of \code{R}
#' data frames.
#' 
#' @param libpath directory to data set(s) as character string; if missing,
#' searches the current working directory
#' @param dsn data set name(s); either \code{data1} or \code{data1.sas7bdat}
#' will work; if missing or \code{NULL}, all \code{.sas7bdat} files found in
#' \code{libpath} will be read
#' @param saspath file path to sas executable as character string
#' @param fmtpath (optional) path to a format \code{.sas} file as character
#' string; \code{SAS} throws an error if the platform used to make the format
#' catalog is not the same as the current platform; \code{fmtpath} is passed
#' to \code{\link{sas_catalog}}
#' @param catalog logical; if \code{FALSE} or multiple catalogs are found in
#' \code{libpath}, catalogs will be ignored; if \code{fmtpath} is used to
#' create a new catalog, any existing catalogs are moved to a new directory
#' to avoid overwriting
#' @param log.file name of \code{SAS} log file; default value will create
#' \code{_temp_.log} in the \code{libpath} directory
#' @param ... additional arguments passed to \code{\link[Hmisc]{sas.get}}
#' @param force logical; by default, user must interactively allow
#' \code{sas_mget} to continue reading all data sets; set to \code{TRUE} to
#' ignore this or for non-interactive \code{R}
#' @param write logical; if \code{TRUE}, each data frame will be written to
#' a \code{.csv} file in a new sub directory, \code{./_sas_mget_}
#' 
#' @return
#' A list of data frames resembling the \code{SAS} data sets.
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{rmacro}}, \code{\link{get_margs}},
#' \code{\link{r2sas}}, \code{\link{source_sas}},
#' \code{\link{parse_formats}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' \dontrun{
#' p <- system.file('testfiles', package = 'rawr')
#' data.list <- sas_mget(p)
#' 
#' ## !!! Two data set(s) will be read
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

sas_mget <- function(libpath = getwd(), dsn = dsn, saspath = sas_path(),
                     fmtpath = NULL, catalog = length(dcf) == 1L,
                     log.file = '_temp_.log', ..., force = FALSE, write = FALSE) {
  dsn <- list.files(libpath, pattern = '\\.sas7bdat$')
  dcf <- list.files(libpath, pattern = '\\.sas7bcat$')
  wdir <- file.path(libpath, '_sas_mget_')
  dsn <- rm_ext(dsf <- dsn)
  if (!length(dsn)) {
    message(sprintf('No sas data sets found in %s\n', shQuote(libpath)),
            domain = NA)
    return(invisible())
  }
  dsi <- `colnames<-`(round(file.info(list.files(
    libpath, full.names = TRUE, pattern = '\\.sas7bdat$'))['size'] / 1000),
    'size (Kb)')
  
  if (!is.character(fmtpath) & !catalog) {
    no.format <- TRUE
  } else {
    ## if a format.sas file is given, create a format catalog
    if (is.character(fmtpath)) {
      stopifnot(file.exists(fmtpath))
      if (catalog)
        stopifnot(move_formats(libpath, dcf))
      status <- sas_catalog(fmtpath, libpath, saspath)
      no.format <- if (status != 0L) {
        message('Error in getting formats -- formatting ignored', domain = NA)
        message('See log: ', shQuote(file.path(libpath, log.file)), domain = NA)
        TRUE
      } else FALSE
    } else {
      ## no format path, use catalog if one/ignore if multiple
      no.format <- if (length(dcf) > 1L) {
        message(sprintf('\nMultiple catalogs found in %s:\n', shQuote(libpath)),
                domain = NA)
        cat(dcf, sep = '\n')
        message('\nCatalogs will be ignored', domain = NA)
        TRUE
      } else FALSE
    }
  }
  
  ## final warning for reading all dsn
  if (interactive() && !force) {
    cat(sprintf('\n!!! %s data set%s will be read%s \n\n',
                num2char(length(dsn)), ifelse(length(dsn) > 1, 's', ''),
                ifelse(write, paste(' and written to\n!!! ', wdir), '')))
    dd <- dsi[which(rm_ext(rownames(dsi)) %in%
                      gsub('\\/+','/', file.path(libpath, dsn))), , drop = FALSE]
    print(`rownames<-`(dd, dsn))
    cat('\n\n\n\n\n')
    check <- readline('Do you want to continue? (y/n): ')
    if (tolower(substr(check, 1, 1)) != 'y')
      return(invisible())
  }
  
  ## sas.get wrapper
  if (force || !interactive() || tolower(substr(check, 1, 1)) == 'y') {
    zzz <- setNames(lapply(dsn, function(x)
      tryCatch(
        Hmisc::sas.get(libraryName = libpath, member = x, sasprog = saspath,
                       log.file = file.path(libpath, log.file),
                       formats = !no.format, ...),
        error = function(e) {
          message('Error reading ', shQuote(x), ':\n', e, 'Skipping read')
          data.frame(dsn = x, error = e$message, stringsAsFactors = FALSE)
        })
    ), dsn)
    
    ## print dims for user
    cat('\nread summary:\n\n')
    dims <- sapply(zzz, dim)
    print(`rownames<-`(dims, c('rows','columns')))
    message(sprintf('Log file created: \'%s\'\n', file.path(libpath, log.file)),
            domain = NA)
    if (write) {
      dir.create(wdir)
      f <- function(x, file) write.csv(x, file, row.names = FALSE)
      mapply(f, x = zzz, file = file.path(wdir, paste0(names(zzz), '.csv')))
    }
    zzz
  } else invisible()
}

#' Source \code{SAS} code
#' 
#' \code{\link{source}}-esque function for \code{.sas} files.
#' 
#' @param path path to \code{.sas} file as character string
#' @param ... additional parameters passed to \code{\link{r2sas}}
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{r2sas}}, \code{\link{rmacro}},
#' \code{\link{get_margs}}, \code{\link{sas_mget}},
#' \code{\link{parse_formats}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' \dontrun{
#' source_sas(system.file('testfiles', 'onemacro.sas', package = 'rawr'))
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

#' Get formats from \code{SAS} format files
#' 
#' Reads a text file (usually \code{.sas}) and extracts \code{SAS} format
#' variable names and format values.
#' 
#' @param path character string of path to \code{.sas} file
#' @param fmt a character string with formats to parse
#' @param clean logical; if \code{TRUE} (default), the parsed formats will be
#' cleaned of quotes and extra whitespace
#' @param x a vector of data usually taking a small number of distinct values
#' @param droplevels logical; if \code{TRUE}, unused factor levels will be
#' dropped; default is not to drop unused levels
#' 
#' @return
#' A list with format names in \code{path} and their respective values and
#' labels as named character vectors.
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{rmacro}}, \code{\link{r2sas}},
#' \code{\link{get_margs}}, \code{\link{sas_mget}},
#' \code{\link{source_sas}}, \code{\link{sas_catalog}}
#' 
#' @examples
#' p <- system.file('testfiles', 'formats.sas', package = 'rawr')
#' cat(readLines(p), sep = '\n')
#' parse_formats(p)
#' 
#' 
#' ## named vector of formats from string
#' fmt <- '0 = something, 1 = something else; 2 = yada. -9=yadayadayada, .C=blah'
#' parse_formats2(fmt)
#' 
#' ## use to format factor variables
#' x <- sample(0:2, 10, TRUE)
#' table(apply_formats(x, fmt), x)
#' apply_formats(x, fmt, droplevels = TRUE)
#' 
#' ## reordered if all levels are numeric and positive
#' parse_formats2('1=yes, 0=no, 2=maybe')
#' 
#' @export

parse_formats <- function(path) {
  fmt <- rm_nonascii(readLines(path))
  fmt <- gsub('\\*.*;|\\/\\*.*\\*\\/', '', fmt)
  vars <- gsub('(?i)value\\W+(\\w*)|.', '\\1', fmt, perl = TRUE)
  #   vals <- gsub("(?i)([\'\"]?[a-z\\d -]+[\'\"]?\\s*=\\s*[\'\"]?[a-z\\d -,]+[\'\"]?)|.", '\\1',
  #                fmt, perl = TRUE)
  regex <- '[\'\"].*[\'\"]|[\\w\\d-]+'
  vals <- gsub(sprintf('(?i)\\s*(%s)\\s*(=)\\s*(%s)|.', regex, regex),
               '\\1\\2\\3', fmt, perl = TRUE)
  vars <- locf(vars)
  dd <- data.frame(values = vars, formats = vals, stringsAsFactors = FALSE)
  dd <- dd[with(dd, is.na(values) | nzchar(formats)), ]
  sp <- split(dd$formats, dd$values)
  lapply(sp, function(x) {
    x <- strsplit(Filter(nzchar, x), '=')
    sapply(x, function(y) setNames(trimws(y[1]), trimws(y[2])))
  })
}

#' @rdname parse_formats
#' @export
parse_formats2 <- function(fmt, clean = TRUE) {
  stopifnot(length(fmt) == 1L)
  x <- rm_nonascii(fmt)
  
  ## capture unique levels
  p <- '([0-9.\\-A-Z]+)\\s*='
  m <- gregexpr(p, x, perl = TRUE)
  levels <- c(regcaptures(x, m)[[1]])
  
  ## capture corresponding labels
  p <- '[A-Z.\\-0-9]+\\s*=\\s*(.*?)(?=[ ,.;]*[0-9.\\-A-Z]+\\s*=|[ ,.;]*$)'
  m <- gregexpr(p, x, perl = TRUE)
  labels <- c(regcaptures(x, m)[[1]])
  
  if (clean)
    labels <- trimws(gsub('[\'\"]', '', labels))
  
  ## sort if all levels are numeric and positive
  ok <- !any(grepl('\\D', levels))
  setNames(labels, levels)[if (ok) order(as.numeric(levels)) else seq(levels)]
}

#' @rdname parse_formats
#' @export
apply_formats <- function(x, fmt, clean = TRUE, droplevels = FALSE) {
  fmt <- parse_formats2(fmt, clean)
  out <- factor(x, names(fmt), fmt)
  if (droplevels)
    out <- droplevels(out)
  ok <- sum(diag(table(out, x))) == sum(table(x))
  if (!ok)
    warning('Number of non-missing vales does not match original vector.',
            call. = FALSE)
  out
}

#' \code{SAS} catalog
#'
#' Create a \code{SAS} format catalog from a \code{.sas} file or macro. Note
#' that if the \code{libpath} directory contains other \code{.sas7bdat} files,
#' these will be moved to a new directory, \code{./_old_formats_} which is
#' created if needed and old catalog files are renamed if this directory
#' exists. The file created by \code{sas_catalog} will be "formats.sas7bcat"
#' since this name is required by \code{\link[Hmisc]{sas.get}}.
#'
#' @param path file path to a \code{.sas} file
#' @param libpath directory to data set(s) as character string; if missing,
#' searches the current working directory
#' @param saspath file path to sas executable as character string
#' @param log name of log file as character string
#' 
#' @return
#' This function has no useful return value (other than an error code, see
#' \code{\link{system2}}) but is used for its side effect of creating a
#' \code{formats.sas7bcat} file and corresponding log file in \code{libpath}.
#' Because this function is intended to be used in the \code{\link{sas_mget}}
#' pipeline, \code{sas_catalog} will move existing catalogs to a new directory
#' keeping only a single catalog ("formats.sas7bcat") in \code{libpath} to
#' avoid conflicts with \code{\link[Hmisc]{sas.get}}.
#' 
#' @seealso
#' \code{\link{sas_path}}, \code{\link{r2sas}}, \code{\link{rmacro}},
#' \code{\link{get_margs}}, \code{\link{sas_mget}}, \code{\link{source_sas}},
#' \code{\link{parse_formats}}
#'
#' @examples
#' \dontrun{
#' p <- system.file('testfiles', 'formats.sas', package = 'rawr')
#' cat(readLines(p), sep = '\n')
#' sas_catalog(p, getwd())
#' }
#'
#' @export

sas_catalog <- function(path, libpath = dirname(path), saspath = sas_path(),
                        log = '_temp_formats_.log') {
  stopifnot(move_formats(libpath))
  ## rawr:::move_formats
  log <- file.path(libpath, log)
  sass <- c(sprintf('x \"cd %s\";', libpath),
            sprintf('libname temp_fmt \"%s\";', libpath),
            sprintf('%%include \"%s\";', path),
            'proc catalog catalog =  work.formats;',
            'copy out = temp_fmt.formats;',
            'quit;')
  sasin <- file.path(libpath, '_temp_formats_.sas')
  on.exit(unlink(sasin))
  cat(sass, sep = '\n', file = sasin, append = TRUE)
  sys_args <- paste(sasin, '-log', log)
  invisible(system2(saspath, sys_args))
}

move_formats <- function(dir, dcf = list.files(dir, pattern = '\\.sas7bcat$'),
                         unique_string = trunc(abs(rnorm(1)) * 1e6)) {
  ## if dir contains catalogs, move bcat files to another directory
  ## since catalog created must be called "formats.sas7bcat"
  if (!length(dcf))
    return(TRUE)
  newdir <- file.path(dir, '_old_formats_')
  message('NOTE: moving old format catalog(s) to ', shQuote(newdir), domain = NA)
  tryCatch({
    if (!file.exists(newdir)) {
      dir.create(newdir)
    } else {
      ## cheap way to be (almost) sure that if newdir exists and if newdir
      ## includes some sas7bdat files, then the old will not be overwritten
      file.rename(dcf, dcf <- gsub('\\.', paste0('_', unique_string, '.'), dcf))
    }
    file.copy(dcf, newdir)
    unlink(dcf)
  }, error = function(e)
    stop('Failed to move old formats to ', sQuote(newdir), domain = NA))
  TRUE
}
