clean <- function () function (build) unlink (build, recursive = TRUE);

inspect <- function () function (base, binder) {
  define ('local', function () file.path (path.expand (file.path ('~', '.raven')), 'repository'), eager, binder);
  define ('build', function () file.path (base, 'target'), eager, binder);
  define ('sources', function () base, eager, binder);

  dependency <- collection ('dependencies', b = binder);
  remote <- collection ('remote', b = binder);
  constant <- function (name, value) define (name, function () value, eager, binder);

  with (list (project = function (name, version, packaging) {
                constant ('name', name);
                constant ('version', version);
                for (name in ls (packaging, all.names = TRUE)) (function (key) define (key, packaging[[ key ]], b = binder)) (name);
              },
              modules = function (...)
                list (.prepare = function (goals, base, binder, restore.libPaths) {
                        for (module in c (...))
                          raven (goals, base = file.path (base, module), parent = binder, restore.libPaths = FALSE);
                        'ok';
                      },
                      .import = function () 'not applicable',
                      .install = function () 'not applicable',
                      .test = function () 'not applicable',
                      .package = function () 'not applicable'),
              script = function (sources = '.', tests = NULL)
                list (.scripts = function ()
                        function (root, scripts)
                          vapply (Filter (function (script) "raven.R" != script, scripts),
                              function (x) file.path (root, x), c (''), USE.NAMES = FALSE),
                      .prepare = function () 'ok',
                      .import = function (base, execution, name, version, .scripts) tryCatch ({
                        save <- getwd ();
                        setwd (base);
                        for (script in .scripts (sources, list.files (path = sources, pattern = '*\\.R$')))
                          source (script, local = execution);
                        'ok';
                      }, finally = setwd (save)),
                      .install = function (name, version, local) {
                        untar (file.path (local, name, version, paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.')),
                               exdir = file.path (local, name, version), tar = 'tar');
                        'ok';
                      },
                      .test = function (base, execution, .scripts) tryCatch ({
                        save <- getwd ();
                        setwd (base);
                        if (!is.null (tests))
                          for (test in .scripts (tests, list.files (path = tests, pattern = '*\\.R$', recursive = TRUE)))
                            source (test, local = execution);
                        'ok';
                      }, finally = setwd (save)),
                      .package = function (build, name, version, base, .scripts) tryCatch ({
                        save <- getwd ();
                        setwd (base);
                        if (!file.exists (build)) dir.create (build, recursive = TRUE);
                          tar (file.path (build, paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.')),
                               files = .scripts (sources, list.files (sources, pattern = '*\\.R$', recursive = TRUE)),
                               tar = 'tar');
                        'ok';
                      }, finally = setwd (save))),
            cran = function ()
              list (.cran = function (name, version, local) 
                      file.path (local, name, version, paste ('.r', eval (expression (version$`svn rev`), baseenv ()), sep = '')),
                    .prepare = function () 'ok',
                    .import = function (.cran) {
                      if (!(.cran %in% .libPaths ())) .libPaths (c (.cran, .libPaths ()));
                      'ok';
                    },
                    .install = function (name, version, local, .cran) {
                      if (!file.exists (.cran)) tryCatch ({
                        dir.create (.cran, recursive = TRUE);
                        install.packages (file.path (local, name, version, paste (paste (name, version, sep = '_'), 
                                                                                  'tar', 'gz', sep = '.')),
                                          type = 'source', repo = NULL, lib = .cran);
                      }, error = function (error) {
                        unlink (.cran, recursive = TRUE);
                        stop (error);
                      });
                      inject (function (.import) .import, b = binder);
                    },
                    .test = function () 'ok',
                    .package = function (base, name, version) {
                      if (!file.exists (file.path (base, paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.'))))
                        stop (paste ("Unable to find source tarball for cran package", paste (name, version, sep = ':')));
                      'ok';
                    }),
            remote = function (url) remote (function () url),
            build = function (path) constant ('build', path),
            dependency = function (name, version, required = TRUE)
              dependency (function () list (name = name, version = version, required = required)),
            local = function (path) define ('local', function () path, b = binder)),
        source (file.path (base, 'raven.R'), local = TRUE));
    
  inject (function (.prepare) .prepare, binder);
};

fetch <- function (force = FALSE) function (name, version, local, remote, dependencies, binder)
  for (dependency in dependencies)
    (function (name, version, required) {
      base <- file.path (local, name, version);
      archive <- paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.');
      if (force) unlink (base, recursive = TRUE);
      if (!file.exists (base)) dir.create (base, recursive = TRUE, showWarnings = FALSE);
      if (!(file.exists (file.path (base, archive)) && file.exists (file.path (base, 'raven.R'))))
      for (root in remote) tryCatch ((function (...) for (file in c (...))
        download.file (paste (root, name, version, file, sep = '/'), file.path (base, file), 'curl')) ('raven.R', archive));
      if (file.exists (file.path (base, archive)) && file.exists (file.path (base, 'raven.R'))) {
        raven (inspect (), fetch (force), function (.install) .install, base = base, restore.libPaths = FALSE);
        'ok';
      } else if (required) stop (paste ("Unable to fetch required dependency", paste (name, version, sep = ':')));
    }) (dependency$name, dependency$version, dependency$required);

import <- function (execution) function (binder, name, version) {
  define ('execution', function () execution, b = binder);
  inject (function (.import) .import, b = binder);
};

install <- function () function (.package, local, name, version, build, base)
  if (.package == 'ok') (function (...) tryCatch ({
    if (!file.exists (home <- file.path (local, name, version))) dir.create (home, recursive = TRUE);
    for (file in c (...)) file.copy (file, home);
      'ok';
  }, error = function (error) {
    unlink (home, recursive = TRUE);
    stop (error);
  })) (file.path (base, 'raven.R'), file.path (build, paste (paste (name, version, sep = '_'), 'tar', 'gz', sep = '.')));

test <- function (execution) function (binder, name, version) {
  define ('execution', function () execution, b = binder);
  inject (function (.test) .test, b = binder);
};

raven <- function (..., base = getwd (), parent = binder (), restore.libPaths = FALSE) binder (parent, function (binder) tryCatch ({
  save <- .libPaths ();

  define ('base', function () base, b = binder);

  goals <- c (...);
  inject (function (execution = parent.frame (1))
            if (length (goals) < 1) goals <<- c (inspect (), fetch (FALSE), import (execution), test (new.env (execution)), install ()),
          binder);

  define ('goals', function () goals, eager, binder);

  for (goal in goals) inject (goal, b = binder);

  'ok';
}, finally = if (restore.libPaths) .libPaths (save)));