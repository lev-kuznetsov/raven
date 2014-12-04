raven
=====

R equivalent of maven. The following code will import it to your namespace:
```
(function (...) for (s in c (...)) tryCatch ({ download.file (s, '.s', method = 'curl'); source ('.s'); }, finally = unlink ('.s')); }) ('https://raw.githubusercontent.com/dfci-cccb/injectoR/master/R/injector.R', 'https://raw.githubusercontent.com/dfci-cccb/raven/master/R/raven.R');
```
Provided raven() function becomes available which given a base directory (defaults to
current working directory) will inspect the descriptor file named "raven.R" and act
according to its instructions against the goals specified in the raven command. An
example of the descriptor file would look like this:
```
project ('foo', # name of the project
         'first', # version
         script ( # packaging, this scripts ships with three packaging types, script
                  # which sources all R sources found under the subfolder provided by
                  # sources argument, modules which defines submodules, and cran for
                  # legacy CRAN-like packaging
                 sources = '.', tests = NULL)) # default values
dependency ('bar', 'first')
# See source for other options
```
Invoke raven by calling the raven() with as many goals as you wish. A goal is an injectable
function, raven ships with the following goals: inspect(), fetch(force = FALSE),
import(environment), test(environment), install(), clean(). Invoking raven without any goals
will execute all of the above goals in that order except clean. See source for injectables