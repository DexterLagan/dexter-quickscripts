# Quickscript Extra

Some scripts for [Quickscript](https://github.com/Metaxal/quickscript).

## 1. Installation

In DrRacket, in `File|Package manager|Source`, enter
`quickscript-extra`.

Or, on the command line, type: `raco pkg install quickscript-extra`.

If DrRacket is already running, click on `Scripts|Manage scripts|Compile
scripts and reload menu`.

## 2. Scripts

* **compile-to-standalone**: compile the current program to standalone
  executable.

  Compiles the current program to a standalone binary, and automatically imports an icon (located in ./icon). Unlike DrRacket's default compilation option, it does not zip the resulting files.

* **escape-double-quotes**: escape double-quotes.

  Adds a back-slash ('\') before every double-quote ('"') in the selection.

* **generate-provide**: generate a provide form for the current module.

  Generates a provide form for the current module, and automatically writes a summary of each exported function.

* **generate-skeleton**: generate a program skeleton, optionally with a
  few practical definitions.

  Generates a program skeleton with nicely commented sections, and optionally includes useful macros and definitions.

## 3. url2script

The `url2script` script is special: it allows you to easily fetch
single-file quickscripts from anywhere on the internet by providing the
url to the raw code. It is actually a little smarter than that because
it understands non-raw urls from [github
gists](https://gist.github.com), [gitlab
snippets](https://gitlab.com/snippets), [pastebin](https://pastebin.com)
and [pasterack](http://pasterack.org).

Some single-file scripts can be found on the [Racket
wiki](https://github.com/racket/racket/wiki/Quickscript-Scripts-for-DrRacket).

A script previously fetched with url2script can also be easily updated
by first opening it via `Scrits|Manage|Open script...` then clicking on
`Scripts|url2script|Update current script`.

When a script is fetched by `url2script`, a `"url2script-info"`
submodule is automatically added (unless one already exists) with
information about the filename in which the script is to be saved (or
has been saved), and the original url of the script. The latter is used
for updating the script as described above. The submodule looks like
this:

```racket
(module url2script-info racket/base                             
 (provide url filename)                                         
 (define filename "the-default-filename-to-save-the-script.rkt")
 (define url "https://url.of.the/script.rkt"))                  
```

If you want to publish a single-file quickscript without making a
package, consider adding this submodule so as to provide a default
filename (otherwise the user who fetches your script will have to type
one themselves, and may be unsure what name to pick).

Also consider adding a permissive license. We recommend a dual license
Apache 2.0 / MIT:

```racket
;;; Copyright <year> <email or name or entity>                        
;;; License: [Apache License, Version                                 
2.0](http://www.apache.org/licenses/LICENSE-2.0) or                   
;;;          [MIT license](http://opensource.org/licenses/MIT) at your
option.                                                               
```

Scripts fetched by `url2script` are added to the default script
directory. They can be modified as desired (as long as the license
permits it)

## 4. Customizing

Scripts can be selectively deactivated from the library
\(`Scripts|Manage scripts|Library`).

If you change the source code of a script installed from the
`quickscript-extra` package \(or from any package containing
quickscripts\), you will lose all your modifications when the package is
updated. To avoid this, you can use Quickscript’s [shadow
scripts](https://docs.racket-lang.org/quickscript/index.html?q=quickscripts#%28part._.Shadow_scripts%29):
The shadow script calls the original script without modifying it, and
can be modified to your taste without being modified when the original
script is updated.

In particular, if you want to change the default label, menu path or
keybinding of a script installed from `quickscript-extra`, go to
`Scripts|Manage|Library...`, select the `quickscript-extra` directory,
then the script you want, and click on `Shadow`. This opens a new
(shadow) script that calls the original script where you can change what
you want.

Note that the shadowed script is deactivated so as to avoid duplicate
menu entries and keybindings.
