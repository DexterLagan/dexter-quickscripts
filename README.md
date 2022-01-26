# Dexter's Quickscripts

Some scripts for [Quickscript](https://github.com/Metaxal/quickscript).

## 1. Installation

In DrRacket, in `File|Package manager|Source`, enter
`dexter-quickscripts`.

Or, on the command line, type: `raco pkg install dexter-quickscripts`.

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

## 3. Customizing

Scripts can be selectively deactivated from the library
\(`Scripts|Manage scripts|Library`).

If you change the source code of a script installed from the
`dexter-quickscripts` package \(or from any package containing
quickscripts\), you will lose all your modifications when the package is
updated. To avoid this, you can use Quickscript’s [shadow
scripts](https://docs.racket-lang.org/quickscript/index.html?q=quickscripts#%28part._.Shadow_scripts%29):
The shadow script calls the original script without modifying it, and
can be modified to your taste without being modified when the original
script is updated.

In particular, if you want to change the default label, menu path or
keybinding of a script installed from `dexter-quickscripts`, go to
`Scripts|Manage|Library...`, select the `dexter-quickscripts` directory,
then the script you want, and click on `Shadow`. This opens a new
(shadow) script that calls the original script where you can change what
you want.

Note that the shadowed script is deactivated so as to avoid duplicate
menu entries and keybindings.

## 4. License

Dexter's QuickScripts are free software; see [LICENSE](https://github.com/DexterLagan/dexter-quickscripts/blob/main/LICENSE) for more details.
