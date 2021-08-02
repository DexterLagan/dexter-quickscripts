# Quick Scripts for DrRacket

A collection of useful QuickScripts for DrRacket.

## How to Install:

Copy the scripts to your quickscript folder. On Windows, this path is:<br>
<pre>
%USERPROFILE%\AppData\Roaming\Racket\quickscript\user-scripts
</pre>

## How to Use
In DrRacket, click on the 'Scripts' menu, and click on the script you want to use.

## What does each script do?

<b>compile-standalone</b><br>
compiles the current program to a standalone binary, and automatically imports an icon (located in ./icon). Unlike DrRacket's default compilation option, it does not zip the resulting files.

<b>generate-provide</b><br>
generates a provide form for the current module, and automatically writes a summary of each exported function.

<b>generate-skeleton</b><br>
generates a program skeleton with nicely commented sections, and optionally includes useful macros and definitions.

<b>escape-double-quotes</b><br>
adds a back-slash ('\') before every double-quote ('"') in the selection.

## License

My Quick Scripts is free software; see [LICENSE](https://github.com/DexterLagan/quick-scripts/blob/master/LICENSE) for more details.
