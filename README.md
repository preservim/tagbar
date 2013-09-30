# Overview

Tagbar is a vim plugin for browsing the tags of source code files.
It provides a sidebar that displays the ctags-generated tags of the current file, ordered by their scope. This means that for example methods in C++ are displayed under the class they are defined in.

Check out the homepage at http://majutsushi.github.io/tagbar/ for more information.

# Support for additional filetypes

For filetypes that are not supported by Exuberant Ctags check out [the wiki](https://github.com/majutsushi/tagbar/wiki) to see whether other projects offer support for them and how to use them. Please add any other projects/configurations that you find or create yourself so that others can benefit from them, too.

# Important: If the file structure is displayed wrong

If you notice that there are some errors in the way your file's structure is displayed in Tagbar, please make sure that the bug is actually in Tagbar before you report a bug. Since Tagbar uses [exuberant-ctags](http://ctags.sourceforge.net/) and compatible programs to do the actual file parsing, it is likely that the bug is actually in one of those programs instead.

There is an example in `:h tagbar-issues` about how to run ctags manually so you can determine where the bug actually is. If the bug is actually in ctags, please report it on their website instead, as there is nothing I can do about it in Tagbar. Thank you!

You can also have a look at ctags bugs that have previously been filed on Tagbar here:  
https://github.com/majutsushi/tagbar/issues?labels=ctags-bug&page=1&state=closed
