# cl-wget

This file documents the cl-wget utility for downloading network data.
Only the most basic functionalities have been ported.
Refer to [GNU Wget 1.21.1-dirty Manual](https://www.gnu.org/software/wget/manual/wget.html) as inspiration for development.

### [1](https://www.gnu.org/software/wget/manual/wget.html#Overview) Overview

cl-wget is a free software for non-interactive download of files from the Web.

### [2](https://www.gnu.org/software/wget/manual/wget.html#Invoking) Invoking

By default, cl-wget is very simple to invoke. The basic syntax is:  
`(wget urls &key quiet restrict-file-names page-requisites)` => `result`

The arguments and values are:  
`urls` --- a [string designator](http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_s.htm#string_designator), or a [tree](http://clhs.lisp.se/Body/26_glo_t.htm#tree).  
`quiet` --- a [generalized boolean](http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_g.htm#generalized_boolean).  
`restrict-file-names` --- one of `:nocontrol`, `:lowercase`, `:uppercase`, `:unix`, `:windows`, or a [function designator](http://clhs.lisp.se/Body/26_glo_f.htm#function_designator).  
`page-requisites` --- a [generalized boolean](http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_g.htm#generalized_boolean).  
`result` --- a [proper sequence](http://clhs.lisp.se/Body/26_glo_p.htm#proper_sequence).  

### [2.4](https://www.gnu.org/software/wget/manual/wget.html#Logging-and-Input-File-Options) Logging and Input File Options

`quiet` 
 
Turn off cl-wget’s output. Suppress [\*standard-output\*](http://clhs.lisp.se/Body/26_glo_s.htm#standard_output).

### [2.5](https://www.gnu.org/software/wget/manual/wget.html#Download-Options) Download Options

`restrict-file-names`

Change which characters found in remote URLs must be escaped during generation of local filenames.
This option may also be used to force all alphabetical cases to be either lower- or uppercase.
By default, cl-wget escapes the characters that are not valid or safe as part of file names on your operating system.
This option is useful for changing these defaults, perhaps because you are downloading to a non-native partition.
The acceptable values are `:nocontrol`, `:lowercase`, `:uppercase`, `:unix`, `:windows`, and [function designator](http://clhs.lisp.se/Body/26_glo_f.htm#function_designator).

When `:windows` is given, cl-wget uses `+` instead of `:` to separate host and port in local file names, and uses `@` instead of `?` to separate the query portion of the file name from the rest.
Therefore, a URL that would be saved as `www.xemacs.org:4300/search.pl?input=blah` in `:unix` mode would be saved as `www.xemacs.org+4300/search.pl@input=blah` in `:windows` mode.
cl-wget is in `:windows` mode by default, since Microsoft Windows is incapable of handling filenames containing characters `?` and `:`.

When a function designator is given, cl-wget uses `(funcall restrict-file-names url)` as the pathname.
```cl
(defvar url "https://www.gnu.org/software/wget/manual/wget.html")
```
```cl
(wget url :restrict-file-names :uppercase)
; is equivalent to
(wget url :restrict-file-names #'string-upcase)
```
```cl
(wget url :restrict-file-names :windows)
; is equivalent to
(wget url :restrict-file-names (lambda (url) (substitute #\+ #\: (substitute #\@ #\? url))))
```

### [2.11](https://www.gnu.org/software/wget/manual/wget.html#Recursive-Retrieval-Options) Recursive Retrieval Options

`page-requisites`

Ordinarily, when downloading a single HTML page, any requisite documents that may be needed to display it properly are not downloaded.
This option causes cl-wget to download all the files that are necessary to properly display a given HTML page.
This includes such things as inlined images, sounds, and referenced stylesheets.
```cl
CL-USER> (ql:quickload :cl-wget)
To load "cl-wget":
  Load 1 ASDF system:
    cl-wget
; Loading "cl-wget"
.....
(:CL-WGET)
CL-USER> (cl-wget:wget
          '("https://www.wolframalpha.com/input"
            "https://mitpress.mit.edu/sites/default/files/sicp/index.html"
            ((((("https://www.gnu.org/software/wget/manual/wget.html"
                 "https://gigamonkeys.com/book/introduction-why-lisp.html"))))
             "http://www.paulgraham.com/lisp.html"
             (((((((("https://www.gnu.org/philosophy/philosophy.html")))))))))
            ("https://lispcookbook.github.io/cl-cookbook/getting-started.html")
            "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
          :page-requisites t)

Downloading "https://www.wolframalpha.com/input" (16.003 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/images/running-man_5SPZTwCb.png" (111.585 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/images/Logo_3KbuDCMc.svg" (29.037 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/runtime/polyfills-e21575077778f414a069.js" (79.824 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/FsRzyQyGxyz9ToJRTJuna/pages/input.js" (1.892 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/FsRzyQyGxyz9ToJRTJuna/pages/_app.js" (131.202 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/runtime/webpack-758ed5a910c073549032.js" (6.749 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/chunks/framework.7e7abc056a23dbc17509.js" (130.929 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/chunks/b7dac8a5.d68380b3825feee444fd.js" (57.111 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/chunks/f40a3f00.4c5bd6d85d7b5f4b9ea2.js" (291.884 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "https://www.wolframalpha.com/_next/static/chunks/commons.fb36529629248d0d9ae3.js" (502.153 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
```
Multiple URLs can together form a list or a tree in any way you like.
cl-wget will properly traverse and do its best to download all resources.
