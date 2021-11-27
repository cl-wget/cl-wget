# cl-wget

This file documents the cl-wget utility for downloading network data.
Only the most basic functionalities have been ported.
Refer to [GNU Wget documentation](https://www.gnu.org/software/wget/manual/wget.html) as inspiration for development.

## 1 Overview [\*](https://www.gnu.org/software/wget/manual/wget.html#Overview)

cl-wget is a free software for non-interactive download of files from the Web.

## 2 Invoking [\*](https://www.gnu.org/software/wget/manual/wget.html#Invoking)

By default, cl-wget is very simple to invoke. The basic syntax is:  
`(wget urls &key quiet restrict-file-names page-requisites)` → `urls`

Multiple URLs can together form a list or a tree in any way you like.
cl-wget will properly traverse and do its best to download all resources.

The arguments and values are:  
`urls` - a [string designator](https://franz.com/support/documentation/current/ansicl/glossary/s.htm#stringdesignator), or a [tree](https://franz.com/support/documentation/current/ansicl/glossary/t.htm#tree) made up of string designators.  
`quiet` - a [generalized boolean](https://franz.com/support/documentation/current/ansicl/glossary/g.htm#generalizedboolean).  
`restrict-file-names` - one of `:nocontrol`, `:lowercase`, `:uppercase`, `:unix`, `:windows`, or a [function designator](https://franz.com/support/documentation/current/ansicl/glossary/f.htm#functiondesignator).  
`page-requisites` - a generalized boolean.  

### 2.4 Logging and Input File Options [\*](https://www.gnu.org/software/wget/manual/wget.html#Logging-and-Input-File-Options)

#### `quiet` 
 
Turn off cl-wget’s output. Suppress [\*standard-output\*](https://franz.com/support/documentation/current/ansicl/dictentr/debug-io.htm).

### 2.5 Download Options [\*](https://www.gnu.org/software/wget/manual/wget.html#Download-Options)

#### `restrict-file-names`

Change which characters found in remote URLs must be escaped during generation of local filenames.
This option may also be used to force all alphabetical cases to be either lower- or uppercase.
By default, cl-wget escapes the characters that are not valid or safe as part of file names on your operating system.
This option is useful for changing these defaults, perhaps because you are downloading to a non-native partition.
The acceptable values are `:nocontrol`, `:lowercase`, `:uppercase`, `:unix`, `:windows`, and function designator.

When `:windows` is given, cl-wget uses `+` instead of `:` to separate host and port in local file names, and uses `@` instead of `?` to separate the query portion of the file name from the rest.
Therefore, a URL that would be saved as `www.xemacs.org:4300/search.pl?input=blah` in `:unix` mode would be saved as `www.xemacs.org+4300/search.pl@input=blah` in `:windows` mode.
For consistency and compatibility, cl-wget is in `:windows` mode by default, since Microsoft Windows is incapable of handling filenames containing characters `?` and `:`.

When a function designator is given, cl-wget uses `(funcall restrict-file-names url)` as the pathname. For example,
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

### 2.11 Recursive Retrieval Options [\*](https://www.gnu.org/software/wget/manual/wget.html#Recursive-Retrieval-Options) 

#### `page-requisites`

Ordinarily, when downloading a single HTML page, any requisite documents that may be needed to display it properly are not downloaded.
This option causes cl-wget to download all the files that are necessary to properly display a given HTML page.
This includes such things as inlined images, sounds, and referenced stylesheets.

## 7 Examples [\*](https://www.gnu.org/software/wget/manual/wget.html#Examples)
### 7.1 Simple Usage [\*](https://www.gnu.org/software/wget/manual/wget.html#Simple-Usage)
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
