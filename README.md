# cl-wget
This file documents the cl-wget utility for downloading network data.

### Overview
cl-wget is a free software for non-interactive download of files from the Web.

### Syntax:

`wget urls &key --page-requisites --quiet` => `result`

### Arguments and Values:
`urls` --- a [proper sequence](http://clhs.lisp.se/Body/26_glo_p.htm#proper_sequence).  
`result` --- a [proper sequence](http://clhs.lisp.se/Body/26_glo_p.htm#proper_sequence).  
`--page-requisites` --- a [generalized boolean](http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_g.htm#generalized_boolean).  
`--quiet` --- a [generalized boolean](http://www.lispworks.com/documentation/lw50/CLHS/Body/26_glo_g.htm#generalized_boolean).  

### Invoking
By default, cl-wget is very simple to invoke. The basic syntax is:
```cl
(cl-wget:wget (list url0 url1 url2 ...))
(cl-wget:wget (vector url0 url1 url2 ...))
```
cl-wget will simply download all the URLs specified on the command line.
However, you may wish to change some of the default parameters of cl-wget.

### Logging Options
`--quiet`:  
Turn off cl-wget’s output.

### Recursive Retrieval Options
`--page-requisites`:  
Ordinarily, when downloading a single HTML page, any requisite documents that may be needed to display it properly are not downloaded.
This option causes cl-wget to download all the files that are necessary to properly display a given HTML page.
This includes such things as inlined images, sounds, and referenced stylesheets.

### Example Usage
The sequence returned indicates that cl-wget skipped four URLs in this case:
```cl
CL-USER> (ql:quickload :cl-wget)
To load "cl-wget":
  Load 1 ASDF system:
    cl-wget
; Loading "cl-wget"

(:CL-WGET)
CL-USER> (cl-wget:wget
          '("http://www.paulgraham.com/"
            "https://www.wolframalpha.com/input/index.html")
          :--page-requisites t)

Downloading "http://www.virtumundo.com/images/spacer.gif" (43.0 B)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "http://www.virtumundo.com/images/spacer.gif" (43.0 B)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "http://ycombinator.com/arc/arc.png" (Unknown size)

Downloading "http://www.paulgraham.com/articles.html" (Unknown size)

Downloading "http://www.amazon.com/gp/product/0596006624" (2.15 kB)
.........10%.........20%.........30%.........40%.........50%.........60%.........70%.........80%.........90%.........100%
Downloading "http://www.paulgraham.com/books.html" (Unknown size)

(#("http://www.paulgraham.com/" T T T T T T T T T T T T
   "http://ycombinator.com/" T T T T T T T T T T T T T T T T T T T)
 #(T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T
   T T T T T T T T T "https://enable-javascript.com/"
   "https://www.wolframalpha.com/" T))
```
