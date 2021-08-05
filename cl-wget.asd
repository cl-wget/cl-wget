(defsystem cl-wget
  :depends-on (drakma
               lquery
               puri
               trivial-download)
  :components ((:file cl-wget)))
