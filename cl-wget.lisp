"
    cl-wget is a free utility for retrieving network data using HTTPS.
    Copyright (C) 2021  Lin2Jing4

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU Affero General Public License as published
    by the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Affero General Public License for more details.

    You should have received a copy of the GNU Affero General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
"

(uiop:define-package
    cl-wget
    (:mix
     drakma
     lquery
     puri
     trivial-download)
  (:export wget))

(in-package cl-wget)

(defun wget (urls &key quiet restrict-file-names page-requisites)
  (subst
   t t (list urls) :key
   (lambda (url)
     (unless (listp url)
       (map
        'vector
        (lambda (ur)
          (let ((u (render-uri (merge-uris ur url) nil)))
            (handler-case
                (download
                 u
                 (case restrict-file-names
                   ((:nocontrol) u)
                   ((:lowercase) (string-downcase u))
                   ((:uppercase) (string-upcase u))
                   ((:unix) u)
                   ((:windows nil) (substitute #\+ #\: (substitute #\@ #\? u)))
                   (otherwise (funcall restrict-file-names u)))
                 :quiet (or quiet (terpri)))
              (t (cond) (or quiet (warn "~%~a~%~a~%~a" ur u cond))))))
        (if
         page-requisites
         (let ((dom ($ (initialize (http-request url)))))
           (concatenate
            'vector
            (vector url)
            ($ dom "[src]" (attr :src))
            ($ dom "[href]" (attr :href))))
         (vector url)))))))
