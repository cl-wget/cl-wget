"
    cl-wget is a free utility for retrieving network data using HTTPS.
    Copyright (C) <year>  <name of author>

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

(defun wget (uris &key --page-requisites --quiet)
  (map
   (type-of uris)
   (lambda (uri)
     (if
      --page-requisites
      (wget
       (map
        'vector
        (lambda (u) (render-uri (merge-uris u uri) nil))
        (let ((dom ($ (initialize (http-request uri)))))
          (concatenate
           'vector
           (vector uri)
           ($ dom "[src]" (attr :src))
           ($ dom "[href]" (attr :href)))))
       :--quiet --quiet)
      (if
       (uiop:file-pathname-p uri)
       (download uri uri :quiet (or --quiet (terpri)))
       uri)))
   uris))
