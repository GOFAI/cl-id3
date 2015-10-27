;;;; package.lisp

(defpackage #:cl-id3
  (:use :common-lisp
        :com.gigamonkeys.binary-data
        :com.gigamonkeys.binary-data.common-datatypes
        :com.gigamonkeys.pathnames)
  (:nicknames :id3)
  (:export
   :read-id3
   :mp3-p
   :id3-p
   :album
   :composer
   :genre
   :encoding-program
   :artist
   :part-of-set
   :track
   :title
   :year
   :size
   :translated-genre
   :make-text-info-frame
   :update-text-info-frame
   :make-id3-tag
   :replace-id3))


