;;;; cl-id3.lisp

(in-package #:cl-id3)

(define-binary-type id3-tag-size () (unsigned-integer :bytes 4 :bits-per-byte 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ID3 

(defun frame-compressed-p (flags) (logbitp 7 flags))

(defun frame-encrypted-p (flags) (logbitp 6 flags))

(defun frame-grouped-p (flags) (logbitp 5 flags))

(define-binary-type optional (type if)
  (:reader (in)
    (when if (read-value type in)))
  (:writer (out value)
    (when if (write-value type out value))))

;;; find-frame

(defun find-frame-class (name)
  (cond
    ((and (char= (char name 0) #\T)
          (not (member name '("TXX" "TXXX") :test #'string=)))
     (ecase (length name)
       (3 'text-info-frame-v2.2)
       (4 'text-info-frame-v2.3)))
    ((string= name "COM")  'comment-frame-v2.2)
    ((string= name "COMM") 'comment-frame-v2.3)
    (t
     (ecase (length name)
       (3 'generic-frame-v2.2)
       (4 'generic-frame-v2.3)))))

(define-tagged-binary-class id3v2.2-frame ()
  ((id (frame-id :length 3))
   (size u3))
  (:dispatch (find-frame-class id)))

(define-tagged-binary-class id3v2.3-frame ()
  ((id                (frame-id :length 4))
   (size              u4)
   (flags             u2)
   (decompressed-size (optional :type 'u4 :if (frame-compressed-p flags)))
   (encryption-scheme (optional :type 'u1 :if (frame-encrypted-p flags)))
   (grouping-identity (optional :type 'u1 :if (frame-grouped-p flags))))
  (:dispatch (find-frame-class id)))

;;; id3-frames

(defgeneric frame-header-size (frame))

(defmethod frame-header-size ((frame id3v2.2-frame)) 6)

(defmethod frame-header-size ((frame id3v2.3-frame)) 10)

(defun read-frame (frame-type in)
  (handler-case (read-value frame-type in)
    (in-padding ()  nil)))

(define-condition in-padding () ())

(define-binary-type id3-frames (tag-size frame-type)
  (:reader (in)
    (loop with to-read = tag-size
          while (plusp to-read)
          for frame = (read-frame frame-type in)
          while frame
          do (decf to-read (+ (frame-header-size frame) (size frame)))
          collect frame
          finally (loop repeat (1- to-read) do (read-byte in))))
  (:writer (out frames)
    (loop with to-write = tag-size
          for frame in frames
          do (write-value frame-type out frame)
          (decf to-write (+ (frame-header-size frame) (size frame)))
          finally (loop repeat to-write do (write-byte 0 out)))))

(define-binary-type frame-id (length)
  (:reader (in)
    (let ((first-byte (read-byte in)))
      (when (= first-byte 0) (signal 'in-padding))
      (let ((rest (read-value 'iso-8859-1-string in :length (1- length))))
        (concatenate
         'string (string (code-char first-byte)) rest))))
  (:writer (out id)
    (write-value 'iso-8859-1-string out id :length length)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ID3 tag class

;;; Handle ID3v2.2 and ID3v2.3 (but not ID3v2.4 since it mostly just
;;; requires a bunch more string encoding foo.) (Well, we can mostly
;;; read v2.4 tags as v2.3 tags.)

(define-tagged-binary-class id3-tag ()
  ((identifier     (iso-8859-1-string :length 3))
   (major-version  u1)
   (revision       u1)
   (flags          u1)
   (size           id3-tag-size))
  (:dispatch 
   (ecase major-version
     (2 'id3v2.2-tag)
     (3 'id3v2.3-tag))))



(define-binary-class id3v2.2-tag (id3-tag)
  ((frames (id3-frames :tag-size size :frame-type 'id3v2.2-frame))))

(defun extended-p (flags) (logbitp 6 flags))

(defun crc-p (flags extra-flags)
  (and (extended-p flags) (logbitp 15 extra-flags)))

(define-binary-class id3v2.3-tag (id3-tag)
  ((extended-header-size (optional :type 'u4 :if (extended-p flags)))
   (extra-flags          (optional :type 'u2 :if (extended-p flags)))
   (padding-size         (optional :type 'u4 :if (extended-p flags)))
   (crc                  (optional :type 'u4 :if (crc-p flags extra-flags)))
   (frames               (id3-frames :tag-size size :frame-type 'id3v2.3-frame))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic frames

(defgeneric data-bytes (frame))

(defmethod data-bytes ((frame id3v2.2-frame))
  (size frame))

(defmethod data-bytes ((frame id3v2.3-frame))
  (let ((flags (flags frame)))
    (- (size frame)
       (if (frame-compressed-p flags) 4 0)
       (if (frame-encrypted-p flags) 1 0)
       (if (frame-grouped-p flags) 1 0))))

(define-binary-type raw-bytes (size)
  (:reader (in)
    (let ((buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf))
  (:writer (out buf)
    (write-sequence buf out)))

(define-binary-class generic-frame ()
  ((data (raw-bytes :size (data-bytes (current-binary-object))))))

(define-binary-class generic-frame-v2.2 (id3v2.2-frame generic-frame) ())

(define-binary-class generic-frame-v2.3 (id3v2.3-frame generic-frame) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ID3 encoded string

(defun non-terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-string)
    (1 'ucs-2-string)))

(defun terminated-type (encoding)
  (ecase encoding
    (0 'iso-8859-1-terminated-string)
    (1 'ucs-2-terminated-string)))

(defun string-args (encoding length terminator)
  (cond 
    (length
     (values (non-terminated-type encoding) :length length))
    (terminator
     (values (terminated-type encoding) :terminator terminator))))

(define-binary-type id3-encoded-string (encoding length terminator)
  (:reader (in) 
    (multiple-value-bind (type keyword arg)
        (string-args encoding length terminator)
      (read-value type in keyword arg)))
  (:writer (out string)
    (multiple-value-bind (type keyword arg)
        (string-args encoding length terminator)
      (write-value type out string keyword arg))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Text info and comment frames

(defun bytes-left (bytes-read)
  (- (size (current-binary-object)) bytes-read))

(defun encoded-string-length (string encoding terminated)
  (let ((characters (+ (length string)
                       (if terminated 1 0)
                       (ecase encoding (0 0) (1 1)))))
    (* characters (ecase encoding (0 1) (1 2)))))

(define-binary-class text-info-frame ()
  ((encoding u1)
   (information (id3-encoded-string :encoding encoding :length (bytes-left 1)))))

(define-binary-class comment-frame ()
  ((encoding u1)
   (language (iso-8859-1-string :length 3))
   (description (id3-encoded-string :encoding encoding :terminator +null+))
   (text (id3-encoded-string
          :encoding encoding
          :length (bytes-left
                   (+ 1 ;; encoding 
                      3 ;; language
                      (encoded-string-length description encoding t)))))))

(defmethod (setf information) :after (value (frame text-info-frame))
  (declare (ignore value))
  (with-slots (encoding size information) frame
    (setf size (encoded-string-length information encoding nil))))

;;; Had to reverse order of superclasses to make WRITE-VALUE work correctly
(define-binary-class text-info-frame-v2.2 (text-info-frame id3v2.2-frame) ())

(define-binary-class text-info-frame-v2.3 (text-info-frame id3v2.3-frame) ())

(define-binary-class comment-frame-v2.2 (comment-frame id3v2.2-frame) ())

(define-binary-class comment-frame-v2.3 (comment-frame id3v2.3-frame) ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Application code

(defun mp3-p (file)
  (and
   (not (directory-pathname-p file))
   (string-equal "mp3" (pathname-type file))))

(defun id3-p (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (string= "ID3" (read-value 'iso-8859-1-string in :length 3))))

(defun read-id3 (file)
  (with-open-file (in file :element-type '(unsigned-byte 8))
    (read-value 'id3-tag in)))

(defun show-tag-header (file)
  (with-slots (identifier major-version revision flags size) (read-id3 file)
    (format t "~a ~d.~d ~8,'0b ~d bytes -- ~a~%"
            identifier major-version revision flags size (enough-namestring file))))

(defun show-tag-headers (dir) 
  (walk-directory dir #'show-tag-header :test #'mp3-p))

(defun count-versions (dir)
  (let ((versions (mapcar #'(lambda (x) (cons x 0)) '(2 3 4))))
    (flet ((count-version (file)
             (incf (cdr (assoc (major-version (read-id3 file)) versions)))))
      (walk-directory dir #'count-version :test #'mp3-p))
    versions))

(defun frame-types (file)
  (delete-duplicates (mapcar #'id (frames (read-id3 file))) :test #'string=))

(defun frame-types-in-dir (dir)
  (let ((ids ()))
    (flet ((collect (file)
             (setf ids (nunion ids (frame-types file) :test #'string=))))
      (walk-directory dir #'collect :test #'mp3-p))
    ids))

(defun frame-name-member (id)
  (cond
    ((member id '("COM" "COMM") :test #'string=) "Comment")
    ((member id '("TAL" "TALB") :test #'string=) "Album")
    ((member id '("TCM" "TCOM") :test #'string=) "Composer")
    ((member id '("TCO" "TCON") :test #'string=) "Genre")
    ((member id '("TEN" "TENC") :test #'string=) "Encoding program")
    ((member id '("TP1" "TPE1") :test #'string=) "Artist")
    ((member id '("TPA" "TPOS") :test #'string=) "Part of set")
    ((member id '("TRK" "TRCK") :test #'string=) "Track")
    ((member id '("TT2" "TIT2") :test #'string=) "Song")
    ((member id '("TYE" "TYER") :test #'string=) "Year")
    (t id)))

;; As a hack in the ID3 format the string in a text info frame can
;; have an embedded null. Programs are not supposed to display any
;; information beyond the null. SUBSEQ and POSITION work together
;; nicely in this case since a NIL third argument to SUBSEQ is
;; equivalent to the length of the string.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extracting information from ID3 tag

(defun find-frame (id3 ids)
  (find-if #'(lambda (x) (find (id x) ids :test #'string=)) (frames id3)))

(defun upto-null (string)
  (subseq string 0 (position +null+ string)))

(defun get-text-info (id3 &rest ids)
  (let ((frame (find-frame id3 ids)))
    (when frame (upto-null (information frame)))))

(defmethod information ((frame generic-frame-v2.3))
  (with-output-to-string (s)
    (loop for byte across (data frame) do
          (format s "~2,'0x" byte))))

(defun album (id3) (get-text-info id3 "TAL" "TALB"))

(defun composer (id3) (get-text-info id3 "TCM" "TCOM"))

(defun genre (id3) (get-text-info id3 "TCO" "TCON"))

(defun encoding-program (id3) (get-text-info id3 "TEN" "TENC"))

(defun artist (id3) (get-text-info id3 "TP1" "TPE1"))

(defun part-of-set (id3) (get-text-info id3 "TPA" "TPOS"))

(defun track (id3) (get-text-info id3 "TRK" "TRCK"))

(defun title (id3) (get-text-info id3 "TT2" "TIT2"))

(defun year (id3) (get-text-info id3 "TYE" "TYER" "TDRC"))

;;; The first version of the ID3 format used a single byte to encode
;;; the genre. There were originally 80 official v1 genres. The makers
;;; of Winamp extended the list. 

(defparameter *id3-v1-genres*
  #(
    ;; These are the official ID3v1 genres.
    "Blues" "Classic Rock" "Country" "Dance" "Disco" "Funk" "Grunge"
    "Hip-Hop" "Jazz" "Metal" "New Age" "Oldies" "Other" "Pop" "R&B" "Rap"
    "Reggae" "Rock" "Techno" "Industrial" "Alternative" "Ska"
    "Death Metal" "Pranks" "Soundtrack" "Euro-Techno" "Ambient"
    "Trip-Hop" "Vocal" "Jazz+Funk" "Fusion" "Trance" "Classical"
    "Instrumental" "Acid" "House" "Game" "Sound Clip" "Gospel" "Noise"
    "AlternRock" "Bass" "Soul" "Punk" "Space" "Meditative"
    "Instrumental Pop" "Instrumental Rock" "Ethnic" "Gothic" "Darkwave"
    "Techno-Industrial" "Electronic" "Pop-Folk" "Eurodance" "Dream"
    "Southern Rock" "Comedy" "Cult" "Gangsta" "Top 40" "Christian Rap"
    "Pop/Funk" "Jungle" "Native American" "Cabaret" "New Wave"
    "Psychadelic" "Rave" "Showtunes" "Trailer" "Lo-Fi" "Tribal"
    "Acid Punk" "Acid Jazz" "Polka" "Retro" "Musical" "Rock & Roll"
    "Hard Rock"

    ;; These were made up by the authors of Winamp but backported into
    ;; the ID3 spec.
    "Folk" "Folk-Rock" "National Folk" "Swing" "Fast Fusion"
    "Bebob" "Latin" "Revival" "Celtic" "Bluegrass" "Avantgarde"
    "Gothic Rock" "Progressive Rock" "Psychedelic Rock" "Symphonic Rock"
    "Slow Rock" "Big Band" "Chorus" "Easy Listening" "Acoustic" "Humour"
    "Speech" "Chanson" "Opera" "Chamber Music" "Sonata" "Symphony"
    "Booty Bass" "Primus" "Porn Groove" "Satire" "Slow Jam" "Club"
    "Tango" "Samba" "Folklore" "Ballad" "Power Ballad" "Rhythmic Soul"
    "Freestyle" "Duet" "Punk Rock" "Drum Solo" "A capella" "Euro-House"
    "Dance Hall"

    ;; These were also invented by the Winamp folks but ignored by the
    ;; ID3 authors.
    "Goa" "Drum & Bass" "Club-House" "Hardcore" "Terror" "Indie"
    "BritPop" "Negerpunk" "Polsk Punk" "Beat" "Christian Gangsta Rap"
    "Heavy Metal" "Black Metal" "Crossover" "Contemporary Christian"
    "Christian Rock" "Merengue" "Salsa" "Thrash Metal" "Anime" "Jpop"
    "Synthpop"))

(defun translate-v1-genre (genre)
  (aref *id3-v1-genres* (parse-integer genre :start 1 :junk-allowed t)))

(defun translated-genre (id3)
  (let ((genre (genre id3)))
    (if (and genre (char= #\( (char genre 0)))
      (translate-v1-genre genre)
      genre)))

;;; Write ID3V2.3 text frames in UTF-8
;;; Uses FLEXI-STREAMS to determine correct size of frame-frames can be written, but not read
;;; with incorrect size

(defun make-text-info-frame (&key id information)
  (let* ((local-stream (flexi-streams:make-in-memory-output-stream))
         (test-frame (make-instance 'text-info-frame-v2.3 :id id :information information
                       :size 0 :decompressed-size nil :encryption-scheme nil 
                       :grouping-identity nil :encoding 1 :flags 0)))
    (write-value 'text-info-frame-v2.3 local-stream test-frame)
    (make-instance 'text-info-frame-v2.3 :id id :information information
      :size (- (length (flexi-streams:get-output-stream-sequence local-stream)) 10) 
      :decompressed-size nil :encryption-scheme nil :grouping-identity nil 
      :encoding 1 :flags 0)))

(defun update-text-info-frame (id3-tag id information)
  "Rewrites frame of id3-tag with id using information, or creates it if it does not exist."
    (let* ((frames (slot-value id3-tag 'frames))
           (local-stream (flexi-streams:make-in-memory-output-stream))
           (pred (lambda (x)
                   (equal (slot-value x 'id) id)))
           (frames (remove-if pred frames))
           (new-frame (make-text-info-frame :id id :information information)))
      (setf (slot-value id3-tag 'frames) (push new-frame frames))
      (write-value 'id3v2.3-tag local-stream id3-tag)
      (setf (slot-value id3-tag 'size) 
            (- (length (flexi-streams:get-output-stream-sequence local-stream)) 10)))
    id3-tag)

;;; Write ID3V2.3 tag

(defun make-id3-tag (&key title artist album genre year composer encoding-program part-of-set
                          track)
  "Makes an id3v2.3 tag object with text fields matching keys."
  (let ((text-frames (list)))
    (when title (push (make-text-info-frame :id "TIT2" :information title) text-frames))
    (when artist (push (make-text-info-frame :id "TPE1" :information artist) text-frames))
    (when album (push (make-text-info-frame :id "TALB" :information album) text-frames))
    (when genre (push (make-text-info-frame :id "TCON" :information genre) text-frames))
    (when year (push (make-text-info-frame :id "TYER" :information year) text-frames))
    (when composer (push (make-text-info-frame :id "TCOM" :information composer) text-frames))
    (when encoding-program
      (push (make-text-info-frame :id "TENC" :information encoding-program) text-frames))
    (when part-of-set
      (push (make-text-info-frame :id "TPOS" :information part-of-set) text-frames))
    (when track (push (make-text-info-frame :id "TRCK" :information track) text-frames))
    (let* ((local-stream (flexi-streams:make-in-memory-output-stream))
           (test-tag (make-instance 'id3v2.3-tag :identifier "ID3" :major-version 3
                       :revision 0 :flags 0 :size 0 :extended-header-size nil 
                       :extra-flags nil :padding-size nil :crc nil :frames text-frames)))
      (write-value 'id3v2.3-tag local-stream test-tag)
      (make-instance 'id3v2.3-tag :identifier "ID3" :major-version 3 :revision 0 :flags 0
        :size (- (length (flexi-streams:get-output-stream-sequence local-stream)) 10)
        :extended-header-size nil :extra-flags nil :padding-size nil :crc nil 
        :frames text-frames))))

(defun id3-tag-size (file)
  (if (id3-p file) (+ 10 (slot-value (read-id3 file) 'size)) 0))

(defun replace-id3 (id3-tag file)
  "Rewrites mp3 file with provided id3 tag."
  (let ((tag-size (id3-tag-size file))
        (mp3 nil))
    (with-open-file (in file :direction :input :element-type '(unsigned-byte 8))
      (let ((buffer (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence buffer in)
        (setf mp3 (subseq buffer tag-size))))
    (with-open-file (out file :direction :output :if-exists :overwrite
                                             :element-type '(unsigned-byte 8))
      (write-value 'id3v2.3-tag out id3-tag)
      (write-sequence mp3 out)
      (pathname out))))

