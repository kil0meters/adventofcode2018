(defun read-file (filename)
  (with-open-file (stream filename)
  (loop for line = (read-line stream nil)
        while line
        collect line)))

(defun list-box-checksum (filename)
  (defun times-letter-appears (line letter)
    (let ((times-found 0))
      (loop for c across line
        do (when (eq c letter)
           (incf times-found)))
      times-found))

  (defun unique-letters (line)
    (let ((letters ()))
      (loop for c across line
        do (when (not (member c letters))
           (setq letters (append letters (list c)))))
      letters))

  (let ((twos 0) (threes 0) (found-twos 0) (found-threes 0))
    (loop for line in (read-file filename)
      do (loop for c in (unique-letters line)
        do (let ((n))
           (setq n (times-letter-appears line c))
           (when (and (= 2 n) (= 0 found-twos))
             (incf twos)
             (incf found-twos))
           (when (and (= 3 n) (= 0 found-threes))
             (incf threes)
             (incf found-threes))))
         (setq found-twos 0)
         (setq found-threes 0))
    (* twos threes)))

; quality O(n^2) solution
(defun correct-box-id (filename)
  (defun get-different-characters (box boxc)
    (let ((diff 0))
      (loop for c across box for d across boxc
            do (when (not (eq c d))
               (incf diff)))
      diff))

  (defun make-adjustable-string (s)
    (make-array (length s)
                :fill-pointer (length s)
                :adjustable t
                :initial-contents s
                :element-type (array-element-type s)))

  (defun common-characters (box boxc)
    (let ((common (make-adjustable-string "")))
      (loop for c across box for d across boxc
            do (when (eq c d)
               (vector-push-extend c common)))
      common))

  (let ((common nil) (boxes (read-file filename)))
    (loop for box in boxes
          do (loop for boxc in boxes
             do (when (= 1 (get-different-characters box boxc))
                (setq common (common-characters box boxc))
                (return))))
    common))

(format t "checksum: ~D~%" (list-box-checksum "2.data"))
(format t "box id: ~A~%" (correct-box-id "2.data"))
