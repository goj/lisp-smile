(define (require file)
  (let* ((dot-pos (string-rindex file #\.))
         (base-name (substring file 0 dot-pos))
         (scm-name (string-append base-name ".scm")))
    (system
      (string-append
        "translate-smile "
        file
        " > "
        scm-name))
    (load scm-name)))
