(in-package #:cl-ganzhi.test)

(defun run-tests ()
  (1am:run))

(defun nth-values (n fn &rest args)
  (nth n (multiple-value-list (apply fn args))))

(test the-time
  (is (multiple-value-bind (yg yz mg mz dg dz hg hz)
          (convert (local-time:encode-timestamp 0 0 57 13 15 12 2024
                                                :offset (* 8 3600)))
        (and (eq yg '甲)
             (eq yz '辰)
             (eq mg '丙)
             (eq mz '子)
             (eq dg '癸)
             (eq dz '丑)
             (eq hg '己)
             (eq hz '未)))))

(test in-term-junction
  (signals confirm-term
    (convert-timestring "2024-02-03T00:00:00")))

(test day-moved-at-23
  (is (let ((dz-before-23 (nth-values 5 #'convert-timestring "2024-12-15T22:59:59"))
            (dz-after-23 (nth-values 5 #'convert-timestring "2024-12-15T23:00:00")))
        (and (eq dz-before-23 '丑)
             (eq dz-after-23 '寅)))))

(test year-not-move-befor-spring
  (is (let ((yz (nth-values 1 #'convert-timestring "2024-02-01T00:00:00"))
            (yz-after (nth-values 1 #'convert-timestring "2024-02-06T00:00:00")))
        (and (eq yz '卯)
             (eq yz-after '辰)))))
