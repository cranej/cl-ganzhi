(in-package #:cl-ganzhi.test)

(defun run-tests ()
  (1am:run))

(test the-time
  (is (destructuring-bind ((yg . yz) (mg . mz) (dg . dz) (hg . hz))
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
  (is (let ((before-23 (nth 2 (convert-timestring "2024-12-15T22:59:59")))
            (after-23 (nth 2 (convert-timestring "2024-12-15T23:00:00"))))
        (and (equal before-23 '(癸 . 丑))
             (equal after-23 '(甲 . 寅))))))

(test year-not-move-befor-spring
  (is (let ((before (car (convert-timestring "2024-02-01T00:00:00")))
            (after (car (convert-timestring "2024-02-06T00:00:00"))))
        (and (equal before '(癸 . 卯))
             (equal after '(甲 . 辰))))))

(test day-not-move-at-23-when-split
  (is (let ((*split-zi-shi* t))
        (let ((before-23 (nth 2 (convert-timestring "2024-12-15T22:59:59")))
              (after-23 (nth 2 (convert-timestring "2024-12-15T23:00:00"))))
          (and (equal before-23 '(癸 . 丑))
               (equal after-23 '(癸 . 丑)))))))

(test split-not-affect-hour-gan-zhi
  (is (let ((split-off (let ((*split-zi-shi* nil))
                           (convert-timestring "2024-12-15T23:00:00")))
            (split-on (let ((*split-zi-shi* t))
                        (convert-timestring "2024-12-15T23:00:00"))))
        (and (equal (nth 3 split-off) (nth 3 split-on))
             (not (equal (nth 2 split-off) (nth 2 split-on)))))))

(test split-not-affect-after-zero-clock
  (is (let ((split-off (let ((*split-zi-shi* nil))
                           (convert-timestring "2024-12-15T00:10:00")))
            (split-on (let ((*split-zi-shi* t))
                        (convert-timestring "2024-12-15T00:10:00"))))
        (and (equal (nth 3 split-off) (nth 3 split-on))
             (equal (nth 2 split-off) (nth 2 split-on))))))

(test no-chinese-character-leak
  (is (let ((*no-chinese-character* t))
        (let ((ganzhi (convert-now))
              (xunkong (calc-xunkong 'Jia 'Yin)))
          (and (loop for (g . z) in ganzhi
                     when (or (find g cl-ganzhi::+tiangan+)
                              (find z cl-ganzhi::+dizhi+))
                       do (return nil)
                     finally (return t))
               (not (find (car xunkong) cl-ganzhi::+dizhi+))
               (not (find (cdr xunkong) cl-ganzhi::+dizhi+)))))))

(test no-chinese-character-leak-in-condition
  (is (let ((*no-chinese-character* t))
        (handler-case (convert-timestring "2024-02-03T15:00:00+08:00")
          (confirm-term (c)
            (string= (confirm-term-term c) "Spring Commences"))
          (:no-error (result)
            (declare (ignore result))
            nil)))))
