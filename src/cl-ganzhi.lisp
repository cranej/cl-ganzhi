(in-package #:cl-ganzhi)

(defun convert-timestring (timestring &key term-passed)
  (if timestring
      (convert (local-time:parse-timestring timestring
                                            :allow-missing-timezone-part t
                                            :offset #.(* 8 3600))
               :term-passed term-passed)
      (convert-now :term-passed term-passed)))

(defun convert-now (&key term-passed)
  (convert (local-time:now) :term-passed term-passed))

(defun convert (time &key term-passed)
  (let* ((month-zhi (calc-month-zhi time :term-passed term-passed))
         (year-ganzhi (calc-year-ganzhi time month-zhi))
         (year-gan (car year-ganzhi))
         (year-zhi (cdr year-ganzhi))
         (month-gan (calc-month-gan month-zhi year-gan))
         (day-ganzhi (calc-day-ganzhi time))
         (day-gan (car day-ganzhi))
         (day-zhi (cdr day-ganzhi))
         (hour-zhi (calc-hour-zhi (local-time:timestamp-hour time)))
         (hour-gan (calc-hour-gan hour-zhi day-gan)))
    (values year-gan
            year-zhi
            month-gan
            month-zhi
            day-gan
            day-zhi
            hour-gan
            hour-zhi)))

(define-condition confirm-term ()
  ((term :reader confirm-term-term :initarg :term))
  (:report (lambda (condition stream)
             (format stream "Need to confirm whether term ~a is already passed."
                     (confirm-term-term condition)))))

(defun calc-month-zhi (time &key term-passed)
  (local-time:with-decoded-timestamp (:month month :day day) time
    (let* ((term-index (position-if
                        #'(lambda (term) (= (st-month term) month))
                        +12-solar-terms+))
           (term (aref +12-solar-terms+ term-index))
           (junc-start (st-junc-start term))
           (junc-end (st-junc-end term)))
      (cond ((< day junc-start)
             (st-dizhi (prev-solar-term term-index)))
            ((> day junc-end)
             (st-dizhi term))
            (term-passed (st-dizhi term))
            (t (restart-case (error 'confirm-term :term (st-name term))
                 (as-passed ()
                   :report "Treat as solar-term passed."
                   (st-dizhi term))
                 (as-not-passed ()
                   :report "Treat as solar-term has not passed yet."
                   (st-dizhi (prev-solar-term term-index)))))))))

(defun calc-month-gan (month-zhi year-gan)
  (let ((gan-start (position (ecase year-gan
                               ((甲 己) '丙)
                               ((乙 庚) '戊)
                               ((丙 辛) '庚)
                               ((丁 壬) '壬)
                               ((戊 癸) '甲))
                             +tiangan+))
        (gan-offset
          (mod (- (position month-zhi +dizhi+)
                  (position '寅 +dizhi+))
               12)))
    (aref +tiangan+
          (mod (+ gan-start gan-offset) 10))))

(defun calc-year-ganzhi (time &optional month-zhi)
  (local-time:with-decoded-timestamp (:month month :year year) time
    (let ((effective-year (cond ((> month 2) year)
                                ((= month 2)
                                 (if (eq (or month-zhi (calc-month-zhi time))
                                         '寅)
                                     year
                                     (1- year)))
                                ((= month 1) (1- year)))))
      (aref +60-GAN-ZHI+
            (mod (- effective-year
                    (local-time:timestamp-year +anchor-time+))
                 60)))))

(defun calc-day-ganzhi (time)
  (let ((day-diff (+ (ltd:duration-as
                      (ltd:timestamp-difference time +anchor-time+)
                      :day)
                     (if (= (local-time:timestamp-hour time) 23) 1 0))))
    (aref +60-GAN-ZHI+
          (mod day-diff 60))))

(defun calc-hour-zhi (hour)
  (aref +dizhi+ (mod (ceiling hour 2) 12)))

(defun calc-hour-gan (hour-zhi day-gan)
  (let ((gan-start (position (ecase day-gan
                               ((甲 己) '甲)
                               ((乙 庚) '丙)
                               ((丙 辛) '戊)
                               ((丁 壬) '庚)
                               ((戊 癸) '壬))
                             +tiangan+))
        (gan-offset (mod (position hour-zhi +dizhi+) 12)))
    (aref +tiangan+
          (mod (+ gan-start gan-offset) 10))))

(defun calc-xunkong (day-gan day-zhi)
  "计算旬空。"
  (let* ((day-gan-index (position day-gan +tiangan+))
	 (day-zhi-index (position day-zhi +dizhi+))
	 (kong-1 (+ (- 10 day-gan-index)
		    day-zhi-index))
	 (kong-2 (1+ kong-1)))
    (list (aref +dizhi+ (mod kong-1 12))
	  (aref +dizhi+ (mod kong-2 12)))))
