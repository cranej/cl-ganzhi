(in-package #:cl-ganzhi)

(defvar *split-zi-shi* nil
  "如果值为 t , 以零点区分早晚子时。 这是个争论了几百年的问题了，影响 23：00 到 23：59：59 时间段的\"天干\"的计算。

If value is 't', split 子时 at 0:00 . This is a controversial issue that people have been augmenting for hundreds years. Impact the calculation of 天干 of the day during 23:00 ~ 23:59:59.")

(defun convert-timestring (timestring &key term-passed)
  "转换公历 timestring 到干支历。 

* 如果 timestring 是 nil ， 约等于 ``(convert-now)``；
* 否则约等于 ``(convert (parse-timestring timestring))``。

Convenient wrapper over ``convert``:

* if ``timestring`` is nil , roughly equals ``(convert-now)``;
* otherwise roughly equals ``(convert (parse-timestring timestring))``."
  (if timestring
      (convert (local-time:parse-timestring timestring
                                            :allow-missing-timezone-part t
                                            :offset #.(* 8 3600))
               :term-passed term-passed)
      (convert-now :term-passed term-passed)))

(defun convert-now (&key term-passed)
  "转换当前时间到干支历 == ``(convert (local-time:now))``。

Convenient wrapper over ``convert``: ``(convert (local-time:now))``."
  (convert (local-time:now) :term-passed term-passed))

(defun convert (time &key term-passed)
  "转换 ``time`` （``local-time:timestamp``的实例）到干支历。 返回一个包含四个 dotted list 的 list： 分别是年、月、日、时的干支对。

这个 function **不处理** 节气转换的问题。 如果 ``time`` 处在十二节中某一节的交接时期内（例如 2 月 3 日到 2 月 5 日之间）， 会 signal 一个 ``confirm-term`` condition。 此时调用者可以在确认节气是否已经交接后（比如要求用户确认），调用预先提供好的两个 restart ``as-passed`` 或者 ``as-not-passed``。 或者再次调用 ``convert``， 设置参数 ``term-passed``。

日干支的计算受到变量 ``*split-zi-zhi`` 的影响，请参考该变量的文档。

Convert ``time`` which is a ``local-time:timestamp`` to Chinese GanZhi calendar datetime. Returns a list of four dotted lists: GanZhi pair for year, month, day, and hour parts.

This function **does not** handle solar term junction. If the ``time`` is inside the junction period of one of the 12 minor solar terms (十二节), a ``confirm-term`` condition is signaled. Caller should handle the condition by either invoking one of the two provided restarts ``as-passed`` and ``as-not-passed``, or by calling ``convert`` again with parameter ``term-passed`` set. 

Affected by variable ``*split-zi-shi*``, please refer to the variable's doc."
  (let* ((month-zhi (calc-month-zhi time :term-passed term-passed))
         (year-ganzhi (calc-year-ganzhi time month-zhi))
         (month-gan (calc-month-gan month-zhi (car year-ganzhi)))
         (day-ganzhi (calc-day-ganzhi time))
         (hour-ganzhi (calc-hour-ganzhi (local-time:timestamp-hour time)
                                        (car day-ganzhi))))
    (list year-ganzhi
          (cons month-gan month-zhi)
          day-ganzhi
          hour-ganzhi)))

(defun calc-xunkong (day-gan day-zhi)
  "计算旬空。 返回 dotted list (旬空1 . 旬空2) 。

Calculation the two DiZhi which having a bye. Returns dotted list (bye1 . bye2)."
  (let* ((day-gan-index (position day-gan +tiangan+))
	 (day-zhi-index (position day-zhi +dizhi+))
	 (kong-1 (+ (- 10 day-gan-index)
		    day-zhi-index))
	 (kong-2 (1+ kong-1)))
    (cons (aref +dizhi+ (mod kong-1 12))
	  (aref +dizhi+ (mod kong-2 12)))))

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
                     (if (and (not *split-zi-shi*)
                              (= (local-time:timestamp-hour time) 23))
                         1
                         0))))
    (aref +60-GAN-ZHI+
          (mod day-diff 60))))

(defun calc-hour-ganzhi (hour day-gan)
  (let ((day-gan (if (and *split-zi-shi* (= hour 23))
                     (shift-in-array +tiangan+ day-gan)
                     day-gan))
        (hour-zhi (aref +dizhi+ (mod (ceiling hour 2) 12))))
    (let ((gan-start (position (ecase day-gan
                                 ((甲 己) '甲)
                                 ((乙 庚) '丙)
                                 ((丙 辛) '戊)
                                 ((丁 壬) '庚)
                                 ((戊 癸) '壬))
                               +tiangan+))
          (gan-offset (mod (position hour-zhi +dizhi+) 12)))
      (cons (aref +tiangan+
                  (mod (+ gan-start gan-offset) 10))
            hour-zhi))))
