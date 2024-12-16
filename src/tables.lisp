(in-package :cl-ganzhi)

(defconstant +symbol-pinyin-map+
  '((甲 . Jia) (乙 . Yi) (丙 . Bing) (丁 . Ding) (戊 . Wu)
    (己 . Ji) (庚 . Geng) (辛 . Xin) (壬 . Ren) (癸 . Gui)

    (子 . Zi) (丑 . Chou) (寅 . Yin) (卯 . Mao) (辰 . Chen) (巳 . Si)
    (午 . Wu) (未 . Wei) (申 . Shen) (酉 . You) (戌 . Xu) (亥 . Hai)

    (木 . Wood) (火 . Fire) (土 . Earth)(金 . Metal) (水 . Water)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-rrstruct (solar-term (:conc-name st-))
      (name string) (month fixnum) (junc-start fixnum) (junc-end fixnum) (dizhi symbol))

  (defun translate-symbol (sym)
    (cdr (assoc sym +symbol-pinyin-map+)))

  (defun symbol-with-plist (sym-data)
    (destructuring-bind (sym &rest plist) sym-data
      (setf (symbol-plist sym) plist)
      (let ((pinyin-sym (translate-symbol sym)))
        (when pinyin-sym
          (setf (symbol-plist pinyin-sym)
                (loop for (key value) on plist by #'cddr
                      append (list key
                                   (or (translate-symbol value)
                                       value))))))
      sym)))

(defconstant +12-solar-terms+
  (map 'vector
       #'(lambda (term-data)
           (destructuring-bind (name month junc-start junc-end dizhi) term-data
             (make-solar-term :name name
                              :month month
                              :junc-start junc-start
                              :junc-end junc-end
                              :dizhi dizhi)))
       '(("立春" 2 3 5 寅) ("惊蛰" 3 5 6 卯) ("清明" 4 5 6 辰)
         ("立夏" 5 5 7 巳) ("芒种" 6 5 7 午) ("小暑" 7 6 8 未)
         ("立秋" 8 7 9 申) ("白露" 9 7 9 酉) ("寒露" 10 8 9 戌)
         ("立冬" 11 7 8 亥) ("大雪" 12 6 8 子) ("小寒" 1 5 7 丑))))

(defconstant +tiangan+
  (map 'vector
       #'symbol-with-plist
       '((甲 :wuxing 木) (乙 :wuxing 木)
         (丙 :wuxing 火) (丁 :wuxing 火)
         (戊 :wuxing 土) (己 :wuxing 土)
         (庚 :wuxing 金) (辛 :wuxing 金)
         (壬 :wuxing 水) (癸 :wuxing 水))))

(defconstant +dizhi+
  (map 'vector
       #'symbol-with-plist
       '((子 :wuxing 水) (丑 :wuxing 土) (寅 :wuxing 木)
         (卯 :wuxing 木) (辰 :wuxing 土) (巳 :wuxing 火)
         (午 :wuxing 火) (未 :wuxing 土) (申 :wuxing 金)
         (酉 :wuxing 金) (戌 :wuxing 土) (亥 :wuxing 水))))

(defconstant +solar-terms-english+
  '(("立春" . "Spring Commences") ("惊蛰" . "Insects Waken")
    ("清明" . "Bright and Clear") ("立夏" . "Summer Commences")
    ("芒种" . "Grain in Ear") ("小暑" . "Moderate Heat")
    ("立秋" . "Autumn Commences") ("白露" . "White Dew")
    ("寒露" . "Cold Dew") ("立冬" . "Winter Commences")
    ("大雪" . "Heavy Snow") ("小寒" . "Moderate Cold")))

(defun shift-in-array (array item &key (offset 1) (test 'eql))
  (let ((pos (position item array :test test)))
    (when pos
      (aref array
            (mod (+ pos offset) (length array))))))

(defun next-solar-term (i)
  (aref +12-solar-terms+ (mod (1+ i) 12)))

(defun prev-solar-term (i)
  (aref +12-solar-terms+ (mod (1- i) 12)))

(defconstant +anchor-time+
  (local-time:encode-timestamp 0 0 0 0 15 2 1924)
  "The anchor date tiem point.")

(defconstant +60-gan-zhi+
  (coerce (loop for i from 0 upto 59
                for tg = (aref +tiangan+ (mod i 10))
                for dz = (aref +dizhi+ (mod i 12))
                collect (cons tg dz))
          'vector))
