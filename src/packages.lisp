(defpackage #:cl-ganzhi
  (:use :common-lisp)
  (:shadow #:defconstant)
  (:export #:convert
           #:convert-now
           #:convert-timestring
           #:calc-xunkong
           #:confirm-term
           #:*split-zi-shi*
           #:*no-chinese-character*
           #:甲 #:乙 #:丙 #:丁 #:戊 #:己 #:庚 #:辛 #:壬 #:癸
           #:Jia #:Yi #:Bing #:Ding #:Wu #:Ji #:Geng #:Xin #:Ren #:Gui
           #:子 #:丑 #:寅 #:卯 #:辰 #:巳 #:午 #:未 #:申 #:酉 #:戌 #:亥
           #:Zi #:Chou #:Yin #:Mao #:Chen #:Si #:Wu #:Wei #:Shen #:You #:Xu #:Hai
           #:金 #:木 #:水 #:火 #:土
           #:Metal #:Wood #:Water #:Fire #:Earth))
