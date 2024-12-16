(in-package #:cl-ganzhi)

(defvar *split-zi-shi* nil
  "默认为 nil 。 如果值为 t , 以零点区分早晚子时。 这是个争论了几百年的问题了，影响 23：00 到 23：59：59 时间段的\"天干\"的计算。

Initial value is 'nil'. If value is 't', split 子时 at 0:00 . This is a controversial issue that people have been auguring for hundreds years. Impact the calculation of 天干 of the day during 23:00 ~ 23:59:59.")

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

(defvar *no-chinese-character* nil
  "默认为 nil 。 如果值为 t， 对外接口返回的 symbol 以及 condition 消息将用拼音或者英语代替汉字符号和字符串。

Initial value is 'nil'. If value is 't', Symbols and strings returned by public APIs will be Pinyin and English instead of Chinese character symbol and string. ")
