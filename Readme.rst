=========================================================
Convert the Gregorian calendar to Chinese GanZhi calendar
=========================================================
.. title:: cl-ganzhi - Convert the Gregorian calendar to Chinese GanZhi calendar

Abstract
========

项目 ``cl-ganzhi`` 转换公历日期到干支历。

This project ``cl-ganzhi`` converts date time in Gregorian calendar to date time in Chinese GanZhi (干支) calendar (also known as Sexagenary Cycle Calendar).

I doubt that people who don't speak Chinese would have interests in ``cl-ganzhi``. Nevertheless, a special variable is provided to control the output character set, and English version documents are also provided.

.. contents:: Table Of Contents

Download
========

Please download from codeberg__.

.. __: https://codeberg.org/cranej/cl-ganzhi

Examples
========

::

   CL-GANZHI> (convert-now)
   ((甲 . 辰) (丙 . 子) (辛 . 未) (乙 . 未))
   
   CL-GANZHI> (let ((*no-chinese-character* t))
                (convert-timestring "2024-12-31T20:15:00+08:00"))
   ((JIA . CHEN) (BING . ZI) (JI . SI) (JIA . XU))
   
   CL-GANZHI> (calc-xunkong '辛 '未)
   (戌 . 亥)
   
   CL-GANZHI> (let ((*no-chinese-character* t))
                (calc-xunkong 'XIN 'WEI))
   (XU . HAI)
   
   CL-GANZHI> 

Api
===

Functions
---------

* ``(convert time &term-passed)``
  
  转换 ``time`` （ ``local-time:timestamp`` 的实例）到干支历。 返回一个包含四个 dotted list 的 list： 分别是年、月、日、时的干支对。

  这个 function **不处理** 节气转换的问题。 如果 ``time`` 处在十二节中某一节的交接时期内（例如 2 月 3 日到 2 月 5 日之间）， 会 signal 一个 ``confirm-term`` condition。 此时调用者可以在确认节气是否已经交接后（比如要求用户确认），调用预先提供好的两个 restart ``as-passed`` 或者 ``as-not-passed``。 或者再次调用 ``convert``， 设置参数 ``term-passed``。

  日干支的计算受到变量 ``*split-zi-zhi`` 的影响，请参考该变量的文档。

  Convert ``time`` which is a ``local-time:timestamp`` to Chinese GanZhi calendar date time. Returns a list of four dotted lists: GanZhi pair for year, month, day, and hour parts.

  This function **does not** handle solar term junction. If the ``time`` is inside the junction period of one of the 12 minor solar terms (十二节), a ``confirm-term`` condition is signaled. Caller should handle the condition by either invoking one of the two provided restarts ``as-term-passed`` and ``as-term-not-passed``, or by calling ``convert`` again with parameter ``term-passed`` set. 

  Affected by variable ``*split-zi-shi*``, please refer to the variable's doc.

* ``(convert-now &term-passed)``
  
  转换当前时间到干支历， 相当于 ``(convert (local-time:now))``。

  Convenient wrapper over ``convert``: ``(convert (local-time:now))``.
  
* ``(convert-timestring timestring &term-passed)``
  
  转换公历 timestring 到干支历。 

  + 如果 timestring 是 nil ， 相当于 ``(convert-now)``；
  + 否则相当于 ``(convert (parse-timestring timestring))``。

  Convenient wrapper over ``convert``:

  + if ``timestring`` is nil , roughly equals ``(convert-now)``;
  + otherwise roughly equals ``(convert (parse-timestring timestring))``.
    
* ``(calc-xunkong day-gan day-zhi)``
  
  计算旬空。 返回 dotted list (旬空1 . 旬空2) 。

  Calculate the two DiZhi which having a bye. Returns dotted list (bye1 . bye2)."

* ``(confirm-term-term condition)``
  Reader of slot ``term`` of  condition type ``confirm-term``. Returns the name of the solar term of the condition.
  
Variables
---------

* ``*split-zi-shi*``
  
  默认值为 ``nil`` 。 如果值为 ``t`` , 以零点区分早晚子时。 这是个争论了几百年的问题了，影响 23：00 到 23：59：59 时间段的"天干"的计算。

  Initial value is 'nil'. If value is ``t``, split 子时 at 0:00 . This is a controversial issue that people have been auguring for hundreds years. Impact the calculation of 天干 of the day during 23:00 ~ 23:59:59.
  
* ``*no-chinese-character*``
  
  默认为 ``nil`` 。 如果值为 ``t`` ， 对外接口返回的 symbol 以及 condition 消息将用拼音或者英语代替汉字符号和字符串。

  Initial value is ``nil``. If value is ``t``, Symbols and strings returned by public Api will be Pinyin and English instead of Chinese character symbol and string. 

Implementation Notes
====================

节气交接(solar term junction)
-----------------------------

精确的十二节（24节气中每个公历月的第一个节气为节，第二个为气）交接时间是干支历的基石 —— 干支纪年以十二节的交接时间为月份的开始，以立春交接为新一个干支年的开始。

``cl-ganzhi`` 不计算节气的具体交接时间。据我所知，没有算法可以把计算节气交接时间精确到秒或者分钟。我的理解是节气的精确计算需要依赖天文台的观测数据，需要定期地进行校正。一个只能精确到天的算法没有太多意义，反而对那些不满足于“天”这个精确度的调用者造成阻碍。

``cl-ganzhi`` 把做这个决定的责任交给调用者。 如果 ``cl-ganzhi`` 需要转换的日期正好落在某个节的交接时间区间内，它会 signal 一个 condition ， 调用者需要处理这个 condition 然后选择使用 ``cl-ganzhi`` 建立好的两个 restart 中的一个（参见 ``convert`` 函数的文档)。 例如如果调用者是一个 command line 程序，它可以向用户询问节气是否已经交接（用户可以查询历书）。 或者如果调用方不是很在意节气交接的精确度，它可以自己计算一个精确到天的结果，或者任意决定一个值等等。

由于每个节气的交接时间都是固定的，都是在一个两到三天的时间区间内（比如立春的交接时间一定是在 2 月 3 号到 5 号之间），实际使用过程中需要处理这个 condition 的次数是比较少的。

Exact time of solar term beginning is essential to Chinese Sexagenary Cycle Calendar - months start at beginning of each 12 minor solar term, years start at beginning of Spring Commences.

``cl-ganzhi`` does not calculate exact solar term beginning time (exact to the second or minute) as there is no algorithm able to do that. To calculate the exact time, we needs astronomical data, and requires regular calibration. Algorithms accurate to the day do exist, but I don't think they are useful for this scenario. On the contrary, leveraging such algorithms in ``cl-ganzhi`` is even harmful for users who need more accuracy.

``cl-ganzhi`` relies on the users to provide such information. If ``cl-ganzhi`` is converting a date time which falls into the junction time period of one of the 12 minor solar terms, it signals a condition, users need to handle it (please refer to the documentation of function ``convert`` for details). For example, if the caller is a command line application, it may query end user whether the particular solar term is already passed or not. Or if the accuracy of the beginning of such a solar term is not important to the caller, it may choose a random value or calculate a result which is only exact to the day.

The junction time period of each solar term is a two or three days period (for example, the beginning of Spring Commences is always some point between Feb. 2 and Feb. 3 ). So for average use cases, users does not have to query it's end user about solar term junction frequently. 

中文 symbols (Chinese character in symbols)
-------------------------------------------

十天干和十二地支是整个干支历法中的原语。在其它没有 symbol 的编程语言中，通常需要用 string 来表示它们。但这其实是该编程语言的缺陷 —— 它们应该用 symbol 而不是 string 来表示。所以在 ``cl-ganzhi`` 中直接使用了中文字符的 Symbols 来表示这些原语，例如 ``'甲 '乙 '子 '丑`` 等。 而且这些名词本身也没有有意义的英语翻译，现在一般译为拼音，比如 Jia Yi 等。

但是考虑到有用户输入中文可能有困难，以及有些用户可能希望编程的时候尽量避免输入法切换， ``cl-ganzhi`` 提供了一个 special variable ``*no-chinese-character*`` 用来控制公共 Api 输出的字符集，详见该变量的文档。

The ten Heavenly Stems and twelve Earthly Branches are the primitives of Chinese Sexagenary Cycle Calendar. These terms has no meaningful translations in English other then Pinyin, and in a programming language has ``symbol`` type, they **should** be represented as ``symbol``, not ``string``. Thus ``cl-ganzhi`` exposes these primitives as Chinese character symbols like ``'甲 '乙 '子 '丑``, etc.

However, consider that people who don't speak Chinese may have difficulties on typing these characters on computer, a special variable ``*no-chinese-character*`` is provided - when set to ``t``, outputs of all public Api are translated into Pinyin or English translations(if there is one). For example, ``'甲 '乙 '子 '丑`` becomes ``'Jia 'Yi 'Zi 'Chou``, '立春' becomes 'Spring Commences', and so on. 

Bug and Suggestion
==================
Please email me mailto:cranejin.com or open an issue on codeberg.

License
=======

``cl-ganzhi`` comes with a 3-Clause BSD license.


