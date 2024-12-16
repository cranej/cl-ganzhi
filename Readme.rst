=========================================================
Convert the Gregorian calendar to Chinese GanZhi calendar
=========================================================

系统 ``cl-ganzhi`` 转换公历日期到干支历。

System ``cl-ganzhi`` converts date time in Gregorian calendar to date time in Chinese GanZhi (干支) calendar (also known as Sexagenary Cycle Calendar).

Api
=====

Functions
---------

* ``(convert time &term-passed)``
  
  转换 ``time`` （ ``local-time:timestamp`` 的实例）到干支历。 返回一个包含四个 dotted list 的 list： 分别是年、月、日、时的干支对。

  这个 function **不处理** 节气转换的问题。 如果 ``time`` 处在十二节中某一节的交接时期内（例如 2 月 3 日到 2 月 5 日之间）， 会 signal 一个 ``confirm-term`` condition。 此时调用者可以在确认节气是否已经交接后（比如要求用户确认），调用预先提供好的两个 restart ``as-passed`` 或者 ``as-not-passed``。 或者再次调用 ``convert``， 设置参数 ``term-passed``。

  日干支的计算受到变量 ``*split-zi-zhi`` 的影响，请参考该变量的文档。

  Convert ``time`` which is a ``local-time:timestamp`` to Chinese GanZhi calendar date time. Returns a list of four dotted lists: GanZhi pair for year, month, day, and hour parts.

  This function **does not** handle solar term junction. If the ``time`` is inside the junction period of one of the 12 minor solar terms (十二节), a ``confirm-term`` condition is signaled. Caller should handle the condition by either invoking one of the two provided restarts ``as-passed`` and ``as-not-passed``, or by calling ``convert`` again with parameter ``term-passed`` set. 

  Affected by variable ``*split-zi-shi*``, please refer to the variable's doc.

* ``(convert-now &term-passed)``
  
  转换当前时间到干支历 == ``(convert (local-time:now))``。

  Convenient wrapper over ``convert``: ``(convert (local-time:now))``.
  
* ``(convert-timestring timestring &term-passed)``
  
  转换公历 timestring 到干支历。 

  + 如果 timestring 是 nil ， 约等于 ``(convert-now)``；
  + 否则约等于 ``(convert (parse-timestring timestring))``。

  Convenient wrapper over ``convert``:

  + if ``timestring`` is nil , roughly equals ``(convert-now)``;
  + otherwise roughly equals ``(convert (parse-timestring timestring))``.
    
* ``(calc-xunkong day-gan day-zhi)``
  
  计算旬空。 返回 dotted list (旬空1 . 旬空2) 。

  Calculate the two DiZhi which having a bye. Returns dotted list (bye1 . bye2)."
  
Variables
---------

* ``*split-zi-shi*``
  
  如果值为 ``t`` , 以零点区分早晚子时。 这是个争论了几百年的问题了，影响 23：00 到 23：59：59 时间段的"天干"的计算。

  If value is ``t``, split 子时 at 0:00 . This is a controversial issue that people have been auguring for hundreds years. Impact the calculation of 天干 of the day during 23:00 ~ 23:59:59.

转换算法(algorithm)
===================

日期锚点 M ：
  1924年2月15日 - 甲子年 丙寅月 甲子日 。

月
==

1. 十二节界定月支
2. 如果日期处于十二节交接范围内，比如 2月3号～2月5号，询问调用方节气是否已经交接，调整月支
3. 月干 - 确定年干支之后五虎遁年起月诀::

    甲己之年丙作首，乙庚之岁戊为头。
    丙辛岁首寻庚起，丁壬壬位顺行流。
    若言戊癸何方求，甲寅之上好追求

年
==

1. 确定计算年干支所用的公元年 Y ：
   * 日期在1月1号到立春交节时分之前，``公元年 - 1``
   * 日期在立春交节之后到12月31号，取``公元年``
2. ``(mod (year-diff year M) 60)`` 取年干支

日
==

1. 确定用于计算的日期 date :
   * 时间在 23：00 分之前，day 不变
   * 时间在 23：00 之后， day + 1
2. ``(mod (daydiff date M) 60)`` 取日干支

时
==

1. 时支查表
2. 时干 - 五鼠遁日上起时::

     甲己还加甲，乙庚丙作初。
     丙辛从戊起，丁壬庚子是。
     戊癸何方觅，壬子是真途。
