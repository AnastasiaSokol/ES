(deffunction ask-yes-or-no (?question)
   (printout t ?question " (yes or no) ")
   (bind ?answer (read))
   (while (and (neq ?answer yes) (neq ?answer no))
      (printout t ?question " (yes or no) ")
      (bind ?answer (read)))
   (return ?answer)
)
   
(defrule determine-working-state  ""
	(not (working-state printer ?))
	(not (repair ?))
	=>
	(bind ?answer (ask-yes-or-no "Принтер включается(yes/no)? "))  ; ввод ответа
   (if (eq ?answer yes) 
		;----------------------------------------------------------------------------------
       then ;Принтер включается 
	   (bind ?answer2 (ask-yes-or-no "Принтер работает нормально(yes/no)? "))  ; ввод ответа
       (if (eq ?answer2 yes)
	   
           then (assert (power-state printer on))
				(assert (working-state printer successful))
				(assert (repair "Ремонт НЕ нужен"))
           else (assert (power-state printer on))
				(assert (working-state printer unsatisfactory))
		)
		;----------------------------------------------------------------------------------
       else ;Принтер не включается
		(assert (working-state printer does-not-start))
	)
)

;====================================================================================
;Правило, определяющее подключен ли принтер в розетку
(defrule power-state  ""
	(working-state printer does-not-start)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Принтер подключен к сети(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
			;принтер подключен к сети но не включается
           then (assert (power-state printer on))
				(assert (repair "Возможно проблема в электрической розетке!"))
		    ;принтер Не подключен к сети и не включается
           else (assert (power-state printer off))
				(assert (repair "Нет питания!"))
		)
)
;====================================================================================
;Правило определяющее принимает ли принтер команды от ПК
(defrule reaction-state  ""
	(working-state printer unsatisfactory)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Принтер реагирует на комманды от ЭВМ(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
			;на комманды реагирует
           then (assert (reaction-state printer success))
           else (assert (reaction-state printer noreaction))
		)
)
;====================================================================================
;Правило, определяющее установлен ли драйвер принтера
(defrule drivers-state  ""
	(reaction-state printer noreaction)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Установлен ли драйвер(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
			;драйвер установлен
           then (assert (drivers-state printer have))
				(assert (repair "Проверим подлючен ли он в пк!"))
           else (assert (drivers-state printer have-not))
				(assert (repair "Установите драйвера для принтера!"))
		)
)
;====================================================================================
;Правило, определяющее пытается ли принтер печатать
(defrule print-state  ""
	(reaction-state printer success)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Принтер печатает(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
			;принтер печатает
           then (assert (print-state printer can-print))
           else (assert (print-state printer can-not-print))
		)
)
;====================================================================================
;Правило, определяющее есть ли бумага в принтере
(defrule paper-state  ""
	(print-state printer can-not-print)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "В принтере есть бумага(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
           then (assert (paper-state paper-in-printer))
           else (assert (print-state printer-have-not-paper))
				(assert (repair "Вставьте бумагу в принтер!"))
		)
)
;====================================================================================
;Правило, определяющее не застряла ли бумага 
(defrule paper-is-jammed  ""
	(paper-state paper-in-printer)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Принтер замял бумагу(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
           then (assert (paper-is-jammed printer jammed paper))
				(assert (repair "Достаньте замятую бумагу!"))
           else (assert (paper-is-jammed printer does not jammed paper))
				(assert (repair "Мы не можем обнаружить причину паломки"))
		)
)
;====================================================================================
;Проверка заправленности картриджа
(defrule cartridge-state  ""
	(print-state printer can-print)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Принтер печатает, но на выходе белые листы(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
			;принтер не заправлен
           then (assert (cartridge-state cartridge-is-empty))
				(assert (repair "Заправьте картридж!"))
           else (assert (cartridge-state cartridge-no-empty))
		)
)
;====================================================================================
;Проверка состояния картриджа
(defrule cartridge-condition-state  ""
	(cartridge-state cartridge-no-empty)
	(not (repair ?))
	=>
	 (bind ?answer3 (ask-yes-or-no "Принтер печатает, но на выходе замаранные листы(yes/no)? "))  ; ввод ответа
       (if (eq ?answer3 yes)
           then (assert (cartridge-condition-state cartridge-need-clean))
				(assert (repair "Требуется почистить картридж!"))
           else (assert (cartridge-condition-state cartridge-dont-need-clean))
		)
)
;====================================================================================
;Правило print-repair выводит на экран диагностическое сообщение
;по устранению найденной неисправности.
(defrule print-repair ""
    (declare (salience 10))
    (repair ?item)
  =>
    (printout t crlf crlf)
    (printout t "Рекомендации по ремонту:")
    (printout t crlf crlf)
    (format t " %s%n%n%n" ?item)
)