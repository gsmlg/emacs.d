(require-package 'calfw)
(require-package 'calfw-org)
(require-package 'calfw-ical)

(require 'calfw)
(require 'calfw-org)
(require 'calfw-ical)

;; Month
(setq calendar-month-name-array
      ["一月" "二月" "三月"  "四月"  "五月"  "六月"
       "七月"  "八月"  "九月" "十月" "十一月" "十二月"])

;; Week days
(setq calendar-day-name-array
      ["星期日" "星期一" "星期二" "星期三" "星期四" "星期五" "星期六"])

;; First day of the week
(setq calendar-week-start-day 1) ; 0:Sunday, 1:Monday


(defun gsmlg/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "DarkGreen")  ; orgmode source
    (cfw:ical-create-source "节假日" "https://p38-calendars.icloud.com/holidays/cn_zh.ics" "IndianRed") ; devorah calender
    (cfw:ical-create-source "海淀－天气" "http://w.mdeve.com/101010200,2.ics" "Orange")
    ))
  (setq cfw:org-overwrite-default-keybinding t))


(provide 'init-calfw)
