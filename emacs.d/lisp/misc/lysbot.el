;;; lysbot.el --- Lys açıklandı mı?                  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst lysbot-url "https://sonuc.osym.gov.tr")

(defun lysbot-check ()
  (url-retrieve 
   lysbot-url
   (lambda (status &rest ignore)
     (when-let (err (plist-get status :error))
       (signal (car err) (cdr err)))
     (let ((s "LYS"))
       (search-forward "grdSonuclar")
       (condition-case c
           (progn
             (search-forward s)
             (message "LYSler Açıklandı!!!!!!"))
         (search-failed (message "LYS daha açıklanmadı :)")))))))

(defvar lysbot-timer nil)

(defun lysbot-run (interval)
  (interactive
   (list (read-number "Bot interval: " 360)))
  (when (timerp lysbot-timer)
    (cancel-timer lysbot-timer))
  (setf lysbot-timer (run-at-time "1 min" interval #'lysbot-check))
  (message "Started lysbot"))

(defun lysbot-stop ()
  (interactive)
  (cancel-timer lysbot-timer)
  (setf lysbot-timer nil)
  (message "Stopped lysbot"))

(provide 'lysbot)
;;; lysbot.el ends here
