;; forecast for multiple cities

(let* ((cities
        (list
         ;; City      Lat       Long
         ["İstanbul"  41.168602 29.047024]
         ["Eskişehir" 39.783333 30.516667]))
       (vars '(calendar-latitude
                        calendar-longitude
                        calendar-location-name))
       (settings
        (loop for var in vars
              nconc (list var (eval var))))
       (buf (get-buffer-create "*Combined Reports*"))
       str)
  (dolist (city cities)
    (pcase city
      (`[,name ,lat ,long]
       (setq calendar-location-name name
             calendar-latitude lat
             calendar-longitude long)
       (save-window-excursion
         (forecast)
         (with-current-buffer "*Weather Forecast*"
           (sit-for 3 t)
           (setq str (buffer-substring (point-min) (point-max)))
           (with-current-buffer buf
             (goto-char (point-max))
             (insert str) (newline 2)))))))
  (eval (cons 'setq settings))
  (display-buffer buf)
  nil)
