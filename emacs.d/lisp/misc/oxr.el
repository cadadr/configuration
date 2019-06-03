;;; oxr.el --- Open Exchange Rates API library       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: data

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

;; Interface to Open Exchange Rates API.
;; https://docs.openexchangerates.org/

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar oxr-api-key "e4483cd95f184ea8b4846b2b7c8ffe5f")
(defvar oxr-api-url "https://openexchangerates.org/api/")

(defun oxr-fetch-data (type base async-callback &rest plist)
  "Fetch data from Open Exchange Rates.

TYPE is a symbol denoting the type of the query, one of: latest,
historical, currencies, time-series, convert, usage.

BASE is a symbol, an upper-case tree-letter currency symbol. Set
nil to omit.  Denotes the base currency.

If ASYNC-CALLBACK is a function, the call will be asynchronous.
The callback will be passed two arguments: the data in JSON
format, and the value of :cbarg from PLIST.

PLIST is a property list, and it's values have different meanings
for each TYPE.

If TYPE is latest, PLIST may contain a :symbols item, a list of
three-letter currency symbols.  Limit the result to specified
currencies.

If TYPE is historical, PLIST must contain a :date item, a time
object à la ‘current-time’, and may contain :symbols, like above.

If TYPE is currencies, PLIST may contain an :alternatives item,
if its value is non-nil,includes alternative currencies in the
results.

If TYPE is time-series, PLIST must contain :start and :end, two
dates items, each like historical, and may contain :symbols, as
above.

if TYPE is convert, PLIST must contain :value, a number, the
value to be converted, and :to, a symbol, the three-letter code
of the target currency.  BASE will be interpreted as the currency
to convert from.

This function, if synchronous, either returns data, or signals
nil.  The data returned is a ‘json-read’ of the response from the
server.  If asynchronous, the callback will only be called upon
success."
  (let* ((syms (when-let (s (plist-get plist :symbols))
                 (concat "&symbols="
                         (mapconcat (lambda (s) (upcase (symbol-name s))) s ",")
                          "&")))
         (req (cl-case type
		('latest (concat "latest.json?" syms))
		('historical (concat (format-time-string
                                      "historical/%F.json?"
                                      (or (plist-get plist :date)
                                          (error
                                           "Historical data requires date specified")))
                                     (or syms "")))
                ('currencies (concat "currencies.json?"
                                     (when (plist-get plist :alternatives)
                                       "show_alternative=1&")))
                ('time-series (let ((start (plist-get plist :start))
                                    (end (plist-get plist :end)))
                                (unless (and start end)
                                  (error
                                   "Time series data requires start and end dates"))
                                (concat "time-series.json?start="
                                        (format-time-string "%F" start) "&end="
                                        (format-time-string "%F" end)
                                        (or syms ""))))
                ('convert (concat
                           "convert/" (number-to-string (plist-get plist :amount))
                           (plist-get plist :to "?")))
                ('usage "usage.json?")))
	 (url (concat oxr-api-url req "app_id=" oxr-api-key "&prettyprint=0"
                      (when (and (not (eq type 'convert)) base)
                        (concat "&base=" (upcase (symbol-name base)))))))
    (if (functionp async-callback)
        (url-retrieve url (lambda (status cbarg)
                            (unless (plist-get :error status)
                              (goto-char (point-min))
                              (re-search-forward "\n\n") ; past the mime headers
                              (funcall async-callback (json-read) cbarg)))
                      (plist-get plist :cbarg) t t)
      (with-current-buffer (url-retrieve-synchronously url async-callback)
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (json-read)))))

(defun oxr-latest (&optional symbols base async-callback cbarg)
  "Fetch latest exchange data.

SYMBOLS is a list of three-letter currency symbols.  Limit the
result to specified currencies.

BASE is a symbol, an upper-case tree-letter currency symbol,
denotes the base currency.

If ASYNC-CALLBACK is a function, the call will be asynchronous.
The callback will be passed two arguments: the data in JSON
format, and the value of CBARG."
  (oxr-fetch-data 'latest base async-callback :symbols symbols :cbarg cbarg))

(provide 'oxr)
;;; oxr.el ends here
