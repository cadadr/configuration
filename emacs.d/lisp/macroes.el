;;; macroes.el -- A collection of recorded macroes -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Göktuğ Kayaalp

;; Author: Göktuğ Kayaalp <self@gkayaalp.com>
;; Keywords: macroes, utility
;; Package-Version: 0.0.0-DEV
;; Package-Requires: (())
;; Version: DEV

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

(defun gk-insert-buffer-mode-selectingly (&optional arg)
  "Keyboard macro."
  (interactive "p")
  (kmacro-exec-ring-item
   (quote ([3 119 21 24 113 return 24 114 105 77] 0 "%d")) arg))

(provide 'macroes)
;;; macroes.el ends here
