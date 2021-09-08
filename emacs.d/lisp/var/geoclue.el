;;; geoclue.el --- GeoClue2 support                -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Ian Eure

;; Author: Ian Eure <public@lowbar.fyi>
;; URL: https://github.com/ieure/geoclue
;; Version: 0.8.1
;; Package-Requires: ((emacs "25"))
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

;; Usage: (geoclue-location)

;;; Code:

(require 'dbus)

(defvar geoclue--client nil
  "The path to the GeoClue2 client for Emacs.")

(defvar geoclue--location nil
  "Path of the current (physical) location of the host.")

(defvar geoclue--signal-handler nil
  "Handle for the signal which fires when the location updates.")

(defconst geoclue--service "org.freedesktop.GeoClue2"
  "GeoClue2 service name.")

(defconst geoclue--manager-path "/org/freedesktop/GeoClue2/Manager"
  "Path to the GeoClue2 Manager.")

(defconst geoclue--manager-if (concat geoclue--service ".Manager")
  "Interface of the GeoClue2 Manager.")

(defconst geoclue--client-if (concat geoclue--service ".Client")
  "Interface of GeoClue2 Clients.")

(defconst geoclue--location-if (concat geoclue--service ".Location")
  "Interface of GeoClue2 Locations.")

(defconst geoclue--location-properties
  '("Accuracy" "Altitude"
    "Heading" "Latitude" "Longitude" "Speed" "Description" "Timestamp")
  "Properties of GeoClue2 Location objects.")

(defun geoclue--manager (method &rest args)
  "Call METHOD with ARGS on the GeoClue manager."
  (apply 'dbus-call-method :system geoclue--service geoclue--manager-path geoclue--manager-if method args))

(defun geoclue--client (client-path method &rest args)
  "Call METHOD with ARGS on the GeoClue client at CLIENT-PATH."
  (apply 'dbus-call-method :system geoclue--service client-path geoclue--client-if method args))

(defun geoclue--client-get-prop (client-path property)
  "Return PROPERTY from GeoClue client at CLIENT-PATH."
  (dbus-get-property :system geoclue--service client-path geoclue--client-if property))

(defun geoclue--client-set-prop (client-path property value)
  "Set PROPERTY = VALUE on GeoClue client at CLIENT-PATH."
  (dbus-set-property :system geoclue--service client-path geoclue--client-if property value))

(defun geoclue--client-active? (client-path)
  "Is the client at CLIENT-PATH active?"
  (geoclue--client-get-prop client-path "Active"))

(defun geoclue--location-get-prop (location-path property)
  "Return value of PROPERTY from location object at LOCATION-PATH."
  (dbus-get-property :system geoclue--service location-path geoclue--location-if property))

(defun geoclue--location-updated (old new)
  "Handler for when a location change occurs.

   OLD is the old location, NEW is the new one."
  (setq geoclue--location
        (cl-loop for prop in geoclue--location-properties
                 collect (cons (intern (downcase prop))
                               (geoclue--location-get-prop new prop)))))

(defun geoclue--location* ()
  "Start GeoClue2 and wait for it to provide the location of the host."
  (geoclue-start)
  (let ((times 0))
    (while (and (not geoclue--location)
               (<= times 5))
      (incf times)
      (sleep-for 0 250)))
  (or geoclue--location
      (warn "Unable to determine current location")))

 ;; User-serviceable parts

;;;###autoload
(defun geoclue-start ()
  "Start GeoClue2."
  (let ((client (or geoclue--client (geoclue--manager "GetClient"))))
    (unless (geoclue--client-active? client)
      (geoclue--client-set-prop client "DesktopId" "emacs")
      (setq geoclue--signal-handler
            (dbus-register-signal :system geoclue--service client
                                  geoclue--client-if
                                  "LocationUpdated" 'geoclue--location-updated))
      (geoclue--client client "Start")
      (setq geoclue--client client))))

(defun geoclue-stop ()
  "Release Emacs-specific GeoClue2 resources."
  (when geoclue--signal-handler
    (dbus-unregister-object geoclue--signal-handler)
    (setq geoclue--signal-handler nil))
  (when geoclue--client
    (geoclue--client geoclue--client "Stop")
    (geoclue--manager "DeleteClient" :object-path geoclue--client)
    (setq geoclue--client nil))
  (setq geoclue--location nil))

;;;###autoload
(defun geoclue-location ()
  "Return the current physical location of the host."
  (or geoclue--location
      (geoclue--location*)))

(provide 'geoclue)

;;; geoclue.el ends here
