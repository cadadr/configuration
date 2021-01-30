;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
	     (srfi srfi-1))
(use-service-modules desktop networking ssh xorg)

(operating-system
 (locale "en_GB.utf8")
 (timezone "Europe/Istanbul")
 (keyboard-layout (keyboard-layout "gb"))
 (host-name "guixtest")
 (users (cons* (user-account
                (name "g")
                (comment "cadadr")
                (group "users")
                (home-directory "/home/g")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (packages
  (append
   (list (specification->package "i3-gaps")
         (specification->package "i3status")
         (specification->package "dmenu")
         (specification->package "xterm")
         (specification->package "vim-full")
         (specification->package "git")
         (specification->package "nss-certs"))
   %base-packages))
 (services
  ;; The default display manager is GDM. Which is bad.
  ;; GDM is bad. It's not good. So we change that to SLiM.
  (cons*
   (service openssh-service-type)
   (service slim-service-type
	    (slim-configuration (display ":0") (vt "vt1")))
   (set-xorg-configuration
    (xorg-configuration
     (keyboard-layout keyboard-layout)))
   (remove
    (lambda (s) (eq? (service-kind s) gdm-service-type))
    %desktop-services)))
 (bootloader
  (bootloader-configuration
   (bootloader grub-bootloader)
   (target "/dev/sda")
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (uuid "5bc9aeae-e6cb-419f-be31-02c71d5b8084")))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "5f0cedfe-5650-48b8-842b-02a0b6da1fbd"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
