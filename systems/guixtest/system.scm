;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules (gnu)
             (gnu system nss)
             (srfi srfi-1))

(use-service-modules cups desktop networking ssh xorg)
(use-package-modules cups)

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
  (cons*
   (specification->package "i3-gaps")
   (specification->package "i3status")
   (specification->package "vim-full")
   (specification->package "dmenu")
   (specification->package "xterm")
   (specification->package "git")
   (specification->package "nss-mdns")
   (specification->package "nss-certs")
   %base-packages))
 (name-service-switch %mdns-host-lookup-nss)
 (services
  (cons* (service openssh-service-type
				  (openssh-configuration
					(x11-forwarding? #t)))
         (service cups-service-type
                  (cups-configuration
                   (extensions
                    (list cups-filters hplip-minimal))))
         (service sane-service-type)
         (geoclue-service
		  #:applications
		  (cons* (geoclue-application "redshift-gtk" #:allowed? #t #:system? #t)
				 (geoclue-application "redshift" #:allowed? #t #:system? #t)
				 (geoclue-application "emacs" #:allowed? #t)
				 %standard-geoclue-applications))
		 (service gnome-keyring-service-type)
         (service slim-service-type
                  (slim-configuration
                   (xorg-configuration (xorg-configuration (keyboard-layout keyboard-layout)))))
		 (service elogind-service-type
				  (elogind-configuration
				   ;; hibernation is not implemented in GuixSD yet.
				   ;; (handle-power-key 'hibernate)
				   (handle-power-key 'suspend)
				   (handle-lid-switch-external-power 'suspend)))
         (remove (lambda (s) (or (eq? (service-kind s) gdm-service-type)
                                 (eq? (service-kind s) geoclue-service-type)
                                 (eq? (service-kind s) slim-service-type)
                                 (eq? (service-kind s) elogind-service-type)
                                 (eq? (service-kind s) sane-service-type)))
                 %desktop-services)))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")
   (keyboard-layout keyboard-layout)))
 (swap-devices
  (list (uuid "14dee6a1-c28e-49cf-a274-6ad9fc8ff500")))
 (file-systems
  (cons* (file-system
          (mount-point "/boot/efi")
          (device (uuid "82F8-18A6" 'fat32))
          (type "vfat"))
         (file-system
          (mount-point "/")
          (device
           (uuid "e4058621-c98a-493f-8a30-a58d03f9db6f"
                 'ext4))
          (type "ext4"))
         %base-file-systems)))
