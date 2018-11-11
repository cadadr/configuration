;;; system.scm --- system configuration

(use-modules (gnu) (gnu system nss))
(use-service-modules desktop ssh)
(use-package-modules certs gnome m4 perl version-control)

(operating-system
 (host-name "beta")
 (timezone "Europe/Istanbul")
 (locale "en_GB.utf8")

 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (target "/boot/efi")))

 (file-systems
  (cons*
   (file-system
    (device (file-system-label "root"))
    (mount-point "/")
    (type "ext4"))
   %base-file-systems))

 (users
  (cons
   (user-account
    (name "g")
    (comment "cadadr")
    (group "users") ;; XXX -> g
    (supplementary-groups
     '("wheel" "netdev"
       "audio" "video"))
    (home-directory "/home/g"))
   %base-user-accounts))

 (packages
  (cons*
   nss-certs                            ;for HTTPS access
   gvfs                                 ;for user mounts
   git make m4 perl                     ;for user initialisation
   %base-packages))

 (services
  (cons*
   (xfce-desktop-service)
   (service openssh-service-type)
   (extra-special-file
    "/usr/bin/env" (file-append coreutils "/bin/env"))
   %desktop-services))

 (name-service-switch %mdns-host-lookup-nss))

