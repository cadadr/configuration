;; Goktug's Operating System

(use-modules (gnu)
             (guix-packages))
(use-service-modules desktop networking ssh)
(use-package-modules admin certs dns)

(operating-system
 (host-name "xi.bootis")
 (timezone "Europe/Istanbul")
 (locale "en_GB.utf8")

 (kernel linux-nonfree)

 (bootloader (grub-configuration (device "/dev/sda")))
 (swap-devices '("/dev/sda2"))
 (file-systems
  (cons*
   (file-system
    (device "/dev/sda1") (mount-point "/") (type "ext4")
    (needed-for-boot? #t))
   (file-system
    (device "/dev/sda5") (mount-point "/home") (type "ext4"))
   (file-system
    (device "/dev/sda6") (mount-point "/igk") (type "ext4")
    (create-mount-point? #t))
   %base-file-systems))

 (groups (cons* (user-group (name "g") (id 1993)) %base-groups))

 (users
  (cons*
   (user-account
    (comment "G K") (name "g") (group "g") (uid 1993)
    (create-home-directory? #f)
    (supplementary-groups '("users" "wheel" "audio" "video" "netdev"))
    (home-directory "/home/g"))
   %base-user-accounts))

 ;; Globally-installed packages.
 (packages (cons* wpa-supplicant-minimal nss-certs %base-packages))

 (services
  (cons*
   ;;(service nginx-service #:server-list '("/home/g/.config/GK/deb/etc/nginx/sites-enabled/goktug"))
   (service openssh-service-type (openssh-configuration (port-number 22)))
   %desktop-services)))

