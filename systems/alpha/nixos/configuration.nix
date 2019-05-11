# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Seems that systemd-boot is required for EFI.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.efiSysMountPoint = "/boot/efi";
  boot.loader.systemd-boot.editor = false;	# prevent easy root access
  boot.loader.timeout = null;			# wait indefinitely
  boot.plymouth.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.cleanTmpDir = true;
  # TODO: boot.kernel.sysctl = {...};
  # TODO: containers

  fileSystems = {
    "/"		= { device = "/dev/alpha/root"; fsType = "ext4"; 
                    options = [ "errors=remount-ro" ]; };
    "/boot"	= { device = "/dev/sda2"; fsType = "ext4";
                    neededForBoot = true; };
    "/boot/efi"	= { device = "/dev/sda1"; fsType = "vfat";
                    options = [ "umask=0077" ]; neededForBoot = true; };
    "/home"	= { device = "/dev/alpha/home"; fsType = "ext4"; };
    "/igk"	= { device = "/dev/alpha/igk"; fsType = "ext4";
                    options = [ "errors=remount-ro" ]; };
    "/backups"	= { device = "/dev/alpha/backups"; fsType = "ext4"; 
                    options = [ "noatime" ]; };
  };

  swapDevices = [ { device = "/dev/alpha/swap"; } ];

  hardware.brightnessctl.enable = true;		# control brightness if 
						# in group `video'

  networking.hostName = "alpha";
  networking.networkmanager.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
    supportedLocales = [ "en_US.UTF-8/UTF-8" "tr_TR.UTF-8/UTF-8" ];
  };

  time.timeZone = "Europe/Istanbul";

  documentation.dev.enable = true;		# install dev docs for packages
  environment = {
    enableDebugInfo = true;			# install debug symbols
    systemPackages = with pkgs; [
      # most basic
      vim file git m4 gnumake qemu moreutils smartmontools gawk
      entr 
      # internet
      wget curl dnsutils nettools netcat inetutils
      lftp bridge-utils rsync w3m firefox
    ];
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  services.openssh.enable = true;
  services.printing.enable = true;

  networking.firewall.allowedTCPPorts = [ 80 443 ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # networking.networkmanager.enable = true;

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "gb";
  services.xserver.xkbOptions = "eurosign:e ctrl:swapcaps";

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  # Enable the KDE Desktop Environment.
  services.xserver.displayManager.sddm.enable = true;
  services.xserver.desktopManager.plasma5.enable = true;

  virtualisation.docker.enable = true;

  users.users.g = {
    isNormalUser = true;
    createHome = true;
    description = "Göktuğ Kayaalp";
    extraGroups = [ "wheel" "docker" "video" "networkmanager" ];
    shell = pkgs.bashInteractive;
    uid = 1993;
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.03"; # Did you read the comment?

}
