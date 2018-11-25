# configuration.nix --- NixOS configuration fo alpha

{ config, pkgs, callPackage, ... }:

{
  imports =
    [ ./hardware-configuration.nix ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "beta";
  networking.wireless.enable = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "uk";
    defaultLocale = "en_GB.UTF-8";
  };

  time.timeZone = "Europe/Istanbul";

  environment.systemPackages = with pkgs; [
    wget vim
  ];

  programs.mtr.enable = true;
  programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Services:
  services.openssh.enable = true;
  services.printing.enable = true;
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  services.xserver = {
    enable = true;
    layout = "gb";
    xkbOptions = "ctrl:nocaps";
    desktopManager = {
      default = "xfce";
      xterm.enable = false;
      xfce.enable = true;
    };
  };

  services.xserver.libinput.enable = true;

  # User:
  users.users.g = {
    isNormalUser = true;
    uid = 1993;
    extraGroups = ["wheel"];
  };

  system.stateVersion = "18.09";

}
