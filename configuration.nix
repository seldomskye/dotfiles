# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.extraModprobeConfig = ''
    options snd slots=snd-hda-intel
  '';
  nixpkgs.config = {
    allowUnfree = true;  
    firefox = {
      enableAdobeFlash = true;
    };
 };
 users.extraUsers.sean = 
    { isNormalUser = true;
      home = "/home/sean";
      description = "Quit Being an Idiot";
      extraGroups = [ "wheel" ];
      shell = "/run/current-system/sw/bin/zsh";
    };
  boot.loader = {
    # Use the gummiboot efi boot loader.
    gummiboot.enable = true;
    gummiboot.timeout = 4;
    efi.canTouchEfiVariables = true;
  };
  
  # networking.hostName = "nixos"; # Define your hostname.
  networking = {
    hostId = "22b8ad6a";
    hostName = "nixos";
    wireless.enable = true;
  };


  time.timeZone = "US/Eastern";
  # Select internationalisation properties.
  # i18n = {
  #   consoleFont = "lat9w-16";
  #   consoleKeyMap = "us";
  #   defaultLocale = "en_US.UTF-8";
  # };

  security.sudo.configFile = ''
  root	ALL=(ALL) SETENV: ALL
  sean	ALL=(ALL) SETENV: ALL
  '';
  programs.zsh.enable = true;
  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    # networking
    wpa_supplicant_gui
    
    xlibs.xmessage

    # Desktop manager
    haskellPackages.xmonad
    haskellPackages.xmonad-contrib
    haskellPackages.xmonad-extras
    dmenu
    trayer
    haskellPackages.xmobar

    # haskell dev
    haskellPackages.ghc
    haskellPackages.cabal-install
    haskellPackages.hakyll
    cabal2nix

    # Python dev
    pythonPackages.python
    pythonPackages.pip
    pythonPackages.virtualenv
    
    # dev tools
    git
    silver-searcher
    unzip
    emacs
    rxvt_unicode
    wget

    owncloudclient
    redshift
    firefoxWrapper
    python34Packages.dantalian
    nox

    # Media/Entertainment
    vlc
    mpd
    ncmpcpp
    mcomix
    youtube-dl
    pythonPackages.livestreamer
    deluge
    steam
  ];

  # List services that you want to enable:

  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "caps:swapescape";
    xkbVariant = "colemak";
    synaptics.enable = true;
    windowManager.xmonad.enable = true;
    windowManager.xmonad.enableContribAndExtras = true;
    desktopManager.xterm.enable = false;
    driSupport32Bit = true;
  };
  services.mpd = {
    enable = true; 
    musicDirectory = "/media/music/"; 
  };

  services.deluge = {
    enable = true;
  };

  services.redshift = {
    enable = true;
    latitude = "42.3583333";
    longitude = "-71.0602778";
  };
  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  # Enable the X11 windowing system.
  # services.xserver.enable = true;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.extraUsers.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };

}
