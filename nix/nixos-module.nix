{ pkgs, config, lib, ... }:

let cfg = config.services.kmonad;
in

with lib;
{
  options.services.kmonad = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        If enabled, run kmonad after boot.
      '';
    };

    configfile = mkOption {
      type = types.path;
      default = "";
      example = "my-config.kbd";
      description = ''
        The config file for kmonad.
      '';
    };

    package = mkOption {
      type = types.package;
      default = import ./default.nix;
      example = "import ./default.nix";
      description = ''
        The kmonad package.
      '';
    };
  };

  config = {
    environment.systemPackages = [ cfg.package ];

    users.groups.uinput = {};

    services.udev.extraRules = mkIf cfg.enable
      ''
        # KMonad user access to /dev/uinput
        KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
      '';

    systemd.services.kmonad = mkIf cfg.enable {
      enable = true;
      description = "KMonad";
      serviceConfig = {
        Type = "simple";
        ExecStart = "${cfg.package}/bin/kmonad " + cfg.configfile;
      };
      wantedBy = [ "graphical.target" ];
    };
  };
}
