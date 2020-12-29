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

    configfiles = mkOption {
      type = types.listOf types.path;
      default = [];
      example = "[ my-config.kbd ]";
      description = ''
        Config files for dedicated kmonad instances.
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

    systemd.services = with lib; with builtins;
      let
        mk-kmonad-service = kbd-path:
          let conf-name = lists.last (strings.splitString "/" (toString kbd-path));
          in {
          name = "kmonad-" +conf-name;
          value = {
            enable = true;
            description = "KMonad Instance for: " +conf-name;
            serviceConfig = {
              Type = "simple";
              ExecStart = "${cfg.package}/bin/kmonad " + kbd-path;
            };
            wantedBy = [ "graphical.target" ];
          };
        };
      in mkIf cfg.enable (listToAttrs (map mk-kmonad-service cfg.configfiles));
  };
}
