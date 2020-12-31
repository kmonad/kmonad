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

    systemd = with lib; with builtins;
      let
        make-group = length cfg.configfiles > 1;
        wantedBy = [ "graphical.target" ];
        mk-kmonad-target = services: {
          description = "KMonad target";
          requires = map (service: service.name + ".service") services;
          inherit wantedBy;
        };
        mk-kmonad-service = kbd-path:
          let
            conf-file = lists.last (strings.splitString "/" (toString kbd-path));
            conf-name = lists.head (strings.splitString "." conf-file);
          in {
          name = "kmonad-" +conf-name;
          value = {
            enable = true;
            description = "KMonad Instance for: " +conf-name;
            serviceConfig = {
              Type = "simple";
              ExecStart = "${cfg.package}/bin/kmonad ${kbd-path}";
            };
          } // (if make-group
                then { partOf = [ "kmonad.target" ]; }
                else { inherit wantedBy; });
        };
        units = map mk-kmonad-service cfg.configfiles;

      in mkIf cfg.enable ({ services = listToAttrs units; } // (attrsets.optionalAttrs
        make-group { targets.kmonad = mk-kmonad-target units; }));
  };
}
