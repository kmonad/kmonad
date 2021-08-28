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

    optionalconfigs = mkOption {
      type = types.listOf types.path;
      default = [];
      example = "[ optional.kbd ]";
      description = ''
        Config files for dedicated kmonad instances which may not always be present.
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
        # If only one config file is supplied, unify all kmonad units under a target
        make-group = (length cfg.configfiles + length cfg.optionalconfigs) > 1;

        # All systemd units require the graphics target directly (if a single config),
        # or indirectly (via kmonad.target).
        wantedBy = [ "graphical.target" ];

        mk-kmonad-target = services: {
          # The kmonad.target allows you to restart all kmonad instances with:
          #
          #     systemctl restart kmonad.target
          #
          # this works because this unit requires all config-based services
          description = "KMonad target";
          requires = map (service: service.name + ".service") services;
          inherit wantedBy;
        };

        mk-kmonad-service = { is-optional }: kbd-path:
          let
            # prettify the service's name by taking the config filename...
            conf-file = lists.last (strings.splitString "/" (toString kbd-path));
            # ...and dropping the extension
            conf-name = lists.head (strings.splitString "." conf-file);
          in {
          name = "kmonad-" +conf-name;
          value = {
            enable = true;
            description = "KMonad Instance for: " +conf-name;
            serviceConfig = {
              Type = "simple";
              Restart = "always";
              RestartSec = 3;
              Nice = -20;
              ExecStart =
                "${cfg.package}/bin/kmonad ${kbd-path}" +
                  # kmonad will error on initialization for any unplugged keyboards
                  # when run in systemd. All optional configs will silently error
                  #
                  # TODO: maybe try to restart the unit?
                  (if is-optional then " || true" else "");
            };
          } // (if make-group
                then { partOf = [ "kmonad.target" ]; }
                else { inherit wantedBy; });
        };

        required-units = map (mk-kmonad-service { is-optional=false; }) cfg.configfiles;

        optional-units = map (mk-kmonad-service { is-optional=true;  }) cfg.optionalconfigs;

      in
        mkIf cfg.enable ({
            # convert our output [{name=_; value=_;}] map to {name=value;} for the systemd module
            services = listToAttrs (required-units ++ optional-units);
          } // (
            # additionally, if make-group is true, add the targets.kmonad attr and pass in all units
            attrsets.optionalAttrs make-group
              { targets.kmonad = mk-kmonad-target (required-units ++ optional-units); })
          );
  };
}
