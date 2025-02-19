{ config, lib, pkgs, ... }:

let
  cfg-boot = config.boot.initrd.services.kmonad;
  cfg = config.services.kmonad;

  # Per-keyboard options:
  keyboard = { name, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        example = "laptop-internal";
        description = "Keyboard name.";
      };

      device = lib.mkOption {
        type = lib.types.path;
        example = "/dev/input/by-id/some-dev";
        description = "Path to the keyboard's device file.";
      };

      extraGroups = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        example = [ "openrazer" ];
        description = ''
          Extra permission groups to attach to the KMonad instance for
          this keyboard.

          Since KMonad runs as an unprivileged user, it may sometimes
          need extra permissions in order to read the keyboard device
          file.  If your keyboard's device file isn't in the input
          group, you'll need to list its group in this option.
        '';
      };

      defcfg = {
        enable = lib.mkEnableOption ''
          automatic generation of the defcfg block.

          When this is option is set to true, the config option for
          this keyboard should not include a defcfg block
        '';

        compose = {
          key = lib.mkOption {
            type = lib.types.str;
            default = "ralt";
            description = "The (optional) compose key to use.";
          };

          delay = lib.mkOption {
            type = lib.types.ints.unsigned;
            default = 0;
            example = 5;
            description = "The delay (in milliseconds) between compose key sequences.";
          };
        };

        keySeqDelay = lib.mkOption {
          type = lib.types.ints.unsigned;
          default = 1;
          example = 0;
          description = "The delay (in milliseconds) after each key outputted event.";
        };

        fallthrough = lib.mkEnableOption "reemitting unhandled key events";

        allowCommands = lib.mkEnableOption "keys to run shell commands";
      };

      config = lib.mkOption {
        type = lib.types.lines;
        description = "Keyboard configuration.";
      };
    };
  };

  # Create a complete KMonad configuration file:
  mkCfg = keyboard:
    let defcfg = ''
      (defcfg
        input  (device-file "${keyboard.device}")
        output (uinput-sink "kmonad-${keyboard.name}")
        cmp-seq ${keyboard.defcfg.compose.key}
        cmp-seq-delay ${toString keyboard.defcfg.compose.delay}
        key-seq-delay ${toString keyboard.defcfg.keySeqDelay}
        fallthrough ${lib.boolToString keyboard.defcfg.fallthrough}
        allow-cmd ${lib.boolToString keyboard.defcfg.allowCommands}
      )
    '';
    in
    pkgs.writeTextFile {
      name = "kmonad-${keyboard.name}.kbd";
      text = lib.optionalString keyboard.defcfg.enable (defcfg + "\n") + keyboard.config;
      checkPhase = "${cfg.package}/bin/kmonad -d $out";
    };

  # Build a systemd path config that starts the service below when a
  # keyboard device appears:
  mkPath = keyboard: rec {
    name = "kmonad-${keyboard.name}";
    value = {
      description = "KMonad trigger for ${keyboard.device}";
      wantedBy = [ "default.target" ];
      pathConfig.Unit = "${name}.service";
      pathConfig.PathExists = keyboard.device;
    };
  };

  # Build a systemd service that starts KMonad:
  mkService = in-initrd: keyboard:
    let
      cmd = [
        "${cfg.package}/bin/kmonad"
        "--input"
        ''device-file "${keyboard.device}"''
      ] ++ cfg.extraArgs ++ [
        "${mkCfg keyboard}"
      ];

      groups = [
        "input"
        "uinput"
      ] ++ keyboard.extraGroups;
    in
    {
      name = "kmonad-${keyboard.name}";
      value = {
        description = "KMonad for ${keyboard.device}";
        script = lib.escapeShellArgs cmd;
        serviceConfig.Restart = "always";
        serviceConfig.User = if in-initrd then null else "kmonad";
        serviceConfig.SupplementaryGroups = if in-initrd then null else groups;
        serviceConfig.Nice = -20;
      };
    };

  paths = builtins.listToAttrs (map mkPath (builtins.attrValues cfg.keyboards));
  services = in-initrd: builtins.listToAttrs (map (mkService in-initrd) (builtins.attrValues cfg.keyboards));
in
{
  # Don't conflict with existing module in nixpkgs.
  disabledModules = [ "services/hardware/kmonad.nix" ];

  options.boot.initrd.services.kmonad.enable = lib.mkEnableOption "KMonad" // {
    description = ''
      *This will only be used when systemd is used in stage 1.*

      Whether to enable KMonad: an advanced keyboard manager.

      This requires `services.kmonad.enable` to be set.
      All other config options are also taken from `services.kmonad`.
    '';
  };
  options.services.kmonad = {
    enable = lib.mkEnableOption "KMonad: an advanced keyboard manager";

    package = lib.mkPackageOption pkgs "KMonad" {
      default = "kmonad";
      example = "pkgs.haskellPackages.kmonad";
    };

    keyboards = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule keyboard);
      default = { };
      description = "Keyboard configuration.";
    };

    extraArgs = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ ];
      example = [ "--log-level" "debug" ];
      description = "Extra arguments to pass to KMonad.";
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      environment.systemPackages = [ cfg.package ];

      users.groups.uinput = { };
      users.groups.kmonad = { };

      users.users.kmonad = {
        description = "KMonad system user";
        group = "kmonad";
        isSystemUser = true;
      };

      hardware.uinput.enable = true;

      systemd.paths = paths;
      systemd.services = services false;
    })
    (lib.mkIf cfg-boot.enable {
      boot.initrd = {
        systemd.storePaths = [ cfg.package ];

        # Does not work, since `users.groups.uinput` does not specify a `gid`
        # and this field isn't optional.
        #systemd.groups.uinput = { };
        #systemd.groups.kmonad = { };

        # Same for users with `uid`.
        #systemd.users.kmonad.group = "kmonad";
        #services.udev.rules = ''
        #  # KMonad user access to /dev/uinput
        #  KERNEL=="uinput", MODE="0660", GROUP="uinput", OPTIONS+="static_node=uinput"
        #'';

        systemd.paths = paths;
        systemd.services = services true;

        availableKernelModules = [ "evdev" "uinput" ];
      };
    })
    { assertions = [
        {
          assertion = cfg-boot.enable -> cfg.enable;
          message = "To enable KMonad in the initrd, it must be enabled globally";
        }
      ];
    }
  ];
}
