{ sources ? import ./sources.nix
, pkgs ? (import sources.nixpkgs { })
}:

let
  config = pkgs.runCommand "kmonad-config" { } ''
    ${pkgs.gnused}/bin/sed -E \
      '/^\(defcfg/,/^\)/d' \
      ${../keymap/tutorial.kbd} > $out
  '';
in
pkgs.nixosTest {
  name = "kmonad-test";

  nodes = {
    machine = { ... }: {
      imports = [
        ./nixos-module.nix
      ];

      users.users.jdoe = {
        createHome = true;
        isNormalUser = true;
        password = "password";
        group = "users";
      };

      services.kmonad = {
        enable = true;
        keyboards.qemu = {
          config = builtins.readFile (toString config);
          compose.key = null;
          fallthrough = true;
          device = "/dev/input/event0";
        };
      };
    };
  };

  testScript = ''
    with subtest("Start machines"):
        start_all()

    with subtest("Verify KMonad started"):
        machine.wait_for_unit("kmonad-qemu.service")

    with subtest("Log In"):
        machine.wait_until_tty_matches(1, "login: ")
        machine.send_chars("jdoe\n")
        machine.wait_until_tty_matches(1, "Password: ")
        machine.send_chars("password\n")
        machine.wait_until_tty_matches(1, "$")

    with subtest("Test Tutorial Numbers Layer"):
        machine.send_chars("echo ")
        machine.send_key("meta_l-k") # Should send "5"
        machine.send_key("meta_l-l") # Should send "6"
        machine.send_chars(" > /tmp/keys\n")
        machine.wait_until_succeeds("test -e /tmp/keys")
        machine.succeed("test \"$(cat /tmp/keys)\" -eq 56")
  '';
}
