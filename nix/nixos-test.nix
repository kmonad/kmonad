# To run this test:
#
#   $ nix flake check
#
# To run this test with debugging output:
#
#   $ nix flake check --print-build-logs
#
# To run the tests and interact with the VM:
#
#   $ nix build .#checks.x86_64-linux.default.driverInteractive
#   $ result/bin/nixos-test-driver
#
{ pkgs, module }:

let
  config = pkgs.runCommand "kmonad-config" { } ''
    ${pkgs.gnused}/bin/sed -E \
      '/^\(defcfg/,/^\)/d' \
      ${../keymap/tutorial.kbd} > $out
  '';

  qemu-keyboard = "/dev/input/by-path/pci-0000:00:0a.0-event-kbd";

  users = { ... }: {
    users.users.jdoe = {
      createHome = true;
      isNormalUser = true;
      password = "password";
      group = "users";
    };

  };
in
pkgs.nixosTest {
  name = "kmonad-test";

  nodes = {
    tutorial = { ... }: {
      imports = [
        module
        users
      ];

      services.kmonad = {
        enable = true;
        keyboards.qemu = {
          device = qemu-keyboard;
          config = builtins.readFile ../keymap/tutorial.kbd;
        };
      };
    };

    defcfgGenerated = { ... }: {
      imports = [
        module
        users
      ];

      services.kmonad = {
        enable = true;
        keyboards.qemu = {
          device = qemu-keyboard;
          config = builtins.readFile (toString config);

          defcfg = {
            enable = true;
            compose.key = null;
            fallthrough = true;
          };
        };
      };
    };
  };

  testScript =
    let node = name: ''
      with subtest("Verify KMonad started"):
          ${name}.wait_for_unit("kmonad-qemu.service")
          ${name}.wait_until_succeeds("pgrep kmonad")

      with subtest("Log In"):
          ${name}.wait_until_tty_matches(1, "login: ")
          ${name}.send_chars("jdoe\n")
          ${name}.wait_until_tty_matches(1, "Password: ")
          ${name}.send_chars("password\n")
          ${name}.wait_until_tty_matches(1, "$")

      with subtest("Test Tutorial Numbers Layer"):
          ${name}.send_chars("echo ")
          ${name}.send_key("meta_l-k") # Should send "5"
          ${name}.send_key("meta_l-l") # Should send "6"
          ${name}.send_chars(" > /tmp/keys\n")
          ${name}.wait_until_succeeds("test -e /tmp/keys")
          ${name}.succeed("test \"$(cat /tmp/keys)\" -eq 56")
    '';
    in
    ''
      with subtest("Start nodes"):
          start_all()

    ''
    + node "tutorial"
    + node "defcfgGenerated";
}
