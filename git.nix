{ email, ... }: {
  enable = true;
  userName = "Randall Fulton";
  userEmail = email;
  ignores = [
    ".DS_Store"
    # emacs
    "*/.saves"
    "*~"
    "*#*"
    ".dir-locals.el"
    # python
    "*venv/"
    "*.venv/"
  ];
  extraConfig = {
    url = {
      "ssh://git@github.com/" = {
        insteadOf = "https://github.com/";
      };
    };
  };
}
