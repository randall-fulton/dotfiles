(module nvim-config
  {autoload {core aniseed.core
             comments config.comments
             git config.git
             plugins config.plugins}})

(set vim.g.maplocalleader ",")

(plugins.setup)
(comments.setup)
(git.setup)
