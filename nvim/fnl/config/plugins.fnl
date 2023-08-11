(module config.plugins
  {autoload {: packer}})

(defn setup []
  (packer.startup
    (fn [use]
      (use "numToStr/Comment.nvim"))))
