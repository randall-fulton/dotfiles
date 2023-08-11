(module config.git
  {autoload {: neogit
             : which-key}})

(defn setup []
  (which-key.register 
    {"<leader>" 
     {:g {:name "Git"
          :g [ neogit.open "Open Neogit" ]}}})
  (neogit.setup { :integrations { :diffview true }}))
