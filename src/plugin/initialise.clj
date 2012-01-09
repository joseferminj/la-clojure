(ns plugin.initialise
  (:require plugin.formatting
            plugin.typing
            plugin.actions.editor))


(defn initialise-all []
  (plugin.formatting/initialise)
  (plugin.typing/initialise))
