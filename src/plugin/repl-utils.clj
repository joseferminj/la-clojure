(ns plugin.repl-utils)

(defn project-manager [] (com.intellij.openapi.project.ProjectManager/getInstance))

(defn open-projects [] (seq (.getOpenProjects (project-manager))))

(defn make-file [text]
  (.createFileFromText (com.intellij.psi.PsiFileFactory/getInstance (first (open-projects)))
                       "dummy-file.clj"
                       text))

(def psi-file (make-file "(defn n [] 0)"))

(def psi-list (.getFirstChild psi-file))

(.getText psi-list)

(seq (.getChildren psi-list))


