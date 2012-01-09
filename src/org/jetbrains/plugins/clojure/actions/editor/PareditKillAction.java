package org.jetbrains.plugins.clojure.actions.editor;

import com.google.common.collect.Lists;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.actionSystem.EditorWriteActionHandler;
import com.intellij.openapi.editor.actions.CutLineEndAction;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.plugins.clojure.psi.api.ClBraced;
import org.jetbrains.plugins.clojure.psi.util.ClojurePsiUtil;

import java.util.Collection;

/**
 * Kill a line as if with CutLineEndAction, but respecting delimiters.
 *
 * On a line with no S-expressions on it starting after the point or
 * within a comment, act exactly as CutLineEndAction.
 * Otherwise, kill all S-expressions that start after the point.
 * In a string, act exactly as CutLineEndAction but do not kill past the
 * closing string delimiter.
 *
 * @see  paredit-kill http://mumble.net/~campbell/emacs/paredit.html
 * @author <a href="mailto:joseferminj@gmail.com">Jose Fermin</a>
 */
public class PareditKillAction extends ClojureEditorAction {

    private static CutLineEndAction cutLineAction = new CutLineEndAction();

    public PareditKillAction() {
        super(new PareditKillActionHandler());
    }

    private static class PareditKillActionHandler extends EditorWriteActionHandler {

        @Override
        public void executeWriteAction(Editor editor, DataContext dataContext) {
            PsiElement element = ClojurePsiUtil.findElementAtCaret(editor, false);
            //The caret is not at any sexp. The caret is in top-level
            if (element == null) {
                //Find the next sexp and deleted
                element = ClojurePsiUtil.findNextSexpAtCaretInCurrentLine(editor);
                if (element == null) {
                    cutLineAction.actionPerformed(editor, dataContext);
                    return;
                }
                element.delete();
                return;
            }

            //The caret is at a element in a sexp
            PsiElement parent = element.getParent();
            if (parent == null || !(parent instanceof ClBraced)) {
                return;
            }
            //Deleted all next sibling in the sexp
            ClBraced clBraced = (ClBraced) parent;
            Collection<PsiElement> elementsToDelete = Lists.newArrayList();
            while (element != null && !element.equals(clBraced.getLastBrace())) {
                if (!(element instanceof PsiWhiteSpace)) {
                    elementsToDelete.add(element);
                }
                element = PsiTreeUtil.getNextSiblingOfType(element, PsiElement.class);
            }

            for (PsiElement psiElement : elementsToDelete) {
                psiElement.delete();
            }
        }
    }
}
