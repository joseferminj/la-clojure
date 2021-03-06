package org.jetbrains.plugins.clojure.repl.toolwindow.actions;

import org.jetbrains.plugins.clojure.repl.REPLProvider;
import org.jetbrains.plugins.clojure.repl.impl.ClojureProcessREPL;

public class NewExternalConsoleAction extends NewConsoleActionBase
{
  @Override
  protected REPLProvider getProvider()
  {
    return new ClojureProcessREPL.Provider();
  }
}
