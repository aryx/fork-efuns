; foo 
(setq 
 pad-ocaml-project-path "/home/pad/github/fork-efuns"
 pad-ocaml-project-subdirs 
 (split-string 
  "commons core features graphics
   major_modes minor_modes prog_modes
   graphics/gtk_cairo
   external/pfff-commons
  "
  )
 pad-ocaml-project-toplevel "pfff.top"
)


(setq
 pad-ocaml-project-prog     "efuns"
 pad-ocaml-project-args 
 (join-string 
  (list 
   "-debugger"
   (case 3
     
     (0 "/home/pad/github/fork-efuns/changes.txt")
     (1 "/home/pad/github/fork-efuns/main.ml /home/pad/github/fork-efuns/std_efunsrc.ml")
     (2 "/home/pad/pfff/lang_nw/parsing/lexer_nw.mll")
     (3 "/home/pad/github/fork-efuns/tests/tabs.txt")
     
     )
   ))
 )
