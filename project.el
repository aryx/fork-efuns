; foo 
(setq 
 pad-ocaml-project-path "/home/pad/github/fork-efuns"
 pad-ocaml-project-subdirs 
 (split-string 
  "commons core features graphics
   major_modes minor_modes prog_modes
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
   (case 1
     
     (0 "/home/pad/github/fork-efuns/changes.txt")
     (1 "/home/pad/github/fork-efuns/main.ml")
     
     )
   ))
 )
