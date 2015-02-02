#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################

##############################################################################
# Developer rules
##############################################################################

##############################################################################
# Literate Programming rules
##############################################################################

include $(TOP)/docs/latex/Makefile.common


TEXMAIN=Efuns.nw
TEX=Efuns.tex

SRC_ORIG=Efuns.nw Efuns_extra.nw

SRC_VIEWS= \
  core/efuns.ml\
  core/text.ml\
  core/ebuffer.ml\
  core/keymap.ml\
  core/minibuffer.ml\
  core/window.ml\
  core/frame.ml\
  features/simple.ml\
  features/complex.ml\
  features/system.ml\
  features/select.ml\
  features/search.ml\
  features/interactive.ml\
  features/abbrevs.ml\
  features/multi_frames.ml\
  features/compil.ml\
  server/server.ml\
  client/efuns_client.ml\
  dynamic/eval.ml\
  graphics/top_window.ml\
  std_efunsrc.ml\
  main.ml\
  major_modes/dired.ml\
  minor_modes/minor_mode_sample.ml\
  minor_modes/abbrevs_mode.ml\
  minor_modes/accents_mode.ml\
  minor_modes/fill_mode.ml\
  minor_modes/paren_mode.ml\
  minor_modes/tab_mode.ml\
  misc/efuns_filebrowser.ml\
  misc/efuns_texbrowser.ml\
  misc/local.ml\
  prog_modes/makefile_mode.ml\
  prog_modes/ocaml_toplevel.ml\

