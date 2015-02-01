all: byte opt

include ../Makefile.config

INCLUDES=  -I ../ocamlsrc -I ../$(EFUNS_DYNLINK) -I ../toolkit $(LIBDIR)


BYTE_LINK_WX= $(CURDIR)/toolkit/WXlib.cma
OPT_LINK_WX= $(CURDIR)/toolkit/WXlib.cmxa

BYTE_DEP_WX= $(CURDIR)/toolkit/WXlib.cma
BYTE_OPT_WX= $(CURDIR)/toolkit/WXlib.cmxa

BYTE_LINK_DYN= $(EFUNS_BYTE_DYN_LINK) $(EFUNS_DYNLINK).cma $(CURDIR)/$(EFUNS_DYNLINK)/dyneval.cmo
OPT_LINK_DYN=  $(EFUNS_OPT_DYN_LINK) $(CURDIR)/$(EFUNS_DYNLINK)/lib$(EFUNS_DYNLINK).a $(CURDIR)/$(EFUNS_DYNLINK)/$(EFUNS_DYNLINK).cmxa $(CURDIR)/$(EFUNS_DYNLINK)/dyneval.cmx
OPT_DEP_DYN=  $(CURDIR)/$(EFUNS_DYNLINK)/lib$(EFUNS_DYNLINK).a $(CURDIR)/$(EFUNS_DYNLINK)/$(EFUNS_DYNLINK).cmxa $(CURDIR)/$(EFUNS_DYNLINK)/dyneval.cmx
BYTE_DEP_DYN=  $(CURDIR)/$(EFUNS_DYNLINK)/dyneval.cmo
BYTE_DEPS= $(BYTE_DEP_DYN) $(BYTE_DEP_X) $(BYTE_DEP_COM) $(BYTE_DEP_CCR) $(BYTE_DEP_WX)
OPT_DEPS= $(OPT_DEP_DYN) $(OPT_DEP_X) $(OPT_DEP_COM) $(OPT_DEP_CCR) $(OPT_DEP_WX)

EDIT_OBJS= local.cmo efuns.cmo text.cmo keymap.cmo ebuffer.cmo window.cmo frame.cmo top_window.cmo simple.cmo minibuffer.cmo  multi_frames.cmo select.cmo search.cmo system.cmo abbrevs.cmo complex.cmo compil.cmo eval.cmo interactive.cmo server.cmo
MINOR_MODES= accents_mode.cmo paren_mode.cmo abbrevs_mode.cmo fill_mode.cmo tab_mode.cmo
OCAML_MODE=ocaml_mode.cmo ocaml_$(EFUNS_DYNLINK).cmo 
MAJOR_MODES= makefile_mode.cmo tex_mode.cmo c_mode.cmo lisp_mode.cmo html_mode.cmo dired.cmo
STD_CONFIG= std_efunsrc.cmo
MAIN_OBJS= main.cmo

BYTE_OBJS= $(EDIT_OBJS) $(MINOR_MODES) $(MAJOR_MODES) $(OCAML_MODE) $(STD_CONFIG) $(MAIN_OBJS)
OPT_OBJS= $(BYTE_OBJS:.cmo=.cmx)

TMPFILES=tex_mode.ml ocaml_mode.ml c_mode.ml lisp_mode.ml html_mode.ml

CAMLDIR=-I ../ocamlsrc/utils -I ../ocamlsrc/parsing -I ../ocamlsrc/typing -I ../ocamlsrc/bytecomp -I ../ocamlsrc/asmcomp -I ../ocamlsrc/driver -I $(CURDIR)/common

######################### standard rules

distrib: opt
	gzip efuns
	mv efuns.gz $(HOME)/public_html/src/efuns-`arch`-`uname`.gz

tmpfiles: $(TMPFILES)

byte: efuns.byte efunsrc.cmo tmpfiles efuns_client.byte efuns_texbrowser.byte efuns_filebrowser.byte

opt: efuns.opt efunsrc.cmo tmpfiles efuns_client.opt efuns_texbrowser.opt efuns_filebrowser.opt

top: 
	ocamlmktop -custom -o top unix.cma str.cma -cclib "-lunix -lstr"


ocaml_toplevel.cmo: ocaml_toplevel.ml
	$(OCAMLC) $(CAMLDIR) $(INCLUDES) -c ocaml_toplevel.ml

ocaml_toplevel.cmx: ocaml_toplevel.ml
	$(OCAMLOPT) $(CAMLDIR) $(INCLUDES) -c ocaml_toplevel.ml



efuns_texbrowser.byte: efuns_texbrowser.cmo
	$(OCAMLCL) -custom $(INCLUDES) -o efuns_texbrowser.byte $(BYTE_LINK_COM) $(BYTE_ALL_X) $(BYTE_LINK_STR) WXlib.cma efuns_texbrowser.cmo

efuns_texbrowser.opt: efuns_texbrowser.cmx
	$(OCAMLOPT) $(INCLUDES) -o efuns_texbrowser.opt $(OPT_ALL_X) $(OPT_LINK_STR) common.cmxa WXlib.cmxa efuns_texbrowser.cmx

efuns_filebrowser.byte: efuns_filebrowser.cmo
	$(OCAMLCL) -custom $(INCLUDES) -o efuns_filebrowser.byte $(BYTE_ALL_X) $(BYTE_LINK_STR) common.cma WXlib.cma efuns_filebrowser.cmo

efuns_filebrowser.opt: efuns_filebrowser.cmx
	$(OCAMLOPT) $(INCLUDES) -o efuns_filebrowser.opt $(OPT_ALL_X) $(OPT_LINK_STR) common.cmxa WXlib.cmxa efuns_filebrowser.cmx

efuns_client.byte: efuns_client.ml
	$(OCAMLCL) $(INCLUDES) -o efuns_client.byte  $(BYTE_ALL_X) efuns_client.ml

efuns_client.opt: efuns_client.ml
	$(OCAMLOPT) $(INCLUDES) -o efuns_client.opt  $(OPT_ALL_X) efuns_client.ml


efuns.byte: $(BYTE_OBJS)  $(BYTE_DEPS)
	$(OCAMLCL) $(INCLUDES) -o efuns.byte  $(BYTE_ALL_X) $(BYTE_LINK_STR) $(BYTE_LINK_COM) $(BYTE_LINK_WX) $(BYTE_LINK_DYN) $(BYTE_OBJS)

efuns.opt: $(OPT_OBJS)  $(OPT_DEPS) 
	$(OCAMLOPT) $(INCLUDES) -o efuns.opt  $(OPT_ALL_X)  $(OPT_LINK_STR) $(OPT_LINK_COM) $(OPT_LINK_WX) $(OPT_LINK_DYN) $(OPT_OBJS)

install: efunsrc.cmo
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f efuns.byte $(installroot)$(INSTALLBIN)/efuns.byte
	cp -f efuns_client.byte $(installroot)$(INSTALLBIN)/efuns_client.byte
	cp -f efuns_texbrowser.byte $(installroot)$(INSTALLBIN)/efuns_texbrowser.byte
	cp -f efuns_filebrowser.byte $(installroot)$(INSTALLBIN)/efuns_filebrowser.byte
	mkdir -p $(installroot)$(EFUNSLIB)
	cp -f *.cmi efunsrc.cmo $(installroot)$(EFUNSLIB)

installopt: efunsrc.cmo
	mkdir -p $(installroot)$(INSTALLBIN)
	cp -f efuns.opt $(installroot)$(INSTALLBIN)/efuns
	cp -f efuns_client.opt $(installroot)$(INSTALLBIN)/efuns_client
	cp -f efuns_texbrowser.opt $(installroot)$(INSTALLBIN)/efuns_texbrowser
	cp -f efuns_filebrowser.opt $(installroot)$(INSTALLBIN)/efuns_filebrowser
	mkdir -p $(installroot)$(EFUNSLIB)
	mkdir -p $(installroot)$(EFUNSLIB)/sources
	cp -f *.cmi efunsrc.cmo $(installroot)$(EFUNSLIB)
	cp -f *.mli  $(installroot)$(EFUNSLIB)/sources
	(for i in $(BYTE_OBJS:.cmo=.ml); do \
	if [ -e "$$i"l ]; then \
	  cp "$$i"l $(installroot)$(EFUNSLIB)/sources; \
	elif [ -e "$$i"y ]; then \
	  cp "$$i"y $(installroot)$(EFUNSLIB)/sources; \
	else \
	 cp "$$i" $(installroot)$(EFUNSLIB)/sources; fi; done)

clean: 
	rm -f *.cm? *.mlg *.o efuns efuns.byte efuns.opt *~ *.output core *.s $(TMPFILES) efuns.gz top
	rm -f *.byte *.opt

depend: tmpfiles
	ocamldep $(INCLUDES) *.ml *.mli  > .depend

include .depend
include ../Makefile.rules

