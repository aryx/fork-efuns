#include <stdlib.h>
#include <unistd.h>
#include <draw.h>
#include <event.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>

Image *colors[256];
Image *current_color = NULL;

int mk_color(int red, int green, int blue) {
  return (red <<3*8) |  (green <<2*8) |  (blue <<1*8) | 0xFF;
}

void caml_draw_initdraw(value x, value y) {
  CAMLparam2(x, y);
  int i;

  USED(x); USED(y);
  initdraw(nil, nil, "test_draw");

  einit(Ekeyboard);

  for(i = 0; i < 256; i++) {
      colors[i] = NULL;
  }

  // fg: wheat
  colors[0] = allocimage(display, Rect(0,0,1,1), view->chan, 1,
                         mk_color(245, 222, 179));

  // bg: DarkSlateGray
  colors[1] = allocimage(display, Rect(0,0,1,1), view->chan, 1,
                         mk_color(47, 79, 79));

  // highlight: 
  colors[2] = allocimage(display, Rect(0,0,1,1), view->chan, 1,
                         DCyan);

  CAMLreturn0;
}

void  eresized(int isnew) {
  //print("eresized %d\n", isnew);
  if(isnew && getwindow(display, Refnone) < 0){
    fprint(2, "colors: can't reattach to window: %r\n");
    exit(2);
  }

  flushimage(display, 1);

}

/**************************************************************************/
/* Tests */
/**************************************************************************/

void caml_draw_set_color(value red, value green, value blue, value alpha) {

  CAMLparam4(red, green, blue, alpha);
  int rgba = (Int_val(red)<<3*8) | 
             (Int_val(green)<<2*8) | 
             (Int_val(blue)<<1*8) | 
             (Int_val(alpha)<<0*8);
  current_color = allocimage(display, Rect(0,0,1,1), view->chan, 1, rgba);
  CAMLreturn0;
}

void caml_draw_line(value x1, value y1, value x2, value y2) {
  CAMLparam4(x1, y1, x2, y2);

  line(view, Pt(Int_val(x1), Int_val(y1)), 
             Pt(Int_val(x2), Int_val(y2)), 
       0, 0, 0, current_color, ZP);
  flushimage(display, 1);
  CAMLreturn0;
}



void caml_draw_string(value x, value y, value str) {
  CAMLparam3(x, y, str);

  string(view, Pt(Int_val(x), Int_val(y)), colors[1], ZP, font,
         String_val(str));
  flushimage(display, 1);
  CAMLreturn0;
}


/**************************************************************************/
/* Efuns backend */
/**************************************************************************/

Image *current_fg = NULL;
Image *current_bg = NULL;

void set_color_idx(int idx, int red, int green, int blue) {
  if (colors[idx] == NULL) {
    colors[idx] = allocimage(display, Rect(0,0,1,1), view->chan, 1, 
                             mk_color(red, green, blue));

  }
}

  // used to be just current_bg = colors[0]
void caml_draw_set_bg_color(value i, value r, value g, value b) {
  CAMLparam4(i, r, g, b);
  int idx = Int_val(i);
  int red = Int_val(r);
  int green = Int_val(g);
  int blue = Int_val(b);

  set_color_idx(idx, red, green, blue);
  current_bg = colors[idx];
  CAMLreturn0;
}

  // used to be just current_fg = colors[1]
void caml_draw_set_fg_color(value i, value r, value g, value b) {
  CAMLparam4(i, r, g, b);
  int idx = Int_val(i);
  int red = Int_val(r);
  int green = Int_val(g);
  int blue = Int_val(b);

  set_color_idx(idx, red, green, blue);
  current_fg = colors[idx];
  CAMLreturn0;
}


void caml_draw_clear_eol(value c, value l, value length) {
  CAMLparam3(c, l, length);
  int w = font->width;
  int h = font->height;
  int col = Int_val(c);
  int line = Int_val(l);
  int len = Int_val(length);
  int x = col * w;
  int y = line * h;
  Rectangle r = Rect(x, y, x + len * w, y + h);

  draw(view, rectaddpt(r, view->r.min), current_bg, nil, ZP);
  flushimage(display, 1);
  CAMLreturn0;
}

void caml_draw_draw_string(value c, value l, value s) {
  CAMLparam3(c, l, s);
  int w = font->width;
  int h = font->height;
  int col = Int_val(c);
  int line = Int_val(l);
  char *str = String_val(s);
  int x = col * w;
  int y = line * h;
  Point pt = Pt(x, y);
  string(view, addpt(pt, view->r.min) , current_fg, ZP, font, str);
  flushimage(display, 1);
  CAMLreturn0;
}

value caml_draw_ekbd(void) {
  int res;
  //print("before caml_draw_ekbd\n");
  res = ekbd();
  //print("res = %d\n", res);
  return Val_int(res);
}

