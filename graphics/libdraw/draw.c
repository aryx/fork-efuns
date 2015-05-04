#include <stdlib.h>
#include <unistd.h>
#include <draw.h>
#include <event.h>
#include <caml/mlvalues.h>

Image *colors[256];
Image *current_color = NULL;

int mk_color(int red, int green, int blue) {
  return (red <<3*8) |  (green <<2*8) |  (blue <<1*8);
}

void caml_draw_initdraw(value x, value y) {
  int i;

  USED(x); USED(y);
  initdraw(nil, nil, "test_draw");

  einit(Ekeyboard);

  for(i = 0; i < 256; i++) {
      colors[i] = NULL;
  }

  // DarkSlateGray
  colors[0] = allocimage(display, Rect(0,0,1,1), view->chan, 1,
                         mk_color(47, 79, 79));
  // wheat
  colors[1] = allocimage(display, Rect(0,0,1,1), view->chan, 1,
                         mk_color(245, 222, 179));

}

void  eresized(int isnew) {
  //print("eresized %d\n", isnew);
  if(isnew && getwindow(display, Refnone) < 0){
    fprint(2, "colors: can't reattach to window: %r\n");
    exit(2);
  }

  flushimage(display, 1);

}

//**************************************************************************
// Tests
//**************************************************************************

void caml_draw_set_color(value red, value green, value blue, value alpha) {

  int rgba = (Int_val(red)<<3*8) | 
             (Int_val(green)<<2*8) | 
             (Int_val(blue)<<1*8) | 
             (Int_val(alpha)<<0*8);
  current_color = allocimage(display, Rect(0,0,1,1), view->chan, 1, rgba);

}

void caml_draw_line(value x1, value y1, value x2, value y2) {

  line(view, Pt(Int_val(x1), Int_val(y1)), 
             Pt(Int_val(x2), Int_val(y2)), 
       0, 0, 0, current_color, ZP);
  flushimage(display, 1);
}



void caml_draw_string(value x, value y, value str) {
  string(view, Pt(Int_val(x), Int_val(y)), colors[1], ZP, font,
         String_val(str));
  flushimage(display, 1);
}


//**************************************************************************
// Efuns backend
//**************************************************************************

void caml_draw_clear_eol(value c, value l, value length) {
  int w = font->width;
  int h = font->height;
  int col = Int_val(c);
  int line = Int_val(l);
  int len = Int_val(length);
  int x = col * w;
  int y = line * h;
  Rectangle r = Rect(x, y, x + len * w, y + h);

  draw(view, rectaddpt(r, view->r.min), colors[0], nil, ZP);
  flushimage(display, 1);
}

void caml_draw_draw_string(value c, value l, value s) {
  int w = font->width;
  int h = font->height;
  int col = Int_val(c);
  int line = Int_val(l);
  char *str = String_val(s);
  int x = col * w;
  int y = line * h;
  Point pt = Pt(x, y);
  string(view, addpt(pt, view->r.min) , colors[1], ZP, font, str);
  flushimage(display, 1);
}

value caml_draw_ekbd(void) {
  int res;
  //print("before caml_draw_ekbd\n");
  res = ekbd();
  //print("res = %d\n", res);
  return Val_int(res);
}

