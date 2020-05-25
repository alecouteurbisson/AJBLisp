////////////////////////////////////////////////////////////////////////////
//
//  AJBLisp - A Lisp Interpreter
//
//  (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// gfunc.cc
//
// Lisp graphics function implementation (SUBRs & FSUBRS)
// These are here because func.cpp is apparently as big as the compiler
// can handle(!)  Status is here as it needs listenerif.h
////////////////////////////////////////////////////////////////////////////
#pragma hdrstop

#include "ajblisp.h"
#include "listenerif.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

//f (status [<string>])
//$ Copy the string to the Listener status bar. Very handy
//$ for debugging.
LISP Status(byte nargs, LISP args[])
{
  CHECK_ARITY_LE(1);
  if(nargs == 0)
  {
    ListenerStatus("");
  }
  else
  {
    EXPECT_STR(ARG1);
    ListenerStatus(text(ARG1));
  }
  return NIL;
}

///////////////////////////////////////////////////////////////////////////////
// Graphics functions
///////////////////////////////////////////////////////////////////////////////

//////////////////////////
// Utility functions
//////////////////////////

// Get two function arguments as coordinates and apply the view tranform
// to yield integer pixel coordinates.
void GGetCoords(LISP w, LISP a1, LISP a2, int& x, int& y)
{
  double dx = IsInteger(a1) ? (double)ivalue(a1)
                              : rvalue(a1);
  double dy = IsInteger(a2) ? (double)ivalue(a2)
                              : rvalue(a2);
  View2& v2 = view2(w);

  x = (int)floor(dx * v2.vt[0][0] +
                 dy * v2.vt[0][1] +
                      v2.vt[0][2] + 0.5);
  y = (int)floor(dx * v2.vt[1][0] +
                 dy * v2.vt[1][1] +
                      v2.vt[1][2] + 0.5);
}
// ---------------------------------------------------------------------------
// Get two function arguments as vector elements and apply the view tranform
// to yield integer pixel offsets.  Note that translation via the view
// transform is not applied to vectors (vectors are not points and cannot
// be translated.)
void GGetVector(LISP w, LISP a1, LISP a2, int& x, int& y)
{
  double dx = IsInteger(a1) ? (double)ivalue(a1)
                              : rvalue(a1);
  double dy = IsInteger(a2) ? (double)ivalue(a2)
                              : rvalue(a2);
  View2& v2 = view2(w);

  x = (int)floor(dx * v2.vt[0][0] +
                 dy * v2.vt[0][1] + 0.5);
  y = (int)floor(dx * v2.vt[1][0] +
                 dy * v2.vt[1][1] + 0.5);
}

// ---------------------------------------------------------------------------
// Get two function arguments as real numbers.
void GGetNum2(LISP a1, LISP a2, double& x, double& y)
{
  x = IsInteger(a1) ? (double)ivalue(a1)
                    : rvalue(a1);
  y = IsInteger(a2) ? (double)ivalue(a2)
                    : rvalue(a2);
}

// ---------------------------------------------------------------------------
// The graphics user functions
// ---------------------------------------------------------------------------
//
// These are slightly complicated by the fact that the VCL runs in another
// thread.  Therefore all communication must take place via synchronised
// methods of the lisp thread.  Since no arguments are allowed there is a
// shared data area for argument passing.  The variables names do imply
// their usage even though they are shared by many functions.
// Also note that the interface is a little more complicated than it need
// be since vcl.h cannot be included in here or the compiler crashes.
// (apparently 512M RAM is not enough for its symbol table!?)

//f (gwindow <width> <height> [<caption>])
//$ Create a graphics window of the specified size and with an optional
//$ title bar caption. Returns a window handle object.
//$ The user may close a window but the internal object will still exist
//$ and will accept further operations without error.
LISP GWindow(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(2, 3);
  EXPECT_INT2(ARG1, ARG2);
  if(nargs == 3)
  {
    EXPECT_STR(ARG3);
    s.Text = text(ARG3);
  }
  else
  {
    s.Text = "AJBLisp Graphics Window";
  }

  s.x = ivalue(ARG1);
  s.y = ivalue(ARG2);

  ListenerGWindow();

  if(!s.Win)
    LispError(ERR_WIN_OPEN);

  return NewWindow(s.Win, s.x, s.y);
}

//m (gclose <window>)
//$ Closes a graphics window. Sets <window> to nil and returns nil.
//$ It's not a problem if you lose the handle before you close the
//$ window as you can close it manually (X marks the spot!)
LISP GClose(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_SYM(ARG1);
  LISP win = iEval(ARG1);
  EXPECT_WIND(win);
  s.Win = window(win);

  ListenerGClose();
  iSet(win, NIL);
  return NIL;
}

//f (gclear <window> [<colour>])
//$ Clears the window to the current brush colour or to the
//$ colour specified. Returns nil.
LISP GClear(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(1, 2);
  EXPECT_WIND(ARG1);

  s.Win = window(ARG1);
  if(nargs == 2)
    s.Colour = ivalue(ARG2);
  else
    s.Colour = -1;

  ListenerGClear();
  return NIL;
}

//f (gmove <window> <x> <y>)
//$ Move the graphics cursor to coordinates <x>, <y>.
//$ These may be floating point values and will be
//$ affected by the windows viewport setting.
LISP GMove(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(3);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);

  ListenerGMove();
  return NIL;
}

//f (gpos <window>)
//$ Get the graphics cursor coordinates in the viewport
//$ coordinate system.
LISP GPos(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(1);
  EXPECT_WIND(ARG1);

  s.Win = window(ARG1);

  ListenerGPos();

  // This odd declaration is a reference to the view array
  double (&v)[2][3] = view2(ARG1).vt;

  // Subtract translations
  double xx = (s.x - v[0][2]);
  double yy = (s.y - v[1][2]);

  // Get determinant
  double det = v[0][0] * v[1][1] - v[0][1] * v[1][0];

  // Test for singularity (the view matrix should always be
  // highly non-singular)
  if(abs(det) < 1e-6)
    LispError(ERR_VT2_SING);

  // Get view coordinates
  double x = ( v[1][1] * xx - v[1][0] * yy) / det;
  double y = (-v[0][1] * xx + v[0][0] * yy) / det;

  LISP r = NewCons(NewReal(y), 0L);
  return NewCons(NewReal(x), r);
}

//f (gdraw <window> <x> <y>)
//$ Draw a line to coordinates x, y from the graphics
//$ cursor location and update the graphics cursor.  x and
//$ y are generally floating point values and will be
//$ affected by the windows viewport setting
LISP GDraw(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(3);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);

  ListenerGDraw();
  return NIL;
}

//f (gpoint <window> <x> <y> [<c>])
//$ Colour the pixel at coordinates (x, y).
//$ Use the current pen colour or the colour specified.
LISP GPoint(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(3, 4);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);
  if(nargs == 4)
    s.Colour = ivalue(ARG4);
  else
    s.Colour = -1;

  ListenerGPoint();
  return NIL;
}

//f (gpick <window> <x> <y>)
//$ Read the pixel at coordinates (x, y).
//$ Returns an integer colour value.
LISP GPick(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(3);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);
  EXPECT_WIND(ARG1);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);

  ListenerGPoint();
  return NewInteger(s.Colour);
}


//f (gellipse <window> <x1> <y1> <x2> <y2>)
//$ Draw a filled, outlined ellipse.  The coordinates
//$ specify its axis-aligned bounding box.  Either the fill
//$ or the outline may be disabled via suitable settings
//$ of the pen and brush styles (eg :bs_clear).
LISP GEllipse(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(5);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);
  EXPECT_NUM2(ARG4, ARG5);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);
  GGetCoords(ARG1, ARG4, ARG5, s.u, s.v);

  ListenerGEllipse();
  return NIL;
}

//f (grectangle <window> <x1> <y1> <x2> <y2>)
//$ Draw a filled, outlined rectangle.  Either the fill
//$ or the outline may be disabled via suitable settings
//$ of the pen and brush styles (eg :bs_clear).
LISP GRectangle(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(5);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);
  EXPECT_NUM2(ARG4, ARG5);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);
  GGetCoords(ARG1, ARG4, ARG5, s.u, s.v);

  ListenerGRectangle();
  return NIL;
}

//f (gviewport <window> <xleft> <ybottom> <xright> <ytop>)
//$ Set the viewport of the window so that the limits
//$ are as specified.  It is up to the user to ensure
//$ that the view aspect ratio is appropriate
//$ Existing window contents are not affected in any way.
LISP GViewport(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(4, 5);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);
  EXPECT_NUM2(ARG4, ARG5);

  View2 v2;
  v2.zero();

  double xmin, xmax, ymin, ymax;
  GGetNum2(ARG2, ARG3, xmin, ymin);
  GGetNum2(ARG4, ARG5, xmax, ymax);


  double scl = xmax - xmin;
  if(fabs(scl) < 1e-99)
    LispError(ERR_INV_VP);
  scl = width(ARG1) / scl;
  v2.vt[0][0] = scl;
  v2.vt[0][2] = -xmin * scl;

  scl = ymax - ymin;
  if(fabs(scl) < 1e-99)
    LispError(ERR_INV_VP);
  scl = height(ARG1) / scl;
  v2.vt[1][1] = -scl;
  v2.vt[1][2] = height(ARG1) + ymin * scl;

  view2(ARG1) = v2;
  return NIL;
}

//f (gvtrans2 <win> [<v00> <v10> <v01> <v11> <v02> <v12>]), (gvtrans2 <win> <vx> <vy>)
//$ Explicitly set the windows 2D view transform or get it if
//$ no view transform values are given.  With three arguments
//$ it returns the plot coordinates corresponding to the view
//$ coordinates of the point supplied.
//$ ~The view transform is a 3 * 2 homogeneous matrix:
//$
//$ .[ vx  vy (1.0) ]  *  [ v00 v10 ]  =  [ px py ]
//$ .                     [ v01 v11 ]
//$ .                     [ v02 v12 ]
//$
//$ ~The value 1.0 indicates that vx, vy represent a point rather
//$ than a vector in which case it would be 0.0.
//$
//$ ~So the view transform for points is:
//$ .vpx = x * v00 + y * v01 + v02;
//$ .vpy = x * v10 + y * v11 + v12;
//$
//$ ~and the view transform for vectors is:
//$ .vpx = x * v00 + y * v01;
//$ .vpy = x * v10 + y * v11;
//$
//$ ~Windows primitive functions are not affected except in
//$ that their argument coordinates will be transformed.
//$ Ellipses and Rectangles will scale and translate properly but
//$ they will always be axis-aligned even if a rotation is implied
//$ by the view transform.
//$
//$ ~The default view transform is (1.0, 0.0, 0.0, 1.0, -1.0, H)
//$ which maps the input so that the bottom left of the window
//$ is the origin and the scale is 1 plot unit per pixel.  H is
//$ the height of the drawable area.  This is upside-down compared
//$ to the Windows default view coordinate system.
//$ .(hence v11 = -1.0 and V12 = -H)
LISP GVTrans2(byte nargs, LISP args[])
{
  EXPECT_WIND(ARG1);
  // This odd declaration is a reference to the view array
  double (&v)[2][3] = view2(ARG1).vt;

  if(nargs == 1)
  {
    // Read view transform
    LISP r;
    r =    NewCons(NewReal(v[1][2]), NIL);
    r =    NewCons(NewReal(v[0][2]), r);
    r =    NewCons(NewReal(v[1][1]), r);
    r =    NewCons(NewReal(v[0][1]), r);
    r =    NewCons(NewReal(v[1][0]), r);
    return NewCons(NewReal(v[0][0]), r);
  }

  if(nargs == 3)
  {
    EXPECT_NUM2(ARG2, ARG3);

    int x, y;
    GGetCoords(ARG1, ARG2, ARG3, x, y);

    LISP r;
    r =    NewCons(NewInteger(x), NIL);
    return NewCons(NewInteger(y), r);

  }

  // Write view transform
  CHECK_ARITY_EQ(7);
  EXPECT_NUM2(ARG4, ARG5);
  EXPECT_NUM2(ARG6, ARG6);


  // Write new values to view
  GGetNum2(ARG2, ARG3, v[0][0], v[1][0]);
  GGetNum2(ARG4, ARG5, v[0][1], v[1][1]);
  GGetNum2(ARG6, ARG7, v[0][2], v[1][2]);
  return NIL;
}

//f (gpen <window> <colour> <width/style>)
//$ Set the line drawing style for a window.
//$ The colour is an integer colour.  The width/style
//$ argument selects the line width if >= 1 and sets
//$ the line style if <= 0.  Note that styled lines are
//$ only available with a width of 1.
//$ ~The :ps_* constants are the available pen line styles:
//$ . :ps_solid,   :ps_dash,       :ps_dot,
//$ . :ps_dashdot, :ps_dashdotdot, :ps_clear


LISP GPen(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(2, 3);
  EXPECT_WIND(ARG1);
  EXPECT_INT(ARG2);

  s.Win = window(ARG1);
  s.Style = 1;                   // Default style is solid, width 1
  if(nargs == 3)
    s.Style = ivalue(ARG3);      // User style / width
  if(s.Style < -6)               // Limit on style codes
    LispError(ERR_INV_PSTYL, ARG3);
  s.Colour = ivalue(ARG2);

  ListenerGPen();
  return NIL;
}

//f (gbrush <window> <colour> [<style>])
//$ Set the fill style for a window and return nil.
//$ The colour is an integer colour.  The style argument
//$ selects the fill style.
//$ ~The :bs_* constants are the available brush fill styles
//$ with :bs_solid being the default style.
//$ .:bs_solid, :bs_clear, :bs_horiz, :bs_vert,
//$ .:bs_fdiag, :bs_bdiag, :bs_square, :bs_diamond,

LISP GBrush(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(2, 3);
  EXPECT_WIND(ARG1);
  EXPECT_INT(ARG2);

  s.Win = window(ARG1);
  s.Style = 0;                    // Default style is solid
  if(nargs == 3)
    s.Style = ivalue(ARG3);
  if((s.Style < 0) || (s.Style > 7))
    LispError(ERR_INV_BSTYL, ARG3);
  s.Colour = ivalue(ARG2);

  ListenerGBrush();
  return NIL;
}

//f (gtext <window> <x> <y> <text>)
//$ Writes the text to the graphics window at the given
//$ location in the viewport.
//$ ~The origin of the text is the upper left corner of
//$ its bounding box and these coordinates will be
//$ subject to the view transform.
//$ The text displayed is otherwise unaffected by the
//$ transform.
LISP GText(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(4);
  EXPECT_WIND(ARG1);
  EXPECT_NUM2(ARG2, ARG3);
  EXPECT_STR(ARG4);

  s.Win = window(ARG1);
  GGetCoords(ARG1, ARG2, ARG3, s.x, s.y);
  s.Text = text(ARG4);

  ListenerGText();
  return NIL;
}

//f (gtextsize <window> <text>)
//$ Returns the width and height that the text would occupy
//$ in the window if written with gtext.
//$ ~These values are returned as (width height) where the
//$ sizes are in pixels (note NOT viewport units.)
//$ ~Text does not scale with the viewport so converting the
//$ units from pixels would not really be useful.
LISP GTextSize(byte nargs, LISP args[])
{
  CHECK_ARITY_EQ(2);
  EXPECT_WIND(ARG1);
  EXPECT_STR(ARG2);

  s.Win = window(ARG1);
  s.Text = text(ARG2);

  ListenerGTextSize();

  LISP r = NewCons(NewInteger(s.y), 0L);
  return NewCons(NewInteger(s.x), r);
}

//f (gcolour <red> <green> <blue>), gcolour(<integer>), gcolour(<grey>)
//$ Construct/deconstruct a colour value.
//$ ~With three arguments it specifies the colour in the fractional parts.
//$ The integer parts are ignored.  This makes it easy to generate palettes
//$ that cycle through many distinct colours.  It also means that
//$ specifying, say, 1.0 for red will make the red component zero.
//$ ~Use, for example, (gcolour 0.999 0.999 0.999) to make white.
//$ ~The output value is an integer of the form 0x00BBGGRR.
//$ ~With a single, integer argument it reverses the process and
//$ supplies the red, green and blue values in a list.  These
//$ are scaled so that the full scale value is 255.0/256.0
//$ ~With a single real argument colour returns an integer grey level.
//x > (gcolour 0.5 0.0 0.5)      ; purple
//x = 8388736                    ; = 0x800080
//x > (gcolour 8388736)          ; convert back to RGB values
//x = (0.5000000000000000 0.000000000000000 0.5000000000000000)
//x > (gbrush win (gcolour 0.5)) ; fill style set to solid mid-grey
//x = nil
LISP GColour(byte nargs, LISP args[])
{
  if(nargs == 1)
  {
    // Value -> Grey
    if(IsReal(ARG1))
    {
      int c = floor((rvalue(ARG1) - floor(rvalue(ARG1))) * 256.0);
      return NewInteger(c | (c << 8) | (c << 16));
    }

    // Colour => Values
    EXPECT_INT(ARG1);
    int c = ivalue(ARG1);

    double r, g, b;
    r = (c & 0xFF) / 256.0;
    g = ((c >> 8) & 0xFF) / 256.0;
    b = ((c >> 16) & 0xFF) / 256.0;

    LISP
    l = NewCons(NewReal(b), 0L);
    l = NewCons(NewReal(g), l);
    return NewCons(NewReal(r), l);
  }

  // Values => Colour
  CHECK_ARITY_EQ(3);
  EXPECT_REAL3(ARG1, ARG2, ARG3);
  int r = floor((rvalue(ARG1) - floor(rvalue(ARG1))) * 256.0);
  int g = floor((rvalue(ARG2) - floor(rvalue(ARG2))) * 256.0);
  int b = floor((rvalue(ARG3) - floor(rvalue(ARG3))) * 256.0);
  return NewInteger(r | (g << 8) | (b << 16));
}

//f (gfont <window> <name> <size> [<style> <pitch> <colour>])
//$ Set the graphics font for <window>.  <name> is the usual Windows
//$ font name.  <size> is the height in pixels, or alternatively,
//$ the negative of the point size.  <style> is the logical-or of
//$ any of :fs_bold, :fs_italic, :fs_underline and :fs_strikeout.
//$ nil or zero indicate none of the above.
//$ ~Pitch may be nil, 0, :fp_fixed or :fp_variable.  nil or zero
//$ means use the font default.  <colour> is the integer colour
//$ value for the font or -1 to leave it unchanged.
//$ ~The defaults for <style>, <pitch> and <colour> are 0, 0 and
//$ -1 respectively,  gfont returns nil.
//$ ~The font actually set may be other than the one requested
//$ since Windows will always select a font whatever you request.
//$ It does this by trying to get as close to the spec
//$ as possible if it cannot meet it exactly.
LISP GFont(byte nargs, LISP args[])
{
  CHECK_ARITY_IN(3, 7);
  EXPECT_WIND(ARG1);
  EXPECT_STR(ARG2);
  EXPECT_INT(ARG3);

  s.Win = window(ARG1);  // Window to set font for
  s.Text = text(ARG2);   // Name
  s.x = ivalue(ARG3);    // Size

  s.Style = 0;
  if((nargs >= 4) && ARG4)
  {
    EXPECT_INT(ARG4);
    s.Style = ivalue(ARG4);
  }

  // Pitch is 0 - Default, 1 - Fixed, 2 - Variable
  s.y = 0;
  if((nargs >= 5) && ARG5)
  {
    EXPECT_INT(ARG5);
    s.y = ivalue(ARG5);
  }

  // Colour = -1 preserves the existing colour
  s.Colour = -1;
  if(nargs >= 6)
  {
    EXPECT_INT(ARG6);
    s.Colour = ivalue(ARG6);
  }

  ListenerGFont();

  return NIL;
}
