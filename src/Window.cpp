////////////////////////////////////////////////////////////////////////////
//
// AJBLisp for Windows
//
// (c) A J Le Couteur Bisson 1997-2007
//
////////////////////////////////////////////////////////////////////////////
// Window.cpp
//
// Graphics window routines
////////////////////////////////////////////////////////////////////////////

#include <vcl.h>
#pragma hdrstop

#include "Window.h"
#include "listener.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TformGWin *formGWin;

//---------------------------------------------------------------------------
__fastcall TformGWin::TformGWin(TComponent* Owner, int w, int h, char* caption)
  : TForm(Owner)
{
  ClientWidth = w;
  ClientHeight = h;
  Graphics::TBitmap* bm = new Graphics::TBitmap;
  bm->Width = ClientWidth;
  bm->Height = ClientHeight;
  bm->PixelFormat = pf24bit;
  Image->Picture->Bitmap = bm;

  Image->Canvas->Brush->Color = clWhite;
  Image->Canvas->FillRect(ClientRect);
  Image->Canvas->Brush->Color = clBlack;
  Image->Canvas->Pen->Color = clBlack;

  Caption = caption;
}
//---------------------------------------------------------------------------

void __fastcall TformGWin::FormKeyPress(TObject *, char &)
{
  formListener->SetFocus();
}
//---------------------------------------------------------------------------


