//---------------------------------------------------------------------------
#ifndef WindowH
#define WindowH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TformGWin : public TForm
{
__published:	// IDE-managed Components
  TImage *Image;
  void __fastcall FormKeyPress(TObject *Sender, char &Key);
private:	// User declarations

public:		// User declarations
  __fastcall TformGWin(TComponent* Owner, int w, int h, char* caption);

  void __fastcall TformGWin::Clear()
  {
    Image->Canvas->FillRect(ClientRect);
  }

  void __fastcall TformGWin::MoveTo(int x, int y)
  {
    Image->Canvas->MoveTo(x, ClientHeight - y);
  }

  void __fastcall TformGWin::DrawTo(int x, int y)
  {
    Image->Canvas->LineTo(x, ClientHeight - y);
  }

  void __fastcall TformGWin::Ellipse(int x, int y, int u, int v)
  {
    Image->Canvas->Ellipse(x, ClientHeight - y, u, ClientHeight - v);
  }

  void __fastcall TformGWin::Rectangle(int x, int y, int u, int v)
  {
    Image->Canvas->Rectangle(x, ClientHeight - y, u, ClientHeight - v);
  }

};
//---------------------------------------------------------------------------
extern PACKAGE TformGWin *formGWin;
//---------------------------------------------------------------------------
#endif
