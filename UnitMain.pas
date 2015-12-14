unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, PngImage, Math, Vcl.GraphUtil,
  ShellAPI;

type
  TFormMain = class(TForm)
    Image: TImage;
    FileOpenDialog: TFileOpenDialog;
    LabeledEditX: TLabeledEdit;
    LabeledEditY: TLabeledEdit;
    LabeledEditColor: TLabeledEdit;
    MemoJSON: TMemo;
    ButtonLoad: TButton;
    ShapeColor: TShape;
    LabeledEditRefColor: TLabeledEdit;
    LabelDistance: TLabel;
    ShapeRefColor: TShape;
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseLeave(Sender: TObject);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormResize(Sender: TObject);
    procedure WMDROPFILES(var Message: TWMDROPFILES); message WM_DROPFILES;
    procedure LoadImage(FileName: TFileName);
    procedure CalculateDistance(Color1, Color2: TColor);
  private
    { Private declarations }
    Bitmap: TBitmap;
    Moving: Boolean;
    LastX, LastY: Integer;
    OffsetX, OffsetY: Integer;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.CalculateDistance(Color1: TColor; Color2: TColor);
var
  RGB1, RGB2: Longint;
begin
  RGB1 := ColorToRGB(Color1);
  RGB2 := ColorTORGB(Color2);
  LabelDistance.Caption := Format('%.1f', [Sqrt(
    Power(GetRValue(RGB1) - GetRValue(RGB2), 2) +
    Power(GetGValue(RGB1) - GetGValue(RGB2), 2) +
    Power(GetBValue(RGB1) - GetBValue(RGB2), 2))]);
end;

procedure TFormMain.WMDROPFILES(var Message: TWMDROPFILES);
var
  Index: Integer;
  Buffer: Array[0..255] of WideChar;
begin
  Index := DragQueryFile(Message.Drop, $FFFFFFFF, nil, 0);
  for Index := 0 to Max(1, Index - 1) do begin
    DragQueryFile(Message.Drop, Index, Buffer, SizeOf(Buffer));
    LoadImage(Buffer);
  end;
  DragFinish(Message.Drop);
end;

procedure TFormMain.ButtonLoadClick(Sender: TObject);
begin
  if FileOpenDialog.Execute then begin
    LoadImage(FileOpenDialog.FileName);
  end;
end;

procedure TFormMain.LoadImage(FileName: TFileName);
begin
    try
      Image.Picture.LoadFromFile(FileName);

      Bitmap.Width := Image.Picture.Width;
      Bitmap.Height := Image.Picture.Height;
      Bitmap.Assign(Image.Picture.Graphic);
      OffsetX := 0;
      OffsetY := 0;
    finally

    end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Image.Picture.RegisterFileFormat('png', 'Portable Network Graphics', TPngImage);
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf24bit;
  DragAcceptFiles(Handle, True);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
end;

procedure TFormMain.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ShowMessage(Format('X: %u, Y: %u', [X, Y]));
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  LabeledEditX.Top := Self.Height - 70;
  LabeledEditY.Top := Self.Height - 70;
  LabeledEditColor.Top := Self.Height - 70;
  ShapeColor.Top := Self.Height - 70;
  ShapeRefColor.Top := Self.Height - 70;
  MemoJSON.Top := Self.Height - 70;
  ButtonLoad.Top := Self.Height - 70;
  LabeledEditRefColor.Top := Self.Height - 70;
  LabelDistance.Top := Self.Height - 65;
  Image.Width := Self.Width - 30;
  Image.Height := Self.Height - 85;
end;

procedure TFormMain.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  RelativeX, RelativeY: Real;
  Color: Integer;
  PNGImage: TPngImage;
begin
  if not Assigned(Image.Picture.Graphic) then Exit;
  if (X > Image.Picture.Width) or (Y > Image.Picture.Height) then Exit;
  Moving := True;
  Image.Cursor := crHandPoint;
  LastX := X;
  LastY := Y;
  X := X - OffsetX;
  Y := Y - OffsetY;
  if Button = mbRight then begin
    Color := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
    ShapeRefColor.Brush.Color := Color;
    LabeledEditRefColor.Text := UpperCase(Format('%.2x%.2x%.2x', [GetRValue(Color),
    GetGValue(Color), GetBValue(Color)]));
  end
  else begin
    RelativeX := X / Image.Picture.Width;
    RelativeY := Y / Image.Picture.Height;
    LabeledEditX.Text := Format('%.4f', [RelativeX]);
    LabeledEditY.Text := Format('%.4f', [RelativeY]);
    MemoJSON.Text := Format('{"X": %.4f, "Y": %.4f}', [RelativeX, RelativeY]);
    Color := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
    LabeledEditColor.Text := UpperCase(Format('%.2x%.2x%.2x', [GetRValue(Color),
    GetGValue(Color), GetBValue(Color)]));
    ShapeColor.Brush.Color := Color;
    PNGImage := (Image.Picture.Graphic as TPngImage);
    PNGImage.Canvas.Brush.Color := clWhite;
    PNGImage.Canvas.Ellipse(LastX - 2, LastY - 2, LastX + 2, LastY + 2);
    Image.Refresh;
  end;
  try
    CalculateDistance(WebColorStrToColor(LabeledEditRefColor.Text),
    WebColorStrToColor(LabeledEditColor.Text));
  except
    LabelDistance.Caption := '-';
  end;
end;

procedure TFormMain.ImageMouseLeave(Sender: TObject);
begin
  Moving := False;
  Image.Cursor := crCross;
end;

procedure TFormMain.ImageMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  PNGImage: TPngImage;
  PrevX, PrevY: Integer;
begin
  if Moving then begin
    Moving := False;
    Image.Cursor := crCross;
    if Assigned(Image.Picture.Graphic) and (LastX <> X) and (LastY <> Y) then begin
      PrevX := OffsetX;
      PrevY := OffsetY;
      OffsetX := OffsetX + (X - LastX);
      OffsetY := OffsetY + (Y - LastY);
      OffsetX := Min(0, Max(Image.Width - Image.Picture.Graphic.Width, OffsetX));
      OffsetY := Min(0, Max(Image.Height - Image.Picture.Graphic.Height, OffsetY));
      if (PrevX = OffsetX) and (PrevY = OffsetY) then Exit;
      PNGImage := (Image.Picture.Graphic as TPngImage);
      PNGImage.Canvas.Draw(OffsetX, OffsetY, Bitmap);
      Image.Refresh;
    end;
  end;
end;

end.
