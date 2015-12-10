unit UnitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, PngImage, Math, Vcl.GraphUtil;

type
  TForm1 = class(TForm)
    Image: TImage;
    FileOpenDialog: TFileOpenDialog;
    LabeledEditX: TLabeledEdit;
    LabeledEditY: TLabeledEdit;
    LabeledEditColor: TLabeledEdit;
    MemoJSON: TMemo;
    ButtonLoad: TButton;
    ShapeColor: TShape;
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ButtonLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageMouseLeave(Sender: TObject);
    procedure ImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
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
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonLoadClick(Sender: TObject);
begin
  if FileOpenDialog.Execute then begin
    Image.Picture.RegisterFileFormat('png', 'Portable Network Graphics', TPngImage);
    try
      Image.Picture.LoadFromFile(FileOpenDialog.FileName);

      Bitmap.Width := Image.Picture.Width;
      Bitmap.Height := Image.Picture.Height;
      Bitmap.Assign(Image.Picture.Graphic);
      OffsetX := 0;
      OffsetY := 0;
      MemoJSON.Lines.Add(Format('Container %uX%u', [Image.Width, Image.Height]));
      MemoJSON.Lines.Add(Format('Image %uX%u', [Bitmap.Width, Bitmap.Height]));
    finally

    end;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Bitmap := TBitmap.Create;
  Bitmap.PixelFormat := pf24bit;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
end;

procedure TForm1.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ShowMessage(Format('X: %u, Y: %u', [X, Y]));
end;

procedure TForm1.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  RelativeX, RelativeY: Real;
  Color: Integer;
begin
  if not Assigned(Image.Picture.Graphic) then Exit;
  Moving := True;
  Image.Cursor := crHandPoint;
  LastX := X;
  LastY := Y;
  X := X - OffsetX;
  Y := Y - OffsetY;
  RelativeX := X / Image.Picture.Width;
  RelativeY := Y / Image.Picture.Height;
  LabeledEditX.Text := Format('%.3f', [RelativeX]);
  LabeledEditY.Text := Format('%.3f', [RelativeY]);
  MemoJSON.Text := Format('[{"X": %.3f, "Y": %.3f}]', [RelativeX, RelativeY]);
  Color := ColorToRGB(Bitmap.Canvas.Pixels[X, Y]);
  LabeledEditColor.Text := Copy(RGBToWebColorStr(Color), 2, 6);
  ShapeColor.Brush.Color := Color;
end;

procedure TForm1.ImageMouseLeave(Sender: TObject);
begin
  Moving := False;
  Image.Cursor := crCross;
end;

procedure TForm1.ImageMouseUp(Sender: TObject; Button: TMouseButton;
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
