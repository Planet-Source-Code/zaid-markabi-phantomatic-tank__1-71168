VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Begin VB.Form Form7 
   BackColor       =   &H00000040&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "ÌÇÑí Ýß ÖÛØ ÇáãáÝÇÊ ..."
   ClientHeight    =   2535
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   6600
   ControlBox      =   0   'False
   Icon            =   "Form7.frx":0000
   LinkTopic       =   "Form7"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   RightToLeft     =   -1  'True
   ScaleHeight     =   2535
   ScaleWidth      =   6600
   StartUpPosition =   2  'CenterScreen
   Begin MSComctlLib.ProgressBar L 
      Height          =   375
      Left            =   120
      TabIndex        =   3
      Top             =   2040
      Width           =   6375
      _ExtentX        =   11245
      _ExtentY        =   661
      _Version        =   393216
      BorderStyle     =   1
      Appearance      =   0
      Scrolling       =   1
   End
   Begin VB.Timer Timer1 
      Interval        =   250
      Left            =   480
      Top             =   840
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0FFFF&
      Height          =   495
      Left            =   0
      TabIndex        =   2
      Top             =   1320
      Width           =   6615
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "Now UnZipping"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0FFC0&
      Height          =   495
      Left            =   0
      TabIndex        =   1
      Top             =   600
      Width           =   6615
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      Caption         =   "ÌÇÑí Ýß ÖÛØ ÇáãáÝ"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   18
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00C0FFC0&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   120
      Width           =   6615
   End
End
Attribute VB_Name = "Form7"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Type SECURITY_ATTRIBUTES
 nLength As Long
 lpSecurityDescriptor As Long
 bInheritHandle As Boolean
End Type
Private Declare Function CreateDirectory Lib "kernel32.dll" Alias "CreateDirectoryA" (ByVal lpPathName As String, lpSecurityAttributes As SECURITY_ATTRIBUTES) As Long

Private Type FILEHEADER
    intNumFiles As Integer
    lngFileSize As Long
End Type

Private Type INFOHEADER
    lngFileSize As Long
    lngFileStart As Long
    strFileName As String * 16
End Type

Private Sub cmdExtract(STRE As String, STRF As String)
Dim I As Integer
Dim I2 As Long
Dim intSampleFile As Integer
Dim intBinaryFile As Integer
Dim bytSampleDATA() As Byte
Dim FileHead As FILEHEADER
Dim InfoHead() As INFOHEADER
    
Dim attr As SECURITY_ATTRIBUTES
Dim rval As Long

attr.nLength = Len(attr)
attr.lpSecurityDescriptor = 0
attr.bInheritHandle = 1

rval = CreateDirectory("C:\ZaNaZeeN\", attr)
rval = CreateDirectory("C:\ZaNaZeeN\" + STRE + "\", attr)
    
L.Value = 0
    
FileCopy App.Path + "\Data Files\Gfx\" + STRF + ".IMG", "C:\ZaNaZeeN\" + STRE + "\DATA.MSI"
DoEvents
    
For I2 = 0 To 999999999
    intBinaryFile = FreeFile
    Open "C:\ZaNaZeeN\" + STRE + "\DATA.MSI" For Binary Access Read Lock Write As intBinaryFile
    
    Get intBinaryFile, 1, FileHead

    If LOF(intBinaryFile) <> FileHead.lngFileSize Then
        Exit Sub
    End If

    ReDim InfoHead(FileHead.intNumFiles - 1)

    Get intBinaryFile, , InfoHead
    
L.Max = FileHead.intNumFiles
    
    For I = 0 To 1

        ReDim bytSampleDATA(InfoHead(I).lngFileSize - 1)

        Get intBinaryFile, InfoHead(I).lngFileStart, bytSampleDATA

        intSampleFile = FreeFile
        Open "C:\ZaNaZeeN\" + STRE + "\" & InfoHead(I).strFileName For Binary Access Write Lock Write As intSampleFile
        Put intSampleFile, 1, bytSampleDATA
        Close intSampleFile
    Next
L.Value = L.Value + 1
DoEvents
    Close intBinaryFile
        
Kill "C:\ZaNaZeeN\" + STRE + "\DATA.MSI"
Name "C:\ZaNaZeeN\" + STRE + "\Temp.MSI" As "C:\ZaNaZeeN\" + STRE + "\DATA.MSI"
Next
    Exit Sub
ErrOut:
Exit Sub
End Sub

Private Sub Timer1_Timer()
Label2.Caption = "MakerGame.IMG"
cmdExtract "MG", "MakerGame"
DoEvents
Label2.Caption = "WinGame.IMG"
cmdExtract "WinGame", "WinGame"
DoEvents
Label2.Caption = "CreateBomb.IMG"
cmdExtract "CreateBomb", "CreateBomb"
DoEvents
Label2.Caption = "CreateTank.IMG"
cmdExtract "CreateTank", "CreateTank"
DoEvents
Label2.Caption = "Textures.IMG"
cmdExtract "Textures", "Textures"
DoEvents
Form4.Show
Unload Me
End Sub
