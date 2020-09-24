VERSION 5.00
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "Mscomctl.ocx"
Begin VB.Form Form8 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Form8"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   12000
   ControlBox      =   0   'False
   Icon            =   "Form8.frx":0000
   LinkTopic       =   "Form8"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   250
      Left            =   7680
      Top             =   6120
   End
   Begin MSComctlLib.ProgressBar L 
      Height          =   375
      Left            =   240
      TabIndex        =   0
      Top             =   8400
      Width           =   11535
      _ExtentX        =   20346
      _ExtentY        =   661
      _Version        =   393216
      BorderStyle     =   1
      Appearance      =   0
      Scrolling       =   1
   End
End
Attribute VB_Name = "Form8"
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
    
FileCopy App.Path + "\Data Files\Missions\" + STRF + ".LVL", "C:\ZaNaZeeN\" + STRE + "\DATA.MSI"
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
Close intSampleFile
Close intBinaryFile
    Exit Sub
ErrOut:
Close intSampleFile
Close intBinaryFile
Exit Sub
End Sub

Private Sub Form_Unload(Cancel As Integer)
Close #1
End Sub

Private Sub Timer1_Timer()
cmdExtract "Mission", Timer1.Tag
DoEvents
Unload Me
Form2.Show
End Sub

