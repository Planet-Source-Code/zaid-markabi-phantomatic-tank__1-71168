VERSION 5.00
Begin VB.Form Form2 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Zaid"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   12000
   Icon            =   "Form2.frx":0000
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   10
      Left            =   4200
      Top             =   1560
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   615
      Left            =   3000
      RightToLeft     =   -1  'True
      TabIndex        =   1
      Top             =   6480
      Width           =   5895
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BackColor       =   &H00FFFFFF&
      BackStyle       =   0  'Transparent
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   24
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   615
      Left            =   3000
      TabIndex        =   0
      Top             =   5640
      Width           =   5895
   End
   Begin VB.Image Image1 
      Height          =   9015
      Left            =   0
      Stretch         =   -1  'True
      Top             =   0
      Width           =   12015
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Timer1_Timer()
On Error Resume Next
Timer1.Interval = 30
If Label1.Caption = "" Then
Label1.Caption = "Zaid Markabi Present"
Label2.Caption = "ÒíÏ íÞÏã"
Image1.Picture = LoadPicture("C:\ZaNaZeeN\Mission\Screen.Bmp")
Exit Sub
End If
If Label1.Caption = "Zaid Markabi Present" Then
Label1.Caption = "Phantomatic TanK"
Label2.Caption = "ÇáÏÈÇÈÉ ÇáÔÈÍíÉ"
Exit Sub
End If
If Label1.Caption = "Phantomatic TanK" Then
Label1.Caption = "Now Loading ..."
Label2.Caption = "ÌÇÑí ÇáÊÍãíá ..."
DoEvents
Form1.Show
Timer1.Enabled = False
Unload Me
Exit Sub
End If
End Sub
