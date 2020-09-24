VERSION 5.00
Begin VB.Form Form5 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Form5"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   12000
   Icon            =   "Form5.frx":0000
   LinkTopic       =   "Form5"
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer3 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   2640
      Top             =   6000
   End
   Begin VB.Timer Timer2 
      Interval        =   1000
      Left            =   6480
      Top             =   7800
   End
   Begin VB.Timer Timer1 
      Interval        =   2000
      Left            =   3960
      Top             =   7920
   End
   Begin VB.TextBox Text1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00008000&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   178
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   420
      Left            =   360
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      RightToLeft     =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   0
      Text            =   "Form5.frx":1CCA
      Top             =   360
      Visible         =   0   'False
      Width           =   11175
   End
   Begin VB.Image Image2 
      Height          =   750
      Left            =   11520
      Picture         =   "Form5.frx":21CB
      Top             =   8040
      Visible         =   0   'False
      Width           =   9630
   End
   Begin VB.Image Image1 
      Height          =   11520
      Left            =   -720
      Picture         =   "Form5.frx":4F51
      Top             =   -480
      Width           =   15360
   End
End
Attribute VB_Name = "Form5"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Image2_Click()
Timer1.Enabled = False
Timer3.Enabled = True
End Sub

Private Sub Timer1_Timer()
Text1.Visible = True
Timer1.Interval = 1
If Text1.Height < 7500 Then
Text1.Height = Text1.Height + 100
Else
Timer1.Enabled = False
End If
End Sub

Private Sub Timer2_Timer()
Image2.Visible = True
Timer2.Interval = 1
If Image2.Left > 1250 Then
Image2.Left = Image2.Left - 400
Else
Image2.Left = 1250
Timer2.Enabled = False
End If
End Sub

Private Sub Timer3_Timer()
If Image2.Left < 12000 Then
Image2.Left = Image2.Left + 400
End If
If Text1.Height > 500 Then
Text1.Height = Text1.Height - 100
Else
Timer2.Enabled = False
Form4.Show
Unload Me
End If
End Sub
