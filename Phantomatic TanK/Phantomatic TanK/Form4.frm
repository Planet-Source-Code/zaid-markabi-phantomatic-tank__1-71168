VERSION 5.00
Object = "{C1A8AF28-1257-101B-8FB0-0020AF039CA3}#1.1#0"; "MCI32.OCX"
Begin VB.Form Form4 
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   Caption         =   "Form4"
   ClientHeight    =   9000
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   12000
   Icon            =   "Form4.frx":0000
   LinkTopic       =   "Form4"
   ScaleHeight     =   9000
   ScaleWidth      =   12000
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer3 
      Interval        =   1
      Left            =   720
      Top             =   120
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   7440
      Top             =   600
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   6120
      Top             =   2640
   End
   Begin MCI.MMControl Snd 
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Visible         =   0   'False
      Width           =   420
      _ExtentX        =   741
      _ExtentY        =   661
      _Version        =   393216
      PrevVisible     =   0   'False
      NextVisible     =   0   'False
      PauseVisible    =   0   'False
      BackVisible     =   0   'False
      StepVisible     =   0   'False
      StopVisible     =   0   'False
      RecordVisible   =   0   'False
      EjectVisible    =   0   'False
      DeviceType      =   ""
      FileName        =   ""
   End
   Begin VB.Image Image6 
      Height          =   750
      Left            =   9240
      Picture         =   "Form4.frx":1CCA
      Top             =   2280
      Width           =   9630
   End
   Begin VB.Image Image4 
      Height          =   750
      Left            =   11595
      Picture         =   "Form4.frx":46C8
      Top             =   6600
      Width           =   9630
   End
   Begin VB.Image Image3 
      Height          =   750
      Left            =   10995
      Picture         =   "Form4.frx":754A
      Top             =   5520
      Width           =   9630
   End
   Begin VB.Image Image2 
      Height          =   750
      Left            =   10395
      Picture         =   "Form4.frx":9D58
      Top             =   4440
      Width           =   9630
   End
   Begin VB.Image Image1 
      Height          =   750
      Left            =   9795
      Picture         =   "Form4.frx":C690
      Top             =   3360
      Width           =   9630
   End
   Begin VB.Image Image5 
      Height          =   11520
      Left            =   -240
      Picture         =   "Form4.frx":EBCE
      Top             =   -360
      Width           =   15360
   End
End
Attribute VB_Name = "Form4"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Form_Unload(Cancel As Integer)
Snd.Command = "Close"
End Sub

Private Sub Image1_Click()
Timer2.Tag = "S"
Timer2.Enabled = True
End Sub

Private Sub Image2_Click()
Timer2.Tag = "I"
Timer2.Enabled = True
End Sub

Private Sub Image3_Click()
Timer2.Tag = "Q"
Timer2.Enabled = True
End Sub

Private Sub Image4_Click()
Timer2.Tag = "M"
Timer2.Enabled = True
End Sub

Private Sub Image6_Click()
Timer2.Tag = "U"
Timer2.Enabled = True
End Sub

Private Sub Timer1_Timer()
If Image6.Left > 1300 Then
Image6.Left = Image6.Left - 400
Else
Image6.Left = 1300
End If
If Image1.Left > 1300 Then
Image1.Left = Image1.Left - 400
Else
Image1.Left = 1300
End If
If Image2.Left > 1330 Then
Image2.Left = Image2.Left - 400
Else
Image2.Left = 1300
End If
If Image3.Left > 1330 Then
Image3.Left = Image3.Left - 400
Else
Image3.Left = 1300
End If
If Image4.Left > 1330 Then
Image4.Left = Image4.Left - 400
Else
Image4.Left = 1300
Timer1.Enabled = False
End If
End Sub

Private Sub Timer2_Timer()
If Image6.Left < 17000 Then
Image6.Left = Image6.Left + 500
End If
If Image1.Left < 17000 Then
Image1.Left = Image1.Left + 450
End If
If Image2.Left < 17000 Then
Image2.Left = Image2.Left + 400
End If
If Image3.Left < 17000 Then
Image3.Left = Image3.Left + 350
End If
If Image4.Left < 17000 Then
Image4.Left = Image4.Left + 300
Else
Timer2.Enabled = False
If Timer2.Tag = "Q" Then
End
End If
If Timer2.Tag = "S" Then
Form3.Show
Unload Me
End If
If Timer2.Tag = "I" Then
Form5.Show
Unload Me
End If
If Timer2.Tag = "U" Then
Form7.Show
Unload Me
End If
If Timer2.Tag = "M" Then
Form6.Show
Unload Me
End If
End If
End Sub

Private Sub Timer3_Timer()
Timer3.Enabled = False
Snd.Command = "Stop"
Snd.FileName = App.Path + "\Data Files\Music.wav"
Snd.Command = "Open"
Snd.Command = "Prev"
Snd.Command = "Play"
End Sub
